struct Int32Bool
    bits::Int32
end

Int32Bool(i::Int32, b::Bool) = Int32Bool(Int32(2b - 1) * abs(i))

int(x::Int32Bool) = abs(x.bits)
bool(x::Int32Bool) = x.bits > 0

setbool(x::Int32Bool, b::Bool) = Int32Bool(int(x), b)
setint(x::Int32Bool, i::Int32) = Int32Bool(i, bool(x))

struct CRQSlot{S}
    index_safe::Int32Bool
    storage::S  # sizeof(S) ≤ 4
end

@inline CRQSlot(; index::Integer, safe::Bool, storage) =
    CRQSlot(Int32Bool(Int32(index), safe), storage)

@inline function Base.getproperty(slot::CRQSlot, name::Symbol)
    if name === :index
        return int(getfield(slot, :index_safe))
    elseif name === :safe
        return bool(getfield(slot, :index_safe))
    end
    return getfield(slot, name)
end

"""
    IndirectConcurrentRingQueueNode{T}

Concurrent Ring Queue (CRQ) node which stores 64 bit slot in the `.ring`.  Each
slot contains a 32 bit value that locates the actual value in the thread-local
`.buffers`; hence _indirect_.

This approach wastes memory but it is (more or less) implementable without
atomics support of boxed element in array.

TODO: Create a 128 bit slot "direct" version that directly points to a boxed
Julia value.
"""
mutable struct IndirectConcurrentRingQueueNode{T}
    @atomic head::Int32
    _head_pad::PadAfter32
    @atomic tail_closed::Int32
    _tail_pad::PadAfter32
    @atomic next::Union{Nothing,IndirectConcurrentRingQueueNode{T}}
    ring::Vector{CRQSlot{UInt32}}
    length::Int
    buffers::Vector{Vector{T}}
end
# TODO: pad

function IndirectConcurrentRingQueueNode{T}(len::Int) where {T}
    buffers = [Vector{T}(undef, len) for _ in 1:Threads.nthreads()]
    ring = [CRQSlot(Int32Bool(Int32(i), true), UInt32(0)) for i in 1:len]
    head = Int32(1)
    tail_closed = Int32(1)
    return IndirectConcurrentRingQueueNode(
        head,
        PadAfter32(),
        tail_closed,
        PadAfter32(),
        nothing,
        ring,
        len,
        buffers,
    )
end

Base.eltype(::Type{<:IndirectConcurrentRingQueueNode{T}}) where {T} = T

const CRQ_STARVATION = 128  # ???

function _close(crq::IndirectConcurrentRingQueueNode, old::Int32)
    while old > 0
        new = typemin(Int32) + old
        old, success = @atomicreplace crq.tail_closed old => new
        success && return
    end
end

function tail_index(crq::IndirectConcurrentRingQueueNode, t = @atomic crq.tail_closed)
    if t < 0
        t = t - typemin(Int32)
    end
    return t
end

isclosed(crq::IndirectConcurrentRingQueueNode, t = @atomic crq.tail_closed) = t ≤ 0

function trypush!(crq::IndirectConcurrentRingQueueNode, x)
    x = convert(eltype(crq), x)

    # TODO: create a Int64 version that that does not require this?
    let t = @atomic crq.tail_closed
        if isclosed(crq, t)
            return false  # closed
        elseif t ≥ typemax(Int32) - Threads.nthreads()
            _close(crq, t)
            return false  # closed
        end
    end

    starvation_ctr = 0
    while true
        t = (@atomic crq.tail_closed += true) - true
        if isclosed(crq, t)
            return false  # closed
        elseif t ≥ typemax(Int32) - Threads.nthreads()
            # Note: this condition is relying on that there are only at most
            # `nthreads()` instances on `trypush!` calls at once; i.e., no yield
            # point.
            _close(crq, t)
            return false  # closed
        end
        # TODO: use shift like DLCRQ
        itemindex = mod1(t, crq.length)
        slotptr = pointer(crq.ring, itemindex)
        slot = UnsafeAtomics.load(slotptr)::CRQSlot{UInt32}
        (; index, safe, storage) = slot
        if iszero(storage)
            if (index ≤ t) && (safe || (@atomic crq.head) ≤ t)
                tid = Threads.threadid()
                buffer = crq.buffers[tid]
                buffer[itemindex] = x
                storage = UInt32(tid)
                # storage = _embed(UInt32, x)
                @assert !iszero(storage)
                newslot = CRQSlot(; safe = true, index = t, storage)
                old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                if old == slot
                    return true
                end
            end
        end
        starvation_ctr += 1
        if (t - (@atomic crq.head) ≥ crq.length) || starvation_ctr > CRQ_STARVATION
            _close(crq, t)
            return false  # closed
        end
        GC.safepoint()  # must not yield
    end
end

function ConcurrentCollections.trypopfirst!(crq::IndirectConcurrentRingQueueNode)
    while true
        h = (@atomic crq.head += true) - true
        itemindex = mod1(h, crq.length)  # TODO: shift
        slotptr = pointer(crq.ring, itemindex)
        while true
            slot = UnsafeAtomics.load(slotptr)::CRQSlot{UInt32}
            (; index, safe, storage) = slot
            if !iszero(storage)
                if index == h
                    threadindex = storage
                    x = crq.buffers[threadindex][itemindex]  # [^crq.buffers]
                    # x = _extract(eltype(crq), storage)

                    newslot = CRQSlot(; safe, index = h + crq.length, storage = UInt32(0))
                    old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                    if old === slot
                        return Some{eltype(crq)}(x)
                    end
                else
                    newslot = CRQSlot(; safe = false, index, storage)
                    old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                    if old == slot
                        break
                    end
                end
            else  # empty slot
                # `max(h, index)` for dealing with tasks stalled after FAI [^maxh]
                newslot = CRQSlot(; safe, index = max(h, index) + crq.length, storage)
                old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                if old == slot
                    break
                end
            end
            GC.safepoint()
        end
        GC.safepoint()

        # Failed to dequeue. Bail out if empty.
        if tail_index(crq) ≤ h + 1
            fixstate!(crq)
            return nothing
        end
    end
end
# [^crq.buffers]: Above read `x = crq.buffers[threadindex][itemindex]` (from
# `itemindex`) is fine without atomics. At this point, the load from the
# corresponding slot (`crq.ring[itemindex]`) has established the happens-before
# edge to the corresponding enqueue.  Since it has been confirmed that the
# storage is non-empty and `h == index`, there cannot be any more concurrent
# writes by the "current" or "past" enqueuers with `tail ≤ h`. The "future"
# enqueuers with `tail > h` will not touch the buffer until the slot is emptied
# by the CAS after this load. Since this dequeuer obtained the exclusive access
# to the index `h`, there's no other dequeuers trying the same CAS.
#
# [^maxh]: `max(h, index)` was not mentioned in Morrison and Afek (2013) (or its
# revised version) so it's not clear if this is needed.  However, it is possible
# that multiple enqueuers and/or dequeuers that would have incremented the
# `slot.index` is suspended just after the FAI.  If so, `slot.index` can be a
# round (or even multiple rounds) behind the current `crq.head`.  So, it seems
# like we need to prevent the enqueuing to this index (hence missing the item)
# by moving this to the next round.
#
# TODO: Is it OK to skip updating the slot if `index > h`?

function fixstate!(crq::IndirectConcurrentRingQueueNode)
    while true
        h = @atomic crq.head
        tail_closed = @atomic crq.tail_closed
        t = tail_index(crq, tail_closed)
        h ≤ t && return
        isclosed(crq, tail_closed) && return  # closed and empty
        _, success = @atomicreplace crq.tail_closed tail_closed => h
        success && return
    end
end

mutable struct LinkedConcurrentRingQueue{T}
    @atomic head::IndirectConcurrentRingQueueNode{T}
    @atomic tail::IndirectConcurrentRingQueueNode{T}
end

# lcrq_default_ringsize() = 2^17
lcrq_default_ringsize() = 2^10
# lcrq_default_ringsize() = 2^5

LinkedConcurrentRingQueue(ringsize::Integer = lcrq_default_ringsize()) =
    LinkedConcurrentRingQueue{Any}(ringsize)
function LinkedConcurrentRingQueue{T}(ringsize::Integer = lcrq_default_ringsize()) where {T}
    node = IndirectConcurrentRingQueueNode{T}(ringsize)
    return LinkedConcurrentRingQueue(node, node)
end

Base.eltype(::Type{<:LinkedConcurrentRingQueue{T}}) where {T} = T

const NEW_CRQ = Threads.Atomic{Int}(0)

function Base.push!(lcrq::LinkedConcurrentRingQueue{T}, x) where {T}
    x = convert(T, x)
    while true
        crq = @atomic lcrq.tail
        next = @atomic crq.next
        if next !== nothing
            @atomicreplace lcrq.tail crq => next
            continue
        end

        trypush!(crq, x) && return lcrq

        Threads.atomic_add!(NEW_CRQ, 1)
        newcrq = IndirectConcurrentRingQueueNode{T}(length(crq.ring))
        ok = trypush!(newcrq, x)
        @assert ok
        _, success = @atomicreplace crq.next nothing => newcrq
        if success
            @atomicreplace lcrq.tail crq => newcrq
            return lcrq
        end
    end
end

function ConcurrentCollections.trypopfirst!(lcrq::LinkedConcurrentRingQueue)
    while true
        crq = @atomic lcrq.head
        x = trypopfirst!(crq)
        x === nothing || return x
        next = @atomic crq.next
        next === nothing && return nothing

        x = trypopfirst!(crq)
        x === nothing || return x
        @atomicreplace lcrq.head crq => next
    end
end

function Base.show(io::IO, ::MIME"text/plain", lcrq::LinkedConcurrentRingQueue)
    nitems = 0
    ncrqs = 0
    crq = @atomic lcrq.head
    while crq !== nothing
        h = @atomic crq.head
        t = tail_index(crq)
        nitems += max(0, t - h)
        ncrqs += 1
        crq = @atomic crq.next
    end
    print(io, "LCRQ: $nitems item(s) in $ncrqs node(s)")
end

function Base.show(io::IO, ::MIME"text/plain", crq::IndirectConcurrentRingQueueNode)
    h = @atomic crq.head
    t = tail_index(crq)
    nitems = max(0, t - h)
    status = isclosed(crq) ? "closed" : "open"
    print(io, "CRQ: $nitems item(s) (status: $status, head: $h, tail: $t)")
end

function Base.NamedTuple(slot::CRQSlot)
    (; index, safe, storage) = slot
    return (; index, safe, storage)
end

function Base.show(io::IO, ::MIME"text/plain", slot::CRQSlot)
    if get(io, :typeinfo, Any) !== typeof(slot)
        show(io, MIME"text/plain"(), typeof(slot))
    end
    show(io, MIME"text/plain"(), NamedTuple(slot))
end
