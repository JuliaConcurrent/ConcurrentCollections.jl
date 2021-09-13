@enum Polarity::Bool DATA ANTIDATA

@enum WaiterState begin
    WAITER_INIT
    WAITER_WAITING
    WAITER_NOTIFYING
    WAITER_SATISFIED
    WAITER_FINISHED
    WAITER_TRASHED
end

mutable struct Waiter{T}
    # _tag_pad::PadAfter32
    task::Union{Task,Nothing}
    value::Union{Nothing,T}
    @atomic state::WaiterState
    # _state_pad::PadAfter64  # TODO: smaller pad
end

Waiter{T}(task::Task = current_task()) where {T} = Waiter{T}(
    # PadAfter32(),
    task,
    nothing,
    WAITER_INIT,
    # PadAfter64(),
)

function reinit!(w::Waiter)
    if assertion_enabled()
        state = @atomic w.state
        @assert state in (WAITER_TRASHED, WAITER_FINISHED)
        @assert w.task === nothing
        @assert w.value === nothing
    end
    w.task = current_task()
    w.value === nothing
    @atomic :monotonic w.state = WAITER_INIT
    return w
end

function Base.fetch(w::Waiter{T}; nspins::Integer = 0) where {T}
    for _ in 1:nspins
        if (@atomic w.state) == WAITER_SATISFIED
            @goto satisifed
        end
        GC.safepoint()
    end

    _, success = @atomicreplace w.state WAITER_INIT => WAITER_WAITING
    success && wait()

    @label satisifed
    let old = @atomic w.state
        @assert old in (WAITER_SATISFIED, WAITER_NOTIFYING)
        old, success = @atomicreplace w.state old => WAITER_FINISHED
        if !success
            error("more than two duals detected: old = $old")
        end
    end

    x = w.value
    # Help GC:
    w.task = nothing
    w.value = nothing
    return x::T
end

function Base.put!(w::Waiter{T}, x::T) where {T}
    # Before the CAS, `put!` owns the field `.value`:
    w.value = x

    old = WAITER_WAITING  # initial guess
    while true
        if old == WAITER_WAITING
            new = WAITER_NOTIFYING
        else
            @assert old == WAITER_INIT "required old == WAITER_INIT but old = $old"
            new = WAITER_SATISFIED
        end
        old, success = @atomicreplace w.state old => new
        if success
            if old == WAITER_WAITING
                schedule(w.task::Task)
            end
            return
        end
    end
end

function trash!(w::Waiter)
    if assertion_enabled()
        old, success = @atomicreplace w.state WAITER_INIT => WAITER_TRASHED
        if !success
            error("unshared waiter mutaed: state: $old")
        end
    else
        @atomic :monotonic w.state = WAITER_TRASHED
    end
    w.task = nothing
end

struct MPCRQSlot{S}
    index_safe_polarity::UInt32
    storage::S  # sizeof(S) ≤ 4
end

const MPCRQSLOT_MAX_INDEX = Int32(typemax(UInt32) >> 2)

@inline function MPCRQSlot(; index::Integer, safe::Bool, polarity::Polarity, storage)
    # index ≤ typemax(UInt32) >> 2 || error("index $index too large")
    index_safe_polarity = (index % UInt32) << 2
    index_safe_polarity |= (safe % UInt32) << 1
    index_safe_polarity |= UInt32(polarity)
    return MPCRQSlot(index_safe_polarity, storage)
end

@inline MPCRQSlot{T}(;
    index::Integer,
    safe::Bool,
    polarity::Polarity,
    storage::T = zero(T),
) where {T<:Number} = MPCRQSlot(; index, safe, polarity, storage)

@inline function Base.getproperty(slot::MPCRQSlot, name::Symbol)
    index_safe_polarity = getfield(slot, :index_safe_polarity)
    if name === :index
        return (index_safe_polarity >> 2) % Int32
    elseif name === :safe
        return ((index_safe_polarity >> 1) & 0x01) % Bool
    elseif name === :polarity
        return Polarity((index_safe_polarity & 0x01) % Bool)
    end
    return getfield(slot, name)
end

@enum MPCRQResult MPCRQ_CLOSED MPCRQ_ENQUEUED

# See: IndirectConcurrentRingQueueNode
mutable struct IndirectMultiPolarityConcurrentRingQueueNode{
    T,
    Data<:Union{Nothing,Vector{Vector{T}}},
}
    _tag_pad::PadAfter32
    @atomic data_idx::Int32
    _data_idx_pad::PadAfter32
    @atomic antidata_idx::Int32
    _antidata_idx_pad::PadAfter32
    @atomic closed_info::Int32
    _closed_info_pad::PadAfter32
    @atomic next::Union{Nothing,IndirectMultiPolarityConcurrentRingQueueNode{T}}
    _next_pad::PadAfter64
    ring::typeof(cacheline_padded_vector(MPCRQSlot{UInt32}, 1))
    log2len::Int
    # buffers::Vector{Vector{Some{T},Waiter{T}}}
    data::Data
    waiters::Vector{Vector{Waiter{T}}}
end

inlinedata(::IndirectMultiPolarityConcurrentRingQueueNode{<:Any,Data}) where {Data} =
    Data === Nothing

# TODO: more types
const SmallSigned = Union{Int8,Int16}
const SmallUnsigned = Union{Bool,UInt8,UInt16}
const NullableInline32 = Union{SmallSigned,SmallUnsigned}

function IndirectMultiPolarityConcurrentRingQueueNode{T}(log2len::Int) where {T}
    len = 2^log2len
    if T <: NullableInline32 && isconcretetype(T)
        data = nothing
    else
        data = [Vector{T}(undef, len) for _ in 1:Threads.nthreads()]
    end
    waiters = [Vector{Waiter{T}}(undef, len) for _ in 1:Threads.nthreads()]
    ring = init_ring!(cacheline_padded_vector(MPCRQSlot{UInt32}, len))
    data_idx = Int32(1)
    antidata_idx = Int32(1)
    closed_info = Int32(1)
    return IndirectMultiPolarityConcurrentRingQueueNode(
        PadAfter32(),
        data_idx,
        PadAfter32(),
        antidata_idx,
        PadAfter32(),
        closed_info,
        PadAfter32(),
        nothing,
        PadAfter64(),
        ring,
        log2len,
        data,
        waiters,
    )
end
# It looks like some non-neglegible amount of time is spent in
# `Vector{Waiter{T}}(undef, _)`, presumably zeroing out the elements.  A tricky
# part of this is detecting that the CRQ is no longer used. Since the CRQ may be
# closed just after the FAI and before the CAS on the slot is complete, (naively
# thinking) linear scan the `.ring` array is required for checking that the CRQ
# is not longer used (without adding other mechanisms like ref conting). So,
# ATM, only the newcrq that lost CAS is cached (which still helps the
# performance).

function init_ring!(ring)
    for index in eachindex(ring)
        ring[index] = MPCRQSlot{UInt32}(; index, safe = true, polarity = DATA)
    end
    return ring
end

function Base.empty!(crq::IndirectMultiPolarityConcurrentRingQueueNode)
    @atomic crq.data_idx = Int32(1)
    @atomic crq.antidata_idx = Int32(1)
    @atomic crq.closed_info = Int32(1)
    @atomic crq.next = nothing
    init_ring!(crq.ring)
    # for waiters in crq.waiters
    #     n = length(waiters)
    #     resize!(empty!(waiters), n)
    # end
    return crq
end

Base.similar(crq::IndirectMultiPolarityConcurrentRingQueueNode{T}) where {T} =
    IndirectMultiPolarityConcurrentRingQueueNode{T}(crq.log2len)

Base.eltype(::Type{<:IndirectMultiPolarityConcurrentRingQueueNode{T}}) where {T} = T

#=
function Base.isempty(crq::IndirectMultiPolarityConcurrentRingQueueNode)
    closed_info = @atomic crq.closed_info
    closed_info_idx = extract_index(closed_info)
    isclosed = closed_info < 0
    isclosed || return false
    data_idx = extract_index(@atomic crq.data_idx)
    data_idx ≥ closed_info_idx || return false
    antidata_idx = extract_index(@atomic crq.antidata_idx)
    antidata_idx ≥ closed_info_idx || return false
    # TODO: relaxed loads plus fence
    max_idx = max(data_idx, antidata_idx)
    for slot in crq.ring
        (; index, safe) = slot
        safe && index ≤ max_idx && return false
    end
    return true
end
=#

@inline function nonnegative(x)
    if assertion_enabled()
        @assert x ≥ 0
    end
    return x
end

@inline function negative(x)
    if assertion_enabled()
        @assert x < 0
    end
    return x
end

@inline set_closing(i) = negative(ifelse(i < zero(i), i, typemin(i) + i))
@inline extract_index(i) = nonnegative(ifelse(i ≥ zero(i), i, i - typemin(i)))

@inline _mod1(i::Int32, crq) = _mod1log2(i, crq.log2len)
@inline _mod1log2(i::Int32, log2x) =
    (UInt32(i - 1) & (typemax(UInt32) >> (32 - log2x))) % Int32 + Int32(1)

_embed(::Type{UInt32}, x::SmallSigned) = _embed(UInt32, unsigned(x))
_embed(::Type{UInt32}, x::SmallUnsigned) = (x % UInt32) + UInt32(1)
_extract(::Type{T}, x::UInt32) where {T<:SmallSigned} = signed(_extract(unsigned(T), x))::T
_extract(::Type{T}, x::UInt32) where {T<:SmallUnsigned} = (x - UInt32(1)) % T

"""
    denqueue!(crq::IMPCRQ{T}, x::T) -> MPCRQ_CLOSED or MPCRQ_ENQUEUED or y::Waiter
    denqueue!(crq::IMPCRQ{T}, x::Waiter{T}) -> MPCRQ_CLOSED or MPCRQ_ENQUEUED or y::Some{T}
    where MPCRQ = IndirectMultiPolarityConcurrentRingQueueNode
"""
function denqueue!(
    crq::IndirectMultiPolarityConcurrentRingQueueNode{T},
    x::Union{T,Waiter{T}},
) where {T}
    nthreads = Int32(Threads.nthreads())

    local polarity::Polarity = x isa Waiter{T} ? ANTIDATA : DATA
    R = 2^crq.log2len
    starvation_counter = 0
    while true
        if x isa Waiter{T}
            # p, _ = modifyfield!(crq, :antidata_idx, +, Int32(1), :sequentially_consistent)
            p = atomic_modifyfield!(crq, Val(:antidata_idx), +, true)::Int32
        else
            # p, _ = modifyfield!(crq, :data_idx, +, Int32(1), :sequentially_consistent)
            p = atomic_modifyfield!(crq, Val(:data_idx), +, true)::Int32
        end
        p_closing = p < 0
        p_idx = extract_index(p)
        if p_closing
            if discovered_closing(crq) ≤ p_idx
                return MPCRQ_CLOSED
            end
        elseif p_idx ≥ MPCRQSLOT_MAX_INDEX - nthreads
            discovered_closing(crq)
            error("NOT WELL TESTED")
            return MPCRQ_CLOSED
        end
        itemindex = _mod1(p_idx, crq)
        slotptr = pointer(crq.ring, itemindex)
        # ncas = 0
        while true
            # ncas += 1
            # if ncas > 10000
            #     let msg = "too many retires tid=$(Threads.threadid())"
            #         print(stderr, "$msg\n")
            #         error(msg)
            #     end
            # end
            # ccall(:jl_breakpoint, Cvoid, (Any,), slotptr)
            slot = UnsafeAtomics.load(slotptr)::MPCRQSlot{UInt32}
            (; index, safe, storage) = slot
            if !iszero(storage)
                if index == p_idx && slot.polarity !=′ polarity
                    threadindex = storage % Int
                    if x isa Waiter{T}
                        if inlinedata(crq)
                            y = _extract(T, storage)
                        else
                            y = crq.data[threadindex][itemindex]
                        end
                    else
                        y = crq.waiters[threadindex][itemindex]
                    end

                    newslot = MPCRQSlot{UInt32}(; safe, index = p_idx + R, polarity)
                    old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                    if old == slot
                        if x isa Waiter{T}
                            return Some(y)
                        else
                            return y
                        end
                    end
                else
                    newslot = MPCRQSlot(; safe = false, index, slot.polarity, storage)
                    old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                    if old == slot
                        break
                    end
                end
                # If no `break` or `return`, reload the slot.
            else
                # Getting here means slot is empty.  Try to enqueue `x`.
                if x isa Waiter{T}
                    q = @atomic crq.data_idx
                else
                    q = @atomic crq.antidata_idx
                end
                q_idx = extract_index(q)
                if safe  # || q_idx ≤ p_idx
                    # if x isa Waiter{T}
                    #     GC.safepoint()
                    #     continue  # spin
                    # end
                    threadindex = Threads.threadid()
                    storage = threadindex % UInt32
                    if x isa Waiter{T}
                        crq.waiters[threadindex][itemindex] = x
                    else
                        if inlinedata(crq)
                            storage = _embed(UInt32, x)
                        else
                            crq.data[threadindex][itemindex] = x
                        end
                    end

                    @assert !iszero(storage)
                    newslot = MPCRQSlot(; safe = true, index = p_idx, polarity, storage)
                    old = UnsafeAtomics.cas!(slotptr, slot, newslot)
                    if old == slot
                        # if slot.index > p_idx
                        #     global CRQ = crq
                        # end
                        return MPCRQ_ENQUEUED
                        # else: reload the slot
                    end
                else
                    break
                end
            end
            GC.safepoint()  # cannot use yield
        end  # of the CAS loop on the slot
        # ccall(:jl_breakpoint, Cvoid, (Any,), nothing)

        # Reaching here means that a slot is successfully marked as unsafe.
        # Check if the CRQ is full and detect starvation.

        # Load the opposite index:
        if x isa Waiter{T}
            q = @atomic crq.data_idx
        else
            q = @atomic crq.antidata_idx
        end
        q_idx = extract_index(q)

        starvation_counter += 1
        if (p_idx - q_idx ≥ R) || starvation_counter > CRQ_STARVATION
            if x isa Waiter{T}
                set_closing!(crq, Val(:antidata_idx))
            else
                set_closing!(crq, Val(:data_idx))
            end
            if discovered_closing(crq) ≤ p_idx
                return MPCRQ_CLOSED
            end
        end
        GC.safepoint()  # cannot use yield
    end
end

# TODO: use bit-wise or?
function set_closing!(
    crq::IndirectMultiPolarityConcurrentRingQueueNode,
    ::Val{field},
) where {field}
    idx = getfield(crq, field, :sequentially_consistent)
    idx < 0 && return idx
    while true
        new = set_closing(idx)
        idx, success = replacefield!(
            crq,
            field,
            idx,
            new,
            :sequentially_consistent,
            :sequentially_consistent,
        )
        success && return new
        idx < 0 && return idx
    end
end

@noinline function discovered_closing(crq::IndirectMultiPolarityConcurrentRingQueueNode)
    closed_info = @atomic crq.closed_info
    closed_info_idx = extract_index(closed_info)
    isclosed = closed_info < 0
    isclosed && return closed_info_idx

    antidata_idx = set_closing!(crq, Val(:antidata_idx))
    data_idx = set_closing!(crq, Val(:data_idx))

    # @assert crq.antidata_idx < 0
    # @assert crq.data_idx < 0

    new = set_closing(max(extract_index(data_idx), extract_index(antidata_idx)))
    # @assert new < 0
    closed_info, success = @atomicreplace crq.closed_info closed_info => new
    # @assert Implementations.isclosed(crq)
    if success
        return extract_index(new)
    else
        return extract_index(closed_info)
    end
end

mutable struct DualLinkedConcurrentRingQueue{
    T,
    CRQ<:IndirectMultiPolarityConcurrentRingQueueNode{T},
}
    # _tag_pad::PadAfter32
    @atomic data::CRQ
    _data_pad::PadAfter64
    @atomic antidata::CRQ
    _antidata_pad::PadAfter64
    crqcache::ThreadLocalCache{CRQ}
    waitercache::ThreadLocalCache{Waiter{T}}
end

DualLinkedConcurrentRingQueue(; kwargs...) = DualLinkedConcurrentRingQueue{Any}(; kwargs...)
function DualLinkedConcurrentRingQueue{T}(; log2ringsize = 11) where {T}
    node = IndirectMultiPolarityConcurrentRingQueueNode{T}(log2ringsize)
    return DualLinkedConcurrentRingQueue(
        # PadAfter32(),
        node,
        PadAfter64(),
        node,
        PadAfter64(),
        ThreadLocalCache{typeof(node)}(),
        ThreadLocalCache{Waiter{T}}(),
    )::DualLinkedConcurrentRingQueue{T}
end

Base.eltype(::Type{<:DualLinkedConcurrentRingQueue{T}}) where {T} = T

function Base.push!(lcrq::DualLinkedConcurrentRingQueue{T}, x) where {T}
    x = convert(eltype(lcrq), x)
    y = denqueue!(lcrq, x)
    if y isa Waiter{T}
        put!(y, x)
    else
        @assert y === MPCRQ_ENQUEUED
    end
    return lcrq
end

function Base.popfirst!(lcrq::DualLinkedConcurrentRingQueue{T}) where {T}
    w = let cached = maybepop!(lcrq.waitercache)
        if cached === nothing
            Waiter{eltype(lcrq)}()
        else
            reinit!(something(cached))
        end
    end
    y = denqueue!(lcrq, w)
    if y === MPCRQ_ENQUEUED
        x = fetch(w)
    else
        trash!(w)
        x = something(y::Some{T})
    end
    trypush!(lcrq.waitercache, w)
    return x::T
end

# const CLOSED_X = Vector{Int}[Int[] for _ in 1:Threads.nthreads()]

"""
    denqueue!(lcrq::DualLCRQ{T}, x::T) -> MPCRQ_ENQUEUED or y::Waiter
    denqueue!(lcrq::DualLCRQ{T}, x::Waiter{T}) -> MPCRQ_ENQUEUED or y::Some{T}
    where DualLCRQ = DualLinkedConcurrentRingQueue
"""
function denqueue!(lcrq::DualLinkedConcurrentRingQueue{T}, x::Union{T,Waiter{T}}) where {T}
    while true
        crq = if x isa Waiter{T}
            @atomic lcrq.antidata
        else
            @atomic lcrq.data
        end
        y = denqueue!(crq, x)
        y === MPCRQ_CLOSED || return y

        # crq is closed (but may NOT be empty)
        next = @atomic crq.next
        if next !== nothing
            if x isa Waiter{T}
                @atomicreplace lcrq.antidata crq => next
            else
                @atomicreplace lcrq.data crq => next
            end
            # success && isempty(crq) && cache_crq!(lcrq, crq)
        else
            newcrq = make_newcrq!(lcrq, crq)
            # newcrq = similar(crq)
            y = denqueue!(newcrq, x)
            @assert y === MPCRQ_ENQUEUED
            _, success = @atomicreplace crq.next nothing => newcrq
            if success
                if x isa Waiter{T}
                    @atomicreplace lcrq.antidata crq => newcrq
                    return y
                else
                    @atomicreplace lcrq.data crq => newcrq
                    return y
                end
            else
                cache_crq!(lcrq, newcrq)
            end
        end
        GC.safepoint()  # yield?
    end
end

function make_newcrq!(lcrq::DualLinkedConcurrentRingQueue, crq)
    oldcrq = maybepop!(lcrq.crqcache)
    if oldcrq === nothing
        return similar(crq)
    else
        return oldcrq
    end
end

function cache_crq!(lcrq::DualLinkedConcurrentRingQueue, crq)
    trypush!(lcrq.crqcache, empty!(crq))
    return
end

function nopop_foreach(f, crq::IndirectMultiPolarityConcurrentRingQueueNode)
    i = extract_index(@atomic crq.antidata_idx)
    while true
        slot = crq.ring[_mod1(i, crq)]
        (; index, polarity, storage) = slot
        if index == i && polarity ==′ DATA
            (; threadindex, itemindex) = storage
            x = crq.data[threadindex][itemindex]
            f(x)
            i += one(i)
        else
            break
        end
    end
end

function nopop_foreach(f, lcrq::DualLinkedConcurrentRingQueue)
    crq = @atomic lcrq.antidata
    while true
        nopop_foreach(f, crq)
        crq = @atomic crq.next
        crq === nothing && return
    end
end

datalength(crq::IndirectMultiPolarityConcurrentRingQueueNode) =
    extract_index(@atomic crq.data_idx) - extract_index(@atomic crq.antidata_idx)

isclosed(crq::IndirectMultiPolarityConcurrentRingQueueNode) = (@atomic crq.closed_info) < 0

function Base.show(io::IO, ::MIME"text/plain", lcrq::DualLinkedConcurrentRingQueue)
    data = @atomic lcrq.data
    antidata = @atomic lcrq.antidata
    if (@atomic antidata.next) === nothing && datalength(antidata) < 0
        node = data  # head of antidata
        polarity = ANTIDATA
    else
        node = antidata  # head of data
        polarity = DATA
    end
    n = 0
    ncrqs = 0
    while true
        n += datalength(node)
        ncrqs += 1
        node = @atomic node.next
        node === nothing && break
    end
    if polarity == DATA
        items = "$n data item" * (n > 1 ? "s" : "")
    else
        items = "$(-n) waiter" * (n < -1 ? "s" : "")
    end
    crqs = "$ncrqs node" * (ncrqs > 1 ? "s" : "")
    print(io, "Dual LCRQ: $items in $crqs")
end

function Base.show(
    io::IO,
    ::MIME"text/plain",
    crq::IndirectMultiPolarityConcurrentRingQueueNode,
)
    data_idx = extract_index(@atomic crq.data_idx)
    antidata_idx = extract_index(@atomic crq.antidata_idx)
    n = data_idx - antidata_idx
    if n == 0
        items = "empty"
    elseif n > 0
        items = "$n data item" * (n > 1 ? "s" : "")
    else
        items = "$(-n) waiter" * (n < -1 ? "s" : "")
    end
    status = isclosed(crq) ? "closed" : "open"
    print(
        io,
        "MP-CRQ: $items (status: $status, data_idx: $data_idx, antidata_idx: $antidata_idx)",
    )
end

function Base.NamedTuple(slot::MPCRQSlot)
    (; index, safe, polarity, storage) = slot
    return (; index, safe, polarity, storage)
end

function Base.show(io::IO, ::MIME"text/plain", slot::MPCRQSlot)
    if get(io, :typeinfo, Any) !== typeof(slot)
        show(io, MIME"text/plain"(), typeof(slot))
    end

    (; index, safe, polarity, storage) = slot
    nt = (;
        index,
        safe,
        polarity = Text(string(polarity)),
        storage = Text(
            sprint(
                show,
                MIME"text/plain"(),
                storage;
                context = IOContext(io, :typeinfo => typeof(storage)),
            ),
        ),
    )
    show(io, MIME"text/plain"(), nt)
end
