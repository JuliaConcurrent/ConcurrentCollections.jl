@enum LPDKeyState::UInt8 LPD_EMPTY LPD_DELETED LPD_MOVED_EMPTY LPD_MOVED LPD_HASKEY
const LPD_NBITS = ceil(Int, log2(maximum(Int.(instances(LPDKeyState))) + 1))
const LPD_BITMASK = ~(typemax(UInt8) << LPD_NBITS)

struct KeyInfo{T<:Union{UInt32,UInt64}}
    bits::T
end

@inline function LPDKeyState(ki::KeyInfo{T}) where {T}
    bits = getfield(ki, :bits)
    statebits = bits & T(LPD_BITMASK)
    state = statebits % UInt8
    if assertion_enabled()
        return LPDKeyState(state)
    end
    if state <= UInt8(LPD_HASKEY)
        return LPDKeyState(state)
    else
        return LPD_HASKEY
    end
end

Base.zero(::Type{KeyInfo{T}}) where {T} = KeyInfo{T}(zero(T))

KeyInfo{T}(state::LPDKeyState, keydata::T) where {T} =
    setstate(KeyInfo(keydata << LPD_NBITS), state)

@inline function Base.getproperty(ki::KeyInfo, name::Symbol)
    bits = getfield(ki, :bits)
    if name === :state
        return LPDKeyState(ki)
    elseif name === :isempty
        if assertion_enabled()
            @assert iszero(bits) == (LPDKeyState(ki) === LPD_EMPTY)
        end
        return iszero(bits)
    elseif name === :isdeleted
        return LPDKeyState(ki) === LPD_DELETED
    elseif name === :ismovedempty
        return LPDKeyState(ki) === LPD_MOVED_EMPTY
    elseif name === :ismoved
        return LPDKeyState(ki) === LPD_MOVED
    elseif name === :haskey
        return LPDKeyState(ki) === LPD_HASKEY
    elseif name === :keydata
        return bits >> LPD_NBITS
    end
    return getfield(ki, name)
end

@inline function setstate(ki::KeyInfo{T}, state::LPDKeyState) where {T}
    bits = getfield(ki, :bits)
    if state === LPD_EMPTY
        return KeyInfo(zero(bits))
    else
        return KeyInfo((bits & ~T(LPD_BITMASK)) | UInt8(state))
    end
end

@inline function setdata(ki::KeyInfo{T}, keydata::T) where {T}
    bits = getfield(ki, :bits)
    state = bits & T(LPD_BITMASK)
    databits = keydata << LPD_NBITS
    return KeyInfo(databits | state)
end

mutable struct AtomicRef{T}
    @atomic value::Union{Nothing,T}
end
AtomicRef{T}() where {T} = AtomicRef{T}(nothing)
Base.eltype(::Type{AtomicRef{T}}) where {T} = T

mutable struct PairNode{Key,Value}
    slotid::UInt64
    key::Key
    @atomic value::Value
    @atomic next::Union{PairNode{Key,Value},Nothing}
end

mutable struct LinearProbingDict{Key,Value} <: ConcurrentDict{Key,Value}
    # TODO: Use `Vector{UInt128}`
    @atomic slots::Union{Vector{UInt64},Nothing}
    # TODO: Use `PairNode{Key,Value}` if `Value` is too large?
    @atomic pairnodes::Vector{AtomicRef{PairNode{Key,Value}}}
    slotids::typeof(cacheline_padded_vector(UInt64, 1))
    migration::ReentrantLock
    # TODO: per-thread non-atomic counter for approximating deleted elements
    nadded::Threads.Atomic{Int}
    ndeleted::Threads.Atomic{Int}
end
# TODO: inline the counters (and pad them)?

ConcurrentCollections.length_upper_bound(dict::LinearProbingDict) =
    dict.nadded[] - dict.ndeleted[]

ConcurrentCollections.length_lower_bound(dict::LinearProbingDict) =
    dict.nadded[] - dict.ndeleted[]

ConcurrentCollections.ConcurrentDict{Key,Value}() where {Key,Value} =
    LinearProbingDict{Key,Value}()

function LinearProbingDict{Key,Value}() where {Key,Value}
    capacity = 4  # TODO: customizable init size?
    # TODO: handle the case where key, value, and the metadata fits in an UInt
    # TODO: check the availability of CAS2?
    # TODO: use `PairNode{Key,Any}` if `Value` is too large
    pairnodes = [AtomicRef{PairNode{Key,Value}}() for _ in 1:capacity]
    slots = zeros(UInt64, 2 * capacity)
    slotids = cacheline_padded_vector(UInt64, Threads.nthreads())
    slotids .= eachindex(slotids)
    return LinearProbingDict{Key,Value}(
        slots,
        pairnodes,
        slotids,
        ReentrantLock(),
        Threads.Atomic{Int}(0),
        Threads.Atomic{Int}(0),
    )
end

@inline function prepare_pairnode!(
    pairnodes::Vector{AtomicRef{Node}},
    index,
    slotid,
    key,
    value,
) where {Node<:PairNode}
    # node = Node(slotid, key, value, nothing)
    ref = pairnodes[index]
    head = @atomic ref.value
    while true
        # Allocating `Node` each time instead of `@atomic node.next = head`
        # below is better.  It looks like avoiding `@atomic` and optimizing for
        # the happy case is better for the performance?
        node = Node(slotid, key, value, head)
        # @atomic node.next = head
        head, success = @atomicreplace ref.value head => node
        success && return
    end
end

@inline function cleanup_pairnode!(slots, pairnodes, index)
    GC.@preserve slots begin
        s2ptr = pointer(slots, 2 * index)
        slotid = UnsafeAtomics.load(s2ptr)
    end
    iszero(slotid) && unreachable()
    ref = pairnodes[index]
    node = (@atomic ref.value)::PairNode
    while true
        node.slotid == slotid && break
        next = @atomic node.next
        old, success = @atomicreplace ref.value node => next
        if success
            node = next::PairNode
        else
            node = old::PairNode
        end
    end
    next = @atomic node.next
    if next !== nothing
        @atomicreplace node.next next => nothing
    end
    return
end

@inline function load_pairnode(pairnodes, index, slotid)
    ref = pairnodes[index]
    node = (@atomic ref.value)::PairNode
    while true
        node.slotid == slotid && return node
        node = (@atomic node.next)::PairNode
    end
end

mutable struct ValueRef{Value,Node<:PairNode}
    node::Node
    isloaded::Bool
    value::Value
    ValueRef{Value}(node::Node) where {Value,Node} = new{Value,Node}(node, false)
end
# Note on `modify!` design: It looks like (even relaxed) atomic load is not
# eliminated when the value is not used (<https://godbolt.org/z/abE7sGfjG>).
# So, let's pass a `Ref`-like object to `modify!` and so that load is not issued
# when the user does not look at the value.

@inline function Base.getindex(ref::ValueRef)
    if !ref.isloaded
        node = ref.node
        ref.value = @atomic node.value
        ref.isloaded = true
    end
    return ref.value
end

@inline function Base.setindex!(ref::ValueRef, x)
    ref.value = x
    ref.isloaded = true
end

function Base.getindex(d::LinearProbingDict{Key}, key) where {Key}
    y = tryget(d, key)
    if y === nothing
        throw(KeyError(key))
    else
        return something(y)
    end
end

function Base.haskey(d::LinearProbingDict, key)
    @inline f(::Nothing) = nothing
    @inline f(_) = Keep(nothing)
    y = modify!(f, d, key)
    return y !== nothing
end

Base.get(d::LinearProbingDict, key, default) =
    something(ConcurrentCollections.tryget(d, key), default)

function ConcurrentCollections.tryget(d::LinearProbingDict, key)
    @inline f(::Nothing) = nothing
    @inline f(x) = Keep(x[])
    y = modify!(f, d, key)
    if y === nothing
        return nothing
    else
        return Some(y.value)
    end
end

function Base.setindex!(d::LinearProbingDict{Key,Value}, v, k) where {Key,Value}
    v′ = convert(Value, v)
    @inline f(_) = Some(v′)
    modify!(f, d, k)
    return d
end

Base.pop!(d::LinearProbingDict, key, default) = something(trypop!(d, key), default)
function Base.pop!(d::LinearProbingDict, key)
    value = trypop!(d, key)
    value === nothing && throw(KeyError(key))
    return something(value)
end

function ConcurrentCollections.trypop!(d::LinearProbingDict, key)
    @inline f(::Nothing) = nothing
    @inline f(ref) = Delete(ref[])
    y = modify!(f, d, key)
    if y === nothing
        return nothing
    else
        return Some(y.value)
    end
end

function ConcurrentCollections.modify!(
    f,
    dict::LinearProbingDict{Key,Value},
    key,
) where {Key,Value}
    key = convert(Key, key)
    slots, pairnodes = slots_and_pairnodes(dict)
    newslotid = UInt64(0)

    h = hash(key)

    # The upper bits of hash that would be stored in `keyinfo.keydata`:
    inlinedhash = h >> LPD_NBITS

    if 4 * length_upper_bound(dict) > length(slots)
        slots, pairnodes = expand!(dict, slots, pairnodes)
    end

    while true
        c = length(slots) ÷ 2
        offset = reinterpret(Int, h) & (c - 1)  # h % c
        nprobes = 0
        GC.@preserve slots begin
            while true
                index = offset + 1
                s1ptr = pointer(slots, 2 * offset + 1)
                s2ptr = pointer(slots, 2 * offset + 2)
                keybits = UnsafeAtomics.load(s1ptr)
                keyinfo = KeyInfo(keybits)

                if keyinfo.ismoved || keyinfo.ismovedempty
                    slots, pairnodes = finishmove!(dict, slots, pairnodes)
                    break  # restart
                elseif keyinfo.isempty
                    reply = f(nothing)::Union{Nothing,Some}
                    reply === nothing && return reply
                    # Insertion:
                    if iszero(newslotid)
                        newslotid = dict.slotids[Threads.threadid()] += Threads.nthreads()
                        # TODO: Handle wrap-around of slotid? Reset it during migration?
                    end
                    prepare_pairnode!(pairnodes, index, newslotid, key, something(reply))
                    oldslot = Pair(keybits, zero(keybits))
                    newslot = Pair(KeyInfo{UInt64}(LPD_HASKEY, inlinedhash).bits, newslotid)
                    s12ptr = Ptr{typeof(oldslot)}(s1ptr)
                    found = UnsafeAtomics.cas!(s12ptr, oldslot, newslot)
                    if found === oldslot
                        Threads.atomic_add!(dict.nadded, 1)
                        return reply
                    end
                    foundinfo = KeyInfo(first(found))
                    if foundinfo.ismoved | foundinfo.ismovedempty
                        slots, pairnodes = finishmove!(dict, slots, pairnodes)
                        break  # restart
                    else
                        # Failed to insert a new entry. It means that there was
                        # another task successfully inserted a new slot. The
                        # linked list in `pairnodes[index]` needs cleanup now
                        # before continue probing.
                        cleanup_pairnode!(slots, pairnodes, index)

                        # Retrying on CAS failure since this key may be inserted
                        # by another task.
                        continue
                        # TODO: Check the hash in `found`? If different, there's
                        # no need to retry.
                    end
                elseif keyinfo.haskey
                    if keyinfo.keydata ==′ inlinedhash
                        slotid = UnsafeAtomics.load(s2ptr)
                        node = load_pairnode(pairnodes, index, slotid)
                        if isequal(node.key, key)
                            vref = ValueRef{Value}(node)
                            while true
                                reply = f(vref)::Union{Keep,Nothing,Delete,Some}
                                reply isa Keep && return reply
                                reply isa Union{Nothing,Delete} && break
                                # Update:
                                old = vref[]
                                new = something(reply::Some)
                                old, success = @atomicreplace node.value old => new
                                success && return reply
                                vref[] = old
                            end

                            # Deletion:
                            oldslot = Pair(keybits, slotid)
                            newslot = Pair(setstate(keyinfo, LPD_DELETED).bits, slotid)
                            s12ptr = Ptr{typeof(oldslot)}(s1ptr)
                            if UnsafeAtomics.cas!(s12ptr, oldslot, newslot) === oldslot
                                ndeleted = Threads.atomic_add!(dict.ndeleted, 1) + 1
                                approx_len = dict.nadded[] - ndeleted
                                half_len = length(slots) ÷ 4
                                if length(slots) > 8 && approx_len < half_len
                                    shrink!(dict, slots, pairnodes)
                                end
                                return reply
                            else
                                continue  # CAS failed; retry
                            end
                        end
                    end
                    # Key doesn't match => continue probing
                elseif keyinfo.isdeleted
                    # =>  continue probing
                else
                    unexpected(keyinfo)
                end

                nprobes += 1
                if nprobes > c ÷ 4
                    let newslots = @atomic dict.slots
                        # Nonblocking check to see if the slots are migrated:
                        if slots === newslots
                            # @info "expand: length(slots) ≈ 2^$(floor(Int, log2(length(slots))))"
                            # global DICT = dict
                            # TODO: Check the approximated table size here. It's
                            # possible that the hash table needs cleanup but not
                            # resize (i.e., too many deleted slots).
                            slots, pairnodes = expand!(dict, slots, pairnodes)
                        else
                            slots, pairnodes = slots_and_pairnodes(dict)
                        end
                    end
                    break  # restart
                end

                offset = (offset + 1) & (c - 1)  # (offset + 1) % c
            end
        end
    end
end

expand!(dict, oldslots, oldpairnodes) = migrate!(dict, oldslots, oldpairnodes, true)
shrink!(dict, oldslots, oldpairnodes) = migrate!(dict, oldslots, oldpairnodes, false)

function new_slots_and_pairnodes(slots, pairnodes, expand)
    newslots = zeros(eltype(slots), expand ? length(slots) * 2 : length(slots) ÷ 2)
    # newslots = Mmap.mmap(Vector{UInt64}, expand ? length(slots) * 2 : length(slots) ÷ 2)
    newpairnodes = [eltype(pairnodes)() for _ in 1:length(newslots)÷2]
    # TODO: Can refs (and not just nodes) be reused?
    return (newslots, newpairnodes)
end

function migrate!(dict::LinearProbingDict, expand::Bool; basesize = nothing)
    slots, pairnodes = slots_and_pairnodes(dict)
    return migrate!(dict, slots, pairnodes, expand; basesize)
end

function migrate!(dict, oldslots, oldpairnodes, expand; basesize = nothing)
    # Since the migration is parallelized, workers running tasks blocked by the
    # lock actually will contribute to the forward progress of the entire
    # system. (Though the OS may suspend this worker thread before the tasks are
    # spawned.)
    lock(dict.migration) do
        slots = (@atomic dict.slots)::Vector{UInt64}
        pairnodes = @atomic dict.pairnodes
        if slots !== oldslots
            return slots, pairnodes
        end
        @atomic dict.slots = nothing
        @assert pairnodes === oldpairnodes
        (newslots, newpairnodes) = new_slots_and_pairnodes(slots, pairnodes, expand)
        if expand
            nadded = expand_parallel!(newslots, newpairnodes, slots, pairnodes, basesize)
        else
            nadded = migrate_serial!(newslots, newpairnodes, slots, pairnodes)
        end
        # TODO: parallelize `shrink!`

        # At this point, no other thread can be mutating the counters (as they
        # will observe `Moved`). Thus, it is safe to update the counter
        # non-atomically:
        dict.ndeleted[] = 0
        dict.nadded[] = nadded

        # This is the atomic "publishing" operation that makes the `newslots`
        # accessible to any tasks (including the ones that are/were not trying
        # to acquire the `migration` lock).
        @atomic dict.pairnodes = newpairnodes
        @atomic dict.slots = newslots

        return newslots, newpairnodes
    end
end

function finishmove!(dict, oldslots, oldpairnodes)
    lock(dict.migration) do
        slots = (@atomic dict.slots)::Vector{UInt64}
        pairnodes = @atomic dict.pairnodes
        # The caller observed `Moved` which only sets inside the `migration`
        # lock. Thus, the migration should be finished once this lock is
        # acquired:
        @assert oldslots !== slots
        @assert oldpairnodes !== pairnodes
        return slots, pairnodes
    end
end

function slots_and_pairnodes(dict)
    while true
        pairnodes = @atomic dict.pairnodes
        slots = @atomic dict.slots
        if slots === nothing
            return finishmove!(dict, slots, pairnodes)
        else
            if pairnodes === @atomic dict.pairnodes
                return (slots, pairnodes)
            end
        end
    end
end

struct Stopped
    i::Int
    nadded::Int
end

"""
    expand_parallel_basecase!(newslots, slots, basesize, start0) -> (nadded, seen_empty)

Process all clusters started within `start0:(start0 + basesize)` (mod `length(slots)`).

That is to say, _try_ to process `start0:(start0 + basesize)` but make sure to
avoid stepping into other base cases by using an empty slot to delimite the base
case boundaries. This uses the property of linear probing dictionary that
non-overlapping clusters (i.e., continuous regions of slots that are non-empty)
are mapped to non-overlapping regions when the `slots` array is doubled in size.

More precisely:

1. Process all clusters started within `start0:(start0 + basesize - 1)`.
2. If more than one cluster is processed, process the cluster that contains the
   start position of the next chunk `start0 + basesize` (mod `length(slots)`).
"""
function expand_parallel_basecase!(
    newslots,
    newpairnodes,
    slots,
    pairnodes,
    basesize,
    start0,
)
    c = length(slots) ÷ 2
    stop0 = min(start0 + basesize - 1, c)
    stpd = migrate_serial!(nothing, nothing, slots, pairnodes, start0, stop0, Val(true))
    if stpd isa Int
        @assert stpd == 0
        # This chunk does not own any clusters.
        return (0, false)
    end

    migrate_between(start, stop, flag) =
        migrate_serial!(newslots, newpairnodes, slots, pairnodes, start, stop, flag)

    # An empty slot is observed. There is at least one cluster started within
    # this chunk.
    stpd::Stopped
    @assert stpd.nadded == 0
    nadded = migrate_between(stpd.i + 1, stop0, Val(false))::Int

    # Process the cluster that includes `start0 + basesize` (if any).
    next_start = start0 + basesize
    if next_start > c
        next_start = 1
    end
    chunk_starts = (
        next_start:basesize:c,
        1:basesize:next_start-1,  # wrap around
    )
    # Using `for half` so that the compiler does not unroll the loop.
    # TODO: check if it is working
    for half in 1:2, start in chunk_starts[half]
        stop = min(start + basesize - 1, c)
        stpd = migrate_between(start, stop, Val(true))
        if stpd isa Stopped
            nadded += stpd.nadded
            return (nadded, true)
        end
        nadded += stpd::Int
    end
    @static_error "unreachable: the empty slot disappeared?"
end

plus_or((a, b), (c, d)) = (a + c, b | d)

# See`BenchDictMigration` for benchmarking this:
const LINEAR_PROBING_DICT_EXPAND_BASESIZE = Ref(2^13)

function expand_parallel!(newslots, newpairnodes, slots, pairnodes, basesize)
    # TODO: Make the default `basesize` configurable?
    basesize = something(basesize, LINEAR_PROBING_DICT_EXPAND_BASESIZE[])
    @assert length(newslots) > length(slots)

    length(slots) <= basesize &&
        return migrate_serial!(newslots, newpairnodes, slots, pairnodes)
    basesize = min(basesize, cld(length(slots), 2 * Threads.nthreads()))

    c = length(slots) ÷ 2
    nadded, seen_empty =
        threaded_typed_mapreduce(Tuple{Int,Bool}, plus_or, 1:basesize:c) do start0
            return expand_parallel_basecase!(
                newslots,
                newpairnodes,
                slots,
                pairnodes,
                basesize,
                start0,
            )
        end
    if seen_empty
        return nadded
    else
        # The `slots` are all non-empty. Fallback to serial migration:
        return migrate_serial!(newslots, newpairnodes, slots, pairnodes)
    end
end

migrate_serial!(newslots, newpairnodes, slots, pairnodes) = migrate_serial!(
    newslots,
    newpairnodes,
    slots,
    pairnodes,
    1,
    length(slots) ÷ 2,
    Val(false),
)::Int

function migrate_serial!(
    newslots,
    newpairnodes,
    slots,
    pairnodes,
    start,
    stop,
    stop_on_empty,
)
    GC.@preserve newslots slots begin
        nadded = unsafe_migrate!(
            newslots,
            newpairnodes,
            slots,
            pairnodes,
            start,
            stop,
            stop_on_empty,
        )
    end
    return nadded
end

function unsafe_migrate!(
    newslots::Union{AbstractVector{UInt64},Nothing},
    newpairnodes::Union{AbstractVector{R},Nothing},
    slots::AbstractVector{UInt64},
    pairnodes::AbstractVector{R},
    start::Int,
    stop::Int,
    stop_on_empty::Union{Val{false},Val{true}},
) where {R<:AtomicRef{<:PairNode}}
    nadded = 0
    for i in start:stop
        offset = i - 1
        s1ptr = pointer(slots, 2 * offset + 1)
        s2ptr = pointer(slots, 2 * offset + 2)
        local tryset
        let s2ptr = s2ptr
            @inline function tryset(keyinfo, newstate)
                local slotid = UnsafeAtomics.load(s2ptr)
                local oldslot = Pair(keyinfo.bits, slotid)
                local newslot = Pair(setstate(keyinfo, newstate).bits, slotid)
                local s12ptr = Ptr{typeof(oldslot)}(s1ptr)
                return UnsafeAtomics.cas!(s12ptr, oldslot, newslot) === oldslot
            end
        end
        local keyinfo
        while true
            keybits = UnsafeAtomics.load(s1ptr)
            keyinfo = KeyInfo(keybits)
            if keyinfo.isdeleted
                break  # next index
            elseif keyinfo.ismovedempty
                stop_on_empty == Val(true) && return Stopped(i, nadded)
                break  # next index
            elseif keyinfo.isempty
                # Mark that this slot is not usable anymore
                if !tryset(keyinfo, LPD_MOVED_EMPTY)
                    continue
                end
                stop_on_empty == Val(true) && return Stopped(i, nadded)
                break  # next index
            elseif keyinfo.haskey
                if !tryset(keyinfo, LPD_MOVED)
                    continue
                end
            else
                @assert keyinfo.ismoved
            end
            @goto move
        end
        continue
        @label move

        newslots === nothing && continue

        newkeybits = setstate(keyinfo, LPD_HASKEY).bits
        slotid = UnsafeAtomics.load(s2ptr)
        node = load_pairnode(pairnodes, i, slotid)
        key = node.key

        # Insertion to `newslots` does not have to use atomics since
        # it's protected by the `.migration` lock.
        c = length(newslots) ÷ 2
        h = reinterpret(Int, hash(key))
        offset = h & (c - 1)  # h % c
        nprobes = 0
        while true
            # TODO: non-atomic ordering
            local keybits = @inbounds newslots[2*offset+1]
            local keyinfo = KeyInfo(keybits)
            if keyinfo.isempty
                @inbounds newslots[2*offset+1] = newkeybits
                @inbounds newslots[2*offset+2] = slotid
                ref = newpairnodes[offset+1]
                @atomic ref.value = node
                nadded += 1
                break
            end
            nprobes += 1
            if nprobes > c
                @static_error "unreachable: too many probings during migration"
            end
            offset = (offset + 1) & (c - 1)  # (offset + 1) % c
        end
    end
    return nadded
end

Base.IteratorSize(::Type{<:LinearProbingDict}) = Base.SizeUnknown()
Base.IteratorSize(::Type{<:Base.KeySet{<:Any,<:LinearProbingDict}}) = Base.SizeUnknown()
Base.IteratorSize(::Type{<:Base.ValueIterator{<:LinearProbingDict}}) = Base.SizeUnknown()

function Base.iterate(dict::LinearProbingDict)
    slots, pairnodes = slots_and_pairnodes(dict)
    return iterate(dict, (slots, pairnodes, 1))
end

function Base.iterate(::LinearProbingDict, (slots, pairnodes, index))
    GC.@preserve slots begin
        index < 1 && return nothing
        while true
            2 * index > length(slots) && return nothing
            offset = index - 1
            s1ptr = pointer(slots, 2 * offset + 1)
            s2ptr = pointer(slots, 2 * offset + 2)
            keybits = UnsafeAtomics.load(s1ptr)
            keyinfo = KeyInfo(keybits)
            if keyinfo.haskey | keyinfo.ismoved
                slotid = UnsafeAtomics.load(s2ptr)
                node = load_pairnode(pairnodes, index, slotid)
                key = node.key
                value = @atomic node.value
                return (key => value), (slots, pairnodes, index + 1)
            end
            index += 1
        end
    end
end

kvtype(::Type{Pair{Key,Value}}) where {Key,Value} = (Key, Value)
kvtype(::Type{Tuple{Key,Value}}) where {Key,Value} = (Key, Value)
kvtype(::Type) = nothing

# TODO: batch and/or racy construction
function LinearProbingDict{Key,Value}(kvs) where {Key,Value}
    dict = LinearProbingDict{Key,Value}()
    for (k, v) in kvs
        dict[k] = v
    end
    return dict
end

function ConcurrentCollections.ConcurrentDict(kvs)
    if IteratorEltype(kvs) isa HasEltype
        typ = kvtype(eltype(kvs))
        if typ !== nothing
            return LinearProbingDict{first(typ),last(typ)}(kvs)
        end
    end
    kvs = [k => v for (k, v) in kvs]
    typ = kvtype(eltype(kvs))
    return LinearProbingDict{first(typ),last(typ)}(kvs)
end

ConcurrentCollections.ConcurrentDict{Key,Value}(kvs) where {Key,Value} =
    LinearProbingDict{Key,Value}(kvs)

function ConcurrentCollections.ConcurrentDict(kv::Pair, kvs::Pair...)
    ks = promote(first(kv), map(first, kvs)...)
    vs = promote(last(kv), map(last, kvs)...)
    allkvs = map(=>, ks, vs)
    Key = typeof(first(ks))
    Value = typeof(first(vs))
    return LinearProbingDict{Key,Value}(allkvs)
end

function ConcurrentCollections.ConcurrentDict{Key,Value}(
    kv::Pair,
    kvs::Pair...,
) where {Key,Value}
    allkvs = map((kv, kvs...)) do (k, v)
        convert(Key, k) => convert(Value, v)
    end
    return LinearProbingDict{Key,Value}(allkvs)
end

function Base.summary(io::IO, dict::LinearProbingDict{Key,Value}) where {Key,Value}
    @nospecialize dict
    print(io, ConcurrentCollections.ConcurrentDict)
    print(io, '{')
    print(io, Key)
    print(io, ", ")
    print(io, Value)
    print(io, '}')
end

function Base.show(io::IO, ::MIME"text/plain", dict::LinearProbingDict)
    @nospecialize dict
    summary(io, dict)
    n = 0
    for kv in dict
        n += 1
        println(io)
        if n > 3
            print(io, "  ⋮")
            break
        end
        print(io, "  ", kv)
    end
end

"""
    clusters(dict) -> ranges
    clusters(slots) -> ranges

Compute clusters in the slots. Used for performance debugging.

```julia
using ConcurrentCollections
using ConcurrentCollections.Implementations: clusters
d = ConcurrentDict(1:1000 .=> 0)
cs = clusters(d.slots)

using UnicodePlots
histogram(map(length, cs))

using StatsBase
describe(map(length, cs))
```
"""
clusters(d::LinearProbingDict) = clusters(d.slots)
function clusters(slots::AbstractVector{UInt64})
    cs = typeof(1:2)[]
    i = 1
    while true
        while true
            2 * i > length(slots) && return cs
            keyinfo = KeyInfo(slots[2*(i-1)+1])
            i += 1
            if keyinfo.isempty | keyinfo.ismovedempty
                break
            end
        end
        start = i - 1
        while true
            if 2 * i > length(slots)
                push!(cs, start:i-1)
                return cs
            end
            keyinfo = KeyInfo(slots[2*(i-1)+1])
            i += 1
            if keyinfo.isempty | keyinfo.ismovedempty
                break
            end
        end
        push!(cs, start:i-1)
    end
end
