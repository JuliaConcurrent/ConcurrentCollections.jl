struct Empty end

# TODO: Rename to `Moving`?
"""
    Moved{Value}

A "tag" used for making that this slot is being moved to a new `slots` vector.
"""
struct Moved{Value}
    value::Value
end

const MOVED_EMPTY = Moved{Empty}(Empty())

# TODO: Support "UInt62"? (i.e., `UInt64` but two bits used as tag)
# TODO: Get rid of Moved{Value}
const ValueUnion{Value} = Union{Value,Empty,Moved{Value},Moved{Empty}}

# Since Julia does not have atomics for arrays, `LinearProbingDict` uses an
# array filled with mutable struct `DictSlot` instead. This has the disadvantage
# that it creates one more indirection. But a somewhat nice effect is that it
# makes copying a key-value pair cheap and easy to do with non-atomic write.
# Re-using small mutable objects like this can be bad for memory locality but it
# seems that the effect is neglegible. The performance is much better than
# creating small objects and copy the key and value separately.
#
# Due to the type tag, storing two UInt64 takes space as large as storing three
# UInt64. So, we can store hash for "free".
# TODO: Check if it is actually better to store the hash in an `Vector{UInt}`.
# Even though it will waste some memory, it will reduce indirection and should
# make probing much more efficient.
mutable struct DictSlot{
    Key,
    Value,
    KeyStorage>:Union{Key,Empty},
    ValueStorage>:ValueUnion{Value},
}
    @atomic hash::UInt
    @atomic key::KeyStorage
    @atomic value::ValueStorage

    # Defining constructor manually to workaround:
    # https://github.com/JuliaLang/julia/issues/42353
    DictSlot{Key,Value,KeyStorage,ValueStorage}(
        hash,
        key,
        value,
    ) where {Key,Value,KeyStorage,ValueStorage} =
        new{Key,Value,KeyStorage,ValueStorage}(hash, key, value)
end

DictSlot{Key,Value,KeyStorage,ValueStorage}() where {Key,Value,KeyStorage,ValueStorage} =
    DictSlot{Key,Value,KeyStorage,ValueStorage}(zero(UInt), Empty(), Empty())

mutable struct LinearProbingDict{Key,Value,KeyStorage,ValueStorage} <:
               ConcurrentDict{Key,Value}
    @atomic slots::Union{Vector{DictSlot{Key,Value,KeyStorage,ValueStorage}},Nothing}
    migration::ReentrantLock
    # TODO: per-thread non-atomic counter for approximating deleted elements
    nadded::Threads.Atomic{Int}
    ndeleted::Threads.Atomic{Int}
end
# TODO: inline the counters (and pad them)?
# TODO: relaxed counter

slots_type(
    ::LinearProbingDict{Key,Value,KeyStorage,ValueStorage},
) where {Key,Value,KeyStorage,ValueStorage} =
    Vector{DictSlot{Key,Value,KeyStorage,ValueStorage}}

ConcurrentCollections.length_upper_bound(dict::LinearProbingDict) =
    dict.nadded[] - dict.ndeleted[]

ConcurrentCollections.length_lower_bound(dict::LinearProbingDict) =
    dict.nadded[] - dict.ndeleted[]

ConcurrentCollections.ConcurrentDict{Key,Value}() where {Key,Value} =
    LinearProbingDict{Key,Value}()

# Extracted as a function so that `T` is captured as a type parameter in the
# closure for the generator.
fillwith(::Type{T}, n) where {T} = [T() for _ in 1:n]

function LinearProbingDict{Key,Value}() where {Key,Value}
    capacity = 4  # TODO: customizable init size?
    if sizeof(Some{Union{Key,Empty}}) > sizeof(UInt)
        KeyStorage = Any
    else
        KeyStorage = Union{Key,Empty}
    end
    if sizeof(Some{ValueUnion{Value}}) > sizeof(UInt)
        ValueStorage = Any
    else
        ValueStorage = ValueUnion{Value}
    end
    slots = fillwith(DictSlot{Key,Value,KeyStorage,ValueStorage}, capacity)
    return LinearProbingDict{Key,Value,KeyStorage,ValueStorage}(
        slots,
        ReentrantLock(),
        Threads.Atomic{Int}(0),
        Threads.Atomic{Int}(0),
    )
end

struct ValueRef{Value}
    value::Value
end
# Note on `modify!` design: It looks like (even relaxed) atomic load is not
# eliminated when the value is not used (<https://godbolt.org/z/abE7sGfjG>).
# So, let's pass a `Ref`-like object to `modify!` and so that load is not issued
# when the user does not look at the value.

@inline Base.getindex(ref::ValueRef) = ref.value

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

function ConcurrentCollections.tryget(d::LinearProbingDict{<:Any,V}, key) where {V}
    @inline f(::Nothing) = nothing
    @inline f(x) = Keep(x[])
    y = modify!(f, d, key)
    if y === nothing
        return nothing
    else
        return Some{V}(y.value::V)
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

function record_added!(dict)
    Threads.atomic_add!(dict.nadded, 1) + 1
end

function record_deleted!(dict)
    Threads.atomic_add!(dict.ndeleted, 1) + 1
end

# The main linearization points are the change in the value, not the key.  The
# CAS on key only determines the internal "physical" location of the slot.
function ConcurrentCollections.modify!(
    f,
    dict::LinearProbingDict{Key,Value},
    key,
) where {Key,Value}
    key = convert(Key, key)
    slots = load_slots(dict)
    h = hash(key)

    if 1.5 * length_upper_bound(dict) > length(slots)
        slots = expand!(dict, slots)
    end

    while true
        @label restart
        c = length(slots)
        offset = reinterpret(Int, h) & (c - 1)  # h % c
        nprobes = 0
        while true
            index = offset + 1
            offset = (offset + 1) & (c - 1)  # (offset + 1) % c

            ref = slots[index]
            hstored = @atomic :monotonic ref.hash
            if (!iszero(hstored)) & (h != hstored)
                nprobes += 1
                if nprobes > c ÷ 4
                    @goto too_many_probes
                end
                continue  # already occupied; continue probing
            end
            # It's possible to reach here even when the key matches (i.e., the
            # maching key has been stored but the hash has not). This is fine
            # since the key is compared directly with `isequal` below anyway.

            kstored = (@atomic ref.key)::Union{Empty,Key}
            if kstored isa Empty
                reply = f(nothing)::Union{Nothing,Some}
                reply === nothing && return nothing
                oldkey, ok = @atomicreplace ref.key Empty() => key
                if ok
                    @atomic :monotonic ref.hash = h
                else
                    hstored = @atomic :monotonic ref.hash
                    if (!iszero(hstored) & (h != hstored)) || !isequal(oldkey, key)
                        continue  # failed to obtain this slot; continue probing
                    end
                    # Reaching here means two tasks simultaneously inserted the
                    # same key.  The value must be inserted to this slot.
                end
                v = convert(Value, something(reply))
                vstored, ok = @atomicreplace ref.value Empty() => v
                if ok
                    record_added!(dict)
                    return reply
                end
                vstored = vstored::Union{Value,Moved{Value},Moved{Empty}}
            elseif isequal(kstored, key)
                vstored = (@atomic ref.value)::ValueUnion{Value}
            else
                continue  # already occupied; continue probing
            end
            # Here, we know that this is the correct slot but the value is not
            # stored yet.
            while true
                if vstored isa Empty
                    reply = f(nothing)::Union{Nothing,Some}
                    reply === nothing && return nothing
                    v = convert(Value, something(reply))
                    vstored, ok = @atomicreplace ref.value Empty() => v
                    if ok
                        record_added!(dict)
                        return reply
                    end
                    vstored = vstored::Union{Value,Moved{Value},Moved{Empty}}
                end
                if vstored isa Moved
                    slots = finishmove!(dict, slots)
                    @goto restart
                end
                vref = ValueRef{Value}(vstored)
                reply = f(vref)::Union{Keep,Nothing,Delete,Some}
                if reply isa Keep
                    # @show nprobes
                    return reply
                end
                if reply isa Union{Nothing,Delete}
                    # Deletion:
                    vstored, ok = @atomicreplace ref.value vstored => Empty()
                    if ok
                        ndeleted = record_deleted!(dict)
                        approx_len = dict.nadded[] - ndeleted
                        half_len = length(slots) ÷ 2
                        if length(slots) > 8 && approx_len < half_len
                            shrink!(dict, slots)
                        end
                        return reply
                    end
                else
                    # Update:
                    new = convert(Value, something(reply::Some))
                    vstored, ok = @atomicreplace ref.value vstored => new
                    ok && return reply
                end
                vstored = vstored::ValueUnion{Value}
            end

            nprobes += 1
            if nprobes > c ÷ 4
                @label too_many_probes
                let newslots = @atomic dict.slots
                    if (slots === newslots) || newslots === nothing
                        # @info "expand: length(slots) ≈ 2^$(floor(Int, log2(length(slots))))"
                        # global DICT = dict
                        # TODO: Check the approximated table size here. It's
                        # possible that the hash table needs cleanup but not
                        # resize (i.e., too many deleted slots).
                        slots = expand!(dict, slots)
                    else
                        slots = load_slots(dict)
                    end
                end
                @goto restart
            end
        end  # ...of the inner "CAS loop"
    end  # ...of the restart loop
end

expand!(dict, oldslots) = migrate!(dict, oldslots, true)
shrink!(dict, oldslots) = migrate!(dict, oldslots, false)

function migrate!(dict::LinearProbingDict, expand::Bool; basesize = nothing)
    slots = load_slots(dict)
    return migrate!(dict, slots, expand; basesize)
end

function load_slots(dict)
    while true
        slots = @atomic dict.slots
        if slots === nothing
            return finishmove!(dict, slots)
        else
            return slots
        end
    end
end

function migrate!(dict::LinearProbingDict, oldslots, expand; basesize = nothing)
    # Since the migration is parallelized, workers running tasks blocked by the
    # lock actually will contribute to the forward progress of the entire
    # system. (Though the OS may suspend this worker thread before the tasks are
    # spawned.)
    lock(dict.migration) do
        slots = (@atomic dict.slots)::slots_type(dict)
        if slots !== oldslots
            return slots
        end
        @atomic dict.slots = nothing
        newslots = similar(slots, expand ? length(slots) * 2 : length(slots) ÷ 2)
        if expand
            expand_parallel!(newslots, slots, basesize)
        else
            if migrate_serial!(newslots, slots, Val(true)) === nothing
                # Shrinking the slots have failed and rolled back. Keep using
                # the old slots:
                @atomic dict.slots = slots
                return slots
            end
        end
        # TODO: parallelize `shrink!`

        # This is the atomic "publishing" operation that makes the `newslots`
        # accessible to any tasks (including the ones that are/were not trying
        # to acquire the `migration` lock).
        @atomic dict.slots = newslots

        return newslots
    end
end

function finishmove!(dict::LinearProbingDict, oldslots)
    lock(dict.migration) do
        slots = (@atomic dict.slots)::slots_type(dict)
        # TODO: `oldslots` was used for a sanity check that `slots` is updated.
        # However, now that shrink can rollback migration, it's not correct any
        # more. Still keeping this code since it might make sense to actually
        # not support shrinking.
        #=
        @assert oldslots !== slots
        =#
        return slots
    end
end

struct Stopped
    i::Int
    nadded::Int
end

"""
    expand_parallel_basecase!(...) -> (nadded, seen_empty)

Process all clusters started within `start0:(start0 + basesize)` (mod `length(slots)`).

That is to say, _try_ to process `start0:(start0 + basesize)` but make sure to
avoid stepping into other base cases by using an empty slot to delimite the base
case boundaries. This uses the property of linear probing dictionary that
non-overlapping clusters (i.e., continuous regions of slots that are non-empty)
are mapped to non-overlapping regions when the `slots` array is doubled in size.

More precisely:

1. Process all clusters started within `start0:(start0 + basesize - 1)`.
2. If at least one cluster is processed, process the cluster that contains the
   start position of the next chunk `start0 + basesize` (mod `length(slots)`).

Unlike the original trick mentioned in Maier et al. (2019), there is a
difference due to that `DictSlot` is reused in the `newslots`.  Since the slot
is re-used, we can't use it as the reliable marker for delimiting the clusters.
Other tasks can empty a slot at any moment.  So, instead, each basecase sets its
own promise `firstempties[ichunk]::Promise{Int}` to the index of the first empty
slot that it sees and successfully CAS'ed to `Moved{Empty}`.  If no empty slot
is observed, the promise is set to -1.  When the basecase reaches the end of
chunk (`start0 + basesize`), it confirms the end of the cluster it owns by
finding out the first valid `firstempties[mod1((ichunk+i),end)]` with the
smallest `i > 0`. If consecutive basecase tasks are started at the same time, by
the time a basecase task needs the promise, it is likely already is ready since
that's the first thing that the other task does.

---

This trick is mentioned in:

> Maier, Tobias, Peter Sanders, and Roman Dementiev. “Concurrent Hash Tables:
> Fast and General(?)!” ACM Transactions on Parallel Computing 5, no. 4
> (February 22, 2019): 16:1–16:32. https://doi.org/10.1145/3309206.
"""
function expand_parallel_basecase!(newslots, slots, basesize, start0, ichunk, firstempties)
    c = length(slots)
    stop0 = min(start0 + basesize - 1, c)
    stpd = migrate_serial_nofill!(nothing, slots, start0, stop0, Val(true))
    if stpd isa Int
        @assert stpd == 0
        # This chunk does not own any clusters.
        put!(firstempties[ichunk], -1)
        return (0, false)
    end

    migrate_between(start, stop) = migrate_serial_nofill!(newslots, slots, start, stop)::Int

    # An empty slot is observed. There is at least one cluster started within
    # this chunk.
    stpd::Stopped
    @assert stpd.nadded == 0
    put!(firstempties[ichunk], stpd.i)
    nadded = migrate_between(stpd.i + 1, stop0)

    # Process the cluster that includes `start0 + basesize` (if any).
    chunk_starts = (
        start0+basesize:basesize:c,
        1:basesize:start0-1,  # wrap around
    )
    chunk_indices = (
        ichunk+1:length(firstempties),
        1:ichunk-1,  # wrap around
    )
    for half in 1:2
        for (jchunk, start) in zip_strict(chunk_indices[half], chunk_starts[half])
            nextempty = fetch(firstempties[jchunk])
            if nextempty == -1  # next chunk is owned by this task
                stop = min(start + basesize - 1, c)
                nadded += migrate_between(start, stop)
            else
                nadded += migrate_between(start, nextempty - 1)
                return (nadded, true)
            end
        end
    end
    # Reaching here means there was only one empty slot.
    nadded += migrate_between(start0, stpd.i - 1)
    return (nadded, true)
end

plus_or((a, b), (c, d)) = (a + c, b | d)

# See`BenchDictMigration` for benchmarking this:
const LINEAR_PROBING_DICT_EXPAND_BASESIZE = Ref(2^13)

function expand_parallel!(newslots, slots, basesize)
    # TODO: Make the default `basesize` configurable?
    basesize = something(basesize, LINEAR_PROBING_DICT_EXPAND_BASESIZE[])
    @assert length(newslots) > length(slots)

    if length(slots) <= basesize || Threads.nthreads() == 1
        return migrate_serial!(newslots, slots)
    end
    basesize = min(basesize, cld(length(slots), Threads.nthreads()))

    # TODO: since `nadded` from this is not used any more, `|` can be used
    # instead of `plus_or`.
    c = length(slots)
    chunkstarts = 1:basesize:c
    firstempties = [Promise{Int}() for _ in chunkstarts]
    nadded, seen_empty = threaded_typed_mapreduce(
        Tuple{Int,Bool},  # return type of the do block and the doamin of `plus_or`
        plus_or,
        eachindex(chunkstarts),
    ) do ichunk
        start0 = chunkstarts[ichunk]
        return expand_parallel_basecase!(
            newslots,
            slots,
            basesize,
            start0,
            ichunk,
            firstempties,
        )
    end
    if seen_empty
        # fill_undef!(newslots)  # TODO: parallel?
        Threads.@threads for i in eachindex(newslots)
            @inbounds fill_undef_at!(newslots, i)
        end
        return nadded
    else
        # The `slots` are all non-empty. Fallback to serial migration:
        return migrate_serial!(newslots, slots)
    end
end

function migrate_serial!(
    newslots,
    slots,
    rollback_on_error::Union{Val{false},Val{true}} = Val(false),
)
    nadded = migrate_serial_nofill!(
        newslots,
        slots,
        1,
        length(slots),
        Val(false),
        rollback_on_error,
    )
    if nadded === nothing
        rollback_on_error::Val{true}
        return nothing
    end
    nadded = nadded::Int
    fill_undef!(newslots)
    return nadded
end

Base.@propagate_inbounds function fill_undef_at!(newslots, i)
    if !isassigned(newslots, i)
        newslots[i] = eltype(newslots)()
    end
end

function fill_undef!(newslots)
    for i in eachindex(newslots)
        @inbounds fill_undef_at!(newslots, i)
    end
    return newslots
end

# TODO: Create "defrag" version that actually copies keys/values into pre-filled
# (continuously allocated) slots?
"""
Migrate `DictSlots` entries to `slots` to `newslots`.
"""
function migrate_serial_nofill!(
    newslots::Union{AbstractVector{Slot},Nothing},
    slots::AbstractVector{Slot},
    start::Int,
    stop::Int,
    # TODO: use custom singletong rather than Val:
    stop_on_empty::Union{Val{false},Val{true}} = Val(false),
    rollback_on_error::Union{Val{false},Val{true}} = Val(false),
) where {K,V,Slot<:DictSlot{K,V}}
    nadded = 0
    for i in start:stop
        ref = slots[i]
        while true
            local value
            value = @atomic ref.value
            if value isa Empty
                _, ok = @atomicreplace ref.value Empty() => MOVED_EMPTY
                ok || continue
                stop_on_empty == Val(true) && return Stopped(i, nadded)
                break  # next index
            elseif value isa Moved{Empty}
                @static_error("unexpected Moved{Empty}")
            end
            @goto move
        end
        continue
        @label move

        newslots === nothing && continue

        h = @atomic :monotonic ref.hash
        if iszero(h)
            key = @atomic ref.key
            key = key::K
            h = hash(key)
        end

        # Insertion to `newslots` does not have to use atomics since
        # it's protected by the `.migration` lock.
        c = length(newslots)
        offset = reinterpret(Int, h) & (c - 1)  # h % c
        nprobes = 0
        while true
            index = offset + 1
            offset = (offset + 1) & (c - 1)  # (offset + 1) % c

            if !isassigned(newslots, index)
                newslots[index] = ref
                nadded += 1
                break
            end

            nprobes += 1
            if nprobes > c
                if rollback_on_error === Val(true)
                    rollback_migration!(slots, start, i)
                    return nothing
                end
                @static_error "unreachable: too many probings during migration"
            end
        end
    end
    return nadded
end

function rollback_migration!(slots, start, stop)
    for i in start:stop
        ref = slots[i]
        value = @atomic ref.value
        while true
            if value isa Moved{Empty}
                value, ok = @atomicreplace ref.value value => Empty()
                ok && break
                # TODO: maybe simple store is OK, since no one should be touching it?
            elseif value isa Moved
                @static_error("unexpected Moved{Value}")
            else
                break
            end
        end
    end
end

Base.IteratorSize(::Type{<:LinearProbingDict}) = Base.SizeUnknown()
Base.IteratorSize(::Type{<:Base.KeySet{<:Any,<:LinearProbingDict}}) = Base.SizeUnknown()
Base.IteratorSize(::Type{<:Base.ValueIterator{<:LinearProbingDict}}) = Base.SizeUnknown()

function Base.iterate(dict::LinearProbingDict)
    slots = load_slots(dict)
    return iterate(dict, (slots, 1))
end

function Base.iterate(::LinearProbingDict, (slots, index))
    index < 1 && return nothing
    while true
        index > length(slots) && return nothing
        ref = slots[index]
        value = @atomic ref.value
        if value isa Empty
        elseif value isa Moved{Empty}
        else
            if value isa Moved
                value = value.value
            end
            key = @atomic ref.key
            return (key => value), (slots, index + 1)
        end
        index += 1
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
function clusters(slots::AbstractVector)
    cs = typeof(1:2)[]
    i = 1
    while true
        while true
            i > length(slots) && return cs
            ref = slots[i]
            key = @atomic ref.key
            i += 1
            if key isa Union{Empty,Moved{Empty}}
                break
            end
        end
        start = i - 1
        while true
            if i > length(slots)
                push!(cs, start:i-1)
                return cs
            end
            ref = slots[i]
            key = @atomic ref.key
            i += 1
            if key isa Union{Empty,Moved{Empty}}
                break
            end
        end
        push!(cs, start:i-1)
    end
end
