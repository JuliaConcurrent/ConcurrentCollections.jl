struct Moved{Key}
    key::Key
end

struct Empty end
struct MovedEmpty end
struct Deleted end
struct NoValue end

const KeyUnion{Key} = Union{
    Key,             # data is stored
    Moved{Key},      # data is moved
    MovedEmpty,      # empty slot not usable anymore due to migration
    Empty,           # empty slot
    Deleted,         # deleted
}

const RefKeyUnion{Key} = Union{
    Key,
    RefValue{Moved{Key}},  # heap allocate Moved{Key} if Key is heap allocated
    MovedEmpty,
    Empty,
    Deleted,
}

abstract type AbstractPair{Key,Value} end
stored_key_type(::Type{AbstractPair{Key}}) where {Key} = Key
stored_value_type(::Type{AbstractPair{<:Any,Value}}) where {Value} = Value

struct InlinedPair{Key,Value,KPad,VPad} <: AbstractPair{Key,Value}
    key::IPadder{Inlined{KeyUnion{Key}},KPad}
    value::IPadder{Value,VPad}
end

@inline getkey(pair::InlinedPair) = pair.key.x.x
@inline getvalue(pair::InlinedPair) = pair.value.x

function inlinedpair_type(::Type{Key}, ::Type{Value}) where {Key,Value}
    KPad = padsize_for_cas(Inlined{KeyUnion{Key}})
    VPad = padsize_for_cas(InlinedPair{Key,Value,KPad,0})
    return InlinedPair{Key,Value,KPad,VPad}
end

@inline InlinedPair{Key,Value}(key::KeyUnion{Key}, value::Value) where {Key,Value} =
    inlinedpair_type(Key, Value)(key, value)

@inline function InlinedPair{Key,Value,KPad,VPad}(
    key::KeyUnion{Key},
    value::Value,
) where {Key,Value,KPad,VPad}
    k = IPadder{Inlined{KeyUnion{Key}},KPad}(key)
    v = IPadder{Value,VPad}(value)
    return InlinedPair{Key,Value,KPad,VPad}(k, v)
end

@inline function InlinedPair{Key,Value,KPad,VPad}(
    key::KeyUnion{Key},
) where {Key,Value,KPad,VPad}
    if NoValue <: Value
        InlinedPair{Key,Value,KPad,VPad}(key, NoValue())
    elseif Value <: Ref
        InlinedPair{Key,Value,KPad,VPad}(key, Value())
    else
        InlinedPair{Key,Value,KPad,VPad}(key, zerofill(Value))
    end
end

macro _deref_moved(ex)
    quote
        x = $(esc(ex))
        if x isa RefValue
            y = x[]
            if y isa Moved
                y
            else
                x
            end
        else
            x
        end
    end
end

struct BoxedKeyPair{Key,Value,VPad} <: AbstractPair{Key,Value}
    key::RefKeyUnion{Key}
    value::IPadder{Value,VPad}
end

@inline getkey(pair::BoxedKeyPair) = @_deref_moved(pair.key)
@inline getvalue(pair::BoxedKeyPair) = pair.value.x

function boxedkeypair_type(::Type{Key}, ::Type{Value}) where {Key,Value}
    P = BoxedKeyPair{Key,Value,8 - sizeof(Value)}
    fieldoffset(P, 2) == 8 || @static_error("invalid key type")
    sizeof(P) == 16 || @static_error("invalid value size")
    return P
end

@inline BoxedKeyPair{Key,Value}(
    key::Union{RefKeyUnion{Key},Moved{Key}},
    value::Value,
) where {Key,Value} = boxedkeypair_type(Key, Value)(key, value)

@inline BoxedKeyPair{Key,Value,VPad}(key::Moved{Key}, value::Value) where {Key,Value,VPad} =
    BoxedKeyPair{Key,Value,VPad}(Ref(Moved(key)), value)

@inline function BoxedKeyPair{Key,Value,VPad}(
    key::RefKeyUnion{Key},
    value::Value,
) where {Key,Value,VPad}
    k = key
    v = IPadder{Value,VPad}(value)
    return BoxedKeyPair{Key,Value,VPad}(k, v)
end

@inline function BoxedKeyPair{Key,Value,VPad}(
    key::Union{RefKeyUnion{Key},Moved{Key}},
) where {Key,Value,VPad}
    if NoValue <: Value
        BoxedKeyPair{Key,Value,VPad}(key, NoValue())
    elseif Value <: Ref
        BoxedKeyPair{Key,Value,VPad}(key, Value())
    else
        BoxedKeyPair{Key,Value,VPad}(key, zerofill(Value))
    end
end

function Base.show(io::IO, pair::AbstractPair)
    @nospecialize pair
    k = getkey(pair)
    v = getvalue(pair)
    if get(io, :typeinfo, nothing) !== typeof(pair)
        print(io, typeof(pair), "(", k, ", ", v, ")")
        return
    end
    show(io, k)
    printstyled(io, " => "; color = :light_black)
    show(io, v)
end

mutable struct LinearProbingDict{Key,Value,Slot} <: ConcurrentDict{Key,Value}
    slots::Vector{Slot}
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
    # TODO: handle the case where key, value, and the metadata fits in an UInt
    # TODO: use BoxedKeyPair if Value is small
    FallbackSlot = RefValue{InlinedPair{Key,Value,0,0}}
    if !isinlinable(Key)
        Slot = boxedkeypair_type(Key, Value)
    elseif aligned_sizeof(fieldtype(inlinedpair_type(Key, Value), 1)) <= 8  # atomic load works
        if isinlinable(Value) && aligned_sizeof(inlinedpair_type(Key, Value)) <= 16
            Slot = inlinedpair_type(Key, Value)
        else
            Slot = something(
                cas_compatible(inlinedpair_type(Key, Union{Value,NoValue})),
                # cas_compatible(inlinedpair_type(Key, RefValue{Value})),
                FallbackSlot,
            )
        end
    else
        Slot = FallbackSlot
    end
    if !(Slot <: Ref)
        @assert Base.allocatedinline(Slot)
    end
    capacity = 4  # TODO: customizable init size?
    slots = emptyslots(Slot, capacity)
    let diff = pointer(slots, 2) - pointer(slots, 1)
        if !ispow2(diff)
            error("implementation error: slot size is not a power of 2: $diff")
        end
        if diff > 16
            error("implementation error: slot size too big: $diff")
        end
    end
    return LinearProbingDict{Key,Value,Slot}(
        slots,
        ReentrantLock(),
        Threads.Atomic{Int}(0),
        Threads.Atomic{Int}(0),
    )
end

emptyslots(::Type{Slot}, length::Integer) where {Slot} =
    fillempty!(Vector{Slot}(undef, length))

fillempty!(slots::AbstractVector{Slot}) where {Slot<:AbstractPair} =
    fill!(slots, Slot(Empty()))

fillempty!(slots::AbstractVector{Slot}) where {P,Slot<:Ref{P}} =
    fill!(slots, Slot(P(Empty())))
# TODO: use undef as empty

value_uint_type(::Type{Slot}) where {Value,Slot<:AbstractPair{<:Any,Value}} =
    uint_for(Value)

mutable struct InlinedSlotRef{Slot,KUInt,VUInt}
    ptr::Ptr{Cvoid}
    keyint::KUInt
    valueint::VUInt
    value_loaded::Bool

    @inline function InlinedSlotRef{Slot}(ptr::Ptr{Cvoid}, keyint::KUInt) where {Slot,KUInt}
        VUInt = value_uint_type(Slot)
        return new{Slot,KUInt,VUInt}(ptr, keyint, VUInt(0), false)
    end
end

@inline function load_slot(
    slots::AbstractVector{Slot},
    index,
) where {Key,Slot<:AbstractPair{Key}}
    ptr = Ptr{Cvoid}(pointer(slots, index))
    if Slot <: InlinedPair
        KUInt = uint_for(Inlined{KeyUnion{Key}})
    else
        KUInt = UInt
    end
    keyptr = Ptr{KUInt}(ptr)
    keyint = UnsafeAtomics.load(keyptr)
    return InlinedSlotRef{Slot}(ptr, keyint)
end

@inline getkey(slotref::InlinedSlotRef{Slot}) where {Key,Slot<:InlinedPair{Key}} =
    from_bytes(Inlined{KeyUnion{Key}}, slotref.keyint).x

@inline getkey(slotref::InlinedSlotRef{Slot}) where {Key,Slot<:BoxedKeyPair{Key}} =
    @_deref_moved(unsafe_pointer_to_objref(Ptr{Ptr{Cvoid}}(slotref.keyint)))::KeyUnion{Key}

@inline function load_valueint(
    ::Type{Slot},
    ptr,
) where {Key,Value,Slot<:AbstractPair{Key,Value}}
    VUInt = uint_for(Value)
    valueptr = Ptr{VUInt}(ptr + fieldoffset(Slot, 2))
    valueint = UnsafeAtomics.load(valueptr)
    return valueint
end

@inline function Base.getindex(
    slotref::InlinedSlotRef{Slot},
) where {Key,Value,Slot<:AbstractPair{Key,Value}}
    if slotref.value_loaded
        valueint = slotref.valueint
    else
        valueint = load_valueint(Slot, slotref.ptr)
        slotref.valueint = valueint
        slotref.value_loaded = true
    end
    value = from_bytes(Value, valueint)
    return value
end

@inline value_ref(slotref::InlinedSlotRef) = slotref
# Note on `modify!` design: It looks like (even relaxed) atomic load is not
# eliminated when the value is not used (<https://godbolt.org/z/abE7sGfjG>).
# So, let's pass a `Ref`-like object to `modify!` and so that load is not issued
# when the user does request.

allocate_slot(::AbstractVector{<:AbstractPair}) = nothing

@inline cas_slot!(slotref, new_slot, root, key) =
    cas_slot!(slotref, new_slot, root, key, NoValue())

@inline function cas_slot!(
    slotref::InlinedSlotRef{Slot},
    ::Nothing,
    root,
    key::KeyUnion{Key},
    value,
) where {Key,Value,Slot<:AbstractPair{Key,Value}}
    ptr = slotref.ptr
    UIntType = uint_for(Slot)
    if slotref.value_loaded
        oldvalueint = slotref.valueint
    else
        oldvalueint = load_valueint(Slot, ptr)
    end
    if value isa NoValue
        # TODO: store NoValue when Value <: NoValue
        newvalueint = oldvalueint
    else
        # TODO: handle `Value isa Union`
        newvalueint = uint_from(value)
    end
    ref = nothing
    if Slot <: InlinedPair
        newkeyint = uint_from(Inlined{KeyUnion{Key}}(key))
    elseif key isa Moved{Key}
        ref = Ref(key)
        newkeyint = UInt(pointer_from_objref(ref))
        julia_write_barrier(ref)
        julia_write_barrier(root, ref)
    else
        newkeyint = UInt(_pointer_from_objref(key))
    end
    # ns = Slot(key, value)
    nu = UIntType(newvalueint)
    nu <<= fieldoffset(Slot, 2) * 8
    nu |= newkeyint
    ou = UIntType(oldvalueint)
    ou <<= fieldoffset(Slot, 2) * 8
    ou |= slotref.keyint
    GC.@preserve ref begin
        fu = UnsafeAtomics.cas!(Ptr{typeof(nu)}(ptr), ou, nu)
    end
    # @show ou nu fu
    return fu == ou
end

struct RefSlotRef{R}
    ptr::Ptr{Cvoid}
    ref::R
end

@inline function load_slot(
    slots::AbstractVector{Slot},
    index,
) where {Key,Value,Slot<:Ref{<:InlinedPair{Key,Value}}}
    ptr = Ptr{Cvoid}(pointer(slots, index))
    int = UnsafeAtomics.load(Ptr{UInt}(ptr))
    ref = unsafe_pointer_to_objref(Ptr{Cvoid}(int))::Slot
    return RefSlotRef(ptr, ref)
end

@inline getkey(slotref::RefSlotRef) = slotref.ref[].key.x.x

struct ImmutableRef{T}
    x::T
end

@inline Base.getindex(r::ImmutableRef) = r.x

@inline value_ref(ref::RefSlotRef) = ImmutableRef(ref.ref[].value.x)

allocate_slot(::AbstractVector{Slot}) where {Slot<:Ref} = Ref(Slot())
# One indirection to force heap allocation

@inline function cas_slot!(
    slotref::RefSlotRef{Slot},
    new_slot_ref::Ref{Slot},
    root,
    key,
    value,
) where {P,Slot<:Ref{P}}
    ptr = slotref.ptr
    new_slot = new_slot_ref[]
    new_slot[] = value isa NoValue ? P(key) : P(key, value)
    GC.@preserve new_slot_ref begin
        ou = UInt(pointer_from_objref(slotref.ref))
        nu = UInt(unsafe_load(Ptr{Ptr{Cvoid}}(pointer_from_objref(new_slot_ref))))
        julia_write_barrier(new_slot)
        julia_write_barrier(root, new_slot)
        fu = UnsafeAtomics.cas!(Ptr{typeof(nu)}(ptr), ou, nu)
    end
    return fu == ou
end

make_slot(::Type{P}, k, v) where {P} = P(k, v)
make_slot(::Type{R}, k, v) where {P,R<:Ref{P}} = R(P(k, v))

function Base.getindex(d::LinearProbingDict{Key}, key) where {Key}
    y = tryget(d, key)
    if y === nothing
        throw(KeyError(key))
    else
        return something(y)
    end
end

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
    dict::LinearProbingDict{Key,Value,Slot},
    key,
) where {Key,Value,Slot}
    key = convert(Key, key)
    GC.@preserve dict key begin
        h = reinterpret(Int, hash(key))

        slots = atomic_getfield(dict, Val(:slots))

        if 2 * length_upper_bound(dict) > length(slots)
            slots = expand!(dict, slots)
        end

        # TODO: check if the allocation is eliminated for getindex
        new_slot = allocate_slot(slots)

        while true
            c = length(slots)
            offset = h & (c - 1)  # h % c
            nprobes = 0
            while true
                index = offset + 1
                slotref = load_slot(slots, index)
                sk = getkey(slotref)
                # @show index sk slotref

                if sk isa Union{Moved{Key},MovedEmpty}
                    slots = finishmove!(dict, slots)
                    break  # restart
                elseif sk isa Empty
                    reply = f(nothing)::Union{Nothing,Some}
                    reply === nothing && return reply  # optimization
                    nsk = key
                elseif sk isa Key
                    if isequal(sk, key)
                        vref = value_ref(slotref)
                        reply = f(vref)::Union{Nothing,Some,Keep,Delete}
                        nsk = sk
                    else
                        @goto probing
                    end
                elseif sk isa Deleted
                    @goto probing
                else
                    unexpected(sk)
                end

                if reply isa Keep
                    return reply
                elseif reply isa Union{Nothing,Delete}
                    if cas_slot!(slotref, new_slot, slots, Deleted())
                        ndeleted = Threads.atomic_add!(dict.ndeleted, 1) + 1
                        approx_len = dict.nadded[] - ndeleted
                        if approx_len < length(slots) ÷ 2
                            shrink!(dict, slots)
                        end
                        return reply
                    end
                else
                    if cas_slot!(slotref, new_slot, slots, nsk, something(reply))
                        if sk isa Empty
                            Threads.atomic_add!(dict.nadded, 1)
                        end
                        return reply
                    end
                end
                # TODO: use the key loaded via CAS
                continue  # retry

                @label probing
                nprobes += 1
                if nprobes > c ÷ 4
                    let oldslots = slots
                        slots = atomic_getfield(dict, Val(:slots))
                        # Nonblocking check to see if the slots are migrated:
                        if slots === oldslots
                            # @info "expand: length(slots) ≈ 2^$(floor(Int, log2(length(slots))))"
                            # global DICT = dict
                            slots = expand!(dict, oldslots)
                        end
                    end
                    break  # restart
                end

                offset = (offset + 1) & (c - 1)  # (offset + 1) % c
            end
        end
    end
end

expand!(dict, oldslots) = migrate!(dict, oldslots, true)
shrink!(dict, oldslots) = migrate!(dict, oldslots, false)

function migrate!(dict, oldslots, expand)
    # Since the migration is parallelized, workers running tasks blocked by the
    # lock actually will contribute to the forward progress of the eintire
    # system. (Though the OS may suspend this worker thread before the tasks are
    # spawned.)
    lock(dict.migration) do
        slots = atomic_getfield(dict, Val(:slots))
        if slots !== oldslots
            return slots
        end
        newslots = similar(slots, expand ? length(slots) * 2 : length(slots) ÷ 2)
        if expand
            nadded = expand_parallel!(newslots, slots)
        else
            nadded = migrate!(newslots, slots)
        end
        # TODO: parallelize `shrink!`

        # At this point, no other thread can be mutating the coutners (as they
        # will observe `Moved`). Thus, it is safe to update the counter
        # non-atomically:
        dict.ndeleted[] = 0
        dict.nadded[] = nadded

        # This is the atomic "publlshing" operation that makes the `newslots`
        # accssible to any tasks (including the ones that are/were not trying to
        # acquire the `migration` lock).
        atomic_setfield!(dict, Val(:slots), newslots)

        return newslots
    end
end

function finishmove!(dict, oldslots)
    lock(dict.migration) do
        slots = atomic_getfield(dict, Val(:slots))
        # The caller observed `Moved` which only sets inside the `migration`
        # lock. Thus, the migration should be finished once this lock is
        # acquired:
        @assert oldslots !== slots
        return slots
    end
end

function migrate!(newslots, slots)
    fillempty!(newslots)
    GC.@preserve newslots slots begin
        nadded = migrate_impl!(newslots, slots)::Int
    end
    return nadded
end

struct Stopped
    i::Int
    nadded::Int
end

"""
    expand_parallel_basecase!(newslots, slots, basesize, start0) -> (nadded, seen_empty)

Process all clusters started within `start0:(start0 + basesize)` (mod `lengh(slots)`).

That is to say:

1. Process all clusters started within `start0:(start0 + basesize - 1)`.
2. If more than one cluster is processed, process a cluster in which the start
   position of the next chunk `start0 + basesize` (mod `lengh(slots)`) is included.
"""
function expand_parallel_basecase!(newslots, slots, basesize, start0)
    stop0 = min(start0 + basesize - 1, lastindex(slots))
    stpd = migrate_impl!(nothing, slots, start0, stop0, Val(true))
    if stpd isa Int
        @assert stpd == 0
        # This chunk does not own any clusters.
        return (0, false)
    end

    # An empty slot is observed. There is at least one cluster started within
    # this chunk.
    stpd::Stopped
    @assert stpd.nadded == 0
    nadded = migrate_impl!(newslots, slots, stpd.i + 1, stop0, Val(false))::Int

    # Process the cluster that includes `start0 + basesize` (if any).
    next_start = start0 + basesize
    if next_start > lastindex(slots)
        next_start = firstindex(slots)
    end
    chunk_starts = (
        next_start:basesize:lastindex(slots),
        firstindex(slots):basesize:next_start-1,  # wrap around
    )
    # Using `for half` so that the compiler does not unroll the loop.
    # TODO: check if it is working
    for half in 1:2, start in chunk_starts[half]
        stop = min(start + basesize - 1, lastindex(slots))
        stpd = migrate_impl!(newslots, slots, start, stop, Val(true))
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

function expand_parallel!(newslots, slots)
    @assert length(newslots) > length(slots)

    minimum_basesize = LINEAR_PROBING_DICT_EXPAND_BASESIZE[]  # TODO: configurable?
    length(slots) <= minimum_basesize && return migrate!(newslots, slots)
    basesize = min(minimum_basesize, cld(length(slots), Threads.nthreads()))

    fillempty!(newslots)  # TODO: parallelize?
    nadded, seen_empty = threaded_typed_mapreduce(
        Tuple{Int,Bool},
        plus_or,
        1:basesize:lastindex(slots),
    ) do start0
        return expand_parallel_basecase!(newslots, slots, basesize, start0)
    end
    if seen_empty
        return nadded
    else
        # The `slots` are all non-empty:
        return migrate!(newslots, slots)
    end
end

migrate_impl!(newslots::AbstractVector, slots::AbstractVector) =
    migrate_impl!(newslots, slots, firstindex(slots), lastindex(slots), Val(false))

function migrate_impl!(
    newslots::Union{AbstractVector{Slot},Nothing},
    slots::AbstractVector{Slot},
    start::Int,
    stop::Int,
    stop_on_empty::Union{Val{false},Val{true}},
) where {Slot}
    nadded = 0
    for i in start:stop
        @label reload
        slotref = load_slot(slots, i)
        sk = getkey(slotref)
        if sk isa Deleted
            continue
        elseif sk isa MovedEmpty
            stop_on_empty == Val(true) && return Stopped(i, nadded)
            continue
        elseif sk isa Empty
            # Mark that this slot is not usable anymore
            if !cas_slot!(slotref, allocate_slot(slots), slots, MovedEmpty())
                @goto reload
            end
            stop_on_empty == Val(true) && return Stopped(i, nadded)
            continue
        end

        newslots === nothing && continue

        sv = value_ref(slotref)[]
        # @show i slotref sk sv
        if sk isa Moved
            key = sk.key
        else
            if !cas_slot!(slotref, allocate_slot(slots), slots, Moved(sk), sv)
                @goto reload
            end
            key = sk
        end
        ns = make_slot(Slot, key, sv)
        # TODO: batch allocation

        # Insertion to `newslots` does not have to use atomics since
        # it's protected by the `.migration` lock.
        c = length(newslots)
        h = reinterpret(Int, hash(key))
        offset = h & (c - 1)  # h % c
        nprobes = 0
        while true
            index = offset + 1
            # TODO: non-atomic ordering
            slotref = load_slot(newslots, index)
            sk = getkey(slotref)
            if sk isa Empty
                # @assert newslots[index].key.x.x === sk
                # @show newslots[index]
                # @show index ns
                # TODO: create AlignedArray type so that we don't have
                # to copy pads for each get/set
                @inbounds newslots[index] = ns
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
    GC.@preserve dict begin
        slots = atomic_getfield(dict, Val(:slots))
    end
    return iterate(dict, (slots, firstindex(slots)))
end

function Base.iterate(::LinearProbingDict, (slots, index))
    GC.@preserve slots begin
        index < firstindex(slots) && return nothing
        while true
            index > lastindex(slots) && return nothing
            s = load_slot(slots, index)
            index += 1
            sk = getkey(s)
            sv = value_ref(s)[]
            sk isa Union{Empty,MovedEmpty,Deleted} && continue
            if sk isa Moved
                sk = sk.key
            end
            return (sk => sv), (slots, index)
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
function clusters(
    slots::AbstractVector{Slot},
) where {Slot<:Union{AbstractPair,<:Ref{<:AbstractPair}}}
    cs = typeof(1:2)[]
    i = firstindex(slots)
    while true
        while true
            i > lastindex(slots) && return cs
            slotref = load_slot(slots, i)
            i += 1
            if !(getkey(slotref) isa Union{Empty,MovedEmpty})
                break
            end
        end
        start = i - 1
        while true
            if i > lastindex(slots)
                push!(cs, start:i-1)
                return cs
            end
            slotref = load_slot(slots, i)
            i += 1
            if getkey(slotref) isa Union{Empty,MovedEmpty}
                break
            end
        end
        push!(cs, start:i-1)
    end
end
