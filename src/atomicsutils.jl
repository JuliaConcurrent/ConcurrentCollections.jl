@inline fieldindex(::T, field::Val) where {T} = fieldindex(T, field)
@generated function fieldindex(::Type{T}, ::Val{field}) where {T,field}
    field isa Symbol || return :(error("`field` must be a symbol; given: $field"))
    return findfirst(n -> n === field, fieldnames(T))
end

@inline function fieldpointer(obj, field::Val)
    offset = fieldoffset(typeof(obj), fieldindex(obj, field))
    return Ptr{UInt}(pointer_from_objref(obj) + offset)
end

@inline atomic_getfield(obj, field::Val) = atomic_getfield(obj, field, seq_cst)
@inline function atomic_getfield(obj, field::Val, order)
    i = something(fieldindex(obj, field))
    offset = fieldoffset(typeof(obj), i)
    fptr = Ptr{UInt}(pointer_from_objref(obj) + offset)
    GC.@preserve obj begin
        uint = UnsafeAtomics.load(fptr, order)
    end
    t = fieldtype(typeof(obj), i)
    value = unsafe_pointer_to_objref(Ptr{Cvoid}(uint))
    return value::t
end
# TODO: support immutables

@inline atomic_setfield!(obj, field::Val, value) =
    atomic_setfield!(obj, field, value, seq_cst)
@inline function atomic_setfield!(obj, field::Val, value, order)
    if Base.issingletontype(typeof(value))
        UnsafeAtomics.store!(
            fieldpointer(obj, field),
            UInt(pointer_from_singleton(value)),
            order,
        )
    else
        fptr = fieldpointer(obj, field)
        ref = Ref{Any}(value)
        GC.@preserve obj ref begin
            vint = unsafe_load(Ptr{UInt}(pointer_from_objref(ref)))
            UnsafeAtomics.store!(fptr, vint, order)
        end
    end
end
# TODO: support immutables

@inline atomic_casfield!(obj, field::Val, cmp, new) =
    atomic_casfield!(obj, field, cmp, new, acq_rel, acquire)
@inline function atomic_casfield!(
    obj,
    field::Val,
    cmp,
    new,
    success_ordering,
    failure_ordering,
)
    fptr = fieldpointer(obj, field)
    cmpint = UInt(_pointer_from_objref(cmp))
    newint = UInt(_pointer_from_objref(new))
    GC.@preserve obj cmp new begin
        found = UnsafeAtomics.cas!(fptr, cmpint, newint, success_ordering, failure_ordering)
    end
    return found == cmpint
end
