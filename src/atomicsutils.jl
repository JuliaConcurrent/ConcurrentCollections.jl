@inline fieldindex(::T, field::Val) where {T} = fieldindex(T, field)
@generated function fieldindex(::Type{T}, ::Val{field}) where {T,field}
    field isa Symbol || return :(error("`field` must be a symbol; given: $field"))
    return findfirst(n -> n === field, fieldnames(T))
end

@inline function fieldpointer(obj, field::Val)
    offset = fieldoffset(typeof(obj), fieldindex(obj, field))
    return Ptr{UInt}(pointer_from_objref(obj) + offset)
end

@inline atomic_modifyfield!(obj, field::Val, op, x) =
    atomic_modifyfield!(obj, field, op, x, seq_cst)
@inline function atomic_modifyfield!(obj, field::Val, op::OP, x, order) where {OP}
    FieldType = fieldtype(typeof(obj), fieldindex(obj, field))
    fptr = Ptr{FieldType}(fieldpointer(obj, field))
    v = convert(FieldType, x)
    GC.@preserve obj begin
        old = UnsafeAtomics.modify!(fptr, op, v, order)
    end
    return old
end
