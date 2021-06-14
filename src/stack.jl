mutable struct TSNode{T}
    value::T
    @atomic next::Union{TSNode{T},Nothing}
end

TSNode{T}(value::T) where {T} = TSNode{T}(value, nothing)

mutable struct ConcurrentStack{T}
    @atomic next::Union{TSNode{T},Nothing}
end

ConcurrentStack{T}() where {T} = ConcurrentStack{T}(nothing)

function Base.push!(stack::ConcurrentStack{T}, v) where {T}
    v = convert(T, v)
    node = TSNode{T}(v)

    next = @atomic stack.next
    while true
        @atomic node.next = next
        next, ok = @atomicreplace(stack.next, next => node)
        ok && break
    end

    return stack
end

function ConcurrentCollections.trypop!(stack::ConcurrentStack)
    while true
        node = @atomic stack.next
        node === nothing && return nothing

        next = @atomic node.next
        next, ok = @atomicreplace(stack.next, node => next)
        if ok
            return Some(node.value)
        end
    end
end

function Base.pop!(stack::ConcurrentStack)
    r = trypop!(stack)
    if r === nothing
        error("stack is empty")
    else
        return something(r)
    end
end
