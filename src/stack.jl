mutable struct TSNode{T}
    next::Union{TSNode{T},Nothing}
    value::T
end

TSNode{T}(value::T) where {T} = TSNode{T}(nothing, value)

mutable struct ConcurrentStack{T}
    @atomic head::Union{TSNode{T},Nothing}
end

ConcurrentStack{T}() where {T} = ConcurrentStack{T}(nothing)

function Base.push!(stack::ConcurrentStack{T}, v) where {T}
    v = convert(T, v)
    node = TSNode{T}(v)

    head = @atomic(:monotonic, stack.head)
    while true
        node.next = head
        head, ok = @atomicreplace(:release, :monotonic, stack.head, head => node)
        ok && break
    end

    return stack
end

function ConcurrentCollections.maybepop!(stack::ConcurrentStack)
    while true
        head = @atomic(:acquire, stack.head)
        head === nothing && return nothing

        next = head.next
        next, ok = @atomicreplace(:monotonic, :monotonic, stack.head, head => next)
        if ok
            return Some(head.value)
        end
    end
end

function Base.pop!(stack::ConcurrentStack)
    r = maybepop!(stack)
    if r === nothing
        error("stack is empty")
    else
        return something(r)
    end
end
