mutable struct MSQNode{T}
    @atomic next::Union{MSQNode{T},Nothing}
    @atomic value::T

    MSQNode{T}() where {T} = new{T}(nothing)
    MSQNode{T}(next::Union{MSQNode{T},Nothing}, value::T) where {T} = new{T}(next, value)
end

mutable struct ConcurrentQueue{T}
    @atomic head::MSQNode{T}
    @atomic tail::MSQNode{T}
end

ConcurrentQueue() = ConcurrentQueue{Any}()
function ConcurrentQueue{T}() where {T}
    n = MSQNode{T}()
    return ConcurrentQueue{T}(n, n)
end

Base.eltype(::Type{ConcurrentQueue{T}}) where {T} = T

function Base.push!(queue::ConcurrentQueue{T}, v) where {T}
    v = convert(T, v)
    node = MSQNode{T}(nothing, v)

    tail = @atomic queue.tail
    while true
        next = @atomic tail.next
        if next === nothing
            next, ok = @atomicreplace(tail.next, next => node)
            if ok
                @atomicreplace(queue.tail, tail => node)
                return queue
            end
            tail = @atomic queue.tail
        else
            tail, ok = @atomicreplace(queue.tail, tail => next)
            if ok
                tail = next
            end
        end
    end
end

function ConcurrentCollections.trypopfirst!(queue::ConcurrentQueue)
    head = @atomic queue.head
    tail = @atomic queue.tail
    while true
        node = @atomic head.next
        head′ = @atomic queue.head
        if head === head′
            if head === tail
                if node === nothing
                    return nothing
                end
                tail, _ = @atomicreplace(queue.tail, tail => node)
                continue  # not need to reload tail
            else
                node::MSQNode
                head, ok = @atomicreplace(queue.head, head => node)
                if ok
                    return Some(node.value)
                end
            end
        else
            head = head′
        end
        tail = @atomic queue.tail
    end
end

function Base.popfirst!(queue::ConcurrentQueue)
    r = trypopfirst!(queue)
    if r === nothing
        error("queue is empty")
    else
        return something(r)
    end
end
