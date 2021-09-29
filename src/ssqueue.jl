struct IsData end
struct IsAntiData end
const PolarityTrait = Union{IsData,IsAntiData}

Base.adjoint(::IsData) = IsAntiData()
Base.adjoint(::IsAntiData) = IsData()

abstract type AbstractSSQNode{T} end

mutable struct SSQDataNode{T} <: AbstractSSQNode{T}
    @atomic next::Union{AbstractSSQNode{T},Nothing}
    value::T

    SSQDataNode{T}() where {T} = new{T}(nothing)
    SSQDataNode{T}(next::Union{AbstractSSQNode{T},Nothing}, value::T) where {T} =
        new{T}(next, value)
end

mutable struct SSQWaiterNode{T} <: AbstractSSQNode{T}
    @atomic next::Union{AbstractSSQNode{T},Nothing}
    value::Waiter{T}

    SSQWaiterNode{T}() where {T} = new{T}(nothing)
    SSQWaiterNode{T}(next::Union{AbstractSSQNode{T},Nothing}, value::Waiter{T}) where {T} =
        new{T}(next, value)
end

const SSQNode{T} = Union{SSQDataNode{T},SSQWaiterNode{T}}

polarityof(::SSQDataNode) = IsData
polarityof(::SSQWaiterNode) = IsAntiData
ssqnodetype(::IsData, ::Type{T}) where {T} = SSQDataNode{T}
ssqnodetype(::IsAntiData, ::Type{T}) where {T} = SSQWaiterNode{T}

#=
mutable struct SSQNode{Polarity<:PolarityTrait,T,V<:Union{T,Waiter{T}}}
    @atomic next::Union{SSQNode{IsData,T,T},SSQNode{IsAntiData,T,Waiter{T}},Nothing}
    value::V

    SSQNode{Polarity,T}() where {Polarity,T} = new{Polarity,T,vtype(Polarity(), T)}(nothing)
    function SSQNode{Polarity,T}(
        next::Union{SSQNode{<:Any,T},Nothing},
        value::Union{T,Waiter{T}},
    ) where {Polarity,T}
        V = vtype(Polarity(), T)
        value = value::V
        return new{Polarity,T,V}(next, value)
    end
end

@inline SSQNode{Polarity,T,V}(
    next::Union{SSQNode{<:Any,T},Nothing},
    value::Union{T,Waiter{T}},
) where {Polarity,T,V} = SSQNode{Polarity,T}(next, value)::SSQNode{Polarity,T,V}

polarityof(::SSQNode{Polarity}) where {Polarity} = Polarity
=#

mutable struct DualLinkedQueue{T}
    @atomic head::SSQNode{T}
    @atomic tail::SSQNode{T}
end

Base.eltype(::Type{DualLinkedQueue{T}}) where {T} = T

DualLinkedQueue() = DualLinkedQueue{Any}()
function DualLinkedQueue{T}() where {T}
    n = SSQDataNode{T}()
    return DualLinkedQueue{T}(n, n)
end

function denqueue!(
    queue::DualLinkedQueue{T},
    x::Union{T,Waiter{T}},
    polarity::PolarityTrait,
) where {T}
    local node::Union{Nothing,SSQNode{T}} = nothing

    head = @atomic queue.head
    tail = @atomic queue.tail
    while true
        next = (@atomic head.next)::Union{Nothing,SSQNode{T}}
        head′ = @atomic queue.head
        if head !== head′  # snapshot failed
            head = head′
            tail = @atomic queue.tail
            continue
        end
        if head === tail
            last = next
            should_enqueue = true
        elseif polarity isa polarityof(next)
            last = @atomic tail.next
            should_enqueue = true
        else
            last = nothing
            should_enqueue = false
        end

        if should_enqueue
            tail′ = @atomic queue.tail
            if tail′ !== tail  # snapshot failed
                tail = tail′
                continue
            end
            if last !== nothing
                old, ok = @atomicreplace(queue.tail, tail => last)
                tail = ok ? next : old
                continue
            end
            node = if node === nothing
                ssqnodetype(polarity, T)(nothing, x)
            else
                node
            end::ssqnodetype(polarity, T)
            last, ok = @atomicreplace(tail.next, nothing => node)
            if ok
                @atomicreplace(queue.tail, tail => node)
                return nothing
            end
            last = last::SSQNode{T}  # can be any polarity
            old, ok = @atomicreplace(queue.tail, tail => last)
            tail = ok ? last : old
        else
            next = next::ssqnodetype(polarity', T)
            value = next.value
            head, ok = @atomicreplace(queue.head, head => next)
            if ok
                return Some(value)
            end
            tail = @atomic queue.tail
        end
    end
end

function Base.push!(queue::DualLinkedQueue{T}, x) where {T}
    x = convert(T, x)
    while true
        y = denqueue!(queue, x, IsData())
        if y isa Some
            w = something(y)::Waiter{T}
            tryput!(w, x) || continue
        else
            y::Nothing
        end
        return queue
    end
end

function Base.popfirst!(queue::DualLinkedQueue{T}) where {T}
    # TODO: cache Waiter
    w = Waiter{T}()
    y = denqueue!(queue, w, IsAntiData())
    if y isa Some
        return something(y)
    else
        y::Nothing
        x = fetch(w)
        return x::T
    end
end

Base.IteratorSize(::Type{<:DualLinkedQueue}) = Base.SizeUnknown()

function Base.iterate(queue::DualLinkedQueue{T}) where {T}
    head = @atomic queue.head
    node = @atomic head.next
    if node === nothing
        return nothing
    else
        if node isa SSQWaiterNode
            return nothing
        else
            return (node.value, node)
        end
    end
end

function Base.iterate(::DualLinkedQueue{T}, prev::SSQDataNode) where {T}
    node = (@atomic prev.next)::Union{Nothing,SSQDataNode}
    if node === nothing
        return nothing
    else
        return (node.value, node)
    end
end

struct NodeIterator{T}
    queue::T
end

Base.IteratorSize(::Type{<:NodeIterator}) = Base.SizeUnknown()

Base.eltype(::Type{NodeIterator{DualLinkedQueue{T}}}) where {T} = SSQNode{T}

function Base.iterate(
    iter::NodeIterator{DualLinkedQueue{T}},
    prev::SSQNode{T} = let queue = iter.queue
        @atomic queue.head
    end,
) where {T}
    node = (@atomic prev.next)::Union{Nothing,SSQNode{T}}
    if node === nothing
        return nothing
    else
        return (node, node)
    end
end

function check_invariance(queue::DualLinkedQueue{T}) where {T}
    isdata = nothing
    for node in NodeIterator(queue)
        if isdata === nothing
            isdata = node isa SSQDataNode
        elseif isdata !=′ (node isa SSQDataNode)
            return false
        end
    end
    return true
end

function summaryinfo(queue::DualLinkedQueue{T}) where {T}
    counter = Ref(0)
    isdata = Ref(true)
    for node in NodeIterator(queue)
        isdata[] = !(node isa SSQWaiterNode)
        counter[] += 1
    end
    return (; nitems = counter[], isdata = isdata[])
end

function Base.summary(io::IO, queue::DualLinkedQueue)
    show(io, MIME"text/plain"(), typeof(queue))
    nitems, isdata = summaryinfo(queue)
    s = nitems > 1 ? "s" : ""
    print(io, " with ", nitems, isdata ? " data item$s" : " waiter$s")
end

function Base.show(io::IO, ::MIME"text/plain", queue::DualLinkedQueue)
    summary(io, queue)
    eio = IOContext(io, :typeinfo => eltype(queue), :limit => true, :compact => true)
    n = 0
    for x in queue
        if n == 0
            println(io, ":")
        else
            println(io)
        end
        n += 1
        if n > 3
            print(io, "  ⋮")
            return
        end
        print(io, "  ")
        show(eio, MIME"text/plain"(), x)
    end
end
