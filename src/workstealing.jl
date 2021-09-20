# This is a `mutable struct` ATM so that it can be replaced with a 64 bit CAS.
# TODO: make it an immutable `struct` and compute `log2length` on-the-fly?
mutable struct CircularVector{T} <: AbstractVector{T}
    log2length::Int
    data::Vector{T}
end

function CircularVector{T}(log2length::Int) where {T}
    @assert log2length >= 0
    data = Vector{T}(undef, 1 << log2length)
    T <: Signed && fill!(data, typemin(T))
    return CircularVector{T}(log2length, data)
end

Base.size(A::CircularVector{T}) where {T} = size(A.data)

@noinline function _check_data_length(A::CircularVector)
    @assert 1 << A.log2length == length(A.data)
end

Base.@propagate_inbounds function indexof(A::CircularVector, i::Int)
    @boundscheck _check_data_length(A)
    return (i - 1) & ((1 << A.log2length) - 1) + 1
end

Base.@propagate_inbounds Base.getindex(A::CircularVector, i::Int) = A.data[indexof(A, i)]

Base.@propagate_inbounds function Base.setindex!(A::CircularVector, v, i::Int)
    v = convert(eltype(A), v)
    A.data[indexof(A, i)] = v
end

Base.pointer(A::CircularVector, i::Integer) = pointer(A.data, indexof(A, i))

function tryresize(A::CircularVector, log2inc::Integer, indices)
    log2length = A.log2length + log2inc
    n = 1 << log2length
    n < length(indices) && return nothing
    B = CircularVector{eltype(A)}(log2length)
    for i in indices
        @inbounds B[i] = A[i]
    end
    return B
end

mutable struct WorkStealingDeque{T,S}
    @atomic buffer::CircularVector{S}
    @atomic top::Int
    @atomic bottom::Int
    # TODO: pad
end

function WorkStealingDeque{T}() where {T}
    if isbitstype(T) || Base.isbitsunion(T)
        # TODO: support Some{Union{Int,Nothing}} etc.
        S = T
    else
        S = Any
    end
    return WorkStealingDeque{T,S}(CircularVector{S}(4), 1, 1)
end

Base.eltype(::Type{WorkStealingDeque{T}}) where {T} = T

function Base.length(deque::WorkStealingDeque)
    bottom = @atomic deque.bottom
    top = @atomic deque.top
    return bottom - top
end

function try_resize_buffer!(
    deque::WorkStealingDeque,
    buffer::CircularVector,
    top::Int,
    bottom::Int,
    log2inc::Integer,
)
    target_size = 1 << (buffer.log2length + log2inc)
    current_size = bottom - top
    if target_size <= max(current_size, 4)
        # too large to shrink
        return nothing
    end

    buffer = tryresize(buffer, log2inc, top:bottom-1)
    if buffer === nothing
        return nothing
    else
        @atomic deque.buffer = buffer
        return buffer
    end
end

grow!(deque::WorkStealingDeque, buffer, top, bottom) =
    something(try_resize_buffer!(deque, buffer, top, bottom, 1))
tryshrink!(deque::WorkStealingDeque, buffer, top, bottom) =
    try_resize_buffer!(deque, buffer, top, bottom, -1)

function Base.push!(deque::WorkStealingDeque, v)
    v = convert(eltype(deque), v)
    bottom = @atomic deque.bottom
    top = @atomic deque.top
    buffer = @atomic deque.buffer
    current_size = bottom - top
    if current_size >= length(buffer) - 1
        buffer = grow!(deque, buffer, top, bottom)
        @atomic deque.buffer = buffer
    end
    # TODO: Technically, this should use atomic store. However, there is no way
    # to do this in a GC-compatible way at the moment.
    buffer[bottom] = v
    bottom += 1
    @atomic deque.bottom = bottom
    return deque
end

function ConcurrentCollections.trypop!(deque::WorkStealingDeque)
    bottom = @atomic deque.bottom
    buffer = @atomic deque.buffer
    bottom -= 1
    @atomic deque.bottom = bottom
    top = @atomic deque.top
    next_size = bottom - top
    if next_size < 0
        @atomic deque.bottom = top
        return nothing
    end
    # TODO: Technically, this should also use atomic load
    r = Some(buffer[bottom])
    if next_size > 0
        tryshrink!(deque, buffer, top, bottom)
        return r
    end
    bottom = top + 1
    if !@atomicreplace(deque.top, top => top + 1)[2]
        r = nothing
    end
    @atomic deque.bottom = bottom
    return r
end

function ConcurrentCollections.trypopfirst!(deque::WorkStealingDeque)
    top = @atomic deque.top
    bottom = @atomic deque.bottom
    buffer = @atomic deque.buffer
    current_size = bottom - top
    if current_size <= 0
        return nothing
    end
    if Base.allocatedinline(eltype(buffer))
        r = Some(buffer[top])
        if @atomicreplace(deque.top, top => top + 1)[2]
            return r
        else
            return nothing
        end
    else
        ptr = UnsafeAtomics.load(Ptr{Ptr{Cvoid}}(pointer(buffer, top)), monotonic)
        if @atomicreplace(deque.top, top => top + 1)[2]
            # Safety: The above CAS verifies that the slot `buffer[top]`
            # contained the valid element. We can now materialize it as a Julia
            # value.
            GC.@preserve buffer begin
                r = Some(unsafe_pointer_to_objref(ptr))
            end
            return r
        else
            return nothing
        end
    end
end

function Base.pop!(deque::WorkStealingDeque)
    r = trypop!(deque)
    if r === nothing
        error("deque is empty")
    else
        return something(r)
    end
end

function Base.popfirst!(deque::WorkStealingDeque)
    r = trypopfirst!(deque)
    if r === nothing
        error("deque is empty")
    else
        return something(r)
    end
end

function Base.sizehint!(deque::WorkStealingDeque, n::Integer)
    bottom = @atomic deque.bottom
    top = @atomic deque.top
    buffer = @atomic deque.buffer
    log2inc = ceillog2(n) - buffer.log2length
    if log2inc != 0
        try_resize_buffer!(deque, buffer, top, bottom, log2inc)
    end
    return deque
end

#=
struct WorkStealingDequeSet{T}
    deques::Vector{WorkStealingDeque{T}}
end

WorkStealingDequeSet{T}() where {T} =
    WorkStealingDequeSet{T}([WorkStealingDeque{T}() for _ in 1:Threads.nthreads()])
=#
