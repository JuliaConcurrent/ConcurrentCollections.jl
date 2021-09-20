# This is a `mutable struct` ATM so that it can be replaced with a 64 bit CAS.
# TODO: make it an immutable `struct` and compute `log2length` on-the-fly?
mutable struct CircularVector{T} <: AbstractVector{T}
    log2length::Int
    data::Vector{T}
end

function CircularVector{T}(log2length::Int) where {T}
    @assert log2length >= 0
    data = Vector{T}(undef, 1 << log2length)
    Nothing <: T && fill!(data, nothing)  # [^speculative_load]
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
    if is_pointerfree_type(T)
        S = T
    else
        # Any element type that may contain a boxed object uses
        # `CircularVector{Any}` which initialize all locations with `nothing`.
        # This is for supporting "blind" `buffer[top]`.  [^speculative_load]
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

function ConcurrentCollections.trypopfirst!(deque::WorkStealingDeque{T}) where {T}
    top = @atomic deque.top
    bottom = @atomic deque.bottom
    buffer = @atomic deque.buffer
    current_size = bottom - top
    if current_size <= 0
        return nothing
    end
    # TODO: Technically, this should be an atomic load.  See below for some
    # discussions.  [^speculative_load]
    y = buffer[top]
    if @atomicreplace(deque.top, top => top + 1)[2]
        y = y::T
        return Some{T}(y)
    else
        return nothing
    end
end
# [^speculative_load]: The difficulty here is that we don't know the validity of
# `y = buffer[top]` until the CAS below but we still need to make sure `y` is a
# ("somewhat") valid Julia object since it has to be rooted before the CAS.
# This is because successful CAS means that `buffer` may not root `y` anymore.
# This can be an issue if the compiler inserts a safepoint between `buffer[top]`
# and the CAS.  The current implementation relies on the stop-the-world behavior
# of the GC (the "somewhat" part).  That is to say, the object `y` is not
# *always* properly accessible at the (potential) safepoint before the CAS.
# However, since the GC (currently) starts traversing the object graph after it
# knows all worker threads reach the safepoint, the implementation above may
# actually be OK for now.
#
# Another issue is that `buffer[top]` may not be assigned and accessing it can
# throw if the element type is a boxed value.  The current workaround is to fill
# `buffer` with `nothing`. Alternatively, checking the buffer with `isassigned`
# should also work. However, since dequeue can happen many times while the
# buffer is initialized less frequently (upon construction and resizes),
# the `isassigned`-based strategy is not used.
# 
# The current implementation also expecting that it is "OK" to load a
# pointerfree value non-atomically (it's a data race in C++20 memory model hence
# an UB). A proper solution may be to store/load these values as a sequence of
# `UInt`s with relaxed atomics and then type-pun `NTuple{_,UInt}` into the Julia
# values.  See also the discussion in the C++ standard: [p0690r1: Tearable
# Atomics](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0690r1.html)

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
