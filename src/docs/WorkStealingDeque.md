    WorkStealingDeque{T}()

Concurrent work-stealing "deque" of objects of type `T`.

This is not a full deque in the sense that:

* `push!` and [`trypop!`](@ref) operating at the tail of the collection can
  only be executed by a single task.
* [`trypopfirst!`](@ref) (aka steal) for retrieving and removing an element at
  the head can be invoked from any tasks. However, there is no `pushfirst!`.

Implementation detail: It implements the dynamic circular work-stealing deque by
Chase and Lev (2005).

# Examples

```julia
julia> using ConcurrentCollections

julia> deque = WorkStealingDeque{Int}();

julia> push!(deque, 1);

julia> push!(deque, 2);

julia> push!(deque, 3);

julia> trypop!(deque)
Some(3)

julia> fetch(Threads.@spawn trypopfirst!(deque))
Some(1)

julia> fetch(Threads.@spawn popfirst!(deque))
2

julia> trypopfirst!(deque)  # returns nothing
``` 
