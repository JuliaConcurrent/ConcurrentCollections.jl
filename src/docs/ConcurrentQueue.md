    ConcurrentQueue{T}()

Concurrent queue of objects of type `T`.

Use `push!` to insert an element at the tail and [`maybepopfirst!`](@ref) to
retrieve and remove an element at the head.

Implementation detail: It implements the Michael and Scott queue.

# Examples

```julia
julia> using ConcurrentCollections

julia> queue = ConcurrentQueue{Int}();

julia> push!(queue, 1);

julia> push!(queue, 2);

julia> popfirst!(queue)
1

julia> maybepopfirst!(queue)
Some(2)

julia> maybepopfirst!(queue)  # returns nothing
``` 
