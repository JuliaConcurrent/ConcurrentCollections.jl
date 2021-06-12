    ConcurrentStack{T}()

Concurrent stack of objects of type `T`.

Use `push!` to insert an element and [`trypop!`](@ref) to retrieve and remove an
element.

It implements the Treiber stack.

# Examples

```julia
julia> using ConcurrentCollections

julia> stack = ConcurrentStack{Int}();

julia> push!(stack, 1);

julia> push!(stack, 2);

julia> pop!(stack)
2

julia> trypop!(stack)
Some(1)

julia> trypop!(stack)  # returns nothing
``` 
