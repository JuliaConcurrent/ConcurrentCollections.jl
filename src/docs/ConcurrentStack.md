    ConcurrentStack{T}()

Concurrent stack of objects of type `T`.

Use `push!` to insert an element and [`maybepop!`](@ref) to retrieve and remove an
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

julia> maybepop!(stack)
Some(1)

julia> maybepop!(stack)  # returns nothing
``` 
