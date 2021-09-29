    maybepop!(collection) -> Some(value::T) or nothing

Try to pop a `value` from the tail of `collection`. Return `Some(value)` if it
is non-empty.  Return `nothing` if empty.

# Examples

```julia
julia> using ConcurrentCollections

julia> stack = ConcurrentStack{Int}();

julia> push!(stack, 1);

julia> maybepop!(stack)
Some(1)

julia> maybepop!(stack)  # returns nothing
``` 
