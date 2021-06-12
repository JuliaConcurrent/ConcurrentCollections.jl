    trypopfirst!(collection) -> Some(value::T) or nothing


Try to pop a `value` from the head of `collection`. Return `Some(value)` if it
is non-empty.  Return `nothing` if empty.

# Examples

```julia
julia> using ConcurrentCollections

julia> queue = ConcurrentQueue{Int}();

julia> push!(queue, 1);

julia> trypopfirst!(queue)
Some(1)

julia> trypopfirst!(queue)  # returns nothing
``` 
