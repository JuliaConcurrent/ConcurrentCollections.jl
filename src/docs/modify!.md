    modify!(f, dict::ConcurrentDict{K,V}, key::K) -> y

Atomically update `key` slot of `dict` using a function `f`.

If `key` does not exist, `f` is called with `nothing`. The call `f(nothing)`
must return either (1) `nothing` to keep the slot unoccupied or (2)
`Some(value::V)` to insert `value`.

If `key` exist, `f` is called with a `ref` such that `ref[]` retrieves the
`value` corresponding to the `key`.  The call `f(ref)` must return either (1)
`nothing` to delete the slot, (2) `Some(value′::V)` to insert `value`, (3)
[`Keep(ans)`](@ref ConcurrentCollection.Keep) to return `y = Keep(ans)` from
`modify!`, or (4) [`Delete(ans)`](@ref ConcurrentCollection.Delete) to delete
slot and return a value `y = Delete(ans)` from `modify!`.

The function `f` may be called more than once if multiple tasks try to modify
the dictionary.

# Examples

```julia
julia> using ConcurrentCollections

julia> dict = ConcurrentDict{String,Int}();

julia> modify!(dict, "hello") do _
           Some(1)
       end
Some(1)

julia> modify!(dict, "hello") do ref
           Some(something(ref[]) + 1)
       end
Some(2)
```
