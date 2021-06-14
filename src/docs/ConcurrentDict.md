    ConcurrentDict{K,V}()

Concurrent dictionary.  All operations are lock-free except when the dictionary
is resized.

!!! warning
    `ConcurrentDict` is experimental because it cannot be implemented with the
    builtin atomics.

!!! note
    Although tasks `wait` on concurrent modifications (e.g., `setindex!`) during
    resize, the worker threads participate in the resize to avoid wasting CPU
    resources.

# Examples

```julia
julia> using ConcurrentCollections

julia> dict = ConcurrentDict{String,Int}();

julia> dict["hello"] = 1;

julia> dict["hello"]
1
``` 
