    DualLinkedQueue{T}()

A concurrent queue with nonblocking `push!` and `popfirst!`.  Calling
`popfirst!` on an empty queue waits for a `push!` in another task.

[`DualLinkedConcurrentRingQueue`](@ref) provides a faster dual queue with a
larger memory footprint.

# Examples
```julia
julia> using ConcurrentCollections

julia> q = DualLinkedQueue{Int}();

julia> push!(q, 111);

julia> push!(q, 222);

julia> popfirst!(q)  # first-in first-out
111

julia> popfirst!(q)
222
```

# Extended help

Since `popfirst!` blocks when called on an empty queue, a `DualLinkedQueue` acts
almost like an unbounded `Base.Channel`.  However, `DualLinkedQueue` does not
support `close` or blocking on `push!` when exceeding a bound.

`DualLinkedQueue` implements the dual queue by Scherer and Scott (2004):

> Scherer, William N., and Michael L. Scott. “Nonblocking Concurrent Data
> Structures with Condition Synchronization.” In Distributed Computing, edited
> by Rachid Guerraoui, 174–87. Lecture Notes in Computer Science. Berlin,
> Heidelberg: Springer, 2004. <https://doi.org/10.1007/978-3-540-30186-8_13>.
