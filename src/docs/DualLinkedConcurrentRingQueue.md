    DualLinkedConcurrentRingQueue{T}()

A concurrent queue with "almost" nonblocking `push!` and `popfirst!`.  Calling
`popfirst!` on empty queue waits for a `push!` in another task.

See also: [`LinkedConcurrentRingQueue`](@ref)

# Examples
```julia
julia> using ConcurrentCollections

julia> q = DualLinkedConcurrentRingQueue{Int}();

julia> push!(q, 111);

julia> push!(q, 222);

julia> popfirst!(q)  # first-in first-out
111

julia> popfirst!(q)
222
```

# Extended help

Since `popfirst!` blocks when called on an empty queue, a
`DualLinkedConcurrentRingQueue` acts almost like an unbounded `Base.Channel`.
However, `DualLinkedConcurrentRingQueue` does not support `close` or blocking on
`push!` when exceeding a bound.

`DualLinkedConcurrentRingQueue` performs very well compared to other concurrent
queue implementations. However, since it is based on linked fixed-size buffers,
it has relatively large memory overhead.

`DualLinkedConcurrentRingQueue` is based on the linked multi-polarity dual ring
queue by Izraelevitz and Scott (2017):

> Izraelevitz, Joseph, and Michael L. Scott. “Generality and Speed in
> Nonblocking Dual Containers.” ACM Transactions on Parallel Computing 3, no. 4
> (March 23, 2017): 22:1–22:37. <https://doi.org/10.1145/3040220>.
