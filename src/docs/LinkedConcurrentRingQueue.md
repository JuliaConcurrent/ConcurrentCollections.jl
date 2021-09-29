    LinkedConcurrentRingQueue{T}()

A concurrent queue with nonblocking `push!` and [`maybepopfirst!`](@ref).

See also: [`DualLinkedConcurrentRingQueue`](@ref)

# Examples
```julia
julia> using ConcurrentCollections

julia> q = LinkedConcurrentRingQueue{Int}();

julia> push!(q, 111);

julia> push!(q, 222);

julia> maybepopfirst!(q)  # first-in first-out
Some(111)

julia> maybepopfirst!(q)
Some(222)

julia> maybepopfirst!(q) === nothing  # queue is empty
true
```

# Extended help

`LinkedConcurrentRingQueue` is based on Linked Concurrent Ring Queue (or List of
Concurrent Ring Queues; LCRQ) by Morrison and Afek (2013):

> Morrison, Adam, and Yehuda Afek. “Fast Concurrent Queues for X86 Processors.”
> In Proceedings of the 18th ACM SIGPLAN Symposium on Principles and Practice of
> Parallel Programming, 103–112. PPoPP ’13. New York, NY, USA: Association for
> Computing Machinery, 2013. <https://doi.org/10.1145/2442516.2442527>.
> (Revised version:
> <https://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf>)
