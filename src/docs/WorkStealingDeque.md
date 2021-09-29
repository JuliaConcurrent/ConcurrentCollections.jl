    WorkStealingDeque{T}()

Concurrent work-stealing "deque" of objects of type `T`.

This is not a full deque in the sense that:

* `push!` and [`maybepop!`](@ref) operating at the tail of the collection can
  only be executed by a single task.
* [`maybepopfirst!`](@ref) (aka steal) for retrieving and removing an element at
  the head can be invoked from any tasks. However, there is no `pushfirst!`.

# Examples

```julia
julia> using ConcurrentCollections

julia> deque = WorkStealingDeque{Int}();

julia> push!(deque, 1);

julia> push!(deque, 2);

julia> push!(deque, 3);

julia> maybepop!(deque)
Some(3)

julia> fetch(Threads.@spawn maybepopfirst!(deque))
Some(1)

julia> fetch(Threads.@spawn popfirst!(deque))
2

julia> maybepopfirst!(deque)  # returns nothing
``` 

# Extended help

The current implementation is known to be not fully compliant with the C/C++
memory model (on which Julia's memory model is designed).

It implements the dynamic circular work-stealing deque by Chase and Lev (2005):

> Chase, David, and Yossi Lev. “Dynamic Circular Work-Stealing Deque.” In
> Proceedings of the Seventeenth Annual ACM Symposium on Parallelism in
> Algorithms and Architectures, 21–28. SPAA ’05. New York, NY, USA: Association
> for Computing Machinery, 2005. <https://doi.org/10.1145/1073970.1073974>.
