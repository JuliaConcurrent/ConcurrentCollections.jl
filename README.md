# ConcurrentCollections

[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/)

ConcurrentCollections.jl provides the following concurrent collections for Julia
≥ 1.7. Most of their operations are (almost) lock-free whenever appropriate.

* [`DualLinkedConcurrentRingQueue`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.DualLinkedConcurrentRingQueue)
* [`DualLinkedQueue`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.DualLinkedQueue)
* [`LinkedConcurrentRingQueue`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.LinkedConcurrentRingQueue)
* [`ConcurrentQueue`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.ConcurrentQueue)
* [`ConcurrentStack`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.ConcurrentStack)
* [`WorkStealingDeque`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.WorkStealingDeque)
* [`ConcurrentDict`](https://juliaconcurrent.github.io/ConcurrentCollections.jl/dev/#ConcurrentCollections.ConcurrentDict)

**NOTE**: If you are trying to find a way to improve performance (especially
the *throughput*) of your program, it is highly recommended to look for ways to
**avoid** using concurrent collections first.  In particular, consider applying
the [data-parallel](https://juliafolds.github.io/data-parallelism/) pattern to
dodge the difficulties in concurrent programming.  For example, it is often a
better idea to use task-local copies of **non**-thread-safe `Dict` instead of
`ConcurrentDict` shared across tasks. For more information, see: [Efficient and
safe approaches to mutation in data
parallelism](https://juliafolds.github.io/data-parallelism/tutorials/mutations/).
