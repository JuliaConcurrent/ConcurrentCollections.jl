var documenterSearchIndex = {"docs":
[{"location":"#ConcurrentCollections.jl","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.jl","text":"","category":"section"},{"location":"","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.jl","text":"","category":"page"},{"location":"#Queue/stack","page":"ConcurrentCollections.jl","title":"Queue/stack","text":"","category":"section"},{"location":"","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.jl","text":"DualLinkedConcurrentRingQueue\nDualLinkedQueue\nLinkedConcurrentRingQueue\nConcurrentQueue\nConcurrentStack\nWorkStealingDeque\nmaybepop!\nmaybepopfirst!","category":"page"},{"location":"#ConcurrentCollections.DualLinkedConcurrentRingQueue","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.DualLinkedConcurrentRingQueue","text":"DualLinkedConcurrentRingQueue{T}()\n\nA concurrent queue with \"almost\" nonblocking push! and popfirst!.  Calling popfirst! on an empty queue waits for a push! in another task.\n\nSee also: LinkedConcurrentRingQueue, DualLinkedQueue\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> q = DualLinkedConcurrentRingQueue{Int}();\n\njulia> push!(q, 111);\n\njulia> push!(q, 222);\n\njulia> popfirst!(q)  # first-in first-out\n111\n\njulia> popfirst!(q)\n222\n\nExtended help\n\nSince popfirst! blocks when called on an empty queue, a DualLinkedConcurrentRingQueue acts almost like an unbounded Base.Channel. However, DualLinkedConcurrentRingQueue does not support close or blocking on push! when exceeding a bound.\n\nDualLinkedConcurrentRingQueue performs very well compared to other concurrent queue implementations. However, since it is based on linked fixed-size buffers, it has relatively large memory overhead.\n\nDualLinkedConcurrentRingQueue is based on the linked multi-polarity dual ring queue by Izraelevitz and Scott (2017):\n\nIzraelevitz, Joseph, and Michael L. Scott. “Generality and Speed in Nonblocking Dual Containers.” ACM Transactions on Parallel Computing 3, no. 4 (March 23, 2017): 22:1–22:37. https://doi.org/10.1145/3040220.\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.DualLinkedQueue","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.DualLinkedQueue","text":"DualLinkedQueue{T}()\n\nA concurrent queue with nonblocking push! and popfirst!.  Calling popfirst! on an empty queue waits for a push! in another task.\n\nDualLinkedConcurrentRingQueue provides a faster dual queue with a larger memory footprint.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> q = DualLinkedQueue{Int}();\n\njulia> push!(q, 111);\n\njulia> push!(q, 222);\n\njulia> popfirst!(q)  # first-in first-out\n111\n\njulia> popfirst!(q)\n222\n\nExtended help\n\nSince popfirst! blocks when called on an empty queue, a DualLinkedQueue acts almost like an unbounded Base.Channel.  However, DualLinkedQueue does not support close or blocking on push! when exceeding a bound.\n\nDualLinkedQueue implements the dual queue by Scherer and Scott (2004):\n\nScherer, William N., and Michael L. Scott. “Nonblocking Concurrent Data Structures with Condition Synchronization.” In Distributed Computing, edited by Rachid Guerraoui, 174–87. Lecture Notes in Computer Science. Berlin, Heidelberg: Springer, 2004. https://doi.org/10.1007/978-3-540-30186-8_13.\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.LinkedConcurrentRingQueue","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.LinkedConcurrentRingQueue","text":"LinkedConcurrentRingQueue{T}()\n\nA concurrent queue with nonblocking push! and maybepopfirst!.\n\nSee also: DualLinkedConcurrentRingQueue\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> q = LinkedConcurrentRingQueue{Int}();\n\njulia> push!(q, 111);\n\njulia> push!(q, 222);\n\njulia> maybepopfirst!(q)  # first-in first-out\nSome(111)\n\njulia> maybepopfirst!(q)\nSome(222)\n\njulia> maybepopfirst!(q) === nothing  # queue is empty\ntrue\n\nExtended help\n\nLinkedConcurrentRingQueue is based on Linked Concurrent Ring Queue (or List of Concurrent Ring Queues; LCRQ) by Morrison and Afek (2013):\n\nMorrison, Adam, and Yehuda Afek. “Fast Concurrent Queues for X86 Processors.” In Proceedings of the 18th ACM SIGPLAN Symposium on Principles and Practice of Parallel Programming, 103–112. PPoPP ’13. New York, NY, USA: Association for Computing Machinery, 2013. https://doi.org/10.1145/2442516.2442527. (Revised version: https://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf)\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.ConcurrentQueue","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.ConcurrentQueue","text":"ConcurrentQueue{T}()\n\nConcurrent queue of objects of type T.\n\nUse push! to insert an element at the tail and maybepopfirst! to retrieve and remove an element at the head.\n\nImplementation detail: It implements the Michael and Scott queue.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> queue = ConcurrentQueue{Int}();\n\njulia> push!(queue, 1);\n\njulia> push!(queue, 2);\n\njulia> popfirst!(queue)\n1\n\njulia> maybepopfirst!(queue)\nSome(2)\n\njulia> maybepopfirst!(queue)  # returns nothing\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.ConcurrentStack","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.ConcurrentStack","text":"ConcurrentStack{T}()\n\nConcurrent stack of objects of type T.\n\nUse push! to insert an element and maybepop! to retrieve and remove an element.\n\nIt implements the Treiber stack.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> stack = ConcurrentStack{Int}();\n\njulia> push!(stack, 1);\n\njulia> push!(stack, 2);\n\njulia> pop!(stack)\n2\n\njulia> maybepop!(stack)\nSome(1)\n\njulia> maybepop!(stack)  # returns nothing\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.WorkStealingDeque","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.WorkStealingDeque","text":"WorkStealingDeque{T}()\n\nConcurrent work-stealing \"deque\" of objects of type T.\n\nThis is not a full deque in the sense that:\n\npush! and maybepop! operating at the tail of the collection can only be executed by a single task.\nmaybepopfirst! (aka steal) for retrieving and removing an element at the head can be invoked from any tasks. However, there is no pushfirst!.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> deque = WorkStealingDeque{Int}();\n\njulia> push!(deque, 1);\n\njulia> push!(deque, 2);\n\njulia> push!(deque, 3);\n\njulia> maybepop!(deque)\nSome(3)\n\njulia> fetch(Threads.@spawn maybepopfirst!(deque))\nSome(1)\n\njulia> fetch(Threads.@spawn popfirst!(deque))\n2\n\njulia> maybepopfirst!(deque)  # returns nothing\n\nExtended help\n\nThe current implementation is known to be not fully compliant with the C/C++ memory model (on which Julia's memory model is designed).\n\nIt implements the dynamic circular work-stealing deque by Chase and Lev (2005):\n\nChase, David, and Yossi Lev. “Dynamic Circular Work-Stealing Deque.” In Proceedings of the Seventeenth Annual ACM Symposium on Parallelism in Algorithms and Architectures, 21–28. SPAA ’05. New York, NY, USA: Association for Computing Machinery, 2005. https://doi.org/10.1145/1073970.1073974.\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.maybepop!","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.maybepop!","text":"maybepop!(collection) -> Some(value::T) or nothing\n\nTry to pop a value from the tail of collection. Return Some(value) if it is non-empty.  Return nothing if empty.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> stack = ConcurrentStack{Int}();\n\njulia> push!(stack, 1);\n\njulia> maybepop!(stack)\nSome(1)\n\njulia> maybepop!(stack)  # returns nothing\n\n\n\n\n\n","category":"function"},{"location":"#ConcurrentCollections.maybepopfirst!","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.maybepopfirst!","text":"maybepopfirst!(collection) -> Some(value::T) or nothing\n\nTry to pop a value from the head of collection. Return Some(value) if it is non-empty.  Return nothing if empty.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> queue = ConcurrentQueue{Int}();\n\njulia> push!(queue, 1);\n\njulia> maybepopfirst!(queue)\nSome(1)\n\njulia> maybepopfirst!(queue)  # returns nothing\n\n\n\n\n\n","category":"function"},{"location":"#Hash-table","page":"ConcurrentCollections.jl","title":"Hash table","text":"","category":"section"},{"location":"","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.jl","text":"ConcurrentDict\nmodify!\nmaybeget\nKeep\nDelete","category":"page"},{"location":"#ConcurrentCollections.ConcurrentDict","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.ConcurrentDict","text":"ConcurrentDict{K,V}()\n\nConcurrent dictionary.  All operations are lock-free except when the dictionary is resized.\n\nnote: Note\nAlthough tasks wait on concurrent modifications (e.g., setindex!) during resize, the worker threads participate in the resize to avoid wasting CPU resources.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> dict = ConcurrentDict{String,Int}();\n\njulia> dict[\"hello\"] = 1;\n\njulia> dict[\"hello\"]\n1\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.modify!","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.modify!","text":"modify!(f, dict::ConcurrentDict{K,V}, key::K) -> y\n\nAtomically update key slot of dict using a function f.\n\nIf key does not exist, f is called with nothing. The call f(nothing) must return either (1) nothing to keep the slot unoccupied or (2) Some(value::V) to insert value.\n\nIf key exist, f is called with a ref such that ref[] retrieves the value corresponding to the key.  The call f(ref) must return either (1) nothing to delete the slot, (2) Some(value′::V) to insert value, (3) Keep(ans) to return y = Keep(ans) from modify!, or (4) Delete(ans) to delete slot and return a value y = Delete(ans) from modify!.\n\nThe function f may be called more than once if multiple tasks try to modify the dictionary.\n\nExamples\n\njulia> using ConcurrentCollections\n\njulia> dict = ConcurrentDict{String,Int}();\n\njulia> modify!(dict, \"hello\") do _\n           Some(1)\n       end\nSome(1)\n\njulia> modify!(dict, \"hello\") do ref\n           Some(something(ref[]) + 1)\n       end\nSome(2)\n\n\n\n\n\n","category":"function"},{"location":"#ConcurrentCollections.maybeget","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.maybeget","text":"maybeget(dict::ConcurrentDict{K,V}, key) -> Some(value::T) or nothing\n\n\n\n\n\n","category":"function"},{"location":"#ConcurrentCollections.Keep","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.Keep","text":"Keep(ans)\n\nA special type used in modify! to indicate that a slot should be remain unchanged while propagating the result ans of some computation to the caller.\n\nThat is to say,\n\ny = modify!(dict, key) do value\n    Keep(f(something(value)))\nend\ny[]\n\nis an optimization of\n\nr = Ref{Any}()\nmodify!(dict, key) do value\n    r[] = f(something(value))\n    Some(value)\nend\nr[]\n\n\n\n\n\n","category":"type"},{"location":"#ConcurrentCollections.Delete","page":"ConcurrentCollections.jl","title":"ConcurrentCollections.Delete","text":"Delete(ans)\n\nA special type used in modify! to indicate that a slot should be removed.\n\nThat is to say\n\ny = modify!(dict, key) do value\n    Delete(f(something(value)))\nend\ny[]\n\nis an optimization of\n\nr = Ref{Any}()\nmodify!(dict, key) do value\n    r[] = f(something(value))\n    nothing\nend\nr[]\n\n\n\n\n\n","category":"type"}]
}