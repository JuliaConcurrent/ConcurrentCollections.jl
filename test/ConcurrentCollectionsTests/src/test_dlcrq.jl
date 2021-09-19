module TestDLCRQ

using Base.Experimental: @sync
using ConcurrentCollections
using ConcurrentCollections.Implementations:
    MPCRQSlot, DATA, ANTIDATA, denqueue!, MPCRQ_ENQUEUED
using ProgressLogging: @logprogress, @withprogress
using Test

function var"test_MPCRQSlot"()
    for index in [111, 222],
        safe in [false, true],
        polarity in [DATA, ANTIDATA],
        storage in UInt32[0xaaa, 0xbbb]

        @test NamedTuple(MPCRQSlot(; index, safe, polarity, storage)) ==
              (; index, safe, polarity, storage)
    end
end

function test_push_pop_once_int()
    q = DualLinkedConcurrentRingQueue{Int}()
    push!(q, 111)
    @test popfirst!(q) == 111
end

function test_push_pop_once_any()
    q = DualLinkedConcurrentRingQueue()
    push!(q, 111)
    @test popfirst!(q) == 111
end

function test_error()
    q = DualLinkedConcurrentRingQueue{Int}()
    t = @task popfirst!(q)
    yield(t)
    msg = "Interrupting DLCRQ @$(time_ns())"
    schedule(t, ErrorException(msg); error = true)
    err = try
        Some(wait(t))
    catch e
        e
    end
    @test err isa TaskFailedException
    @test occursin(msg, sprint(showerror, err))
    push!(q, 111)  # this dequeues but ignores the interrupted waiter
    @test popfirst!(q) == 111
end

function var"test_push-pop 100"()
    n = 100
    q = DualLinkedConcurrentRingQueue{Int}(; log2ringsize = 3)
    foldl(push!, 1:n; init = q)
    ys = Int[]
    for _ in 1:n
        y = popfirst!(q)
        push!(ys, y)
    end
    @test ys == 1:n
end

function var"test_push-pop 100 wait first"()
    n = 100
    q = DualLinkedConcurrentRingQueue{Int}(; log2ringsize = 3)
    task = Threads.@spawn begin
        ys = Int[]
        for _ in 1:n
            y = popfirst!(q)
            push!(ys, y)
        end
        return ys
    end
    sleep(0.01)
    foldl(push!, 1:n; init = q)
    ys = fetch(task)
    @test ys == 1:n
end

function var"test_push-pop 100 inline"()
    n = 100
    q = DualLinkedConcurrentRingQueue{Int16}(; log2ringsize = 3)
    @test q.data.data === nothing
    foldl(push!, 1:n; init = q)
    ys = Int[]
    for _ in 1:n
        y = popfirst!(q)
        push!(ys, y)
    end
    @test ys == 1:n
end

function unfair_sleep(seconds::Real)
    t0 = time_ns()
    ns = seconds * 1e9
    while time_ns() - t0 < ns
        GC.safepoint()
    end
end

function concurrent_push_pop!(q, nitems::Integer, nsend::Integer, nrecv::Integer)
    received = Vector{Int}[]
    tasks = Task[]
    global TASKS = tasks
    activesenders = Threads.Atomic{Int}(nsend)
    @sync begin
        for offset in 1:nsend
            t = Threads.@spawn begin
                for i in offset:nsend:nitems
                    push!(q, i)
                end
                if Threads.atomic_sub!(activesenders, 1) == 1
                    for _ in 1:nrecv
                        push!(q, -1)
                    end
                end
            end
            push!(tasks, t)
        end
        for _ in 1:nrecv
            ys = Int[]
            push!(received, ys)
            t = Threads.@spawn begin
                while true
                    y = popfirst!(q)
                    y == -1 && break
                    push!(ys, y)
                end
            end
            push!(tasks, t)
        end
    end
    return received
end

function check_consecutive(xs)
    notfound = Int[]
    dups = Int[]
    pre = xs[begin] - 1
    for x in xs
        e = pre + 1
        append!(notfound, e:x-1)
        append!(dups, x:e-1)
        pre = x
    end
    return (; notfound, dups)
end

function test_concurrent_push_pop(ntrials = 100)
    @withprogress name = "concurrent push-pop" begin
        @testset for trial in 1:ntrials
            @logprogress (trial - 1) / ntrials
            check_concurrent_push_pop()
        end
    end
end

function check_concurrent_push_pop()
    nsend = cld(Threads.nthreads(), 2)
    nrecv = max(1, Threads.nthreads() - nsend)
    q = DualLinkedConcurrentRingQueue{Int}(; log2ringsize = 5)
    # q = Channel{Int}(Inf)
    nitems = 2^20
    received = concurrent_push_pop!(q, nitems, nsend, nrecv)
    allreceived = reduce(vcat, received)
    sort!(allreceived)
    (; notfound, dups) = check_consecutive(allreceived)
    @test length(allreceived) == nitems
    @test notfound == []
    @test dups == []
    @test allreceived == 1:nitems
end

end  # module
