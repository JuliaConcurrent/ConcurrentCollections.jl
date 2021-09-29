module TestLCRQ

using Base.Experimental: @sync
using ConcurrentCollections
using ProgressLogging: @logprogress, @withprogress
using Test
using ..Utils: ⊏

function test_push_pop_once_int()
    q = LinkedConcurrentRingQueue{Int}()
    push!(q, 111)
    @test maybepopfirst!(q) == Some(111)
end

function test_push_pop_once_any()
    q = LinkedConcurrentRingQueue()
    push!(q, 111)
    @test maybepopfirst!(q) == Some{Any}(111)
end

function test_push_pop_100()
    n = 100
    q = LinkedConcurrentRingQueue{Int}()
    foldl(push!, 1:n; init = q)
    ys = Int[]
    while (y = maybepopfirst!(q)) !== nothing
        push!(ys, something(y))
    end
    @test ys == 1:n
end

function concurrent_push_pop!(q, nitems::Integer, nsend::Integer, nrecv::Integer)
    received = Vector{Int}[]
    activesenders = Threads.Atomic{Int}(nsend)
    @sync begin
        for t in 1:nsend
            Threads.@spawn begin
                for i in t:nsend:nitems
                    push!(q, i)
                end
                if Threads.atomic_sub!(activesenders, 1) == 1
                    for _ in 1:nrecv
                        push!(q, -1)
                    end
                end
            end
        end
        for _ in 1:nrecv
            ys = Int[]
            push!(received, ys)
            Threads.@spawn begin
                while true
                    y = maybepopfirst!(q)
                    if y === nothing
                        yield()
                    else
                        i = something(y)
                        i == -1 && break
                        push!(ys, i)
                    end
                end
            end
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
    Threads.nthreads() > 1 || return
    @withprogress name = "concurrent push-pop" begin
        @testset for trial in 1:ntrials
            @logprogress (trial - 1) / ntrials
            check_concurrent_push_pop()
        end
    end
end

function check_concurrent_push_pop()
    nsend = cld(Threads.nthreads(), 2)
    nrecv = Threads.nthreads() - nsend
    @assert nsend ≥ 1
    @assert nrecv ≥ 1
    q = LinkedConcurrentRingQueue{Int}(32)
    nitems = 2^20
    received = concurrent_push_pop!(q, nitems, nsend, nrecv)
    allreceived = reduce(vcat, received)
    @test length(allreceived) == nitems
    sort!(allreceived)
    (; notfound, dups) = check_consecutive(allreceived)
    @test notfound == []
    @test dups == []
    @test allreceived == 1:nitems
end

function test_print()
    q = LinkedConcurrentRingQueue{Int}()
    push!(q, 333)
    str = sprint(show, "text/plain", q)
    @test "LCRQ: " ⊏ str
    @test "1 item" ⊏ str
end

end  # module
