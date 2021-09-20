module TestMPCRQ

using Base.Experimental: @sync
using ConcurrentCollections
using ConcurrentCollections.Implementations:
    IndirectMultiPolarityConcurrentRingQueueNode,
    MPCRQ_CLOSED,
    MPCRQ_ENQUEUED,
    Waiter,
    denqueue!,
    tryput!
using ProgressLogging: @logprogress, @withprogress
using Test

function test_close()
    crq = IndirectMultiPolarityConcurrentRingQueueNode{Int}(3)
    @testset for i in 1:8
        @test denqueue!(crq, i) === MPCRQ_ENQUEUED
    end
    @test denqueue!(crq, 9) === MPCRQ_CLOSED
    local ys, ylast
    @sync begin
        @async begin
            ys = [denqueue!(crq, Waiter{Int}())::Some{Int} for _ in 1:8]
            ylast = denqueue!(crq, Waiter{Int}())
        end
    end
    @test something.(ys) == 1:8
    @test ylast === MPCRQ_CLOSED
end

function unfair_sleep(seconds::Real)
    t0 = time_ns()
    ns = seconds * 1e9
    while time_ns() - t0 < ns
        GC.safepoint()
    end
end

function concurrent_denqueue!(
    crq::IndirectMultiPolarityConcurrentRingQueueNode,
    nitems::Integer,
    nsend::Integer,
    nrecv::Integer,
)
    @assert nsend > 0
    @assert nrecv > 0
    use_yield = nsend + nrecv > Threads.nthreads()
    # TODO: If `!use_yield`, make sure the tasks are spawned in different
    # threads and they are sticky.

    received = Vector{Int}[]
    senders = Task[]
    receivers = Task[]
    global SENDERS = senders
    global RECEIVERS = receivers
    ref = Threads.Atomic{Int}(0)
    @sync begin
        for offset in 1:nsend
            t = Threads.@spawn begin
                local y = nothing
                local i = 0
                for outer i in offset:nsend:nitems
                    local s = Threads.atomic_add!(ref, 1)
                    if s > nsend
                        while ref[] > 1
                            if use_yield
                                yield()
                            else
                                GC.safepoint()
                            end
                        end
                    end
                    local x = eltype(crq)(i)
                    y = denqueue!(crq, x)
                    y === MPCRQ_CLOSED && break
                    y === MPCRQ_ENQUEUED && continue
                    local ok = tryput!(y::Waiter, x)
                    @assert ok "tryput!(y::Waiter, x)"
                end
                return y, i
            end
            push!(senders, t)
        end
        for _ in 1:nrecv
            ys = Int[]
            push!(received, ys)
            t = Threads.@spawn begin
                local ys_nb = Int[]
                local ys_b = Int[]
                while true
                    local s = Threads.atomic_sub!(ref, 1)
                    if s < -nrecv
                        while ref[] < -1
                            if use_yield
                                yield()
                            else
                                GC.safepoint()
                            end
                        end
                    end
                    local w = Waiter{eltype(crq)}()
                    local y = denqueue!(crq, w)
                    if y === MPCRQ_CLOSED
                        return (; y, ys_nb, ys_b)
                    end
                    local i
                    if y isa Some
                        i = something(y)
                        push!(ys, i)
                        push!(ys_nb, i)
                    else
                        @assert y === MPCRQ_ENQUEUED
                        i = fetch(w)
                        push!(ys, i)
                        push!(ys_b, i)
                    end

                    if i > nitems - nrecv
                        return (; y, ys_nb, ys_b)
                    end
                end
            end
            push!(receivers, t)
        end
    end
    return received, senders, receivers
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
    global received, notfound, dups, allreceived
    nsend = cld(Threads.nthreads(), 2)
    nrecv = max(1, Threads.nthreads() - nsend)
    crq = IndirectMultiPolarityConcurrentRingQueueNode{Int}(7)
    global CRQ = crq
    nitems = 2^20
    received, senders, receivers = concurrent_denqueue!(crq, nitems, nsend, nrecv)
    allreceived = reduce(vcat, received)

    @test length(allreceived) == nitems
    sort!(allreceived)
    (; notfound, dups) = check_consecutive(allreceived)
    @test length(allreceived) == nitems
    @test notfound == []
    @test dups == []
    @test allreceived == 1:nitems
end

end  # module
