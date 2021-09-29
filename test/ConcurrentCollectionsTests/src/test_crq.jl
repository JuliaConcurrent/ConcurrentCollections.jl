module TestCRQ

using Base.Experimental: @sync
using ConcurrentCollections
using ConcurrentCollections.Implementations:
    CRQSlot, IndirectConcurrentRingQueueNode, trypush!, isclosed
using ProgressLogging: @logprogress, @withprogress
using Test
using ..Utils: ⊏

function test_CRQSlot()
    for index in [111, 222],
        safe in [false, true],
        storage in UInt32[0xaaa, 0xbbb]

        @test NamedTuple(CRQSlot(; index, safe, storage)) == (; index, safe, storage)
    end
end

function test_print_crqslot()
    slot = CRQSlot(; index = 111, safe = false, storage = UInt32(0xaaa))
    str = sprint(show, "text/plain", slot)
    @test "CRQSlot" ⊏ str
    @test r"index *= *111" ⊏ str
    @test r"safe *= *false" ⊏ str
    @test r"storage *= *0x0+aaa" ⊏ str
end

function unfair_sleep(seconds::Real)
    t0 = time_ns()
    ns = seconds * 1e9
    while time_ns() - t0 < ns
        GC.safepoint()
    end
end

function concurrent_denqueue!(
    crq::IndirectConcurrentRingQueueNode,
    nitems::Integer,
    nsend::Integer,
    nrecv::Integer,
)
    @assert nsend > 0
    @assert nrecv > 0
    use_yield = nsend + nrecv > Threads.nthreads()
    # TODO: If `!use_yield`, make sure the tasks are spawned in different
    # threads and they are sticky.
    function poll()
        if use_yield
            yield()
        else
            GC.safepoint()
        end
        rand() < 0.1 && unfair_sleep(rand() * 1e-6)
    end

    received = Vector{Int}[]
    senders = Task[]
    receivers = Task[]
    global SENDERS = senders
    global RECEIVERS = receivers
    ref = Threads.Atomic{Int}(0)
    activesenders = Threads.Atomic{Int}(nsend)
    @sync begin
        for offset in 1:nsend
            t = Threads.@spawn try
                local y = nothing
                local i = 0
                local allpushed = true
                for outer i in offset:nsend:nitems
                    local x = eltype(crq)(i)
                    if !trypush!(crq, x)
                        allpushed = false
                        break
                    end
                    local s = Threads.atomic_add!(ref, 1)
                    if s > nsend
                        while ref[] > 1
                            poll()
                        end
                    end
                end
                return (; y, i, allpushed)
            finally
                Threads.atomic_sub!(activesenders, 1)
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
                    local y = maybepopfirst!(crq)
                    if y === nothing
                        if activesenders[] == 0
                            y = maybepopfirst!(crq)
                            if y === nothing  # Confirm that CRQ is empty
                                return y
                            end
                            # Reaching here means that there were some enqueues
                            # between our `maybepopfirst!(crq) === nothing` and
                            # `activesenders[] == 0`.
                        else
                            poll()
                            continue
                        end
                    end
                    local i = something(y)
                    push!(ys, i)

                    local s = Threads.atomic_sub!(ref, 1)
                    if s < -nrecv
                        while ref[] < -1
                            poll()
                        end
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
    crq = IndirectConcurrentRingQueueNode{Int16}(32)
    global CRQ = crq
    nitems = 2^20
    nitems = typemax(Int16)
    received, senders, receivers = concurrent_denqueue!(crq, nitems, nsend, nrecv)
    allreceived = reduce(vcat, received)

    if isclosed(crq)
        @info "CRQ closed. Skipping the tests..."
        return
    end

    @test [fetch(t).allpushed for t in senders] == fill(true, nsend)

    sort!(allreceived)
    (; notfound, dups) = check_consecutive(allreceived)
    @test length(allreceived) == nitems
    @test notfound == []
    @test dups == []
    @test allreceived == 1:nitems
end

function test_print()
    crq = IndirectConcurrentRingQueueNode{Int16}(32)
    str = sprint(show, "text/plain", crq)
    @test "CRQ: " ⊏ str
end

end  # module
