module TestSSQueue

using ConcurrentCollections
using ConcurrentCollections.Implementations: NodeIterator, SSQNode, check_invariance, ==′
using ProgressLogging: @logprogress, @withprogress
using Test
using ..TestDLCRQ: check_concurrent_push_pop!
using ..Utils: ⊏

function test_push_pop_once_int()
    q = DualLinkedQueue{Int}()
    push!(q, 111)
    @test popfirst!(q) == 111
end

function test_push_pop_once_any()
    q = DualLinkedQueue()
    push!(q, 111)
    @test popfirst!(q) == 111
end

function test_error()
    q = DualLinkedQueue{Int}()
    t = @task popfirst!(q)
    yield(t)
    msg = "Interrupting DLQ @$(time_ns())"
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

function test_concurrent_push_pop(ntrials = 100)
    @withprogress name = "concurrent push-pop" begin
        @testset for trial in 1:ntrials
            @logprogress (trial - 1) / ntrials
            q = DualLinkedQueue{Int}()
            check_concurrent_push_pop!(q; nitems = 2^13)
        end
    end
end

function test_iter()
    q = DualLinkedQueue{Int}()
    @test eltype(q) === Int
    push!(q, 111)
    push!(q, 222)
    @test collect(q) ==′ [111, 222]
end

function test_nodeiterator()
    q = DualLinkedQueue{Int}()
    push!(q, 111)
    push!(q, 222)
    itr = NodeIterator(q)
    @test eltype(itr) === SSQNode{Int}
    nodes = collect(itr)
    @test nodes[1].value == 111
    @test nodes[2].value == 222
    @test check_invariance(q)
end

function test_print()
    q = DualLinkedQueue{Int}()
    push!(q, 111)
    push!(q, 222)
    str = sprint(show, "text/plain", q)
    @test "DualLinkedQueue" ⊏ str
end

end  # module
