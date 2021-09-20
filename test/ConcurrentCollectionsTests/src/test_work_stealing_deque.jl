module TestWorkStealingDeque

using ConcurrentCollections
using Test

function test_single_thread_push_pop()
    deque = WorkStealingDeque{Int}()
    xs = 1:50
    foldl(push!, xs; init = deque)
    @test [pop!(deque) for _ in xs] == reverse(xs)
    @test trypop!(deque) === nothing
    @test trypopfirst!(deque) === nothing
    push!(deque, 1)
    @test trypopfirst!(deque) === Some(1)
    @test trypopfirst!(deque) === nothing
    @test trypop!(deque) === nothing

    foldl(push!, xs; init = deque)
    n = length(deque.buffer)
    sizehint!(deque, 2 * n)
    @test length(deque.buffer) > n
    @test [pop!(deque) for _ in xs] == reverse(xs)
end

function random_pushpop(xs; ntasks = Threads.nthreads() - 1, sentinel = -1)
    sentinel = convert(eltype(xs), sentinel)  # verify argument
    ntasks = max(1, ntasks)

    deque = WorkStealingDeque{eltype(xs)}()

    local tasks, zs
    try
        tasks = map(1:ntasks) do _
            Threads.@spawn begin
                local ys = eltype(xs)[]
                while true
                    local r = trypopfirst!(deque)
                    if r === nothing
                        GC.safepoint()
                        continue
                    end
                    local y = something(r)
                    y == $sentinel && break
                    push!(ys, y)
                end
                ys
            end
        end

        zs = eltype(xs)[]
        for (i, x) in enumerate(xs)
            push!(deque, x)
            # continue
            if mod(i, 8) == 0
                r = trypop!(deque)
                GC.safepoint()
                r === nothing && continue
                push!(zs, something(r))
            end
        end
    finally
        for _ in 1:ntasks
            push!(deque, sentinel)
        end
    end

    return zs, fetch.(tasks)
end

function test_random_push_pop()
    @testset for trial in 1:100
        @testset for T in [Int, Any, Pair{Any,Int}]
            test_random_push_pop(T)
        end
    end
end

function test_random_push_pop(T::Type)
    sentinel = -1
    xs = 1:2^10
    return test_random_push_pop(T::Type, xs, sentinel)
end

function test_random_push_pop(T::Type{Pair{Any,Int}})
    sentinel = T(-1, -1)
    xs = [T(x, x) for x in 1:2^10]
    return test_random_push_pop(T::Type, xs, sentinel)
end

function test_random_push_pop(T::Type, xs, sentinel)
    if T !== eltype(xs)
        xs = collect(T, xs)
    end
    zs, yss = random_pushpop(xs; sentinel)
    @test allunique(zs)
    @test all(allunique, yss)
    @debug "random_pushpop(xs)" length(zs) length.(yss)
    ys = sort!(foldl(append!, yss; init = copy(zs)))
    @test length(ys) == length(xs)
    @test setdiff(ys, xs) == []
    @test setdiff(xs, ys) == []
    @test ys == xs
    return (; zs, yss)
end

end  # module
