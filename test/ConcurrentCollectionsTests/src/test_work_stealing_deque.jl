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

function random_pushpop(xs, ntasks = Threads.nthreads() - 1)
    ntasks = max(1, ntasks)

    deque = WorkStealingDeque{eltype(xs)}()

    local tasks, zs
    done = Threads.Atomic{Bool}(false)
    try
        tasks = map(1:ntasks) do _
            Threads.@spawn begin
                local ys = eltype(xs)[]
                while true
                    r = trypopfirst!(deque)
                    if r === nothing
                        done[] && break
                        continue
                    end
                    push!(ys, something(r))
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
                r === nothing && continue
                push!(zs, something(r))
            end
        end
    finally
        done[] = true
    end

    return zs, fetch.(tasks)
end

function test_random_push_pop()
    @testset for trial in 1:100
        @testset for T in [Int, Any]
            test_random_push_pop(T)
        end
    end
end

function test_random_push_pop(T::Type, xs = 1:2^10)
    if T !== eltype(xs)
        xs = collect(T, xs)
    end
    zs, yss = random_pushpop(xs)
    @test allunique(zs)
    @test all(allunique, yss)
    @debug "random_pushpop(xs)" length(zs) length.(yss)
    ys = sort!(foldl(append!, yss; init = copy(zs)))
    @debug "random_pushpop(xs)" setdiff(ys, xs) setdiff(xs, ys) length(xs) length(ys)
    @test ys == xs
end

end  # module
