module TestMSQueue

using ConcurrentCollections
using Test

function test_simple()
    q = ConcurrentQueue{Int}()
    xs = 1:10
    foldl(push!, xs; init = q)
    @test [popfirst!(q) for _ in xs] == xs
    @test trypopfirst!(q) === nothing
end

function pushpop(xs, ntasks = Threads.nthreads())
    queue = ConcurrentQueue{eltype(xs)}()

    local tasks
    done = Threads.Atomic{Bool}(false)
    try
        tasks = map(1:ntasks) do _
            Threads.@spawn begin
                local ys = eltype(xs)[]
                while true
                    r = trypopfirst!(queue)
                    if r === nothing
                        done[] && break
                        continue
                    end
                    push!(ys, something(r))
                end
                ys
            end
        end

        for x in xs
            push!(queue, x)
        end
    finally
        done[] = true
    end

    return fetch.(tasks), queue
end

function var"test_push/pop"()
    @testset for T in [Int, Any, Int, Any]
        xs = 1:2^10
        if T !== eltype(xs)
            xs = collect(T, xs)
        end
        yss, _ = pushpop(xs)
        @test all(allunique, yss)
        @debug "pushpop(xs)" length.(yss)
        ys = sort!(foldl(append!, yss; init = T[]))
        @debug "pushpop(xs)" setdiff(ys, xs) setdiff(xs, ys) length(xs) length(ys)
        @test ys == xs
    end
end

end  # module
