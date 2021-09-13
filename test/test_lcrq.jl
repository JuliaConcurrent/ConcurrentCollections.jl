module TestLCRQ

using ConcurrentCollections
using ConcurrentCollections.Implementations: ICRQIndex
using Test

@testset "ICRQIndex" begin
    idx = ICRQIndex(threadindex = 111, itemindex = 222)
    @test idx.threadindex == 111
    @test idx.itemindex == 222
end

@testset "push-pop once" begin
    q = LinkedConcurrentRingQueue{Int}()
    push!(q, 111)
    @test trypopfirst!(q) == Some(111)
end

@testset "push-pop 100" begin
    n = 100
    q = LinkedConcurrentRingQueue{Int}()
    foldl(push!, 1:n; init = q)
    ys = Int[]
    while (y = trypopfirst!(q)) !== nothing
        push!(ys, something(y))
    end
    @test ys == 1:n
end

function concurrent_push_pop!(q, nitems::Integer, nsend::Integer, nrecv::Integer)
    received = Vector{Int}[]
    @sync begin
        for t in 1:nsend
            Threads.@spawn begin
                for i in t:nsend:nitems
                    push!(q, i)
                end
            end
        end
        for _ in 1:nrecv
            ys = Int[]
            push!(received, ys)
            Threads.@spawn begin
                while true
                    y = trypopfirst!(q)
                    if y === nothing
                        yield()
                    else
                        i = something(y)
                        push!(ys, i)
                        if i > nitems - nrecv
                            break
                        end
                    end
                end
            end
        end
    end
    return received
end

@testset "concurrent push-pop" begin
    @test_broken false
    #=
    if Threads.nthreads() > 1
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
        @test allreceived == 1:nitems
    end
    =#
end

end  # module
