module TestDict

using ConcurrentCollections
using ConcurrentCollections.Implementations: clusters, migrate!
using SyncBarriers: Barrier, cycle!
using Test

function test_expand_and_shrink(n = 17)
    d = ConcurrentDict{Int,Int}()
    @testset "expand" begin
        @testset for i in 1:n
            d[i] = -i
            @testset for k in 1:i
                @test d[k] == -k
            end
        end
    end
    nfull = length(d.slots)
    @testset "shrink" begin
        @testset for i in n:-1:1
            @test pop!(d, i) == -i
            @testset for k in 1:i-1
                @test d[k] == -k
            end
        end
    end
    @test length(d.slots) < nfull
    return d
end

function test_parallel_expand(n = 2^10, basesize = 8)
    d = ConcurrentDict{Int,Int}(pairs(1:n))
    nslots = length(d.slots)
    migrate!(d, true; basesize)
    @test nslots < length(d.slots)
    diffs = Pair{Int,Int}[]
    for k in 1:n
        v = d[k]
        if v != k
            push!(diffs, k => v)
        end
    end
    @test diffs == []
    return d
end

function test_dict()
    @testset for npairs in [2, 100]
        test_dict(npairs)
    end
end

function test_dict(npairs)
    @testset for Key in [Int8, Int32, Int64], Value in [Int]
        d = ConcurrentDict{Key,Value}()
        @testset for trial in 1:2
            for i in 1:npairs
                d[i] = -i
            end
            @testset "getindex" begin
                vals = [d[i] for i in 1:2]
                @test vals == [-1, -2]
            end
            @testset "iterate" begin
                kvs = sort!(collect(d))
                @test kvs == [i => -i for i in 1:npairs]
                ks = sort!(collect(keys(d)))
                @test ks == 1:npairs
                vs = sort!(collect(values(d)))
                @test vs == reverse((1:npairs) .* -1)
            end
        end
        @testset "trypop!" begin
            @test tryget(d, 1) === Some(-1)
            @test trypop!(d, 1) === Some(-1)
            @test tryget(d, 1) === nothing
            @test trypop!(d, 1) === nothing
            @test 1 ∉ sort!(collect(keys(d)))
        end
        @testset "length_upper_bound" begin
            @test length_upper_bound(d) == mapreduce(_ -> 1, +, d)
        end

        @testset "typed constructor" begin
            ks = Key.(1:npairs)
            vs = Value.(1:npairs)
            @testset "$label" for (label, dict) in [
                "ConcurrentDict{Key,Value}(zip(ks, vs))" =>
                    ConcurrentDict{Key,Value}(zip(ks, vs)),
                "ConcurrentDict{Key,Value}(map(=>, ks, vs))" =>
                    ConcurrentDict{Key,Value}(map(=>, ks, vs)),
            ]
                @test sort!(collect(keys(dict))) == ks
                @test sort!(collect(values(dict))) == ks
            end
        end

        @testset "clusters" begin
            @test length(clusters(d)::Vector{UnitRange{Int}}) > 0
        end
    end

    @testset "ConcurrentDict{String,_}()" begin
        d = ConcurrentDict{String,Int}()
        str(i) = string(i; pad = 3)
        @testset for trial in 1:2
            for i in 1:npairs
                d[str(i)] = -i
            end
            @testset "getindex" begin
                vals = [d[str(i)] for i in 1:2]
                @test vals == [-1, -2]
            end
            @testset "iterate" begin
                kvs = sort!(collect(d))
                @test kvs == [str(i) => -i for i in 1:npairs]
            end
        end
        @testset "trypop!" begin
            @test tryget(d, "001") === Some(-1)
            @test trypop!(d, "001") === Some(-1)
            @test tryget(d, "001") === nothing
            @test trypop!(d, "001") === nothing
            @test "001" ∉ sort!(collect(keys(d)))
        end
        @testset "clusters" begin
            @test length(clusters(d)::Vector{UnitRange{Int}}) > 0
        end
    end
end

function test_pairs_constructor()
    @testset for Key in [Int8, Int32, Int64], Value in [Int]
        @testset "$label" for (label, dict) in [
            "ConcurrentDict{_,_}(_ => _, ...)" =>
                ConcurrentDict{Key,Value}(Key(0) => Value(1), Key(2) => Value(3)),
            "ConcurrentDict(_ => _, ...)" =>
                ConcurrentDict(Key(0) => Value(1), Key(2) => Value(3)),
            "ConcurrentDict{_,_}(false => true, ...)" =>
                ConcurrentDict{Key,Value}(false => true, 2 => 3),
            "ConcurrentDict(false => true, ...)" =>
                ConcurrentDict(false => true, Key(2) => Value(3)),
        ]
            @test dict[0] == 1
            @test dict[2] == 3
        end
    end
end

function test_shrink()
    @testset for Key in [Int8, Int32, Int64], Value in [Int]
        ks = vs = 1:100
        d = ConcurrentDict{Key,Value}(zip(ks, vs))
        nslots = length(d.slots)
        for k in ks
            pop!(d, k)
        end
        @test collect(d) == []
        @test length(d.slots) < nslots
    end
end

function random_mutation!(dict; nkeys = 8, repeat = 2^20, ntasks = Threads.nthreads())
    ks = 1:nkeys
    locals = [
        (
            popped = zeros(valtype(dict), nkeys),  # sum of popped values
            added = zeros(valtype(dict), nkeys),   # sum of all inserted values
        ) for _ in 1:ntasks
    ]
    @sync for (; popped, added) in locals
        Threads.@spawn begin
            for _ in 1:repeat
                k = rand(ks)
                if rand(Bool)
                    y = trypop!(dict, k)
                    if y !== nothing
                        popped[k] += something(y)
                    end
                else
                    added[k] += 1
                    modify!(dict, k) do ref
                        Base.@_inline_meta
                        Some(ref === nothing ? 1 : ref[] + 1)
                    end
                end
            end
        end
    end
    return locals
end

function test_random_mutation(; kwargs...)
    dict = ConcurrentDict{Int,Int}()
    nkeys = 16
    locals = random_mutation!(dict; kwargs..., nkeys)
    actual = zeros(valtype(dict), nkeys)
    desired = zeros(valtype(dict), nkeys)
    for (k, v) in dict
        actual[k] = v
    end
    for (; popped, added) in locals
        actual .+= popped
        desired .+= added
    end
    @test actual == desired
end

function phased_push_pop!(
    dict;
    nkeys = 16,
    repeat = 2^10,
    phases = 2^10,
    ntasks = Threads.nthreads(),
)
    locals = [
        (
            popped = zeros(valtype(dict), nkeys * phases),  # sum of popped values
            added = zeros(valtype(dict), nkeys * phases),   # sum of all inserted values
        ) for _ in 1:ntasks
    ]
    barrier = Barrier(ntasks)
    @sync for (itask, (; popped, added)) in enumerate(locals)
        Threads.@spawn begin
            for p in 1:phases
                k0 = (p - 1) * nkeys + 1
                ks = k0:k0+nkeys-1
                for _ in 1:repeat
                    k = rand(ks)
                    added[k] += 1
                    modify!(dict, k) do ref
                        Base.@_inline_meta
                        Some(ref === nothing ? 1 : ref[] + 1)
                    end
                end
                spin = 10_000  # spin for a few μs
                cycle!(barrier[itask], spin)
                for k in ks
                    popped[k] += something(trypop!(dict, k), 0)
                end
            end
        end
    end
    return locals
end

function test_phased_push_pop(; nkeys = 16, phases = 2^10, kwargs...)
    dict = ConcurrentDict{Int,Int}()
    locals = phased_push_pop!(dict; kwargs..., nkeys, phases)
    actual = zeros(valtype(dict), nkeys * phases)
    desired = zeros(valtype(dict), nkeys * phases)
    for k in eachindex(actual)
        actual[k] = get(dict, k, 0)
    end
    for (; popped, added) in locals
        actual .+= popped
        desired .+= added
    end
    @test actual == desired
end

end  # module
