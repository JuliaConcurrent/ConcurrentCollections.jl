module DontTestDict

using ConcurrentCollections
using ConcurrentCollections.Implementations: clusters
using Test

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

function var"test_ConcurrentDict(key => value, ...)"()
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

end  # module
