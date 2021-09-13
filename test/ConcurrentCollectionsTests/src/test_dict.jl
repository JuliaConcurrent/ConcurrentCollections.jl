module TestDict

using ConcurrentCollections
using ConcurrentCollections.Implementations:
    LPDKeyState,
    LPD_BITMASK,
    LPD_DELETED,
    LPD_EMPTY,
    LPD_HASKEY,
    LPD_MOVED,
    LPD_MOVED_EMPTY,
    LPD_NBITS,
    KeyInfo,
    clusters,
    migrate!,
    setdata,
    setstate
using Test

function test_keyinfo()
    @test KeyInfo(UInt64(0)).state === LPD_EMPTY
    @testset for state in instances(LPDKeyState)
        @test KeyInfo{UInt64}(state, 0x0123456789abcdef).state === state
        if state !== LPD_EMPTY
            @test KeyInfo{UInt64}(state, 0x0123456789abcdef).keydata === 0x0123456789abcdef
        end
        @test setstate(KeyInfo(rand(UInt64)), state).state === state
        keydata = rand(UInt64) >> LPD_NBITS
        @test setdata(KeyInfo(rand(UInt64)), keydata).keydata === keydata
    end
end

function test_keyinfo_properties()
    keyinfo = KeyInfo{UInt64}(rand(UInt64))
    enum_to_property = Dict(
        LPD_EMPTY => :isempty,
        LPD_DELETED => :isdeleted,
        LPD_MOVED_EMPTY => :ismovedempty,
        LPD_MOVED => :ismoved,
        LPD_HASKEY => :haskey,
    )
    properties = collect(values(enum_to_property))
    @testset for state in instances(LPDKeyState), prop in properties
        if enum_to_property[state] === prop
            @test getproperty(setstate(keyinfo, state), prop)
        else
            @test !getproperty(setstate(keyinfo, state), prop)
        end
    end
end

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
