module TestBenchDictHistogram

using ConcurrentCollections
using ConcurrentCollectionsBenchmarks.BenchDictHistogram:
    default_ntasks_list, generate, hist_parallel!, hist_parallel_dac, hist_seq!
using Test

function diffvalues(actual, expected)
    diffs = []
    for (key, ve) in expected
        va = actual[key]
        if ve != va
            push!(diffs, (; key, actual = va, expected = ve))
        end
    end
    return diffs
end

function test()
    datasize_list = [10, 2^5, 2^10, 2^20]
    fulldata = generate(datasize = datasize_list[end])
    @testset for datasize in datasize_list
        data = view(fulldata, 1:datasize)
        test(data)
    end
end

function test(data)
    dbase = hist_seq!(Dict{String,Int}(), data)
    @testset "seq" begin
        cdseq = hist_seq!(ConcurrentDict{String,Int}(), data)
        @test sort(collect(setdiff(keys(dbase), keys(cdseq)))) == []
        @test sort(collect(setdiff(keys(cdseq), keys(dbase)))) == []
        @test diffvalues(cdseq, dbase) == []
        @test Dict(cdseq) == dbase
    end
    @testset for ntasks in default_ntasks_list()
        cdpar = hist_parallel!(ConcurrentDict{String,Int}(), data; ntasks = ntasks)
        #=
        if Dict(cdpar) != dbase
            global FAILED = (; cdpar, dbase)
        end
        =#
        @test sort(collect(setdiff(keys(dbase), keys(cdpar)))) == []
        @test sort(collect(setdiff(keys(cdpar), keys(dbase)))) == []
        @test diffvalues(cdpar, dbase) == []
        @test Dict(cdpar) == dbase
    end
    @testset "dac" begin
        @testset for ntasks in default_ntasks_list()
            dpar = hist_parallel_dac(data; ntasks = ntasks)
            @test sort(collect(setdiff(keys(dbase), keys(dpar)))) == []
            @test sort(collect(setdiff(keys(dpar), keys(dbase)))) == []
            @test diffvalues(dpar, dbase) == []
            @test dpar == dbase
        end
    end
end

end  # module
