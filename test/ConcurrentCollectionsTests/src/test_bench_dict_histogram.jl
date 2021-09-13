module TestBenchDictHistogram

using ConcurrentCollections
using ConcurrentCollectionsBenchmarks.BenchDictHistogram:
    default_ntasks_list, generate, hist_parallel!, hist_seq!
using Test

let datasize_list = [10, 2^5, 2^10, 2^20]
    fulldata = generate(datasize = datasize_list[end])
    @testset for datasize in datasize_list
        data = view(fulldata, 1:datasize)
        dbase = hist_seq!(Dict{String,Int}(), data)
        @testset "seq" begin
            cdseq = hist_seq!(ConcurrentDict{String,Int}(), data)
            @test Dict(cdseq) == dbase
        end
        @testset for ntasks in default_ntasks_list()
            cdpar = hist_parallel!(ConcurrentDict{String,Int}(), data; ntasks = ntasks)
            if Dict(cdpar) != dbase
                global FAILED = (; cdpar, dbase)
            end
            @test Dict(cdpar) == dbase
        end
    end
end

end  # module
