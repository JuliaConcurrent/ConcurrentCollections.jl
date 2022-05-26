import BenchmarkTools
import ConcurrentCollectionsBenchmarks
import JSON

include("../info_dump.jl")

function main(args = ARGS)
    output = get(args, 1, joinpath(@__DIR__, "build", "results.json"))
    mkpath(dirname(output))

    info = InfoDump.info()
    open(joinpath(dirname(output), "info.json"), write = true) do io
        JSON.print(io, info)
    end

    suite = ConcurrentCollectionsBenchmarks.BenchDictHistogram.setup(
        ntasks_list = 1:Threads.nthreads(),
        nkeys_list = [2^13, 2^16, 2^19, 2^25],
    )
    results = run(suite; verbose = true)
    BenchmarkTools.save(output, results)
    return results
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
