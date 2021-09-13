module ConcurrentCollectionsBenchmarks

using BenchmarkTools: Benchmark, BenchmarkGroup

include("utils.jl")
include("bench_dict_histogram.jl")
include("bench_dict_get_existing.jl")
include("bench_dict_migration.jl")
include("bench_queue_pushpop.jl")
include("bench_queue_hot_potato.jl")

function setup()
    suite = BenchmarkGroup()
    suite["DictHistogram"] = BenchDictHistogram.setup()
    suite["DictGetExisting"] = BenchDictGetExisting.setup()
    suite["DictMigration"] = BenchDictMigration.setup()
    suite["QueuePushPop"] = BenchQueuePushPop.setup()
    suite["HotPotato"] = BenchQueueHotPotato.setup()
    return suite
end

function set_smoke_params!(bench)
    bench.params.seconds = 0.001
    bench.params.evals = 1
    bench.params.samples = 1
    bench.params.gctrial = false
    bench.params.gcsample = false
    return bench
end

foreach_benchmark(f!, bench::Benchmark) = f!(bench)
function foreach_benchmark(f!, group::BenchmarkGroup)
    for x in values(group)
        foreach_benchmark(f!, x)
    end
end

function setup_smoke()
    suite = setup()
    foreach_benchmark(set_smoke_params!, suite)
    return suite
end

function clear()
    BenchDictHistogram.clear()
    BenchDictGetExisting.clear()
    BenchDictMigration.clear()
    BenchQueuePushPop.clear()
    BenchQueueHotPotato.clear()
end

end # module
