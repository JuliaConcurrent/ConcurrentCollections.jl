using DataFrames
using FileIO
using JSON
using Statistics
using VegaLite

rawdata = JSON.parsefile(joinpath(@__DIR__, "build/results.json"))

potatos = map(rawdata["potatos"]) do info
    result = info["result"]
    ntasks = info["ntasks"]
    impl = Symbol(info["impl"])
    trialid = info["trialid"]
    stats = result["stats"]
    npush = sum(stat["npush"] for stat in stats)
    npop = sum(stat["npop"] for stat in stats)
    t1min = minimum(stat["t1"] for stat in stats)
    t2max = maximum(stat["t2"] for stat in stats)
    duration = (t2max - t1min) / 1e9
    nops = npush + npop
    throughput = nops / duration
    return (; trialid, ntasks, impl, throughput, duration, nops, npush, npop)
end

fais = map(rawdata["fais"]) do info
    result = info["result"]
    ntasks = info["ntasks"]
    impl = :fai
    trialid = info["trialid"]
    stats = result["stats"]
    nops = sum(stat["nfai"] for stat in stats)
    t1min = minimum(stat["t1"] for stat in stats)
    t2max = maximum(stat["t2"] for stat in stats)
    duration = (t2max - t1min) / 1e9
    throughput = nops / duration
    return (; trialid, ntasks, impl, throughput, duration, nops)
end

datadf = vcat(DataFrame(potatos), DataFrame(fais); cols = :union)

impllabels = Dict(
    # :impl -> :Implementation (for plot)
    :base => "Base (Channel)",
    :dlcrq => "Dual LCRQ",
    :fai => "Hardware \"limit\" (FAI)",
)

datadf[!, :Implementation] = getindex.(Ref(impllabels), datadf.impl)

datadf[!, :diff_push_pop] = datadf.npush .- datadf.npop

plt = @vlplot(
    :point,
    x = {:ntasks, title = "Number of Tasks"},
    y = {:throughput, title = "Throughput [#OP/second]"},
    color = :Implementation,
    title = "Hot Potato Benchmark",
    data = datadf,
)

save(joinpath(@__DIR__, "build/results.png"), plt)

summarydf = let
    idx = datadf.impl .!= :fai
    df = datadf[idx, :]

    sdf = unstack(
        combine(groupby(df, [:impl, :ntasks]), :throughput => median => :throughput),
        :impl,
        :throughput,
    )
    sdf[!, :speedup] = sdf.dlcrq ./ sdf.base
    sdf
end

plt_speedup = @vlplot(
    :point,
    x = {:ntasks, title = "Number of Tasks"},
    y = {:speedup, title = "Speedup [DLCRQ/Base]"},
    title = "Hot Potato Benchmark (Speedup)",
    data = summarydf,
)

save(joinpath(@__DIR__, "build/speedup.png"), plt_speedup)

plt_trialid = @vlplot(
    :point,
    x = {:ntasks, title = "Number of Tasks"},
    y = {:throughput, title = "Throughput [#OP/second]"},
    color = {
        :trialid,
        scale = {scheme = :magma},
    },
    title = "Hot Potato Benchmark",
    data = datadf[datadf.impl .== :base, :],
)

save(joinpath(@__DIR__, "build/trial_dependencies.png"), plt_trialid)

plt_diff_push_pop = @vlplot(
    :point,
    x = :diff_push_pop,
    y = :throughput,
    color = {
        :ntasks,
        # "ntasks:o",
        scale = {scheme = :viridis},
    },
    shape = :impl,
    column = :impl,
    data = datadf,
)

save(joinpath(@__DIR__, "build/diff_push_pop.png"), plt_diff_push_pop)
