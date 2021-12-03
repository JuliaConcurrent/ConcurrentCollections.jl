import BenchmarkConfigSweeps
import BenchmarkTools
using DataFrames
using FileIO
using JSON
using Statistics
using VegaLite

results = only(BenchmarkTools.load(joinpath(@__DIR__, "build/results.json")))
df_raw = DataFrame(BenchmarkConfigSweeps.flattable(results))

access_param = "Access density"

begin
    df = select(df_raw, Not(:trial))
    df = select(df, Not(r"JULIA_.*"))
    df[:, :ms] = map(t -> mean(t).time, df_raw.trial) ./ 1e6
    df[:, :Implementation] = map(df.alg) do alg
        if alg === :base_seq || alg == :base_par
            Symbol("Base.Dict + Divide-and-Conquer")
        elseif alg === :cdict_seq || alg === :cdict_par
            :ConcurrentDict
        else
            error("unknown alg = ", alg)
        end
    end
    datasize = 2^19
    df[:, access_param] = datasize ./ df.nkeys
    df
end
#-

df_speedup = combine(groupby(df, Not([:ms, :ntasks, :alg, :Implementation]))) do g
    baseline = only(g.ms[g.alg.===:base_seq])
    hcat(g, DataFrame((; speedup = baseline ./ g.ms)))
end
#-

function parallel_algorithms(df)
    idx = df.alg .∈ Ref((:base_par, :cdict_par))
    return df[idx, :]
end

plt = @vlplot(
    facet = {column = {field = :Implementation}},
    spec = {
        layer = [
            {
                # :line,
                mark = {:line, point = true},
                encoding = {
                    x = {:ntasks, type = :quantitative, title = "Number of Tasks"},
                    y = {
                        :speedup,
                        type = :quantitative,
                        title = "Speedup wrt sequential program",
                    },
                    color = {field = access_param, type = :ordinal},
                },
            },
            {mark = :rule, encoding = {y = {datum = 1}}},
        ],
    },
    data = parallel_algorithms(df_speedup),
)

save(joinpath(@__DIR__, "build/results.png"), plt)
save(joinpath(@__DIR__, "build/results.svg"), plt)

plt
