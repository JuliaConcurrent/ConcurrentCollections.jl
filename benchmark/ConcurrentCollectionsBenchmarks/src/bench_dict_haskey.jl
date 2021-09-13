module BenchDictHasKey

using BenchmarkTools
using ConcurrentCollections

function generate(;
    datasize = 2^13,  # `Base.Dict` is better on smaller size
    keysize = 50,  # expensive isequal; favors ConcurrentDict
    nkeys = 100,
)
    vs = UInt64.(1:datasize)
    ks = string.(vs; pad = keysize)
    # ks = vs
    # ks = UInt32.(vs)
    cdict = ConcurrentDict{eltype(ks),eltype(vs)}(zip(ks, vs))
    ks_100 = ks[1:nkeys]
    ks_000 = string.(.-vs[1:nkeys])
    ks_050 = ifelse.(isodd.(vs[1:nkeys]), ks_100, ks_000)
    return (; cdict, ks_100, ks_000, ks_050)
end

const CACHE = Ref{Any}()

function setup(; cases = [:ks_050, :ks_000], kwargs...)
    data = generate(; kwargs...)
    (; cdict) = data
    dict = Dict(cdict)
    CACHE[] = (; dict, data...)

    labelmap = Dict(
        :ks_100 => "100% existing",
        :ks_050 => "50% existing",  # `Base.Dict` is better with 50% hit
        :ks_000 => "0% existing",
    )

    suite = BenchmarkGroup()
    for ksprop in cases
        s1 = suite[labelmap[ksprop]] = BenchmarkGroup()
        ks = getproperty(data, ksprop)
        s1["base-seq"] = @benchmarkable(
            count(k -> haskey(dict, k), ks),
            setup = begin
                dict = CACHE[].dict::$(typeof(dict))
                ks = CACHE[].$ksprop::$(typeof(ks))
            end,
            evals = 1,
        )
        s1["cdict-seq"] = @benchmarkable(
            count(k -> haskey(dict, k), ks),
            setup = begin
                dict = CACHE[].cdict::$(typeof(cdict))
                ks = CACHE[].$ksprop::$(typeof(ks))
            end,
            evals = 1,
        )
    end
    return suite
end

function clear()
    CACHE[] = nothing
end

end  # module
