module BenchDictGetExisting

using BenchmarkTools
using ConcurrentCollections

function generate(; datasize = 2^10)
    vs = UInt64.(1:datasize)
    ks = string.(vs; pad = 16)
    # ks = vs
    # ks = UInt32.(vs)
    dict = ConcurrentDict{eltype(ks),eltype(vs)}(zip(ks, vs))
    return dict, ks
end

function sumall(dict, ks)
    s = zero(valtype(dict))
    for k in ks
        s += dict[k]
    end
    return s
end

const CACHE = Ref{Any}()

function setup()
    cdict, ks = generate()
    dict = Dict(cdict)
    CACHE[] = (cdict = cdict, dict = dict, ks = ks)

    suite = BenchmarkGroup()
    suite["base-seq"] = @benchmarkable(
        sumall(dict, ks),
        setup = begin
            dict = CACHE[].dict::$(typeof(dict))
            ks = CACHE[].ks::$(typeof(ks))
        end,
        evals = 1,
    )
    suite["cdict-seq"] = @benchmarkable(
        sumall(dict, ks),
        setup = begin
            dict = CACHE[].cdict::$(typeof(cdict))
            ks = CACHE[].ks::$(typeof(ks))
        end,
        evals = 1,
    )
    return suite
end

function clear()
    CACHE[] = nothing
end

end  # module
