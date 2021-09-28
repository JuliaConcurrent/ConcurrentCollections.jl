module BenchDictMigration

using BenchmarkTools
using ConcurrentCollections
using ConcurrentCollections.Implementations:
    LINEAR_PROBING_DICT_EXPAND_BASESIZE, migrate_serial!

pad16(x) = string(x; pad = 16)

function generate(f = pad16; datasize = LINEAR_PROBING_DICT_EXPAND_BASESIZE[])
    vs = UInt64.(1:datasize)
    ks = f.(vs)
    dict = ConcurrentDict{eltype(ks),eltype(vs)}(zip(ks, vs))
    return dict
end

const CACHE = Ref{Any}()

function setup(; generate_options...)
    CACHE[] = Dict(
        "String-UInt64" => generate(; generate_options...),
        "Uint32-UInt64" => generate(UInt32; generate_options...),
        "UInt64-UInt64" => generate(UInt64; generate_options...),
        # "Uint32-Nothing" => ???,
    )

    suite = BenchmarkGroup()
    for key in keys(CACHE[])
        CacheType = typeof(CACHE[][key])
        suite[key] = @benchmarkable(
            migrate_serial!(newslots, slots),
            setup = begin
                dict = CACHE[][$key]::$CacheType
                slots = copy(dict.slots)
                newslots = similar(slots, length(slots) * 2)
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
