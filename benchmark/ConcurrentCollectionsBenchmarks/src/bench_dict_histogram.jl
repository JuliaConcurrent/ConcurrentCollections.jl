module BenchDictHistogram

using BangBang.Extras: modify!!
using BenchmarkTools
using ConcurrentCollections

function generate(; datasize = 2^19, nkeys = datasize)
    lastkey = string(nkeys)
    prefix = suffix = ""
    # prefix = "9" ^ 30  # adding works for hashing and comparison
    # suffix = "0" ^ 30  # adding works for hashing (but not for comparison)
    ks = prefix .* string.(1:nkeys; pad = length(lastkey)) .* suffix
    data = rand(ks, datasize)
    return data
end

function hist_seq!(dict::AbstractDict{<:Any,Value}, data) where {Value}
    dict′ = nothing
    for k in data
        dict′, _ = modify!!(dict, k) do v
            Base.@_inline_meta
            Some(something(v, zero(Value)) + oneunit(Value))
        end
    end
    @assert dict′ === dict
    return dict
end

function hist_seq!(dict::ConcurrentDict, data)
    for k in data
        modify!(dict, k) do ref
            Base.@_inline_meta
            Some(ref === nothing ? 1 : ref[] + 1)
        end
    end
    return dict
end

function hist_parallel!(dict::ConcurrentDict, data; ntasks = Threads.nthreads())
    # for k in data
    #     dict[k] = 0
    # end
    @sync for chunk in Iterators.partition(data, cld(length(data), ntasks))
        Threads.@spawn hist_seq!(dict, chunk)
    end
    return dict
end

function hist_dac_impl(data, chunk_starts, basesize)
    if length(chunk_starts) == 0
        return Dict{String,Int}()
    elseif length(chunk_starts) == 1
        i = @inbounds chunk_starts[begin]
        chunk = @inbounds data[i:min(i + basesize - 1, end)]
        return hist_seq!(Dict{String,Int}(), chunk)
    else
        h = length(chunk_starts) ÷ 2
        left_chunk = @view chunk_starts[begin:begin+h-1]
        right_chunk = @view chunk_starts[begin+h:end]
        task = Threads.@spawn hist_dac_impl(data, right_chunk, basesize)
        left = hist_dac_impl(data, left_chunk, basesize)
        right = fetch(task)::typeof(left)
        return mergewith!(+, left, right)
    end
end

function hist_parallel_dac(data; ntasks = Threads.nthreads())
    basesize = cld(length(data), ntasks)
    chunk_starts = firstindex(data):basesize:lastindex(data)
    return hist_dac_impl(data, chunk_starts, basesize)
end

function default_ntasks_list()
    ntasks_list = [Threads.nthreads()]
    if Threads.nthreads() > 2
        pushfirst!(ntasks_list, 2)
    end
    return ntasks_list
end

const CACHE = Ref{Any}()

function setup(; ntasks_list = default_ntasks_list(), generate_options...)
    CACHE[] = data = generate(; generate_options...)
    T = typeof(data)

    suite = BenchmarkGroup()
    suite["base-seq"] = @benchmarkable(
        # Base.Dict, sequential
        hist_seq!(dict, CACHE[]::$T),
        setup = (dict = Dict{String,Int}()),
        evals = 1,
    )
    suite["cdict-seq"] = @benchmarkable(
        # ConcurrentDict, sequential
        hist_seq!(dict, CACHE[]::$T),
        setup = (dict = ConcurrentDict{String,Int}()),
        evals = 1,
    )
    for ntasks in ntasks_list
        suite["dict-ntasks=$ntasks"] = @benchmarkable(
            # Base.Dict, parallel
            hist_parallel_dac(CACHE[]::$T; ntasks = $ntasks),
            evals = 1,
        )
        suite["cdict-ntasks=$ntasks"] = @benchmarkable(
            # ConcurrentDict, parallel
            hist_parallel!(dict, CACHE[]::$T; ntasks = $ntasks),
            setup = (dict = ConcurrentDict{String,Int}()),
            evals = 1,
        )
    end
    return suite
end

function clear()
    CACHE[] = nothing
end

end  # module
