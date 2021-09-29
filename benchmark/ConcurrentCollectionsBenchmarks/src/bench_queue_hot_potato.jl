module BenchQueueHotPotato

using BenchmarkTools
using ConcurrentCollections
using ConcurrentCollections.Implementations: atomic_modifyfield!
using Formatting: format
using ..Utils: maptasks

const HOTPOTATO = false
const NOTPOTATO = true

const Stat = typeof((npush = 0, npop = 0, t1 = time_ns(), t2 = time_ns()))

Base.@kwdef struct HotPotatoResult
    duration::Union{Float64,Nothing}
    nrepeat::Int
    stats::Vector{Stat}
end

function unfair_sleep(seconds::Real)
    t0 = time_ns()
    ns = seconds * 1e9
    while time_ns() - t0 < ns
        GC.safepoint()
        # yield()
    end
end

@noinline function hotpotato!(
    q;
    ntasks::Integer = Threads.nthreads(),
    duration::Union{Real,Nothing} = nothing,
    nrepeat::Integer = duration === nothing ? 2^15 : typemax(Int64),
    delay::Real = 0,  # not using 0.001 mentioned in Izraelevitz & Scott (2017)
    nfills::Integer = 0,
    ppush::Real = 0.5,
)
    ntasks < 1 && error("require positive `ntasks`: got $ntasks")
    local tasks
    push!(q, HOTPOTATO)
    t0 = time_ns()
    tasks = maptasks(1:ntasks) do itask
        # Core.print("$itask-th task started\n")
        local t1 = time_ns()
        local npush = 0
        local npop = 0
        for irepeat in 1:nrepeat
            # ccall(:jl_breakpoint, Cvoid, (Any,), (; irepeat, itask))
            if rand() < ppush
                push!(q, NOTPOTATO)
                npush += 1
            else
                y = popfirst!(q)
                npop += 1
                if y == HOTPOTATO
                    for _ in 1:nfills
                        push!(q, NOTPOTATO)
                    end
                    unfair_sleep(delay)
                    # sleep(delay)
                    push!(q, y)
                    npush += 1
                end
            end
            if duration !== nothing
                if (time_ns() - t0) / 1e9 > duration
                    break
                end
            end
        end
        return (; npush, npop, t1, t2 = time_ns())::Stat
    end
    return HotPotatoResult(; nrepeat, duration, stats = map(fetch, tasks))
end

mutable struct AtomicRef{T}
    @atomic x::T
end

const FAIStat = typeof((; nfai = 0, t1 = time_ns(), t2 = time_ns()))

Base.@kwdef struct FAIResult
    duration::Union{Float64,Nothing}
    nrepeat::Int
    refvalue::Int
    stats::Vector{FAIStat}
end

function fai_stats(;
    ntasks::Integer = Threads.nthreads(),
    duration::Union{Real,Nothing} = nothing,
    nrepeat::Integer = duration === nothing ? 2^15 : typemax(Int64),
    impl::Val = Val(:threads),
)
    ntasks < 1 && error("require positive `ntasks`: got $ntasks")
    local tasks
    ref = if impl === Val(:threads)
        Threads.Atomic{Int32}(0)
    else
        AtomicRef{Int32}(0)
    end
    t0 = time_ns()
    tasks = maptasks(1:ntasks) do itask
        local t1 = time_ns()
        local nfai = 0
        for irepeat in 1:nrepeat
            if impl === Val(:threads)
                Threads.atomic_add!(ref, Int32(1))
            elseif impl === Val(:unsafe)
                atomic_modifyfield!(ref, Val(:x), +, Int32(1))
            else
                @atomic ref.x += true
            end
            nfai += 1
            if duration !== nothing
                if (time_ns() - t0) / 1e9 > duration
                    break
                end
            end
        end
        return (; nfai, t1, t2 = time_ns())::FAIStat
    end
    if impl === Val(:threads)
        refvalue = ref[]
    else
        refvalue = @atomic ref.x
    end
    return FAIResult(; nrepeat, duration, refvalue, stats = map(fetch, tasks))
end

function setup(; kwargs...)
    suite = BenchmarkGroup()
    suite["channel"] = @benchmarkable hotpotato!(Channel{Bool}(Inf); $kwargs...)
    suite["dlcrq"] =
        @benchmarkable hotpotato!(DualLinkedConcurrentRingQueue{Bool}(); $kwargs...)
    suite["dlq"] = @benchmarkable hotpotato!(DualLinkedQueue{Bool}(); $kwargs...)
    return suite
end

function clear() end

function summarize(result::HotPotatoResult)
    npush = sum(stat.npush for stat in result.stats)
    npop = sum(stat.npop for stat in result.stats)
    t1min = minimum(stat.t1 for stat in result.stats)
    t2max = maximum(stat.t2 for stat in result.stats)
    duration = (t2max - t1min) / 1e9
    throughput = (npush + npop) / duration
    return (; npush, npop, duration, throughput)
end

function Base.show(io::IO, ::MIME"text/plain", result::HotPotatoResult)
    (; npush, npop, duration, throughput) = summarize(result)
    println(io, "Hot potato benchmark with ", length(result.stats), " tasks")
    println(
        io,
        format(npush; commas = true),
        " pushes and ",
        format(npop; commas = true),
        " pops in ",
        duration,
        " seconds",
    )
    print(
        io,
        "throughput = ",
        format(floor(Int, throughput); commas = true),
        " ops/seconds",
    )
end

function summarize(result::FAIResult)
    nfai = sum(stat.nfai for stat in result.stats)
    t1min = minimum(stat.t1 for stat in result.stats)
    t2max = maximum(stat.t2 for stat in result.stats)
    duration = (t2max - t1min) / 1e9
    throughput = nfai / duration
    return (; nfai, duration, throughput)
end

function Base.show(io::IO, ::MIME"text/plain", result::FAIResult)
    (; nfai, duration, throughput) = summarize(result)
    println(io, "FAI benchmark with ", length(result.stats), " tasks")
    println(io, format(nfai; commas = true), " FAIs in ", duration, " seconds")
    print(
        io,
        "throughput = ",
        format(floor(Int, throughput); commas = true),
        " ops/seconds",
    )
end

function timings(result::HotPotatoResult)
    t1min = minimum(stat.t1 for stat in result.stats)
    table =
        Iterators.map(enumerate(result.stats)) do (taskid, stat)
            (; t1, t2) = stat
            return (
                (; t = (t1 - t1min) / 1e9, event = :begin, taskid),
                (; t = (t2 - t1min) / 1e9, event = :end, taskid),
            )
        end |>
        Iterators.flatten |>
        collect
    sort!(table; by = row -> row.t)
    return table
end

end  # module
