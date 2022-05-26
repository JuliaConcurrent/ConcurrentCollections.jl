using ConcurrentCollections: DualLinkedConcurrentRingQueue
using ConcurrentCollectionsBenchmarks.BenchQueueHotPotato: hotpotato!, fai_stats
using JSON

include("../info_dump.jl")

function sweep(; repeat = 10, duration = 1, maxntasks = Threads.nthreads())
    # cooldown() = sleep(0.1)
    # cooldown() = GC.gc()
    cooldown() = nothing
    potatos = []
    fais = []
    for trialid in 1:repeat
        for ntasks in 1:maxntasks
            println(stderr, "Trial $trialid/$repeat #Tasks $ntasks/$maxntasks")

            cooldown()
            result = hotpotato!(DualLinkedConcurrentRingQueue{Bool}(); ntasks, duration)
            push!(potatos, (; trialid, result, ntasks, impl = :dlcrq))
            cooldown()
            result = hotpotato!(Channel{Bool}(Inf); ntasks, duration)
            push!(potatos, (; trialid, result, ntasks, impl = :base))

            cooldown()
            result = fai_stats(; ntasks, duration)
            push!(fais, (; trialid, result, ntasks))
        end
    end
    return (; potatos, fais, repeat, duration)
end

function main(args = ARGS)
    output = get(args, 1, joinpath(@__DIR__, "build", "results.json"))
    mkpath(dirname(output))
    info = InfoDump.info()
    @info "Warmup..."
    sweep(; repeat = 1, duration = 0.1, maxntasks = 1)
    @info "Benchmarking..."
    results = sweep()
    results = (; results, info)
    open(output, write = true) do io
        JSON.print(io, results)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
