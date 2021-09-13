using ConcurrentCollections: DualLinkedConcurrentRingQueue
using ConcurrentCollectionsBenchmarks.BenchQueueHotPotato: hotpotato!, fai_stats
using JSON

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

function git_info(dir = @__DIR__)
    git(cmd) = strip(read(setenv(`git $cmd`; dir), String))
    return (;
        revision = git(`rev-parse HEAD`),
        status = git(`status --short --untracked-files=no --porcelain`),
    )
end

function main(args = ARGS)
    output = get(args, 1, joinpath(@__DIR__, "build", "results.json"))
    mkpath(dirname(output))
    git = git_info()
    @info "Warmup..."
    sweep(; repeat = 1, duration = 0.1, maxntasks = 1)
    @info "Benchmarking..."
    results = sweep()
    results = (; results..., git)
    open(output, write = true) do io
        JSON.print(io, results)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
