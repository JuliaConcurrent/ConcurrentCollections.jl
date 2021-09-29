module BenchQueuePushPop

using BenchmarkTools
using ConcurrentCollections

function pushpop!(
    q;
    nrepeat::Integer = 2^15,
    nitems::Integer = 11,
    ntasks::Integer = Threads.nthreads(),
    nspins::Integer = 100,
)
    ntasks < 1 && error("require positive `ntasks`: got $ntasks")
    local tasks
    @sync begin
        tasks = map(1:ntasks) do _
            Threads.@spawn begin
                nyields = 0
                for _ in 1:nrepeat
                    for i in 1:nitems
                        push!(q, i)
                    end
                    for _ in 1:nitems
                        while true
                            y = nothing
                            for _ in 1:nspins
                                y = maybepopfirst!(q)
                                y === nothing || break
                            end
                            y === nothing || break
                            yield()
                            nyields += 1
                            if nyields > 2^20
                                @error "Too many yields!"
                                error("Too many yields!")
                            end
                        end
                    end
                end
                return nyields
            end
        end
    end
    return (; nrepeat, nitems, nyields = map(fetch, tasks))
end

function setup(; kwargs...)
    suite = BenchmarkGroup()
    suite["channel"] = @benchmarkable pushpop!(Channel{Int}(Inf); $kwargs...)
    suite["msqueue"] = @benchmarkable pushpop!(ConcurrentQueue{Int}(); $kwargs...)
    suite["lcrq"] = @benchmarkable pushpop!(LinkedConcurrentRingQueue{Int}(); $kwargs...)
    return suite
end

function clear() end

end  # module
