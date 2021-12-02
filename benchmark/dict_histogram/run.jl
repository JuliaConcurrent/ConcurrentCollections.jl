import BenchmarkTools
import ConcurrentCollectionsBenchmarks
import JSON

function git_info(dir = @__DIR__)
    git(cmd) = strip(read(setenv(`git $cmd`; dir), String))
    return (;
        revision = git(`rev-parse HEAD`),
        status = git(`status --short --untracked-files=no --porcelain`),
    )
end

function julia_info()
    return (
        version = string(VERSION),
        git = (
            commit = Base.GIT_VERSION_INFO.commit,
            branch = Base.GIT_VERSION_INFO.branch,
        ),
        is_debugbuild = ccall(:jl_is_debugbuild, Cint, ()) != 0,
        libllvm_version = string(Base.libllvm_version),
        Sys = (
            WORD_SIZE = Sys.WORD_SIZE,
            JIT = Sys.JIT,
            # CPU_NAME = Sys.CPU_NAME,
            # CPU_THREADS = Sys.CPU_THREADS,
        ),
        env = Dict(k => v for (k, v) in ENV if startswith(k, "JULIA_")),
    )
end

function main(args = ARGS)
    output = get(args, 1, joinpath(@__DIR__, "build", "results.json"))
    mkpath(dirname(output))

    info = (; git = git_info(), julia = julia_info())
    open(joinpath(dirname(output), "info.json"), write = true) do io
        JSON.print(io, info)
    end

    suite = ConcurrentCollectionsBenchmarks.BenchDictHistogram.setup(
        ntasks_list = 1:Threads.nthreads(),
        nkeys_list = [2^13, 2^16, 2^19, 2^25],
    )
    results = run(suite; verbose = true)
    BenchmarkTools.save(output, results)
    return results
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
