#!/bin/bash
# -*- mode: julia -*-
#=
JULIA="${JULIA:-julia}"
JULIA_CMD="${JULIA_CMD:-$JULIA --color=yes --startup-file=no --compile=min -O0}"
export JULIA_LOAD_PATH=@
export JULIA_PROJECT="$(dirname "${BASH_SOURCE[0]}")"
exec $JULIA_CMD "${BASH_SOURCE[0]}" "$@"
=#

module InfoDump

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

info() = (; git = git_info(), julia = julia_info())

end  # module

if abspath(PROGRAM_FILE) == @__FILE__
    import JSON
    JSON.print(stdout, InfoDump.info(), 4)
end
