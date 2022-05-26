using Pkg

packages = [
    PackageSpec(
        name = "ConcurrentCollections",
        path = dirname(@__DIR__),
        # url = "https://github.com/JuliaConcurrent/ConcurrentCollections.jl.git",
    ),
    PackageSpec(
        name = "ConcurrentCollectionsBenchmarks",
        path = joinpath(@__DIR__, "ConcurrentCollectionsBenchmarks"),
        # url = "https://github.com/JuliaConcurrent/ConcurrentCollections.jl.git",
        # subdir = "benchmark/ConcurrentCollectionsBenchmarks",
    ),
]

Pkg.develop(packages)
# Pkg.add(packages)

Pkg.instantiate()
