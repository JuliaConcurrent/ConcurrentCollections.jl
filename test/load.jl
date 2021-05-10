try
    using ConcurrentCollectionsBenchmarks
    true
catch
    false
end || begin
    let path = joinpath(@__DIR__, "../benchmark/ConcurrentCollectionsBenchmarks/Project.toml")
        path in LOAD_PATH || push!(LOAD_PATH, path)
    end
    using ConcurrentCollectionsBenchmarks
end
