using Documenter
using ConcurrentCollections

makedocs(
    sitename = "ConcurrentCollections",
    format = Documenter.HTML(),
    modules = [ConcurrentCollections],
)

for (root, dirs, files) in walkdir(joinpath(@__DIR__, "build"))
    for name in files
        path = joinpath(root, name)
        if endswith(name, ".html") || name == "search_index.js"
            html = replace(
                read(path, String),
                "ConcurrentCollections.Implementations" => "ConcurrentCollections",
            )
            write(path, html)
        end
    end
end

deploydocs(
    repo = "github.com/tkf/ConcurrentCollections.jl",
    # push_preview = true,
)
