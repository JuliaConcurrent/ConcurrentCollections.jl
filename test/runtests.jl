module TestConcurrentCollections
using Test

include("load.jl")

@info "Starting test" Threads.nthreads()

@testset "$file" for file in sort([
    file for file in readdir(@__DIR__) if match(r"^test_.*\.jl$", file) !== nothing
])
    file == "test_dict.jl" && continue
    include(file)
end

end  # module
