module TestConcurrentCollections
using Test

include("load.jl")

@info "Starting test" Threads.nthreads()

@testset "$file" for file in sort([
    file for file in readdir(@__DIR__) if match(r"^test_.*\.jl$", file) !== nothing
])
    include(file)
end

end  # module
