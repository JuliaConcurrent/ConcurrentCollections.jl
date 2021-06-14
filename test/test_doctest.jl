module TestDoctest

import ConcurrentCollections
using Documenter: doctest
using Test

@testset "doctest" begin
    doctest(ConcurrentCollections)
end

end  # module
