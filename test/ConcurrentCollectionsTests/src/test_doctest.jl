module TestDoctest

import ConcurrentCollections
using Documenter: doctest
using Test

function test_doctest()
    doctest(ConcurrentCollections)
end

end  # module
