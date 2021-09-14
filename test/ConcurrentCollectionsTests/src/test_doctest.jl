module TestDoctest

import ConcurrentCollections
using Documenter: doctest
using Test

function var"test_doctest"()
    doctest(ConcurrentCollections)
end

end  # module
