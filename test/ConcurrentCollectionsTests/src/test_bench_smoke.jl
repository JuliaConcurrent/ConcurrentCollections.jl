module TestBenchSmoke

using Test
using ConcurrentCollectionsBenchmarks: clear, setup_smoke

function var"test_smoke test benchmarks"()
    try
        local suite
        @test (suite = setup_smoke()) isa Any
        @test run(suite) isa Any
    finally
        clear()
    end
end

end  # module
