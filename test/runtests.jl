if lowercase(get(ENV, "CONCURRENTCOLLECTIONS_JL_ASSERTION", "false")) == "true"
    import ConcurrentCollections
    ConcurrentCollections.Implementations.enable_assertion()
    @assert ConcurrentCollections.Implementations.assertion_enabled()
    @info "ConcurrentCollections: Assertion enabled"
else
    @info "ConcurrentCollections: Assertion disenabled (default)"
end

using TestFunctionRunner
TestFunctionRunner.@run(paths = ["../benchmark/ConcurrentCollectionsBenchmarks"])
