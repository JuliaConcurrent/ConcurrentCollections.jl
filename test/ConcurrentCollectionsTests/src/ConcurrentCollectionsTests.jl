module ConcurrentCollectionsTests

include("test_bench_dict_histogram.jl")
include("test_bench_smoke.jl")
include("test_crq.jl")
include("test_dict.jl")
include("test_dlcrq.jl")
include("test_doctest.jl")
include("test_lcrq.jl")
include("test_mpcrq.jl")
include("test_msqueue.jl")
include("test_tsstack.jl")
include("test_work_stealing_deque.jl")

function __init__()
    @info "Starting test" Threads.nthreads()
end

end  # module ConcurrentCollectionsTests
