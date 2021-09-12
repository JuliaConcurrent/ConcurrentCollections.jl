module Utils

function maptasks(f, xs)
    tasks = Task[]
    for (tid, x) in enumerate(xs)
        t = @task f(x)
        t.sticky = false
        ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, mod1(tid, Threads.nthreads()) - 1)
        schedule(t)
        push!(tasks, t)
    end
    return tasks
end

end  # module
