struct ThreadLocalCache{T}
    cache::Vector{Vector{T}}
    size::Int
end

function ThreadLocalCache{T}(; size::Integer = 4) where {T}
    cache = [empty!(Vector{T}(undef, size)) for _ in 1:Threads.nthreads()]
    return ThreadLocalCache(cache, size)
end

function maybepop!(cache::ThreadLocalCache)
    buffer = cache.cache[Threads.threadid()]
    if isempty(buffer)
        return nothing
    else
        return pop!(buffer)
    end
end

function trypush!(cache::ThreadLocalCache{T}, x::T) where {T}
    buffer = cache.cache[Threads.threadid()]
    if length(buffer) ≥ cache.size
        return false
    else
        push!(buffer, x)
        return true
    end
end
