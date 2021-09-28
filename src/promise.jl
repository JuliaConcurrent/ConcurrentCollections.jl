mutable struct Promise{T,S>:Union{Nothing,Some{T}}}
    @atomic value::S
    notify::Threads.Condition
    # TODO: just use a stack of Tasks?

    function Promise{T}() where {T}
        if sizeof(Some{Union{Nothing,Some{T}}}) > sizeof(UInt64)
            return new{T,Any}(nothing, Threads.Condition())
        else
            return new{T,Union{Nothing,Some{T}}}(nothing, Threads.Condition())
        end
    end
end

Promise() = Promise{Any}()

function tryput!(p::Promise{T}, value) where {T}
    new = Some{T}(value)
    old, ok = @atomicreplace p.value nothing => new
    if ok
        lock(p.notify) do
            notify(p.notify)
        end
    end
    return old
end

function Base.put!(p::Promise{T}, value) where {T}
    if tryput!(p, value) !== nothing
        error("Promise already has a value")
    end
    return p
end

function Base.fetch(p::Promise{T}) where {T}
    value = @atomic p.value
    if value isa Some{T}
        return something(value)
    end
    lock(p.notify) do
        while true
            local value = @atomic p.value
            if value isa Some{T}
                return something(value)
            end
            wait(p.notify)
        end
    end
end
