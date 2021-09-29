function ConcurrentCollections.maybepopfirst!(ch::Channel)
    y = iterate(ch)
    y === nothing && return nothing
    return Some(first(y))
end
