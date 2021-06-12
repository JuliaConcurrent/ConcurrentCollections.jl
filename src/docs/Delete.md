    Delete(ans)

A special type used in [`modify!`](@ref) to indicate that a slot should be
removed.

That is to say

```Julia
y = modify!(dict, key) do value
    Delete(f(something(value)))
end
y[]
```

is an optimization of

```Julia
r = Ref{Any}()
modify!(dict, key) do value
    r[] = f(something(value))
    nothing
end
r[]
```
