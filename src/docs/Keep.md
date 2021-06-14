    Keep(ans)

A special type used in [`modify!`](@ref) to indicate that a slot should be
remain unchanged while propagating the result `ans` of some computation to
the caller.

That is to say,

```Julia
y = modify!(dict, key) do value
    Keep(f(something(value)))
end
y[]
```

is an optimization of

```Julia
r = Ref{Any}()
modify!(dict, key) do value
    r[] = f(something(value))
    Some(value)
end
r[]
```
