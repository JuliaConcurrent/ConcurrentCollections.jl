==′(x::T, y::T) where {T} = x == y
!=′(x::T, y::T) where {T} = x != y

assertion_enabled() = false
# assertion_enabled() = true

function enable_assertion()
    prev = assertion_enabled()
    @eval assertion_enabled() = true
    return prev
end

@noinline unreachable() = error("unreachable reached")

@noinline unexpected(x) = error("unexpected value: $x")

@noinline static_error(::Val{msg}) where {msg} = error(string(msg))

macro static_error(msg::AbstractString)
    sym = Symbol(msg)
    :(static_error(Val{$(QuoteNode(sym))}()))
end

function noinline(f)
    @noinline g() = f()
    return g()
end

"""
    is_pointerfree_type(T::Type) :: Bool

Return `true` if any instances of `T` do not contain boxed Julia objects.
"""
is_pointerfree_type(::Type{T}) where {T} = isconcretetype(T) && Base.datatype_pointerfree(T)

function ceillog2(n::Integer)
    n > 0 || throw(DomainError(n))
    i = trailing_zeros(n)
    j = 8 * sizeof(n) - leading_zeros(n) - 1
    if i == j
        return i
    else
        return j + 1
    end
end

@inline function uint_for(::Type{T}) where {T}
    if sizeof(T) <= 1
        return UInt8
    elseif sizeof(T) <= 2
        return UInt16
    elseif sizeof(T) <= 4
        return UInt32
    elseif sizeof(T) <= 8
        return UInt64
    elseif sizeof(T) <= 16
        return UInt128
    else
        @static_error("size of type is larger than 16 bytes")
    end
end

# TODO: use memcpy?
function uint_from(x::T) where {T}
    UIType = uint_for(T)
    ref = Ref(zero(UIType))
    GC.@preserve ref begin
        unsafe_store!(Ptr{T}(pointer_from_objref(ref)), x)
    end
    return ref[]
end

mutable struct PaddedRef{T,Pad}
    x::T
    pad::NTuple{Pad,UInt8}
    PaddedRef{T,Pad}() where {T,Pad} = new{T,Pad}()
end

@inline paddedref(::Type{T}, ::Type{Desired}) where {T,Desired} =
    PaddedRef{T,sizeof(Desired) - sizeof(T)}()

function from_bytes(::Type{T}, uint::UIntType) where {T,UIntType}
    Base.allocatedinline(T) || @static_error("expected isbits or union of them")
    let ref = paddedref(T, UIntType)
        GC.@preserve ref begin
            unsafe_store!(Ptr{UIntType}(pointer_from_objref(ref)), uint)
        end
        return ref.x
    end
end

function UnsafeAtomics.load(p::Ptr{T}, ord::Val) where {T}
    q = Ptr{uint_for(T)}(p)
    uint = UnsafeAtomics.load(q, ord)
    return from_bytes(T, uint)
end

function UnsafeAtomics.store!(p::Ptr{T}, v::T, ord::Val) where {T}
    uint = uint_from(v)
    q = Ptr{typeof(uint)}(p)
    UnsafeAtomics.store!(q, uint, ord)
end

function UnsafeAtomics.cas!(p::Ptr{T}, cmp::T, new::T, so::Val, fo::Val) where {T}
    ci = uint_from(cmp)
    ni = uint_from(new)
    q = Ptr{typeof(ci)}(p)
    oi = UnsafeAtomics.cas!(q, ci, ni, so, fo)
    return from_bytes(T, oi)
end

# Read /sys/devices/system/cpu/cpu0/cache/index0/coherency_line_size?
const CACHELINE_SIZE = 64

# Workaround
# https://github.com/JuliaconCurrent/ConcurrentCollections.jl/issues/35
# https://github.com/JuliaLang/julia/issues/42326
#=
primitive type PadAfter64 448 end
=#
const PadAfter64 = NTuple{CACHELINE_SIZE - sizeof(UInt64),UInt8}
_PadAfter64() = Ref{PadAfter64}()[]

mutable struct CheckPadAfter64
    a::UInt64
    pad::PadAfter64
    b::UInt64
end
if fieldoffset(CheckPadAfter64, 3) != CACHELINE_SIZE
    @error "Unexpected padding with `PadAfter64`" fieldoffset(CheckPadAfter64, 3)
end

#=
const PadAfter32 = PadAfter64
=#
const PadAfter32 = NTuple{CACHELINE_SIZE - sizeof(UInt32),UInt8}
_PadAfter32() = Ref{PadAfter32}()[]

mutable struct CheckPadAfter32
    a::UInt32
    pad::PadAfter32
    b::UInt32
end
if fieldoffset(CheckPadAfter32, 3) != CACHELINE_SIZE
    @error "Unexpected padding with `PadAfter32`" fieldoffset(CheckPadAfter32, 3)
end

function cacheline_padded_vector(::Type{T}, n::Integer) where {T}
    cacheline = cld(sizeof(T), CACHELINE_SIZE)
    xs = Vector{T}(undef, cacheline * (n + 1))
    ys = view(xs, cacheline+1:cacheline:length(xs))
    @assert length(ys) == n
    return ys
end

function threaded_foreach(f, xs)
    y = iterate(xs)
    y === nothing && return
    x1, st = y
    y = iterate(xs, st)
    if y === nothing
        f(x1)
        return
    end
    tasks = Task[]
    while true
        local x
        x, st = y
        t = Threads.@spawn f(x)
        push!(tasks, t)
        y = iterate(xs, st)
        y === nothing && break
    end
    f(x1)
    foreach(wait, tasks)
    return
end

function is_singleton_collection(xs)
    y = iterate(xs)
    y === nothing && return false
    _, st = y
    y = iterate(xs, st)
    y === nothing && return true
    return false
end

function threaded_typed_mapreduce(f, ::Type{T}, op, xs; kw...) where {T}
    is_singleton_collection(xs) && return f(first(xs))
    refs = [Ref{T}() for _ in xs]
    threaded_foreach(zip(refs, xs)) do (y, x)
        y[] = f(x)
    end
    return mapreduce(getindex, op, refs; kw...)
end

function zip_strict(a, args...)
    lens = map(length, args)
    all(==(length(a)), lens) || noinline() do
        error("collections have non-identical length: ", length(a), ", ", join(lens, ", "))
    end
    return zip(a, args...)
end

function define_docstrings()
    docstrings = [:ConcurrentCollections => joinpath(dirname(@__DIR__), "README.md")]
    docsdir = joinpath(@__DIR__, "docs")
    for filename in readdir(docsdir)
        stem, ext = splitext(filename)
        ext == ".md" || continue
        name = Symbol(stem)
        name in names(ConcurrentCollections, all = true) || continue
        push!(docstrings, name => joinpath(docsdir, filename))
    end
    for (name, path) in docstrings
        include_dependency(path)
        doc = read(path, String)
        doc = replace(doc, r"^```julia"m => "```jldoctest $name")
        doc = replace(doc, "<kbd>TAB</kbd>" => "_TAB_")
        @eval ConcurrentCollections $Base.@doc $doc $name
    end
end
