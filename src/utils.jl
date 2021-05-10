@noinline unreachable() = error("unreachable reached")

@noinline unexpected(x) = error("unexpected value: $x")

@noinline static_error(::Val{msg}) where {msg} = error(string(msg))

macro static_error(msg::AbstractString)
    sym = Symbol(msg)
    :(static_error(Val{$(QuoteNode(sym))}()))
end

struct Inlined{T}
    x::T
end

@inline Base.convert(::Type{Inlined{T}}, x::T) where {T} = Inlined{T}(x)

function new_ipadder end

struct IPadder{T,Pad}
    x::T
    pad::NTuple{Pad,UInt8}
    global new_ipadder(x::T, ::Type{T}, ::Val{Pad}) where {T,Pad} =
        new{T,Pad}(x, ntuple(_ -> 0, Val(Pad)))
end

@inline IPadder{T,Pad}(args...) where {T,Pad} = new_ipadder(T(args...), T, Val(Pad))

padsize(::Type{IPadder{T,Pad}}) where {T,Pad} = Pad
padsize(padder::IPadder) = padsize(typeof(padder))

function Base.show(io::IO, padder::IPadder)
    @nospecialize padder
    print(io, "IPadder{_,", length(padder.pad), "}(")
    show(io, padder.x)
    print(io, ")")
end

paddee(::Type{<:IPadder{T}}) where {T} = T
refee(::Type{<:Ref{T}}) where {T} = T

# paddee(::T) where {T} = paddee(T)
# @noinline paddee(T::Type) = unreachable()

@inline Base.convert(::Type{P}, x::T) where {T,Pad,P<:IPadder{T,Pad}} = P(x)
# Base.convert(::Type{T}, x::IPadder{<:T}) where {T} = x.x

function cas_compatible(::Type{T}) where {T}
    if aligned_sizeof(T) <= 16
        T
    else
        nothing
    end
end

function padsize_for_cas(::Type{T}) where {T}
    if sizeof(T) <= 1
        return 0
    elseif sizeof(T) <= 2
        return 2 - sizeof(T)
    elseif sizeof(T) <= 4
        return 4 - sizeof(T)
    elseif sizeof(T) <= 8
        return 8 - sizeof(T)
    elseif sizeof(T) <= 16
        return 16 - sizeof(T)
    else
        return 0  # should be handled by the caller
    end
end

function padded_type(::Type{T}) where {T}
    Padded = IPadder{T,padsize_for_cas(T)}
    if sizeof(Padded) > 16
        @static_error("size of padded type is larger than 16 bytes")
    end
    return Padded
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
    isinlinable(T) || @static_error("expected isbits or union of them")
    let ref = paddedref(T, UIntType)
        GC.@preserve ref begin
            unsafe_store!(Ptr{UIntType}(pointer_from_objref(ref)), uint)
        end
        return ref.x
    end
end

@inline isinlinable(::Type{Inlined{T}}) where {T} = isinlinable(T)
@inline isinlinable(T::Type) = Base.isbitstype(T) || Base.isbitsunion(T)

@inline function zerofill(::Type{T}) where {T}
    if Base.isbitstype(T)
        return zerofill!(Ref{T}())[]
    elseif Base.isbitsunion(T)
        return zerofill(T.a)
    else
        _error_zerofill(T)
    end
end

@noinline _error_zerofill(T) = error("cannot zero-fill non-isbit type $T")

@inline function zerofill!(ref::Ref{T}) where {T}
    GC.@preserve ref begin
        unsafe_zerofill!(pointer_from_objref(ref), Val(sizeof(T)))
    end
    return ref
end

@generated function unsafe_zerofill!(ptr::Ptr{Cvoid}, ::Val{Bytes}) where {Bytes}
    IR = (
        """
        define void @entry(i$(Base.Sys.WORD_SIZE) %0) #0 {
        top:
            %ptr = inttoptr i$(Base.Sys.WORD_SIZE) %0 to i8*
            call void @llvm.memset.p0i8.i32(i8* %ptr, i8 0, i32 $Bytes, i1 0)
            ret void
        }

        declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)

        attributes #0 = { alwaysinline }
        """,
        "entry",
    )
    quote
        $(Expr(:meta, :inline))
        Base.llvmcall($IR, Cvoid, Tuple{Ptr{Cvoid}}, ptr)
    end
end

const _FALSE_ = Ref(false)

# TODO: a teribble hack to force heap-allocation of `x`; can we get rid of it?
@inline function forceheap(x)
    if _FALSE_[]
        global SINK = x
    end
    return x
end

@generated function julia_write_barrier(args::Vararg{Any,N}) where {N}
    pointer_exprs = map(1:N) do i
        :(_pointer_from_objref(args[$i]))
    end
    jlp = "{} addrspace(10)*"
    llvm_args = string.("%", 0:N-1)
    word = "i$(Base.Sys.WORD_SIZE)"
    entry_sig = join(word .* llvm_args, ", ")
    ptrs = string.("%ptr", 0:N-1)
    wb_sig = join("$jlp " .* ptrs, ", ")
    inttoptr = join(
        (ptrs .* "_tmp = inttoptr $word " .* llvm_args .* " to {}*\n") .*
        (ptrs .* " = addrspacecast {}* " .* ptrs .* "_tmp to $jlp"),
        "\n",
    )
    IR = (
        """
        define void @entry($entry_sig) #0 {
        top:
            $inttoptr
            call void ($jlp, ...) @julia.write_barrier($wb_sig)
            ret void
        }

        declare void @julia.write_barrier($jlp, ...) #1

        attributes #0 = { alwaysinline }
        attributes #1 = { inaccessiblememonly norecurse nounwind }
        """,
        "entry",
    )
    quote
        $(Expr(:meta, :inline))
        Base.llvmcall($IR, Cvoid, NTuple{N,Ptr{Cvoid}}, $(pointer_exprs...))
    end
end

@generated allocate_singleton_ref(::Type{T}) where {T} = Ref{Any}(T.instance)

@inline function pointer_from_singleton(::T) where {T}
    refptr = pointer_from_objref(allocate_singleton_ref(T))
    return unsafe_load(Ptr{Ptr{Cvoid}}(refptr))
end

@inline function _pointer_from_objref(obj::T) where {T}
    if Base.issingletontype(T)
        return pointer_from_singleton(obj)
    else
        return pointer_from_objref(obj)
    end
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
