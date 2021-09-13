module UnsafeAtomics

using Base.Sys: WORD_SIZE
using Base.Threads: inttypes, llvmtypes
using Core.Intrinsics: llvmcall

const unordered = Val{:unordered}()
const monotonic = Val{:monotonic}()
const acquire = Val{:acquire}()
const release = Val{:release}()
const acq_rel = Val{:acq_rel}()
const seq_cst = Val{:seq_cst}()

const orderings = [:unordered, :monotonic, :acquire, :release, :acq_rel, :seq_cst]

@inline load(x) = load(x, seq_cst)
@inline store!(x, v) = store!(x, v, seq_cst)
@inline cas!(x, cmp, new) = cas!(x, cmp, new, seq_cst, seq_cst)
@inline modify!(ptr, op, x) = modify!(ptr, op, x, seq_cst)

right(_, x) = x

const OP_RMW_TABLE = [
    (+) => :add,
    (-) => :sub,
    right => :xchg,
    (&) => :and,
    (⊼) => :nand,
    (|) => :or,
    (⊻) => xor,
    max => :max,
    min => :min,
]

for (op, rmwop) in OP_RMW_TABLE
    fn = Symbol(rmwop, "!")
    @eval @inline $fn(x, v) = $fn(x, v, seq_cst)
    @eval @inline modify!(ptr, ::typeof($op), x, ord::Val) = $fn(ptr, x, ord)
end

for typ in inttypes
    lt = llvmtypes[typ]
    rt = "$lt, $lt*"

    for ord in orderings
        ord in [:release, :acq_rel] && continue

        @eval function load(x::Ptr{$typ}, ::$(Val{ord}))
            return llvmcall(
                $("""
                %ptr = inttoptr i$WORD_SIZE %0 to $lt*
                %rv = load atomic $rt %ptr $ord, align $(sizeof(typ))
                ret $lt %rv
                """),
                $typ,
                Tuple{Ptr{$typ}},
                x,
            )
        end
    end

    for ord in orderings
        ord in [:acquire, :acq_rel] && continue

        @eval function store!(x::Ptr{$typ}, v::$typ, ::$(Val{ord}))
            return llvmcall(
                $("""
                %ptr = inttoptr i$WORD_SIZE %0 to $lt*
                store atomic $lt %1, $lt* %ptr $ord, align $(sizeof(typ))
                ret void
                """),
                Cvoid,
                Tuple{Ptr{$typ},$typ},
                x,
                v,
            )
        end
    end

    for success_ordering in orderings[2:end],
        failure_ordering in [:monotonic, :acquire, :seq_cst]

        @eval function cas!(
            x::Ptr{$typ},
            cmp::$typ,
            new::$typ,
            ::$(Val{success_ordering}),
            ::$(Val{failure_ordering}),
        )
            return llvmcall(
                $(
                    """
                    %ptr = inttoptr i$WORD_SIZE %0 to $lt*
                    %rs = cmpxchg $lt* %ptr, $lt %1, $lt %2 $success_ordering $failure_ordering
                    %rv = extractvalue { $lt, i1 } %rs, 0
                    ret $lt %rv
                    """
                ),
                $typ,
                Tuple{Ptr{$typ},$typ,$typ},
                x,
                cmp,
                new,
            )
        end
    end

    for rmwop in [:add, :sub, :xchg, :and, :nand, :or, :xor, :max, :min]
        rmw = string(rmwop)
        fn = Symbol(rmw, "!")
        if (rmw == "max" || rmw == "min") && typ <: Unsigned
            # LLVM distinguishes signedness in the operation, not the integer type.
            rmw = "u" * rmw
        end
        for ord in orderings
            @eval function $fn(x::Ptr{$typ}, v::$typ, ::$(Val{ord}))
                return llvmcall(
                    $("""
                    %ptr = inttoptr i$WORD_SIZE %0 to $lt*
                    %rv = atomicrmw $rmw $lt* %ptr, $lt %1 $ord
                    ret $lt %rv
                    """),
                    $typ,
                    Tuple{Ptr{$typ},$typ},
                    x,
                    v,
                )
            end
        end
    end

end

end  # module UnsafeAtomics
