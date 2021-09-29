baremodule ConcurrentCollections

export
    #
    ConcurrentDict,
    ConcurrentQueue,
    ConcurrentStack,
    Delete,
    DualLinkedConcurrentRingQueue,
    DualLinkedQueue,
    Keep,
    LinkedConcurrentRingQueue,
    WorkStealingDeque,
    length_upper_bound,
    length_upper_bound,
    modify!,
    maybeget,
    maybepop!,
    maybepopfirst!

import Base

struct Keep{Value}
    value::Value
end

struct Delete{Value}
    value::Value
end

abstract type ConcurrentDict{Key,Value} <: Base.AbstractDict{Key,Value} end

function modify! end
function maybepop! end
function maybepopfirst! end
function maybeget end
function length_lower_bound end
function length_upper_bound end

module Implementations

using Base:
    #
    HasEltype,
    IteratorEltype,
    RefValue,
    aligned_sizeof

using ..ConcurrentCollections:
    ConcurrentCollections,
    ConcurrentDict,
    Delete,
    Keep,
    length_lower_bound,
    length_upper_bound,
    modify!,
    maybeget,
    maybepop!,
    maybepopfirst!

include("UnsafeAtomics.jl")
using .UnsafeAtomics: acq_rel, acquire, monotonic, release, seq_cst, unordered

include("utils.jl")
include("cache.jl")
include("atomicsutils.jl")
include("promise.jl")
include("dict.jl")
include("workstealing.jl")
include("msqueue.jl")
include("stack.jl")
include("lcrq.jl")
include("dlcrq.jl")
include("ssqueue.jl")
include("misc.jl")

end  # module Implementations

using .Implementations:
    ConcurrentQueue,
    ConcurrentStack,
    DualLinkedConcurrentRingQueue,
    DualLinkedQueue,
    LinkedConcurrentRingQueue,
    WorkStealingDeque

Implementations.define_docstrings()

end  # baremodule ConcurrentCollections
