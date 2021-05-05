baremodule ConcurrentCollections

export
    #
    ConcurrentDict,
    Delete,
    Keep,
    length_upper_bound,
    length_upper_bound,
    modify!,
    tryget,
    trypop!

import Base

struct Keep{Value}
    value::Value
end

struct Delete{Value}
    value::Value
end

abstract type ConcurrentDict{Key,Value} <: Base.AbstractDict{Key,Value} end

function modify! end
function trypop! end
function tryget end
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
    tryget,
    trypop!

include("UnsafeAtomics.jl")
using .UnsafeAtomics: acq_rel, acquire, monotonic, release, seq_cst, unordered

include("utils.jl")
include("atomicsutils.jl")
include("dict.jl")

end  # module Implementations

end  # baremodule ConcurrentCollections
