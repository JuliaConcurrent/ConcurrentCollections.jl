module TestAtomicsutils

using ConcurrentCollections.Implementations:
    atomic_getfield, atomic_setfield!, atomic_casfield!
using ConcurrentCollections.Implementations.UnsafeAtomics: monotonic
using Test

mutable struct M1
    xs::Vector{Int}
    ys::Union{Vector{Int},Nothing}
end

function var"test_fields"()
    a = [0]
    b = [1]
    c = [2]
    d = [4]

    @testset "get" begin
        m = M1(a, b)
        @test atomic_getfield(m, Val(:xs)) === a
        @test atomic_getfield(m, Val(:ys)) === b
        @test atomic_getfield(m, Val(:xs), monotonic) === a
        @test atomic_getfield(m, Val(:ys), monotonic) === b
    end

    @testset "set" begin
        m = M1(a, b)
        atomic_setfield!(m, Val(:xs), c)
        @test m.xs === c
        atomic_setfield!(m, Val(:ys), nothing)
        @test m.ys === nothing
        atomic_setfield!(m, Val(:ys), b)
        @test m.ys === b
    end

    @testset "cas" begin
        m = M1(a, b)
        @test atomic_casfield!(m, Val(:xs), a, c)
        @test m.xs === c
        @test !atomic_casfield!(m, Val(:xs), a, d)
        @test m.xs === c
        @test atomic_casfield!(m, Val(:ys), b, nothing)
        @test m.ys === nothing
        @test !atomic_casfield!(m, Val(:ys), b, c)
        @test m.ys === nothing
    end
end

end  # module
