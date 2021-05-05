module TestDictImpl

using ConcurrentCollections
using ConcurrentCollections.Implementations: BoxedKeyPair, InlinedPair
using Test

slottype(Key, Value) = eltype(ConcurrentDict{Key,Value}().slots)

@testset "slot type" begin
    @test slottype(Int8, Int) <: InlinedPair{Int8,Int}
    @test slottype(Int16, Int) <: InlinedPair{Int16,Int}
    @test slottype(Int32, Int) <: InlinedPair{Int32,Int}
    @test slottype(Int, Int) <: Ref
    @test slottype(Int32, Int32) <: InlinedPair{Int32,Int32}
    @test slottype(String, Int) <: BoxedKeyPair{String,Int}
end

end  # module 
