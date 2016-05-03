#!/usr/local/bin/math -script

<<FermionOrder`

GrassmannOdd = {θ, θb, ϑ, ϑb, ψ};

LinearFunctions = {d};

result1 = fermionOrder[θb · θ + ϑb · ϑ];

result2 = fermionOrder[θb ∘ θ];

result3 = fermionOrder[ψ[1] · d[ψ][2]];

Print[
	TestPrint[FermionOrder`Test`Suite[], "onFail", "terminal"]
];

