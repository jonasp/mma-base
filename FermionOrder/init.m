BeginPackage["FermionOrder`", {"Grassmann`", "NCProduct`", "MTest`"}]

FermionOrder::usage = "Order fermionic expressions canonically.";
Nf::usage = "";

Begin["`Private`"]

RegisterNCProduct[CenterDot];

Nf[a_, b__] := Nf[a] Nf[b];
Nf[exp_?GrassmannOddFreeQ] := 1;
Nf[exp_?GrassmannOddQ] := -1;
Nf[] := 1;

(* general rules *)
(*FermionOrder[p_Plus] := Plus @@ (FermionOrder /@ p);*)
(*FermionOrder[n_?NumberQ a_] := n FermionOrder[a];*)
(*FermionOrder[l_List] := FermionOrder /@ l;*)

(* Ordering of non commutative product *)
FermionOrder[exp_] := exp //. Union[CenterDotOrderRules]

CenterDotOrderRules = {
	l___ · (a_ b_) · r___ /; GrassmannOddFreeQ[a] :> a (l · b · r),
	l___ · a_ · r___ /; !GrassmannOddQ[a] :> a (l · r),
	l___ · q_ · q_ · r___ /; GrassmannOddQ[q] -> 0,
	l___ · q1_ · q2_ · r___ /; !OrderedQ[{q1, q2}] &&
			GrassmannOddQ[q1] && GrassmannOddQ[q2] :> - (l · q2 · q1 · r)
}

(*FermionOrder[a_] := a;*)

End[] (* "`Private`" *)

Begin["`Test`"]

ClearAll[Suite];
Suite[] := Block[{GrassmannOddVars, LinearFunctions},
	GrassmannOddVars = {ψ, χ};
	LinearFunctions = {d};

	Describe["FermionOrder",
		Describe["FermionOrder non commutative product",
			It["should work on different objects",
				Expect[FermionOrder[{b · a}]] @ ToBe[{a b}],
				Expect[FermionOrder[b · a == a · b]] @ ToBe[True]
			],
			It["should reduce Grassmann even epressions to standard multiply",
				Expect[FermionOrder[b · a]] @ ToBe[a b],
				Expect[FermionOrder[2 b · a]] @ ToBe[2 a b]
			],
			It["should order Grassmann odd variables",
				Expect[FermionOrder[ψ · χ]] @ ToBe[- (χ · ψ)]
			],
			It["should order Grassmann odd functions",
				Expect[FermionOrder[ψ[1,2] · χ[1,2]]] @ ToBe[- (χ[1,2] · ψ[1,2])],
				Expect[FermionOrder[χ[2,1] · χ[1,2]]] @ ToBe[- (χ[1,2] · χ[2,1])],
				Expect[FermionOrder[χ[1,2] · χ[1,1]]] @ ToBe[- (χ[1,1] · χ[1,2])]
			],
			It["should order when using linear functions",
				Expect[FermionOrder[ψ[1] · d[ψ][2]]] @ ToBe[- FermionOrder[d[ψ][2] · ψ[1]]]
			],
			It["should annihilate double Grassmann functions",
				Expect[FermionOrder[ψ[1,2] · ψ[1,2]]] @ ToBe[0]
			]
		],
		Describe["nf",
			It["should return 1 for even objects",
				Expect[Nf[a, b]] @ ToBe[1],
				Expect[Nf[ψ, χ]] @ ToBe[1]
			],
			It["should return -1 for odd objects",
				Expect[Nf[ψ]] @ ToBe[-1]
			]
		]
	]
];

End[] (* "`Test`" *)

EndPackage[]
