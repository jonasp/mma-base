BeginPackage["Grassmann`", {"MTest`"}]

GrassmannOddVars::usage = "List of Grassmann odd objects.";
GrassmannOddQ::usage = "Checks if object is Grassmann odd or not.";
GrassmannOddFreeQ::usage = "Checks if an expression is free of any Grassmann odd object";
LinearFunctions::usage = "List of linear functions acting on Grassmann variables.";

Begin["`Private`"]

(* Helper functions *)

(* d[x[s][0]] -> {d, x, s, 0} *)
Leaves[expr_] := Select[
	Level[expr, Infinity, Heads -> True],
	LeafCount[#] == 1 &
]

DropWhile[list_, test_] := Drop[list, LengthWhile[list, test]]

(* Exported functions *)

LinearFunctionQ[var_] := MemberQ[LinearFunctions, var];

GrassmannOddFreeQ[exp_] := And @@ (FreeQ[exp, #]& /@ GrassmannOddVars);

GrassmannOddQ::unknown = "Cannot be determined if `1` is Grassmann even or odd.";
GrassmannOddQ[a_, b__] := Xor[GrassmannOddQ[a], GrassmannOddQ[b]];
GrassmannOddQ[n_?GrassmannOddFreeQ a_] := GrassmannOddQ[a];
GrassmannOddQ[exp_Symbol] := MemberQ[GrassmannOddVars, exp];
GrassmannOddQ[exp_?NumberQ] := False;
GrassmannOddQ[exp_] := Module[
	{s},
	s = DropWhile[Leaves[exp], LinearFunctionQ];
	If[Length[s] == 0,
		False,
		If[GrassmannOddFreeQ[Rest[s]],
			GrassmannOddQ[First[s]],
			Message[GrassmannOddQ::unknown, exp];
		]
	]
];

End[] (* "`Private`" *)

Begin["`Test`"]

ClearAll[Suite];
Suite[] := Block[{GrassmannOddVars, LinearFunctions},
	GrassmannOddVars = {ψ, χ};
	LinearFunctions = {d};

	Describe["Grassmann",
		Describe["GrassmannOddQ",
			It["should return true for Grassmann odd objects",
				Expect[GrassmannOddQ[ψ]] @ ToBe[True],
				Expect[GrassmannOddQ[ψ[0]]] @ ToBe[True],
				Expect[GrassmannOddQ[ψ[s]]] @ ToBe[True],
				Expect[GrassmannOddQ[ψ[s][0]]] @ ToBe[True],
				Expect[GrassmannOddQ[d[ψ[s][0]]]] @ ToBe[True],
				Expect[GrassmannOddQ[2 ψ]] @ ToBe[True],
				Expect[GrassmannOddQ[ψ / Sqrt[2]]] @ ToBe[True]
			],
			It["should return false for Grassmann even objects",
				Expect[GrassmannOddQ[x]] @ ToBe[False],
				Expect[GrassmannOddQ[d[x]]] @ ToBe[False],
				Expect[GrassmannOddQ[d[x[s][0]]]] @ ToBe[False],
				Expect[GrassmannOddQ[2 x]] @ ToBe[False],
				Expect[GrassmannOddQ[x / Sqrt[2]]] @ ToBe[False]
			]
		],
		Describe["GrassmannOddFreeQ",
			It["should return true for expressions without Grassmann odd objects",
				Expect[GrassmannOddFreeQ[x]] @ ToBe[True],
				Expect[GrassmannOddFreeQ[{x, y}]] @ ToBe[True],
				Expect[GrassmannOddFreeQ[d[x]]] @ ToBe[True]
			],
			It["should return false for expressions involving Grassmann odd objects",
				Expect[GrassmannOddFreeQ[ψ]] @ ToBe[False],
				Expect[GrassmannOddFreeQ[d[ψ]]] @ ToBe[False],
				Expect[GrassmannOddFreeQ[ψ χ]] @ ToBe[False]
			]
		]
	]
];

End[] (* "`Test`" *)

EndPackage[]
