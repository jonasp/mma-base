(* ::Package:: *)

BeginPackage["OMProduct`", {"MTest`"}]

omp::usage = "An ordered matrix product";

(* Notation`AutoLoadNotationPalette = False; *)
(* Needs["Notation`"]; *)

(* InfixNotation[ParsedBoxWrapper["⊙"], omp]; *)

CircleDot = omp;

Begin["`Private`"]

(* exported *)

omp[a_, b_, c__] := omp[omp[a, b], c];
omp[a_, b_] := Inner[CenterDot, a, b, Plus];

(* private *)


End[] (* "`Private`" *)

Begin["`Test`"]

Suite[] := Describe["OMProduct",
	Describe["Ordered matrix product",
		It["should",
			Expect[1] @ ToBe[1]
		]
	]
];

End[] (* "`Test`" *)


EndPackage[]
