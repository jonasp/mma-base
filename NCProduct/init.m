BeginPackage["NCProduct`", {"MTest`"}]

CenterDot::usage = "A flat ordered product which can be expanded and simplified";

Begin["`Private`"]

(*ClearAll[Simplifications];*)
(*Simplifications = {};*)

(* exported *)

ClearAll[CenterDot];
(*SetAttributes[CenterDot,{Flat}];*)

(* flat - Attribute Flat changes Pattern matching in a weird way *)
CenterDot[a___, CenterDot[b__], c___] := CenterDot[a,b,c];

(* simplify single argument CenterDot *)
CenterDot[a_] := a;

(*CenterDot/:Simplify[CenterDot[s__]] := Module[{chain},*)
	(*chain[{}, arg_] := arg;*)
	(*chain[l_List, arg_] := chain[Rest[l], First[l][arg]];*)
	(*chain[Simplifications, CenterDot[s]]*)
(*];*)

CenterDot[a___, n_ b_, c___] /; NumberQ[n] := n CenterDot[a, b, c];
CenterDot[a___, n_ , b___] /; NumberQ[n] := n CenterDot[a, b];

CenterDot[a___, Plus[b_, c__], d___] := CenterDot[a,b,d] + CenterDot[a,Plus[c],d];

(*CenterDot/:Expand[CenterDot[a_,b_+c_]] :=*)
	(*Expand[CenterDot[a,b]]+Expand[CenterDot[a,c]];*)
(*CenterDot/:Expand[CenterDot[a_+b_,c_]] :=*)
	(*Expand[CenterDot[a,c]]+Expand[CenterDot[b,c]];*)

(*Unprotect[Plus];*)
(*Plus/:Simplify[CenterDot[a_, b_] + CenterDot[a_, c_]] :=*)
	(*Simplify[CenterDot[a,(b + c)]];*)
(*Plus/:Simplify[s__ + t__] :=*)
	(*Simplify[Plus[s]] + Simplify[Plus[t]];*)
(*Protect[Plus];*)

(*Unprotect[Times];*)
(*Times/:Simplify[Times[a__, CenterDot[b__], c___]] := Times[a, Simplify[CenterDot[b]], c];*)
(*Times/:Simplify[Times[a___, CenterDot[b__], c__]] := Times[a, Simplify[CenterDot[b]], c];*)
(*Protect[Times];*)

End[] (* "`Private`" *)

Begin["`Test`"]

With[{NonCommutativeMultiply = CenterDot},
ClearAll[Suite];
Suite[] := Describe["NCProduct",
	Describe["Ordered product",
		Ignore @ It["should simplify",
			Expect[Simplify[CenterDot[a,b] + CenterDot[a,c]]] @ ToBe[a**(b + c)]
		],
		Ignore @ It["should be expandable",
			Expect[Expand[(a + b) ** (c + d)]] @ ToBe[
				CenterDot[a,c] + CenterDot[b,c] + CenterDot[a,d] + CenterDot[b,d]
			]
		],
		It["should auto-expand",
			Expect[(a + b) ** c] @ ToBe[a ** c + b ** c],
			Expect[a ** (b + c) ** d]
				@ ToBe[a ** b ** d + a ** c ** d],
			Expect[a ** (b + c + d)] @ ToBe[a ** b + a ** c + a ** d]
		],
		It["should be flat",
			Expect[(a**b)**c] @ ToBe[a**b**c],
			Expect[a**((b**c)**d)] @ ToBe[CenterDot[a, b, c, d]],
			Expect[a**((b**c)**d**e)]
				@ ToBe[CenterDot[a, b, c, d, e]]
		],
		It["should factor out numbers",
			Expect[(-a)**b] @ ToBe[-(a**b)],
			Expect[a**(-b)] @ ToBe[-(a**b)],
			Expect[(2 a)**b] @ ToBe[2 (a**b)],
			Expect[a**(2 b)**(4 c)] @ ToBe[8 (a**b**c)]
		],
		It["should pull numbers to the front",
			Expect[a ** 2 ** b] @ ToBe[2 (a ** b)],
			Expect[a ** 0 ** b] @ ToBe[0]
		],
		It["should simplify for one argument",
			Expect[Simplify[CenterDot[fn[b]]]] @ ToBe[fn[b]]
		],
		It["should reduce automatically for one argument",
			Expect[CenterDot[fn[b]]] @ ToBe[fn[b]]
		]
	]
];
];

End[] (* "`Test`" *)

EndPackage[]
