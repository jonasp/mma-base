BeginPackage["NCProduct`", {"MTest`"}]

RegisterNCProduct::usage = "Register an ordered product which autoexpands";
FlattenNCProduct::usage = "The ordered product is flat - default: True";

Begin["`Private`"]

RegisteredFunctions = {};

(* exported *)
RegisterNCProduct[fn_] := Module[{},
  If[!MemberQ[RegisteredFunctions, fn],

  AppendTo[RegisteredFunctions, fn];

  (* flat - Attribute Flat changes Pattern matching in a weird way *)
  fn[a___, fn[b__], c___] := fn[a,b,c];

  (* simplify single argument *)
  fn[a_] := a;

  fn[a___, n_ b_, c___] /; NumericQ[n] := n fn[a, b, c];
  fn[a___, n_ , b___] /; NumericQ[n] := n fn[a, b];

  fn[a___, n_ b_, c___] /; NumberQ[n] := n fn[a, b, c];
  fn[a___, n_ , b___] /; NumberQ[n] := n fn[a, b];

  fn[a___, b_Plus, c___] := fn[a, #, c]& /@ b;

  ];
];

End[] (* "`Private`" *)

Begin["`Test`"]

RegisterNCProduct[NCProd];

With[{NonCommutativeMultiply = NCProd},
ClearAll[Suite];
Suite[] := Describe["NCProduct",
  Describe["Ordered product",
    It["should auto-expand",
      Expect[(a + b) ** c] @ ToBe[a ** c + b ** c],
			Expect[a ** (b + c) ** d]
				@ ToBe[a ** b ** d + a ** c ** d],
			Expect[a ** (b + c + d)] @ ToBe[a ** b + a ** c + a ** d]
		],
		It["should be flat",
			Expect[(a**b)**c] @ ToBe[a**b**c],
			Expect[a**((b**c)**d)] @ ToBe[NCProd[a, b, c, d]],
			Expect[a**((b**c)**d**e)]
				@ ToBe[NCProd[a, b, c, d, e]]
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
			Expect[Simplify[NCProd[fn[b]]]] @ ToBe[fn[b]]
		],
		It["should reduce automatically for one argument",
			Expect[NCProd[fn[b]]] @ ToBe[fn[b]]
		]
	]
];
];

End[] (* "`Test`" *)

EndPackage[]
