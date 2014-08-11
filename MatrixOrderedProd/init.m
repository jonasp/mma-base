BeginPackage["MatrixOrderedProd`"]

MOP::usage = "asdf";

Begin["`Private`"]

ClearAll[MOP];

MOP[a_, b_] := Module[{leftRule, rightRule, left, right},
	leftRule = (# -> left[#]) & /@ Level[a, {2}];
	rightRule = (# -> right[#]) & /@ Level[b, {2}];
	(a /. leftRule).(b /. rightRule) //. (left[x_] right[y_] :> x.y)
];

MOP[a_,b_,c__] := MOP[MOP[a,b],c];

End[] (* "`private`" *)

Begin["`Test`"]

ClearAll[Suite];
Suite[] := Describe["MatrixOrderedProd",
	Describe["asdf",
		It["should asdf", 
			Expect[MOP[{{a, b}, {c, d}}, {{e, f}, {g, h}}]] @ ToBe[{{a.e + b.g, a.f + b.h}, {c.e + d.g, c.f + d.h}}]
		]
	];
];

End[] (* "`Test`" *)

EndPackage[]
