BeginPackage["MatrixChain`", {"NCProduct`", "MTest`"}]

toComponents::usage = "Decompose into Components";
tr::usage = "Trace of matrix chain, is not evaluated unless under toComponents";
SmallCircle::usage = "Syntactic sugar for tr[CenterDot[...]]";
registerMatrix::usage = "Register object with left, right or both (default) index";
registerIndex::usage = "Register index";

left::usage = "Left index of matrix object";
right::usage = "Right index of matrix object";

(*CenterDot::usage = "Write ordered Matrix products and split them into components";*)

Begin["`Private`"]

(* exported *)

ClearAll[tr];
ClearAll[SmallCircle];
(*SmallCircle[a___] := tr[CenterDot[a]];*)


ClearAll[registeredMatrix];
registeredMatrix = <||>;

ClearAll[registeredIndex];
registeredIndex = <||>;

ClearAll[registerIndex];
registerIndex[name_, range_] := AssociateTo[registeredIndex, name -> range];

ClearAll[registerMatrix];
registerMatrix[o_, ind_] := AssociateTo[registeredMatrix, o -> ind];

ClearAll[left,right];
left[o_] := registeredMatrix[o][[1]];
right[o_] := registeredMatrix[o][[2]];

ClearAll[toComponents];
toComponents[SmallCircle[a___]] := toComponents[tr[CenterDot[a]]];
toComponents[a_ + b__] := toComponents[a] + toComponents[Plus[b]];
toComponents[a_ b__] := toComponents[a] toComponents[Times[b]];
toComponents[CenterDot[a_, b__]] :=
	CenterDot[toComponents[a], toComponents[CenterDot[b]]];
toComponents[tr[CenterDot[a_, b___, c_]]] /; left[a] == right[c] := Module[
	{indtype, indrange},
	indtype = left[a]; indrange = registeredIndex[indtype];
	Sum[Chain[a, b, c][i, i], {i, indrange}]
];
toComponents[tr[a_]] /; left[a] == right[a] := Module[
	{indtype, indrange},
	indtype = left[a]; indrange = registeredIndex[indtype];
	Sum[a[i, i], {i, indrange}]
];
toComponents[a_] /; left[a] == "" && right[a] == "" := a[];
toComponents[a_] := a;

ClearAll[Chain];
Chain[a_][i_, j_] := a[i, j];
Chain[a_, b_, c___][i_, j_] /; left[b] == right[a] := Module[
	{indtype, indrange, count},
   indtype = right[a]; indrange = registeredIndex[indtype];
   Sum[a[i, count]\[CenterDot]Chain[b, c][count, j], {count, indrange}]
   ];

End[] (* "`Private`" *)

Begin["`Test`"]

ClearAll[Suite];
Suite[] := Describe["MatrixChain",
	Describe["something",
		Ignore @ It["ignored",
			Expect[1] @ ToBe[2]
		],
		It["should tbd",
			Expect[1] @ ToBe[1]
		]
	]
];

End[] (* "`Test`" *)

EndPackage[]
