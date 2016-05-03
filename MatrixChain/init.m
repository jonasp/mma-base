BeginPackage["MatrixChain`", {"NCProduct`", "Grassmann`", "FermionOrder`", "MTest`"}]

toComponents::usage = "Decompose into Components";
tr::usage = "Trace of matrix chain, is not evaluated unless under toComponents";
SmallCircle::usage = "Syntactic sugar for tr[CenterDot[...]]";
registerMatrix::usage = "Register object with left, right or both (default) index";
registerIndex::usage = "Register index";
ScalarQ::usage = "Returns True if expr has no index or empty index, False otherwise";

left::usage = "Left index of matrix object";
right::usage = "Right index of matrix object";

combs::usage = "generate valid matrix chains using the provided objects";

(*CenterDot::usage = "Write ordered Matrix products and split them into components";*)

RegisterNCProduct[SmallCircle];
RegisterNCProduct[CenterDot];

Begin["`Private`"]

(* exported *)

ClearAll[tr];

ClearAll[registeredMatrix];
registeredMatrix = {};

ClearAll[registeredIndex];
registeredIndex = {};

ClearAll[registerIndex];
registerIndex[name_, range_] := AppendTo[registeredIndex, name -> range];

ClearAll[registerMatrix];
registerMatrix[o_, ind_] := AppendTo[registeredMatrix, o -> ind];

ClearAll[left, right];
left[o_] := Module[{p},
	p = If[Head[o] === Symbol,
		(o /. registeredMatrix),
		(Level[o, Infinity, Heads -> True][[1]] /. registeredMatrix)
	];
	If[Head[p] === List,
		p[[1]],
		False
	]
];
right[o_] := Module[{p},
	p = If[Head[o] === Symbol,
		(o /. registeredMatrix),
		(Level[o, Infinity, Heads -> True][[1]] /. registeredMatrix)
	];
	If[Head[p] === List,
		p[[2]],
		False
	]
];

ClearAll[toComponents];
toComponents[n_?NumberQ] := n;
toComponents[SmallCircle[a_, b__]] := toComponents[tr[CenterDot[a, b]]];
toComponents[Power[a_, b_]] := Power[toComponents[a], b];
toComponents[a_ + b__] := toComponents[a] + toComponents[Plus[b]];
toComponents[CenterDot[a_, b__]] := CenterDot[toComponents[a], toComponents[CenterDot[b]]];
toComponents[a_ b__] := toComponents[a] toComponents[Times[b]];
toComponents[tr[CenterDot[a_, b___, c_]]] /; left[a] == right[c] := Module[
	{indtype, indrange},
	indtype = left[a]; 
	indrange = (indtype /. registeredIndex);
	Sum[Chain[a, b, c][i, i], {i, indrange}]
];
toComponents[tr[a_]] /; left[a] == right[a] := Module[
	{indtype, indrange},
	indtype = left[a]; 
	indrange = (indtype /. registeredIndex);
	Sum[a[i, i], {i, indrange}]
];
toComponents[a_] /; left[a] == "" && right[a] == "" := a[];
toComponents[a_] := a;

ClearAll[Chain];
Chain[a_][i_, j_] := a[i, j];
Chain[a_, b_, c___][i_, j_] /; left[b] == right[a] := Module[
	{indtype, indrange, count},
	indtype = right[a]; 
	indrange = (indtype /. registeredIndex);
	Sum[a[i, count]\[CenterDot]Chain[b, c][count, j], {count, indrange}]
];

ClearAll[combs];
combs[ls_List, op_] := Module[{ps, t, l, r},
	l = left[op];
	r = right[op];
	ps = Permutations[ls];
	t = Fold[{#1[[1]] && (#1[[2]] == left[#2]),
			right[#2]} &, {True, r}, #] & /@ ps;
	(SmallCircle @@ #[[2]]) ∘ op & /@
		Cases[Thread[{#[[1]] == True && #[[2]] == l & /@ t, ps}], {True, _}]
];

ScalarQ[a_ b_] := ScalarQ[a] && ScalarQ[b];
ScalarQ[_SmallCircle] := False;
ScalarQ[a_] /; !FreeQ[a, SmallCircle] := False;
ScalarQ[a_] := (left[a] === "" \[Or] left[a] === False) \[And] (right[a] === "" \[Or] right[a] === False);

SmallCircle /: GrassmannOddQ[SmallCircle[a__ ∘ b___]] := GrassmannOddQ[a, b];

(* TODO: only works for Grassmann even objects *)
SmallCircle[ll___, y_?ScalarQ a_, rr___] := y SmallCircle[ll, a, rr];
(*SmallCircle[ll___, y_?ScalarQ, rr___] := y SmallCircle[ll, rr];*)

tr[a_ + b_] := tr[a] + tr[b];
tr[n_?NumberQ a_] := n tr[a];
tr[n_?NumberQ] := n;
tr[a_?ScalarQ b_] := a tr[b];
tr[a_SmallCircle] := a;

End[] (* "`Private`" *)

Begin["`Test`"]

ClearAll[Suite];
Suite[] := Block[{
	MatrixChain`Private`registeredIndex,
	MatrixChain`Private`registeredMatrix
},
MatrixChain`Private`registeredIndex = {};
MatrixChain`Private`registeredMatrix = {};

registerIndex["4", 4];
registerIndex["2", 2];
registerIndex["1", 1];

registerMatrix[a, {"4", "2"}];
registerMatrix[b, {"2", "4"}];

registerMatrix[c, {"1", "2"}];
registerMatrix[d, {"2", "1"}];

registerMatrix[y, {"", ""}];

Describe["MatrixChain",
	Describe["To Components",
		It["should expand into components",
			Expect[toComponents[SmallCircle[a,b]]] @ ToBe[
				a[1,1] · b[1,1] +
				a[1,2] · b[2,1] +
				a[2,1] · b[1,2] +
				a[2,2] · b[2,2] +
				a[3,1] · b[1,3] +
				a[3,2] · b[2,3] +
				a[4,1] · b[1,4] +
				a[4,2] · b[2,4]
			]
		],
		It["should expand functions as well",
			Expect[toComponents[c[1] ∘ d[3]]] @ ToBe[
				c[1][1,1] · d[3][1,1] +
				c[1][1,2] · d[3][2,1]
			]
		],
		It["should not act on numbers",
			Expect[toComponents[0]] @ ToBe[0],
			Expect[toComponents[1]] @ ToBe[1],
			Expect[toComponents[I]] @ ToBe[I]
		]
	],
	Describe["SmallCircle",
		It["should move out objects without indices",
			Expect[a ∘ (y b)] @ ToBe[y (a ∘ b)],
			Expect[a ∘ (y[1]/y[0] b[s][0])] @ ToBe[y[1]/y[0] (a ∘ b[s][0])]
			(* expected to fail!!! *)
			(*Expect[θ ∘ (ψ · θb)] @ ToBe[ψ · (θ ∘ θb)]*)
		]
	],
	Describe["Left/Right",
		It["should work for complicated objects",
			Expect[left[a[1]]] @ ToBe[left[a]],
			Expect[left[a[s][1]]] @ ToBe[left[a]],
			Expect[right[a[1]]] @ ToBe[right[a]],
			Expect[right[a[s][1]]] @ ToBe[right[a]]
		]
	]
]

];

End[] (* "`Test`" *)

EndPackage[]
