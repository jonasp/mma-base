Block[{Notation`AutoLoadNotationPalette = False},
	BeginPackage["xyttb`", {"NCProduct`", "MTest`", "Notation`"}]
];

(*
 * This package defines the bosonic variables x and y as well as the
 * fermionic variables \[Theta] and \[Theta]b. Due to their ordered
 * nature an ordered product given by CenterDot[...] is used.
 * It also defines the derivative w.r.t. these variables which
 * obeys the fermionic Leibniz rule.
 *)

DOp::usage = "DOp[x][i1,i2]: A differntial operator w.r.t. first argument carrying indices";
\[Delta]::usage = "A symbol for the Kronecker delta";
x::usage = "TODO";
\[Theta]::usage = "TODO";
\[Theta]b::usage = "TODO";
y::usage = "TODO";
PrettyForm::usage = "";

Begin["`Private`"]

(* fermionic variables and anti commutation factor *)
ClearAll[Nf];
Nf[a_,b_] := If[FermionicQ[a] && FermionicQ[b], -1, 1];

ClearAll[FermionicQ];
FermionicQ[x_[__]] := FermionicQ[x];
FermionicQ[\[Theta]] = True;
FermionicQ[\[Theta]b] = True;
FermionicQ[_] := False;

(* canonical order in x,y,t,tb *)
ClearAll[variables];
variables = {x, \[Theta], \[Theta]b, y};

ClearAll[VariableQ];
VariableQ[x_] := MemberQ[variables, x];

CenterDot[e___,x_[i__],y_[j__],f___] /; (
		!OrderedQ[{x,y}] && VariableQ[x] && VariableQ[y]
	) := Nf[x,y] CenterDot[e,y[j],x[i],f];
CenterDot[e___,x_[i__],x_[j__],f___] /; (
		!OrderedQ[{{i},{j}}] && VariableQ[x]
	) := Nf[x,x] CenterDot[e,x[j],x[i],f];

CenterDot[a__, \[Delta][i__], b___] /; Head[Last[List[a]]] =!= \[Delta] :=
	CenterDot[\[Delta][i], a, b]
CenterDot[a___, \[Delta][i__], \[Delta][j__], b__] /; !OrderedQ[{\[Delta][i], \[Delta][j]}] := 
	CenterDot[a, \[Delta][j], \[Delta][i], b]
CenterDot[e___, \[Delta][a_,b_], f___] /;
	!FreeQ[CenterDot[e,f],b] := (CenterDot[e,f] /. b -> a);
CenterDot[e___, \[Delta][a_,b_], f___] /;
	!FreeQ[CenterDot[e,f],a] := (CenterDot[e,f] /. a -> b);
CenterDot[e___,\[Delta][i__],\[Delta][j__],f___] /; (
		!OrderedQ[{{i},{j}}]
	) := CenterDot[e,\[Delta][j],\[Delta][i],f];

ClearAll[DOp];
(* action of DOp yields deltas *)
DOp[x][a_,b_][x[c_,d_]] := \[Delta][d,a] \[CenterDot] \[Delta][c,b];
DOp[y][a_,b_][y[c_,d_]] := \[Delta][a,d] \[CenterDot] \[Delta][c,b];
DOp[\[Theta]][a_,b_][\[Theta][c_,d_]] := \[Delta][a,d] \[CenterDot] \[Delta][c,b];
DOp[\[Theta]b][a_,b_][\[Theta]b[c_,d_]] := \[Delta][d,a] \[CenterDot] \[Delta][c,b];
(* fermionic Leibniz *)
DOp[x_][s__][CenterDot[e_,f__]] :=
	CenterDot[DOp[x][s][e],f]+ Nf[x,e] CenterDot[e,DOp[x][s][CenterDot[f]]];
(* linear *)
DOp[x_][s__][e_ + f_] := DOp[x][s][e] + DOp[x][s][f];
(* don't act on other variables *)
DOp[x_][__][y_[__]] /; (VariableQ[y] && x =!= y) := 0;
(* DOp doesn't act on itself *)
DOp[_][__][DOp[_][__][_]] := 0;
(* ignore mathematicas Function[] *)
DOp[x_][i__][fn_[y_, exp_]] /; fn == Function := Function[y, DOp[x][i][exp]];
DOp[x_][i__][Function[exp_]] := Function[DOp[x][i][exp]];
(* ignore numbers in times *)
DOp[x_][i__][Times[n_, a__]] /; NumberQ[n] := n DOp[x][i][Times[a]];

(*ReduceDelta[e___ \[CenterDot] \[Delta][a_,b_] \[CenterDot] f___] /;*)
	(*!FreeQ[CenterDot[e,f],b] := (CenterDot[e,f] /. b -> a);*)
(*ReduceDelta[a_] := a;*)
(*ReduceDelta[expr_] := expr //. CenterDot[e___, \[Delta][a_, b_], f___] /; !FreeQ[CenterDot[e,f],b] :> CenterDot[e,f] //. b -> a;*)

(* private *)

ClearAll[PrettyForm];
PrettyForm[exp_] := exp	/. {
	x[a_, b_] :> Subscript["x", Row[{a, "\[InvisibleComma]", b}]],
	y[a_, b_] :> Superscript[Subscript["y", a], b],
	\[Theta][a_, b_] :> Superscript[Subscript["\[Theta]", a], b],
	\[Theta]b[a_, b_] :>  Subscript[OverBar[\[Theta]], Row[{a, b}]],
	DOp[x][a_, b_] :> Superscript["\[PartialD]", Row[{a, "\[InvisibleComma]", b} ]],
	DOp[y][a_, b_] :> Superscript[Subscript["\[PartialD]", a], b ],
	DOp[\[Theta]][a_, b_] :> Superscript[Subscript["\[PartialD]", a], b ],
	DOp[\[Theta]b][a_, b_] :> Superscript["\[PartialD]", Row[{a, "\[InvisibleComma]", b} ]],
	\[Delta][a_,b_] :> Subsuperscript["\[Delta]",a,b]
};

End[] (* "`private`" *)

Begin["`Test`"]

With[{NonCommutativeMultiply = CenterDot, t = \[Theta], tb = \[Theta]b},
ClearAll[Suite];
Suite[] := Describe["xyttb",
	Describe["partial derivative",
		It["should derive w.r.t. x, \[Theta], \[Theta]b and y" ,
			Expect[DOp[x][ad,a][x[b,bd]]] @ ToBe[\[Delta][bd,ad]**\[Delta][b,a]],
			Expect[DOp[y][ac,bc][y[cc,dc]]] @ ToBe[\[Delta][ac,dc]**\[Delta][cc,bc]],
			Expect[DOp[t][ac,a][t[b,bc]]] @ ToBe[\[Delta][ac,bc]**\[Delta][b,a]],
			Expect[DOp[tb][ad,ac][tb[bc,bd]]] @ ToBe[\[Delta][bc,ac]**\[Delta][bd,ad]]
		],
		It["should be linear",
			Expect[DOp[x][a, b][r + s]] @ ToBe[DOp[x][a, b][r] + DOp[x][a, b][s]]
		],
		It["should repsect the fermionic Leibniz rule",
			Expect[DOp[t][a, b][tb[c, d]**t[e, f]]] @ ToBe[
				DOp[t][a, b][tb[c, d]]**t[e, f] -
					tb[c, d]**DOp[t][a, b][t[e, f]]]
		],
		It["should ignore other variables",
			Expect[DOp[x][a, b][tb[g, h]**x[c, d]**t[e, f]]] 
				@ ToBe[tb[g, h]**DOp[x][a, b][ x[c, d]]**t[e, f]],
			Expect[DOp[x][a, b][t[c,d]]] @ ToBe[0],
			Expect[ToString[DOp[x][a, b][fn[c]]]]
				@ ToBe["DOp[x][xyttb`Test`a, xyttb`Test`b][xyttb`Test`fn[xyttb`Test`c]]"]
		],
		It["should not act on itself",
			Expect[DOp[x][a,b][DOp[x][c,d][f]]] @ ToBe[0]
		],
		It["should ignore mathematica Function[]",
			Expect[DOp[x][a,b][Function[c]]] @ ToBe[Function[DOp[x][a,b][c]]],
			Expect[DOp[x][a,b][Function[c,d]]] @ ToBe[Function[c,DOp[x][a,b][d]]],
			Expect[DOp[x][a,b][Function[{c,d},e]]] @ ToBe[Function[{c,d},DOp[x][a,b][e]]]
		],
		It["should not act on numbers",
			Expect[DOp[x][a,b][-I x[c,d]]] @ ToBe[-I \[Delta][d,a] ** \[Delta][c,b]]
		]
	],
	Describe["Kronecker \[Delta]",
		It["should be reducible",
			Expect[Simplify[\[Delta][a, b] ** x[b, bd]]]
				@ ToBe[x[a,bd]],
			Expect[\[Delta][gd,bd]**\[Delta][a,b]
			       **x[g,ad]**DOp[x][gd,g][f]]
				@ ToBe[\[Delta][a,b]**x[g,ad]**DOp[x][bd, g][f]]
		],
		It["should commute to the front",
			Expect[x[a,b] ** \[Delta][c,d] ** t[e,f]]
				@ ToBe[\[Delta][c,d] ** x[a,b] ** t[e,f]]
		],
		It["should not be orderless",
			Expect[\[Delta][a,b]] @ NotToBe[\[Delta][b,a]]
		],
		It["should have a canonical order",
			Expect[\[Delta][ad, bd]\[CenterDot]\[Delta][a, b]]
				@ ToBe[\[Delta][a, b]\[CenterDot]\[Delta][ad, bd]]
		]
	],
	Describe["variables",
		It["should be OneIdentities of CenterDot",
			Expect[CenterDot[x[a,b]]] @ ToBe[x[a,b]]
		],
		It["should have a canonical order", 
			Expect[x[a,b] ** tb[g,h] ** t[e,f] ** y[c,d]]
				@ ToBe[- x[a,b] ** y[c,d] ** t[e,f] ** tb[g,h]],
			Expect[x[c, d] ** x[a, b]] @ ToBe[x[a, b] ** x[c, d]],
			Expect[t[c, d] ** t[a, b]] @ ToBe[- t[a, b] ** t[c, d]]
		]
	]
];
];

End[] (* "`Test`" *)

EndPackage[]
