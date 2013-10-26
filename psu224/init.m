BeginPackage["psu224`", {"xyttb`", "NCProduct`", "MTest`", "SU2ind`"}]
Needs["SU2ind`"];

p::usage = "";
m::usage = "";
mb::usage = "";
k::usage = "";
q::usage = "";
qb::usage = "";
s::usage = "";
sb::usage = "";

PrettySymbol`Init["undotted", {\[Alpha], \[Beta], \[Gamma], \[Rho], \[Sigma], \[Tau], \[Kappa], \[Xi]}];
PrettySymbol`Init["dotted", {
	\! \(\*OverscriptBox[\(\[Alpha]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Beta]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Gamma]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Rho]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Sigma]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Tau]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Kappa]\),\(.\)]\),
	\! \(\*OverscriptBox[\(\[Xi]\),\(.\)]\)
}];
PrettySymbol`Init["capital", {A, B, C, D, F, G, H, K, L}];

Commutator::usage = "";
AntiCommutator::usage = "";

Begin["`Private`"]
ClearAll[p, m, mb, k, q, qb];
p[ad_,a_] := DOp[x][a,ad];
m[a_,b_] := With[{
		NonCommutativeMultiply = CenterDot,
		ad = PrettySymbol`Create["dotted"],
		ac = PrettySymbol`Create["capital"]},
	- x[a,ad] ** DOp[x][ad, b][#]
	- \[Theta][a,ac] ** DOp[\[Theta]][ac, b][#]&
];
mb[ad_, bd_] := With[{
		NonCommutativeMultiply = CenterDot,
		a = PrettySymbol`Create["undotted"],
		ac = PrettySymbol`Create["capital"]
	},
	- x[a,ad] ** DOp[x][bd, a][#]
	- \[Theta]b[ac,ad] ** DOp[\[Theta]b][bd,ac][#]&
];
k[a_,ad_] := With[{
		NonCommutativeMultiply = CenterDot,
		b = PrettySymbol`Create["undotted"],
		bd = PrettySymbol`Create["dotted"],
		cc = PrettySymbol`Create["capital"],
		dc = PrettySymbol`Create["capital"]
	},
	x[a,bd] ** x[b,ad] ** DOp[x][bd, b][#]
	+ x[b,ad] ** \[Theta][a,cc] ** DOp[\[Theta]][cc, b][#]
	+ x[a,bd] ** \[Theta]b[cc,ad] ** DOp[\[Theta]b][bd,cc][#]
	- I (\[Theta][a,cc] ** \[Theta]b[dc,ad] ** DOp[y][cc,dc][#])&
];
q[a_,ac_] := With[{
		NonCommutativeMultiply = CenterDot,
		bc = PrettySymbol`Create["capital"],
		ad = PrettySymbol`Create["dotted"]
	},
	(- \[Delta][ac,bc] + y[ac,bc]) ** DOp[\[Theta]][bc,a][#]
	+ I \[Theta]b[ac,ad] ** DOp[x][ad,a][#]&
];
qb[ac_,ad_] := With[{
		NonCommutativeMultiply = CenterDot,
		bc = PrettySymbol`Create["capital"],
		a = PrettySymbol`Create["undotted"]
	},
	(\[Delta][bc,ac] + y[bc,ac]) ** DOp[\[Theta]b][ad,bc][#]
	- I \[Theta][a,ac] ** DOp[x][ad,a][#]&
];
s[ac_,a_] := With[{
		NonCommutativeMultiply = CenterDot,
		t = \[Theta],
		tb = \[Theta]b,
		b = PrettySymbol`Create["undotted"],
		bd = PrettySymbol`Create["dotted"],
		bc = PrettySymbol`Create["capital"],
		cc = PrettySymbol`Create["capital"]
	},
	- x[a,bd] ** (\[Delta][bc,ac] + y[bc,ac]) ** DOp[tb][bd,bc][#]
	+ I x[a,bd] ** t[b,ac] ** DOp[x][bd,b][#]
	+ I t[b,ac] ** t[a,bc] ** DOp[t][bc,b][#]
	- I t[a,bc] ** DOp[y][bc,ac][#]
	- I y[bc,ac] ** t[a,cc] ** DOp[y][cc,bc][#]&
];
sb[ad_,ac_] := With[{
		NonCommutativeMultiply = CenterDot,
		t = \[Theta],
		tb = \[Theta]b,
		b = PrettySymbol`Create["undotted"],
		bd = PrettySymbol`Create["dotted"],
		bc = PrettySymbol`Create["capital"],
		cc = PrettySymbol`Create["capital"]
	},
	x[b,ad] ** (\[Delta][ac,bc] - y[ac,bc]) ** DOp[t][bc,b][#]
	- I x[b,ad] ** tb[ac,bd] ** DOp[x][bd,b][#]
	- I tb[ac,bd] ** tb[bc,ad] ** DOp[tb][bd,bc][#]
	- I tb[bc,ad] ** DOp[y][ac,bc][#]
	+ I y[ac,bc] ** tb[cc,ad] ** DOp[y][bc,cc][#]&
];

ClearAll[Commutator];
Commutator[O1_, O2_] := O1[O2[#]] - O2[O1[#]]&;
ClearAll[AntiCommutator];
AntiCommutator[O1_, O2_] := O1[O2[#]] + O2[O1[#]]&;

End[]

Begin["`Test`"]

Clearall[Suite];
Suite[] := Describe["psu224",
	Describe["commutator",
		It["should hold [A,B] = A[B] - B[A]",
			Expect[Commutator[A,B][f]] @ ToBe[A[B[f]] - B[A[f]]]
		]
	](*,
	Describe["algebra",
		It["[p,m]=p",
			Expect[Commutator[p[g,gd],m[a,b]][f]] @ ToBe[\[Delta][a,g] p[b,gd][f]]
		]
	]*)
];

End[]

EndPackage[]
