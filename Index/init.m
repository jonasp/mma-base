BeginPackage["Index`"];

Init::usage = "";
Create::usage = "";
Off[General::shdw];
Clear::usage = "";
On[General::shdw];
Prettify::usage = "";

Begin["`Private`"];

ClearAll[Categories];
Categories = {};

ClearAll[GetIndexsByName];
GetIndexsByName[_] = {};

ClearAll[Init];
Init[category_String, Indexs_List] := Module[{},
	If[FreeQ[Categories, category],
		Categories = Append[Categories, category];
	];
	GetIndexsByName[category] = Indexs;
];

ClearAll[SymbolVars];
SymbolVars[_] := {};
ClearAll[Create];
Create[category_String, symbol_] := Module[{},
	SymbolVars[category] = Append[SymbolVars[category], symbol];
	symbol
];
Create[category_String] := Create[category, Unique[category]];

ClearAll[SelectIndices];
SelectIndices[category_String, expr_] /; MemberQ[SymbolVars[category], expr] := expr;
SelectIndices[category_String, expr_] /; Length[expr] > 0 := Flatten[SelectIndices[category, #]& /@ (List @@ expr), 1];
SelectIndices[category_String, expr_] = {};

ClearAll[GenerateRules];
GenerateRules[category_String, expr_] := Module[{
		(* only replace symbols which appear in expression *)
		syms = DeleteDuplicates[SelectIndices[category, expr]],
		(* take only the pretty symbols which don't show up in the expression provided *)
		vars = Select[GetIndexsByName[category], FreeQ[expr, #]&],
		th
	},
	If[Length[syms] > Length[vars],
		Throw["Number of Categories exceeds number of possible pretty symbols"]
	];
	Thread[syms -> (vars[[1;;Length[syms]]])]
];

ClearAll[PrettifyRules];
PrettifyRules = {};

ClearAll[Prettify];
Prettify[Plus[a_,b__]] := Prettify[a] + Prettify[Plus[b]];
Prettify[exp_] := exp /. ((GenerateRules[#,exp]&/@Categories)//Flatten);


(* HERE BE DRAGONS *)

Off[General::shdw];
ClearAll[Index`Clear];
Index`Clear[category_String] := Module[{},
	SymbolVars[category] = {};
];
Index`Clear[] := Module[{},(Clear/@Categories);];
On[General::shdw];

End[]; (* `Private`" *)

(* don't pollute global namespace with helper package *)
Block[{$ContextPath}, EndPackage[]];
