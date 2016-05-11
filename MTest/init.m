BeginPackage["MTest`"]

Expect::usage = "";
It::usage = "";
Describe::usage = "";
TestPrint::usage = "";
ToBe::usage = "";
NotToBe::usage = "";
Ignore::usage ="";
V::usage = "";

Begin["`Private`"]

(* exported *)

ClearAll[Expect];
Expect[expr_] := Module[{result = #[expr], errorStr},
	If[result[[2]],
		{{"status", "Passed"}},
		errorStr = "- expected (" <> ToString[expr] <>
			") to be (" <> ToString[result[[1]]] <> ")";
		{{"status", "Failed"}, errorStr}
	]
]&;
ClearAll[ToBe];
ToBe[expected_] := {expected, SameQ[expected,#]}&;
ClearAll[NotToBe];
NotToBe[expected_] := {expected, UnsameQ[expected,#]}&;


(* ForAll *)
ClearAll[V];
SetAttributes[V, HoldAll];
V[s__] := Function[e,
  Sequence @@ Flatten[Table[e, s], Length[{s}] - 1],
  HoldAll];


ClearAll[Ignore];
Ignore[{{"status", _}, a_, ___}] := {{"status", "Ignore"}, a};

ClearAll[It];
It::invalidtest = "Invalid test was provided in It[\"`1`\",\[Ellipsis]] at position(s): `2`";
It[text_String, tests__] := Module[{
		failed = Cases[List[tests], {{"status", "Failed"}, ___}],
		(* get index of elements for which first entry is not of form {"status", _} *)
		errInd= Flatten[Position[List[tests], Except[{{"status", _},___}], 1, Heads -> False]]
	},
	If[Length[errInd] > 0,
		Message[It::invalidtest, text, errInd];
	];
	If[Length[failed] > 0,
		{{"status", "Failed"}, text, failed[[1]][[2]]},
		{{"status", "Passed"}, text}
	]
];

ClearAll[Describe];
Describe::invalidtest = "Invalid test was provided in Describe[\"`1`\",\[Ellipsis]]"<>
	" at position(s): `2`";
Describe[text_String, tests__] := Module[{
		failed = Cases[List[tests], {{"status", "Failed"}, ___}],
		(* TODO: DRY error handling *)
		(* get index of elements for which first entry is not of form {"status", _} *)
		errInd = Flatten[Position[List[tests], Except[{{"status", _},___}], 1, Heads -> False]]
	},
	If[Length[errInd] > 0,
		Message[Describe::invalidtest, text, errInd];
	];
	{
		If[Length[failed] == 0, {"status", "Passed"}, {"status", "Failed"}],
		text, tests
	}
];

ClearAll[StringStyle];
StringStyle[arg__] := ToString[Style[arg], StandardForm];

ClearAll[TestPrint];
TestPrint[s_String, _String, _String, _Integer] := s;
TestPrint[{"status", "Passed"}, _String, "notebook", _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Passed", Darker[Green], Bold],
	StringStyle["]", Bold]
];
TestPrint[{"status", "Failed"}, _String, "notebook", _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Failed", Darker[Red], Bold],
	StringStyle["]", Bold]
];
TestPrint[{"status", "Ignore"}, _String, "notebook", _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Ignore", Darker[Yellow], Bold],
	StringStyle["]", Bold]
];
TestPrint[{"status", "Passed"}, _String, "terminal", _Integer] := StringJoin[
	"[",
	"\033[;32mPassed\033[0m",
	"]"
];
TestPrint[{"status", "Failed"}, _String, "terminal", _Integer] := StringJoin[
	"[",
	"\033[;31mFailed\033[0m",
	"]"
];
TestPrint[{"status", "Ignore"}, _String, "terminal", _Integer] := StringJoin[
	"[",
	"\033[;33mIgnore\033[0m",
	"]"
];
TestPrint[l_List, "all", output_String, indent_Integer] := Module[
	{
		isIndented = (indent > 0),
		failed = Cases[l, {{"status", "Failed"}, ___}],
		print = TestPrint
	},
	If[isIndented,
		"\n" <> StringJoin @@ ConstantArray["\t", indent],
		""
	] <>StringJoin @@ Most[Flatten[Map[
		{print[#, "all", output, indent + 1], " "} &, l], 1]]
];
TestPrint[l_List, "onFail", output_String, indent_Integer] := Module[
	{
		isIndented = (indent > 0),
		failed = Cases[l, {{"status", "Failed"}, ___}],
		print
	},
	If[(Length[failed] === 0),
		print[x_List, ___] /; !MatchQ[x, {"status", _}] := "";
		print[s___] := TestPrint[s],
		print[s___] := TestPrint[s];
	];
	If[isIndented,
		"\n" <> StringJoin @@ ConstantArray["\t", indent],
		""
	] <>StringJoin @@ Most[Flatten[Map[
		{print[#, "onFail", output, indent + 1], " "} &, l], 1]]
];
TestPrint[x_] := TestPrint[x, "onFail", "notebook", 0];
TestPrint[x_, s_String] := TestPrint[x, s, "notebook", 0];
TestPrint[x_, s_String, o_String] := TestPrint[x, s, o, 0];

End[]

EndPackage[]
