BeginPackage["MTest`"]

Expect::usage = "";
It::usage = "";
Describe::usage = "";
TestPrint::usage = "";
ToBe::usage = "";
NotToBe::usage = "";
Ignore::usage ="";

Begin["`Private`"]

(* exported *)

ClearAll[Expect];
Expect[expr_][ToBe[expected_]] := Module[{errorStr},
	If[expr === expected,
		{{"status", "Passed"}},
		errorStr = "- expected (" <> ToString[expr] <>
			") to be (" <> ToString[expected] <> ")";
		{{"status", "Failed"}, errorStr}
	]
];
Expect[expr_][NotToBe[expected_]] := Module[{errorStr},
	If[expr =!= expected,
		{{"status", "Passed"}},
		errorStr = "- expected (" <> ToString[expr] <>
			") not to be (" <> ToString[expected] <> ")";
		{{"status", "Failed"}, errorStr}
	]
];

ClearAll[Ignore];
Ignore[{{"status", _}, a_, ___}] := {{"status", "Ignore"}, a};

ClearAll[It];
It[text_String, tests__] := Module[{
		errors = Cases[List[tests], {{"status", "Failed"}, ___}]
		(* TODO: count non errors and compare *)
	},
	If[Length[errors] > 0,
		{{"status", "Failed"}, text, errors[[1]][[2]]},
		{{"status", "Passed"}, text}
	]
];

ClearAll[Describe];
Describe[text_String, tests__] := Module[{
		errors = Cases[List[tests], {{"status", "Failed"}, ___}]
		(* TODO: count non errors and compare *)
	},
	{
		If[Length[errors] == 0, {"status", "Passed"}, {"status", "Failed"}],
		text, tests
	}
];

ClearAll[StringStyle];
StringStyle[arg__] := ToString[Style[arg], StandardForm];

ClearAll[TestPrint];
TestPrint[s_String, _String, _Integer] := s;
TestPrint[{"status", "Passed"}, _String, _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Passed", Darker[Green], Bold],
	StringStyle["]", Bold]
];
TestPrint[{"status", "Failed"}, _String, _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Failed", Darker[Red], Bold],
	StringStyle["]", Bold]
];
TestPrint[{"status", "Ignore"}, _String, _Integer] := StringJoin[
	StringStyle["[", Bold],
	StringStyle["Ignore", Darker[Yellow], Bold],
	StringStyle["]", Bold]
];
TestPrint[l_List, "all", indent_Integer] := Module[
	{
		isIndented = (indent > 0),
		failed = Cases[l, {{"status", "Failed"}, ___}],
		print = TestPrint
	},
	If[isIndented,
		"\n" <> StringJoin @@ ConstantArray["\t", indent],
		""
	] <>StringJoin @@ Most[Flatten[Map[
		{print[#, "all", indent + 1], " "} &, l], 1]]
];
TestPrint[l_List, "onFail", indent_Integer] := Module[
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
		{print[#, "onFail", indent + 1], " "} &, l], 1]]
];
TestPrint[x_] := TestPrint[x, "onFail", 0];
TestPrint[x_, s_String] := TestPrint[x, s, 0];

End[]

EndPackage[]
