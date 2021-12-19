(* ::Package:: *)

(* ::Title:: *)
(*Advent of Code: Day 3*)


(* ::Section:: *)
(*Input*)


SetDirectory@NotebookDirectory[];
Clear@input; input = Characters /@ StringSplit[#, "\n"] &@(*Import@"Day_3_input.txt"*)"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" // ToExpression;


(* ::Section:: *)
(*Code*)


Clear@binaryToDecimal
binaryToDecimal[bin : {(0|1)..}] := bin.(2^Reverse@Range[0, Length@bin - 1])
binaryToDecimal[bin : {{(0|1)..}..}] := bin.(2^Reverse@Range[0, Length@Transpose@bin - 1])


Clear@leastCommonest
leastCommonest[list_List] := MinimalBy[Tally@list, #[[2]]&]


(* ::Subsection:: *)
(*Part 1*)


(* ::Text:: *)
(*What is the power consumption of the submarine?*)


SortBy[Tally@#, Last]& /@ Transpose@input
Clear@minsAndMaxes; minsAndMaxes = AssociationThread[{"mins", "maxes"}, Transpose@%[[;;, ;;, 1]]]
Times@@binaryToDecimal@Values@minsAndMaxes


(* ::Subsection:: *)
(*Part 2*)


(* ::Subsubsection:: *)
(*Procedural Method (Doesn't work)*)


Clear@echo
echo[x_] := (Print[x]; x)


binaryToDecimal@Block[
	{n = 0},
	NestWhile[Function[binList, n++; Select[binList, #[[n]] == Commonest@binList[[;;, n]] &]], input, Length@# > 1 &, 1, Length@Transpose@input]
]


(* ::Subsubsection::Closed:: *)
(*List Method*)


minsAndMaxes["maxes"]


MemberQ[input, minsAndMaxes["maxes"]]
oxygenRating = binaryToDecimal@minsAndMaxes["maxes"]


CO2Rating = First@Block[
	{n = 2},
	While[
		FreeQ[input[[;;, ;;-n]], minsAndMaxes["mins"][[;;-n]]],
		n++
	];
	binaryToDecimal@Cases[input, Append[minsAndMaxes["mins"][[;;-n]], __]]
]


oxygenRating CO2Rating
