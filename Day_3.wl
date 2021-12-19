(* ::Package:: *)

(* ::Title:: *)
(*Advent of Code: Day 3*)


(* ::Section:: *)
(*Input*)


input = Characters /@ StringSplit[#, "\n"]&@Import@"Day_3_input.txt"


(* ::Section:: *)
(*Code*)


(* ::Subsection:: *)
(*Part 1*)


(* ::Text:: *)
(*What is the power consumption of the submarine?*)


SortBy[Tally@#, Last]& /@ Transpose@input
Transpose@%[[;;, ;;, 1]]//ToExpression
%.(2^Reverse@Range[0,Length@Transpose@input-1])//Apply@Times


(* ::Subsection:: *)
(*Part 2*)


(* ::Text:: *)
(*What do you get if you multiply your final horizontal position by your final depth?*)
