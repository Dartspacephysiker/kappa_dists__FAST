(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["LKMwellFunctions`"]
(* Exported symbols added here with SymbolName::usage *)  

LKMwellDensFac::usage="
LKDensFac[phiBar_,RB_]
Gives the factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

LKMwellDensFacRBInf::usage="
LKMwellDensFacRBInf[phiBar_]
Factor by which density changes for RB->infinity and phiBar=deltaPhi/temperature.
";

Begin["`Private`"] (* Begin Private Context *) 
LKMwellDensFac[phiBar_,RB_]:=Module[{factor},
factor=1/2 (Exp[phiBar] Erfc[Sqrt[phiBar]]+2Sqrt[(RB-1)/\[Pi]]DawsonF[Sqrt[phiBar/(RB-1)]])
];

LKMwellDensFacRBInf[phiBar_]:=Module[{factor},
factor=1/2 (Exp[phiBar] Erfc[Sqrt[phiBar]])+Sqrt[phiBar/\[Pi]]
];

End[] (* End Private Context *)

EndPackage[]



