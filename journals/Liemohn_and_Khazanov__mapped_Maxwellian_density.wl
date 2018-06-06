(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["LKMwellFunctions`"]
(* Exported symbols added here with SymbolName::usage *)  

LKMwellDensFac::usage="
LKMwellDensFac[phiBar_,RB_]
Gives the factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

LKMwellDensFacRBInf::usage="
LKMwellDensFacRBInf[phiBar_]
Factor by which density changes for RB->infinity and phiBar=deltaPhi/temperature.
";

LKKappaDensFac::usage="
LKKappaDensFac[phiBar_,RB_,\[Kappa]_]
Gives the factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

LKKappa2p5DensFac::usage="
LKKappa2p5DensFac[phiBar_,RB_]
Gives the factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

Begin["`Private`"] (* Begin Private Context *) 
LKMwellDensFac[phiBar_,RB_]:=Module[{factor},
factor=1/2 (Exp[phiBar] Erfc[Sqrt[phiBar]]+2Sqrt[(RB-1)/\[Pi]]DawsonF[Sqrt[phiBar/(RB-1)]])
];

LKMwellDensFacRBInf[phiBar_]:=Module[{factor},
factor=1/2 (Exp[phiBar] Erfc[Sqrt[phiBar]])+Sqrt[phiBar/\[Pi]]
];

LKKappaDensFac[phiBar_,RB_,\[Kappa]_]:=Module[{factor},
factor=1/(2 Sqrt[phiBar] (-3+2 \[Kappa])^(5/2)) (phiBar (3+2 phiBar-2 \[Kappa])^-\[Kappa] Sqrt[(3+2 phiBar-2 \[Kappa])/phiBar] (-3+2 \[Kappa])^(2+\[Kappa]) Csc[\[Pi] \[Kappa]]+1/(phiBar Gamma[-(3/2)+\[Kappa]]) 2 Sqrt[2/\[Pi]] (1/((-1+RB) (3+2 \[Kappa]) (-1+4 \[Kappa]^2)) Gamma[1+\[Kappa]] ((-1+RB) (-3+2 \[Kappa]) ((-1+RB)^2 (3-2 \[Kappa])^2+2 phiBar (-1+RB) (-3+2 \[Kappa]) (5+2 \[Kappa])-4 phiBar^2 (-1+6 \[Kappa]+4 \[Kappa]^2))-(2 phiBar+(-1+RB) (-3+2 \[Kappa]))^3 Hypergeometric2F1[1,1+\[Kappa],-(1/2),(2 phiBar)/((-1+RB) (3-2 \[Kappa]))])+2 phiBar^2 \[Pi] (-3+2 \[Kappa]) Csc[\[Pi] \[Kappa]] Hypergeometric2F1Regularized[-(1/2),1,1-\[Kappa],(-3+2 \[Kappa])/(2 phiBar)]))
];

LKKappa2p5DensFac[phiBar_,RB_]:=Module[{factor},
factor=1/4 phiBar^(3/2) ((-5+2/phiBar^(3/2)+3 phiBar)/(-1+phiBar)^2+(5-3 phiBar-5 RB)/(-1+phiBar+RB)^2)
];


End[] (* End Private Context *)

EndPackage[]



