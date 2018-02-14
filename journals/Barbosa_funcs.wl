(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["BarbosaFuncs`"]
(* Exported symbols added here with SymbolName::usage *)  

BarbosaMwellDensFac::usage="
BarbosaMwellDensFac[phiBar_,RB_]
Gives the Barbosa [1977] factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

BarbosaMwellDensFacRBInf::usage="
BarbosaMwellDensFacRBInf[phiBar_]
Barbosa [1977] factor by which Maxwellian density changes for RB->infinity and phiBar=deltaPhi/temperature.
";

BarbosaKappaDensFac::usage="
BarbosaKappaDensFac[phiBar_,RB_,\[Kappa]_]
Gives the factor by which the density changes for mirror ratio RB and phiBar = deltaPhi/temperature.
";

BarbosaKappaDensFacRBInf::usage="
BarbosaKappaDensFacRBInf[phiBar_,kappa_]
Barbosa [1977] factor by which kappa dist density changes for RB->infinity and phiBar=deltaPhi/temperature.
";


Begin["`Private`"] (* Begin Private Context *) 
BarbosaMwellDensFac[phiBar_,RB_]:=Module[{factor},
factor=1+(2(1-1/RB)Sqrt[phiBar] Exp[-phiBar/RB])/Erfc[-Sqrt[phiBar]] NIntegrate[Exp[ (x^2)/RB]Erfc[x],{x,-Sqrt[phiBar],\[Infinity]}]
];

BarbosaMwellDensFacRBInf[phiBar_]:=Module[{factor},
factor=1+2 Sqrt[phiBar]/Erfc[-Sqrt[phiBar]] NIntegrate[Erfc[x],{x,-Sqrt[phiBar],\[Infinity]}]
];

BarbosaKappaDensFac[phiBar_,RB_,kappa_]:=Module[{factor},
factor=NIntegrate[rho^2 Sin[phi]  2  /(\[Pi]^(1/2) (kappa - 3/2)^(3/2)) (1/2 Gamma[kappa-1/2]/Gamma[kappa+1]+Sqrt[phiBar]/(Sqrt[\[Pi]]kappa (kappa-3/2)^(1/2)) Hypergeometric2F1[1/2,kappa,3/2,-(phiBar/(kappa-3/2))])^-1
(1+(rho^2+phiBar-2 Sqrt[phiBar] rho (1 - (Sin[phi])^2/RB)^(1/2))/(kappa-3/2 ))^-(kappa+1),{rho,0,\[Infinity]},{phi,0,\[Pi]/2}]
];
BarbosaKappaDensFacRBInf[phiBar_,kappa_]:=Module[{factor},
factor=((kappa^(1/2) Gamma[kappa-1/2])/(2 Gamma[kappa])+Sqrt[phiBar]/(Sqrt[\[Pi]] (1-3/(2 kappa))^(1/2)) Hypergeometric2F1[1/2,kappa,3/2,-(phiBar/(kappa-3/2))])^-1 (((kappa+((-1+2 kappa) phiBar)/(1-3/(2 kappa))) Gamma[-(1/2)+kappa])/(2 Sqrt[kappa] Gamma[kappa])+(2 phiBar^(3/2) Hypergeometric2F1[1/2,1+kappa,3/2,-(phiBar/(kappa-3/2))])/(Sqrt[\[Pi]] (1-3/(2 kappa))^(3/2))-(1/((-1+4 kappa^2) Sqrt[\[Pi]]) (1+phiBar/(kappa-3/2))^-kappa ( ((1+2 kappa)Sqrt[phiBar])/(1-3/(2 kappa))^(1/2)+(2 kappa^2 (1-3/(2 kappa))^(1/2))/Sqrt[phiBar] (1- Hypergeometric2F1[1,-(1/2)-kappa,1/2,-(phiBar/(kappa -3/2))]))))
];


End[] (* End Private Context *)

EndPackage[]



