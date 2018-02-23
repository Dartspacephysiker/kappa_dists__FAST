(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["jvFuncs`"]
(* Exported symbols added here with SymbolName::usage *)  

JVMaxwellian::usage="
JVMaxwellian[pot_,RB_,Tm_,nm_]
Gives the Knight relation current densty as a function of pot (in V), RB, Tm (in eV), and nm (in cm^-3).
";

JVMaxwellianPhiBar::usage="
JVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]
Gives the Knight relation current densty as a function of phiBar (pot/temperature), RB, Tm (in eV), and nm (in cm^-3).
";

JVKappa::usage="
JVKappa[pot_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing current densty as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JVKappaPhiBar::usage="
JVKappa[phiBar_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing current densty as a function of phiBar (pot/temperature), RB, Tm (in eV), nm (in cm^-3), and kappa.
";


Begin["`Private`"] (* Begin Private Context *) 
JVMaxwellian[pot_,RB_,Tm_,nm_]:=Module[{jParallel},
jParallel=0.0266987 nm Tm^(1/2) RB (1-(1-1/RB)Exp[-(pot/Tm)/(RB-1)])
];

JVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]:=Module[{jParallel},
jParallel=0.0266987 nm Tm^(1/2) RB (1-(1-1/RB)Exp[-(phiBar)/(RB-1)])
];

JVKappa[pot_,RB_,Tm_,nm_,kappa_]:=Module[{jParallel},
jParallel=0.0266987 nm ((1-3/(2 kappa))Tm)^(1/2) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB/(1-1/kappa) (1-(1-1/RB)(1+pot/(Tm (kappa-3/2) (RB-1)))^(-kappa+1))
];

JVKappaPhiBar[phiBar_,RB_,Tm_,nm_,kappa_]:=Module[{jParallel},
jParallel=0.0266987 nm ((1-3/(2 kappa))Tm)^(1/2) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB/(1-1/kappa) (1-(1-1/RB)(1+phiBar/((kappa-3/2) (RB-1)))^(-kappa+1))
];
End[] (* End Private Context *)

EndPackage[]

