(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["jvFuncs`"]
(* Exported symbols added here with SymbolName::usage *)  

nVMaxwellian::usage="
nVMaxwellian[pot_,RB_,Tm_,nm_]
Gives the 'Spence relation' density as a function of pot (in V), RB, Tm (in eV), and nm (in cm^-3).
";

JVMaxwellian::usage="
JVMaxwellian[pot_,RB_,Tm_,nm_]
Gives the Knight relation current densty as a function of pot (in V), RB, Tm (in eV), and nm (in cm^-3).
";

JVMaxwellianPhiBar::usage="
JVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]
Gives the Knight relation current densty as a function of phiBar (pot/temperature), RB, Tm (in eV), and nm (in cm^-3).
";

nVKappa::usage="
nVKappa[pot_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

nVKappa2::usage="
nVKappa2[pot_,RB_,Tm_,nm_]
Gives the Dors and Kletzing density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa=2.
";

nVKappa2p5::usage="
nVKappa2p5[pot_,RB_,Tm_,nm_]
Gives the Dors and Kletzing density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa=2.5.
";

nVKappaIntegrate::usage="
nVKappaIntegrate[pot_,RB_,Tm_,nm_,kappa_]
Perform integral to get the Dors and Kletzing density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

nVKappaIntegrate1D::usage="
nVKappaIntegrate[pot_Real,RB_Real,Tm_Real,nm_Real,kappa_Real]
Perform 1-D integral (from kappa 2 paper) to get the Dors and Kletzing density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

nVKappaIntegrate1DPhiBar::usage="
nVKappaIntegratePhiBar[phiBar_Real,RB_Real,Tm_Real,nm_Real,kappa_Real]
Perform 1-D integral (from kappa 2 paper) to get the Dors and Kletzing density as a function of phiBar (=pot/Tm, both in eV), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JVKappa::usage="
JVKappa[pot_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing current density as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JVKappaPhiBar::usage="
JVKappa[phiBar_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing current densty as a function of phiBar (pot/temperature), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JEVMaxwellian::usage="
JEVMaxwellian[pot_,RB_,Tm_,nm_]
Gives the Maxwellian energy flux as a function of pot (in V), RB, Tm (in eV), and nm (in cm^-3).
";

JEVMaxwellianPhiBar::usage="
JEVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]
Gives the Maxwellian energy flux as a function of phiBar (pot/temperature), RB, Tm (in eV), and nm (in cm^-3).
";

JEVKappa::usage="
JEVKappa[pot_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing energy flux as a function of pot (in V), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JEVKappaPhiBar::usage="
JEVKappa[phiBar_,RB_,Tm_,nm_,kappa_]
Gives the Dors and Kletzing energy flux as a function of phiBar (pot/temperature), RB, Tm (in eV), nm (in cm^-3), and kappa.
";

JVRatio::usage="
JVRatio[pot_,RB_,Tm_,kappa_]
Gives the ratio of the Dors and Kletzing J-V relation and the Knight J-V relation as a function of pot, RB, Tm (in eV), and kappa
";

JVRatioPhiBar::usage="
JVRatioPhiBar[phiBar_,RB_,kappa_]
Gives the ratio of the Dors and Kletzing J-V relation and the Knight J-V relation as a function of phiBar (pot/temperature), RB, and kappa
";


Begin["`Private`"] (* Begin Private Context *) 
nVMaxwellian[pot_,RB_,Tm_,nm_]:=Module[{factor},
factor=nm*(1/2 (Exp[pot/Tm] Erfc[Sqrt[pot/Tm]]+2Sqrt[(RB-1)/\[Pi]]DawsonF[Sqrt[(pot/Tm)/(RB-1)]]))
];

JVMaxwellian[pot_,RB_,Tm_,nm_]:=Module[{jParallel},
jParallel=0.0266987 nm Tm^(1/2) RB (1-(1-1/RB)Exp[-(pot/Tm)/(RB-1)])
];

JVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]:=Module[{jParallel},
jParallel=0.0266987 nm Tm^(1/2) RB (1-(1-1/RB)Exp[-(phiBar)/(RB-1)])
];

nVKappa[pot_,RB_,Tm_,nm_,kappa_]:=Module[{factor,kappr},
kappr=kappa;
If[Abs[kappr-Round[kappr]]<= 0.01,kappr=Round[kappr]+0.01*RandomChoice[{1,-1}]];
factor=nm*1/(2 Sqrt[pot/Tm] (-3+2 kappr)^(5/2)) (pot/Tm (3+2 pot/Tm-2 kappr)^-kappr Sqrt[(3+2 pot/Tm-2 kappr)/pot/Tm] (-3+2 kappr)^(2+kappr) Csc[\[Pi] kappr]+1/(pot/Tm Gamma[-(3/2)+kappr]) 2 Sqrt[2/\[Pi]] (1/((-1+RB) (3+2 kappr) (-1+4 kappr^2)) Gamma[1+kappr] ((-1+RB) (-3+2 kappr) ((-1+RB)^2 (3-2 kappr)^2+2 pot/Tm (-1+RB) (-3+2 kappr) (5+2 kappr)-4 pot/Tm^2 (-1+6 kappr+4 kappr^2))-(2 pot/Tm+(-1+RB) (-3+2 kappr))^3 Hypergeometric2F1[1,1+kappr,-(1/2),(2 pot/Tm)/((-1+RB) (3-2 kappr))])+2 pot/Tm^2 \[Pi] (-3+2 kappr) Csc[\[Pi] kappr] Hypergeometric2F1Regularized[-(1/2),1,1-kappr,(-3+2 kappr)/(2 pot/Tm)]))
];

nVKappa2p5[pot_,RB_,Tm_,nm_]:=Module[{factor},
factor=nm ((4 (pot/Tm)^2 RB)/((-1+2 (pot/Tm)) (-1+2 (pot/Tm)+RB))+(Sqrt[2] Sqrt[(pot/Tm)] ArcCos[Sqrt[2] Sqrt[(pot/Tm)]])/(1-2 (pot/Tm))^(3/2)+(Sqrt[2] Sqrt[(pot/Tm)] Sqrt[1+(2 (pot/Tm))/(-1+RB)] (-1+RB)^(5/2) ArcSinh[(Sqrt[2] Sqrt[(pot/Tm)])/Sqrt[-1+RB]])/(-1+2 (pot/Tm)+RB)^2)/(Sqrt[2] Sqrt[(pot/Tm)] \[Pi])
];

nVKappa2p5[pot_,RB_,Tm_,nm_]:=Module[{factor},
factor=nm/4 (pot/Tm)^(3/2) ((-5+2/(pot/Tm)^(3/2)+3 (pot/Tm))/(-1+(pot/Tm))^2+(5-3 (pot/Tm)-5 RB)/(-1+(pot/Tm)+RB)^2)
];

nVKappaIntegrate[pot_,RB_,Tm_,nm_,kappa_]:=Module[{factor},
factor=nm 2/Sqrt[\[Pi]] 1/(1-3/(2 kappa))^(3/2) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) *
NIntegrate[
rho^2 Sin[theta] (1+(rho^2-(pot/Tm))/(kappa-3/2))^-(kappa+1) 
* Piecewise[{{1,rho^2 (1- (Sin[theta])^2/RB)-(pot/Tm)>0&&rho>= 0},{0,rho<= 0},{0,rho^2 (1- (Sin[theta])^2/RB)-(pot/Tm)< 0}}],{theta,0,\[Pi]/2},{rho,0,30}]
];

nVKappaIntegrate1D[pot_Real,RB_Real,Tm_Real,nm_Real,kappa_Real,opts:OptionsPattern[]]:=Module[{factor},
factor=nm *(pot/Tm)^(3/2)/Sqrt[\[Pi]] Gamma[kappa+1]/((kappa-3/2)^(3/2) Gamma[kappa-1/2]) (NIntegrate[Sqrt[Ebar+1] (1+(Ebar (pot/Tm))/(kappa-3/2))^-(kappa+1),{Ebar,0,\[Infinity]},Evaluate[FilterRules[{opts},Options[NIntegrate]]]]-1/(RB-1) NIntegrate[Sqrt[1-Ebar] (1+(Ebar (pot/Tm))/((kappa-3/2) (RB-1)))^-(kappa+1),{Ebar,0,1},Evaluate[FilterRules[{opts},Options[NIntegrate]]]])
];

nVKappaIntegrate1DPhiBar[phiBar_Real,RB_Real,Tm_Real,nm_Real,kappa_Real,opts:OptionsPattern[]]:=Module[{factor},
factor=nm * phiBar^(3/2)/Sqrt[\[Pi]] Gamma[kappa+1]/((kappa-3/2)^(3/2) Gamma[kappa-1/2]) (NIntegrate[Sqrt[Ebar+1] (1+(Ebar phiBar)/(kappa-3/2))^-(kappa+1),{Ebar,0,\[Infinity]},Evaluate[FilterRules[{opts},Options[NIntegrate]]]]-1/(RB-1) NIntegrate[Sqrt[1-Ebar] (1+(Ebar phiBar)/((kappa-3/2) (RB-1)))^-(kappa+1),{Ebar,0,1},Evaluate[FilterRules[{opts},Options[NIntegrate]]]])
];

JVKappa[pot_,RB_,Tm_,nm_,kappa_]:=Module[{jParallel},
jParallel=0.0266987 nm ((1-3/(2 kappa))Tm)^(1/2) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB/(1-1/kappa) (1-(1-1/RB)(1+pot/(Tm (kappa-3/2) (RB-1)))^(-kappa+1))
];

JVKappaPhiBar[phiBar_,RB_,Tm_,nm_,kappa_]:=Module[{jParallel},
jParallel=0.0266987 nm ((1-3/(2 kappa))Tm)^(1/2) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB/(1-1/kappa) (1-(1-1/RB)(1+phiBar/((kappa-3/2) (RB-1)))^(-kappa+1))
];

JEVMaxwellian[pot_,RB_,Tm_,nm_]:=Module[{jeParallel},
jeParallel=2.6805946*^-5 nm Tm^(3/2) RB ((2 + pot/Tm) - (pot/Tm + 2 (1-1/RB) ) Exp[-(pot/Tm)/(RB-1)])
];

JEVMaxwellianPhiBar[phiBar_,RB_,Tm_,nm_]:=Module[{jeParallel},
jeParallel=2.6805946*^-5 nm Tm^(3/2) RB ((2 + phiBar) - (phiBar + 2 (1-1/RB) ) Exp[-phiBar/(RB-1)])
];

JEVKappa[pot_,RB_,Tm_,nm_,kappa_]:=Module[{jeParallel},
jeParallel=2.6805946*^-5 nm Tm^(3/2) (1-3 / (2 kappa ) )^(3/2) kappa^2/((kappa-1)(kappa-2)) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB ((2 + (kappa-2)/(kappa-3/2) pot/Tm)
-(1+pot/(Tm(kappa-3/2)(RB-1)))^(-kappa+1) (1-1/RB)((kappa-2)/(kappa-1)+(kappa-2)/(kappa-3/2) pot/Tm)(kappa/((kappa-1)(RB-1))+1)
-(1+pot/(Tm(kappa-3/2)(RB-1)))^(-kappa+2) (1-1/RB)^2 (1+(1+kappa/(RB-1))/(kappa-1)))
];

JEVKappaPhiBar[phiBar_,RB_,Tm_,nm_,kappa_]:=Module[{jeParallel},
jeParallel=2.6805946*^-5 nm Tm^(3/2) (1-3 / (2 kappa ) )^(3/2) kappa^2/((kappa-1)(kappa-2)) Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) RB ((2 + (kappa-2)/(kappa-3/2) phiBar)
-(1+phiBar/((kappa-3/2)(RB-1)))^(-kappa+1) (1-1/RB)((kappa-2)/(kappa-1)+(kappa-2)/(kappa-3/2) phiBar)(kappa/((kappa-1)(RB-1))+1)
-(1+phiBar/((kappa-3/2)(RB-1)))^(-kappa+2) (1-1/RB)^2 (1+(1+kappa/(RB-1))/(kappa-1)))
];

JVRatio[pot_,RB_,Tm_,kappa_]:=Module[{ratio},
ratio = Sqrt[1-3 /(2 kappa)] Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) 1/(1-1/kappa) ( 1-(1-1/RB) ((1+pot/((kappa-3/2) (RB-1) Tm))^(1-kappa))  )/(  1-(1-1/RB)(E^(-(pot/(Tm(RB-1))))) )
];

JVRatioPhiBar[phiBar_,RB_,kappa_]:=Module[{ratio},
ratio = Sqrt[1-3 /(2 kappa)] Gamma[kappa+1]/(kappa^(3/2) Gamma[kappa-1/2]) 1/(1-1/kappa) ( 1-(1-1/RB) ((1+phiBar/((kappa-3/2) (RB-1)))^(1-kappa))  )/(  1-(1-1/RB)(E^(-(phiBar/(RB-1)))) )
];


End[] (* End Private Context *)

EndPackage[]




