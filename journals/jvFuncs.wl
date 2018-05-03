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




