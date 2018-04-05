(* ::Package:: *)

(*Last updated 2018/03/29*)


(* ::Subsubsection::Initialization:: *)
(*(*(*(*(*(*Helper func*)*)*)*)*)*)


(* ::Input::Initialization:: *)
makeErrorData[x_,y_,yErr_]:=Module[{zeList,xy,errorB},
xy=Partition[Riffle[x,y],2];
errorB=Map[ErrorBar,yErr];
zeList=Partition[Riffle[xy,errorB],2]
];


(* ::Subsubsection::Initialization:: *)
(*(*(*(*(*(*Funcs for plotting stepMonitor output*)*)*)*)*)*)


(* ::Input::Initialization:: *)
kPlotStepMonitorData[kStepVals_]:=Module[{kStepRB,kStepT,kStepN,kStepKappa,kStepCount,NandKap,TandRB},
{kStepRB,kStepT,kStepN,kStepKappa,kStepCount}=Transpose@kStepVals;imgSize=700;
NandKap=Overlay[
{ListLogPlot[kStepN,PlotLabels->Placed[{Style["kStepN",Blue,Bold,13]},{Above}],ImagePadding->{{50,50},{20,10}},PlotStyle->Blue,ImageSize->imgSize,Frame->{True,True,True,False},FrameStyle->{Automatic,Blue,Automatic,Automatic},FrameLabel->Map[(Style[#,Bold,14])&,{{"N",""},{"",""}},{2}],AspectRatio->1/3],
ListPlot[kStepKappa,PlotLabels->Placed[{Style["kStepKappa",Orange,Bold,13]},{Above}],ImagePadding->{{50,50},{20,10}},PlotStyle->Orange,ImageSize->imgSize,Frame->{False,False,False,True},FrameTicks->{{None,All},{None,None}},FrameStyle->{Automatic,Automatic,Automatic,Orange},FrameLabel->Map[(Style[#,Bold,14])&,{{"","Kappa"},{"",""}},{2}],AspectRatio->1/3]}];
TandRB=Overlay[
{ListLogPlot[kStepT,PlotLabels->Placed[{Style["",Blue,Bold,13]},{Above}],ImagePadding->{{50,50},{50,10}},PlotStyle->Blue,ImageSize->imgSize,Frame->{True,True,True,False},FrameStyle->{Automatic,Blue,Automatic,Automatic},FrameLabel->Map[(Style[#,Bold,14])&,{{"T (eV)",""},{"Count",""}},{2}],AspectRatio->1/3],
ListLogPlot[kStepRB,PlotLabels->Placed[{Style["",Orange,Bold,13]},{Above}],FrameTicks->{{None,All},{None,None}},ImagePadding->{{50,50},{50,10}},PlotStyle->Orange,ImageSize->imgSize,Frame->{False,False,False,True},FrameStyle->{Automatic,Automatic,Automatic,Orange},FrameLabel->Map[(Style[#,Bold,14])&,{{"","\!\(\*SubscriptBox[\(R\), \(B\)]\)"},{"",""}},{2}],AspectRatio->1/3]}];
Column[{NandKap,TandRB}]
]


(* ::Input::Initialization:: *)
gPlotStepMonitorData[gStepVals_]:=Module[{gStepRB,gStepT,gStepN,gStepCount,NandKap,TandRB},
{gStepRB,gStepT,gStepN,gStepCount}=Transpose@gStepVals;imgSize=700;
NandKap=ListLogPlot[gStepN,PlotLabels->Placed[{Style["gStepN",Blue,Bold,13]},{Above}],ImagePadding->{{50,50},{20,10}},PlotStyle->Blue,ImageSize->imgSize,Frame->{True,True,True,False},FrameStyle->{Automatic,Blue,Automatic,Automatic},FrameLabel->Map[(Style[#,Bold,14])&,{{"N",""},{"",""}},{2}],AspectRatio->1/3];
TandRB=Overlay[
{ListLogPlot[gStepT,PlotLabels->Placed[{Style["",Blue,Bold,13]},{Above}],ImagePadding->{{50,50},{50,10}},PlotStyle->Blue,ImageSize->imgSize,Frame->{True,True,True,False},FrameStyle->{Automatic,Blue,Automatic,Automatic},FrameLabel->Map[(Style[#,Bold,14])&,{{"T (eV)",""},{"Count",""}},{2}],AspectRatio->1/3],
ListLogPlot[gStepRB,PlotLabels->Placed[{Style["",Orange,Bold,13]},{Above}],FrameTicks->{{None,All},{None,None}},ImagePadding->{{50,50},{50,10}},PlotStyle->Orange,ImageSize->imgSize,Frame->{False,False,False,True},FrameStyle->{Automatic,Automatic,Automatic,Orange},FrameLabel->Map[(Style[#,Bold,14])&,{{"","\!\(\*SubscriptBox[\(R\), \(B\)]\)"},{"",""}},{2}],AspectRatio->1/3]}];
Column[{NandKap,TandRB}]
]


(* ::Subsubsection::Initialization:: *)
(*(*(*(*(*(*Funcs for string conv*)*)*)*)*)*)


(* ::Input::Initialization:: *)
tStringToSec[tString_]:=Module[{diag,hr,min,sec,msec,secs},
diag=0;
hr=ToExpression@StringTake[tString,2];
min=ToExpression@StringTake[tString,{4,5}];
sec=ToExpression@StringTake[tString,{7,8}];
msec=If[Length@tString==0,
If[StringLength[tString]>= 10,ToExpression@StringTake[tString,{10,StringLength[tString]}],0],(If[StringLength[#]>= 10,ToExpression@StringTake[#,{10,StringLength[#]}],0])&/@tString];
If[diag==1,Print[hr];
Print[min];
Print[sec];
Print[msec];];
(*msec=ToExpression@StringTake[tString,{10,12}];*)
secs=hr*3600+min*60+sec+msec/1000.
]


(* ::Input::Initialization:: *)
minTidInd[tSearchString_,tStringArr_]:=Module[{tConv},
tConv=Abs[tStringToSec[tSearchString]-tStringToSec[tStringArr]];
inds=Position[tConv,Min[tConv],1,1]
]


(* ::Subsubsection::Initialization:: *)
(*(*(*(*(*(*Func for plotting a set of inds or a range of inds*)*)*)*)*)*)


(*Use pots by default*)
Options[tPlotInds]={usePot->pots};
Options[tPlotRange]={usePot->pots};


(* ::Input::Initialization:: *)
tPlotInds[inds_,mom_,momErr_,pRange_,xTitle_,yTitle_,OptionsPattern[]]:=ErrorListPlot[(makeErrorData[(OptionValue[usePot])[[#]],mom[[#]],momErr[[#]]])&[inds],AxesStyle->(FontSize->24),PlotRange->pRange,Epilog->Style[Text[StringJoin@Riffle[time[[{inds[[1]],inds[[-1]]}]],"\[Dash]"],Scaled[{0.05,0.95}],{-1,1}],24],ImageSize->500,Frame->True,FrameLabel->{xTitle,yTitle},FrameStyle->Directive[Black,15]]


(* ::Input::Initialization:: *)
tPlotRange[ind1_,ind2_,mom_,momErr_,pRange_,xTitle_,yTitle_,OptionsPattern[]]:=ErrorListPlot[
(makeErrorData[(OptionValue[usePot])[[#1;;#2]],
mom[[#1;;#2]],
momErr[[#1;;#2]]])&[ind1,ind2],
AxesStyle->(FontSize->24),
PlotRange->pRange,
Epilog->Style[Text[StringJoin@Riffle[time[[{ind1,ind2}]],"\[Dash]"],Scaled[{0.05,0.95}],{-1,1}],24],
ImageSize->500,
Frame->True,FrameLabel->{xTitle,yTitle},FrameStyle->Directive[Black,15]]


(* ::Input::Initialization:: *)
(*Use the following with ListAnimate[tablePlotMov] or Export[filename,tablePlotMov] to make majic*)


(* ::Input::Initialization:: *)
tablePlotMov[indStart_,indEnd1_,indEnd2_,mom_,momErr_,pRange_,xTitle_,yTitle_]:= Table[ErrorListPlot[(makeErrorData[pots[[#1;;#2]],mom[[#1;;#2]],momErr[[#1;;#2]]])&[indStart,lastInd],AxesStyle->(FontSize->24),PlotRange->pRange,Epilog->Style[Text[StringJoin@Riffle[time[[{ind1Start,lastInd}]],"\[Dash]"],Scaled[{0.05,0.95}],{-1,1}],24],ImageSize->500,Frame->True,FrameLabel->{xTitle,yTitle},FrameStyle->Directive[Black,15]],{lastInd,indEnd1,indEnd2}];


(* ::Input::Initialization:: *)
tablePlotsCombineAndAnimate[mov1_,mov2_]:=Module[{fixem},
fixem=Partition[Riffle[mov1,mov2],2];
ListAnimate[Row/@fixem]
]
