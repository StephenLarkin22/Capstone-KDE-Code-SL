(* ::Package:: *)

(*Case Study 1: KDE on R*)
(*Log Returns Data on R*)
(*logreturns.csv is attached in the repository under 'data' branch. If the following import doesn't work, resolve by downloading the dataset and changing the file path in Import[] below *)
LogReturnsData = Import["data/logreturns.csv"];
LogReturnsData = Flatten@Rest[LogReturnsData];
n=Length[LogReturnsData];

(*Silverman's Rule of Thumb bandwidth*)
SampStDev = StandardDeviation[LogReturnsData];
h=2.345*SampStDev *n^(-1/(5));
(*Defining the KDE*)
EpanKer[u_?NumericQ] := Piecewise[{{(3/4) (1-u^2),Abs[u] <=1}},0];
KDER[x_?NumericQ,h_,data_] := 1/(h*Length[data]) Sum[EpanKer[(xi-x)/h],{xi,data}];

(*KDE plot*)
LRKDE = Plot[KDER[x,h,LogReturnsData],{x,-0.1,0.1},
PlotPoints->1000,
PlotRange->{0,KDER[0,h,LogReturnsData]+1},
MaxRecursion->4,
PlotLabel->"KDE for the Log Returns Data",
Axes->True,AxesOrigin->{-0.1,0},AxesLabel->{Row[{Subscript[Style["r",Italic],Style["t",Italic]]}],"Density"},Filling->Axis,GridLines->Automatic,PlotRange->All]

(*Normal Density approximation*)
LRmean= Mean[LogReturnsData];
s= StandardDeviation[LogReturnsData]; (*sd of log returns data*)

(*Normal Density approximation plot*)
LRPlot = Plot[PDF[NormalDistribution[LRmean,s],x],{x,-0.1,0.1},
Axes->True,AxesOrigin->{-0.1,0},AxesLabel->{Row[{Subscript[Style["r",Italic],Style["t",Italic]]}],"Density"},
PlotLabel->"Normal Density Fit to Daily Log Returns (AAPL,2020-2025)",
Filling->Axis,GridLines->Automatic,PlotRange->All]



