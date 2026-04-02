(* ::Package:: *)

(*KDE on Real Line*)
(*Mixed Normal Distribution and simulated points*)
n = 1000;
SeedRandom[15];
MixedDist = MixtureDistribution[
{0.6,0.4},
{NormalDistribution[3,0.75],NormalDistribution[7,1.25]}
];

DataMixed=RandomVariate[MixedDist,n];

(*Silverman Rule of Thumb Bandwidth computation*)
SSD = StandardDeviation[DataMixed];
h = 2.345*SSD*n^(-1/5);

(*Epanechnikov and KDE on Real Line using this kernel, with the plot*)
EpanKer[u_?NumericQ] := Piecewise[{{(3/4) (1-u^2),Abs[u] <=1}},0]
KDER[x_?NumericQ,h_,data_] := 1/(h*Length[data]) Sum[EpanKer[(xi-x)/h],{xi,data}];
KDERPlot = Plot[KDER2[x,h,DataMixed],{x,0,10},
PlotPoints->3000,
MaxRecursion->4,
PlotLabel->"Kernel Density Estimator",
AxesLabel->{"x","Density"}];

(*Probability Estimation, frequencies and actual probabilities*)
MixedCDF = CDF[MixedDist];
ActualProbs = Table[MixedCDF[2.5*(i+1)]-MixedCDF[2.5*(i)],{i,0,3}]
EstimatedProbs = Table[NIntegrate[KDER[x,h,DataMixed],{x,5/2*(i),5/2*(i+1)}],{i,0,3}]
BinsR={0.,2.5,5.,7.5,10.};

ProportionsMixedR=N[BinCounts[DataMixed,{BinsR}]/Length[DataMixed]];

Transpose[{Partition[BinsR,2,1],ProportionsMixedR}];

(*MISE calculation for the simulation on the real line*)
MISER = ConstantArray[0,30];
SetSharedVariable[MISER];
ParallelDo[
n=1000;
SeedRandom[ti];
DataMixedMISE=RandomVariate[MixedDist,n];

SSD = StandardDeviation[DataMixedMISE];
h = 2.345*SSD*n^(-1/5);

KDER[x_,h_,data_] := 1/(h*Length[data]) Sum[EpanKer[(xi-x)/h],{xi,data}];

MISER[[ti]] = NIntegrate[(PDF[MixedDist,x]-KDER[x,h,DataMixedMISE])^2,{x,-\[Infinity],\[Infinity]}, MaxRecursion->20,AccuracyGoal->5,PrecisionGoal->5]
,{ti,1,30}];
Print[Mean[MISER]];
