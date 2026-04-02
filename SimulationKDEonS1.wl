(* ::Package:: *)

(*KDE on S^1 Simulation*)
(*Uniform Distribution on S^1 Plot*)
UnifS1Plot = Legended[Graphics[{DarkGreen,Annulus[{0,0},{0.9,1.2}]}, PlotRange ->{{-1.25,1.25},{-1.25,1.25}},
Axes -> True, Ticks -> None], Placed[SwatchLegend[{DarkGreen}, {\!\(TraditionalForm\`1/2  \[Pi] \[TildeTilde] 0.15915\)}],Below]]

(*Simulating from the uniform distribution on S^1*)
SeedRandom[123];
UniformDistS1= Table[
Module[{rand1,rand2},
 rand1 = RandomVariate[NormalDistribution[0,1]];
 rand2 = RandomVariate[NormalDistribution[0,1]];{rand1,rand2}/Sqrt[rand1^2+rand2^2]], 
{i,1,1000}];
n=1000;

(*Convert to polar coordinates*)
data\[Theta] = ArcTan @@@ UniformDistS1;

(*Lets jitter the data for better visualisation on the points plot*)
JitterS1data = ({Cos[data\[Theta]],Sin[data\[Theta]]}//Transpose)*RandomVariate[UniformDistribution[{0.9,1.2}],n];

(*Plotting the data here so we can see the sampled points from the uniform distribution*)
Plot1 = Show[PolarPlot[1,{t,0,2\[Pi]}, PlotStyle -> White],
ListPlot[JitterS1data, PlotStyle -> {PointSize[0.015], Red, Opacity[0.2]}],
PlotRange ->{{-1.25,1.25},{-1.25,1.25}},Ticks-> None];

(*Now lets construct the KDEs for s=2. For s=1 and 0.5 case just change the first value s*)
s=2;
NewCeilFunc[x_]:= If[Ceiling[x]==x,x+1,Ceiling[x]];
NewFloorFunc[x_]:=If[Floor[x]==x,x-1,Floor[x]];
r=2+NewCeilFunc[s]+1;
TruncPointS1= NewFloorFunc[(1/(\[Pi]*(r-1)) n^((s+r)/(2s+1)))^(1/(r-1))]+1;
gfunc[x_]:=1/(1+Abs[x]^r);
h = n^(-1/(2*s+1));

KDES1[t_]:=1/(2\[Pi]*n) (n+2*Sum[gfunc[h l]*Sum[Cos[l (data\[Theta][[j]]-t)],{j,1,n}],{l,1,TruncPointS1}]);

(*The min1 and max1 here are for the rainbow color bar legend beside the plot*)
min1=KDES1[#]&/@Range[0,2\[Pi],0.1]//Min;
max1=KDES1[#]&/@Range[0,2\[Pi],0.1]//Max;
UniS1Plot2=Show[PolarPlot[1.05,{\[Theta],0,2\[Pi]},PlotStyle->{Thickness[.15],CapForm["Butt"]},ColorFunction->Function[{x,y,\[Theta],r},(ColorData["Rainbow"][Rescale[KDES1[\[Theta]],{min1,max1}]])]
 ,ColorFunctionScaling->False,PlotPoints->3000,MaxRecursion->5,PlotRange->{{-1.25,1.25},{-1.25,1.25}}],PolarPlot[1.4,{t,0,2\[Pi]},PlotStyle->{Thickness[.2],White}],Ticks->None]
legend2=BarLegend[{"Rainbow",{min1,max1}},LegendLayout->"Row",LegendMarkerSize->200];

(*Probability Estimation S1*)
(*Frequencies*)
n=1000;
rightHalf=Count[data\[Theta],_?(-Pi/2<#<=0&)];
leftHalf=n-rightHalf;
proportionshalvesS1 = {leftHalf,rightHalf}/n

quartersS1={Count[data\[Theta],_?(-Pi/2<#<=0&)],
Count[data\[Theta],_?(0<#<=Pi/2&)],
Count[data\[Theta],_?(Pi/2<#<=Pi&)],
Count[data\[Theta],_?(-Pi<#<=-Pi/2&)]
      };

proportionsquartersS1 =  quartersS1/n;

EPhalf1S1 = NIntegrate[KDES1[\[Theta]],{\[Theta],-\[Pi],0}]
EPhalf2S1 = NIntegrate[KDES1[\[Theta]],{\[Theta],0,\[Pi]}]
(*Estimated probabilities*)
EPquarter1S1 = NIntegrate[KDES1[\[Theta]],{\[Theta],-\[Pi],-(\[Pi]/2)}]
EPquarter2S1 =NIntegrate[KDES1[\[Theta]],{\[Theta],-(\[Pi]/2),0}]
EPquarter3S1 =NIntegrate[KDES1[\[Theta]],{\[Theta],0,\[Pi]/2}]
EPquarter4S1 =NIntegrate[KDES1[\[Theta]],{\[Theta],\[Pi]/2,\[Pi]}]

(*Now we move onto calculating the MISE for S^1 simulation, change the s parameter to look at cases s=1,0.5*)
MISES1=ConstantArray[0,30];
SetSharedVariable[MISES1];
ParallelDo[
UniDistS1 = 1/(2\[Pi]);
n=1000;
SeedRandom[ti];
UniformDistS1= Table[
Module[{rand1,rand2},
 rand1 = RandomVariate[NormalDistribution[0,1]];
 rand2 = RandomVariate[NormalDistribution[0,1]];{rand1,rand2}/Sqrt[rand1^2+rand2^2]], 
{i,1,n}];
data\[Theta] = ArcTan @@@ UniformDistS1;
s=2;
NewCeilFunc[x_]:= If[Ceiling[x]==x,x+1,Ceiling[x]];
NewFloorFunc[x_]:=If[Floor[x]==x,x-1,Floor[x]];
r=2+NewCeilFunc[s]+1;
TruncPointS1= NewFloorFunc[(1/(\[Pi]*(r-1)) n^((s+r)/(2s+1)))^(1/(r-1))]+1;
gfunc[x_]:=1/(1+Abs[x]^r);
h = n^(-1/(2*s+1));

KDES1[t_]:=1/(2\[Pi]*n) (n+2*Sum[gfunc[h l]*Sum[Cos[l (data\[Theta][[j]]-t)],{j,1,n}],{l,1,TruncPointS1}]);
SquaredInt=NIntegrate[UniDistS1^2,{\[Theta],0,2\[Pi]}]; (*Computed outside NIntegrate to save time*)
MISES1[[ti]]=
NIntegrate[(KDES1[\[Theta]])^2 -2*KDES1[\[Theta]]*UniDistS1,{\[Theta],-\[Pi],\[Pi]}]+SquaredInt
,{ti,1,30}];
Print[Mean[MISES1]];



