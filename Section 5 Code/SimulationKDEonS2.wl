(* ::Package:: *)

(*KDE on S^2 Simulation*)
(*Uniform Distribution on S^2 Plot*)
UnifS2Plot= Legended[SphericalPlot3D[1,{\[Theta],0,\[Pi]},{\[Phi],-\[Pi],\[Pi]}, PlotStyle->{ DarkGreen}, Mesh -> None, Axes-> False, Boxed -> False, PlotRangePadding->None,ImagePadding->0, PlotRange->{{-1.2,1.2},{-1.2,1.2},{-1.2,1.2}}],
Placed[SwatchLegend[{DarkGreen}, {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"1\", \"/\", \"4\", \"\[Pi]\", \"\[TildeTilde]\", \"0.07958\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"1/4\\\\pi \\\\approx 0.07958\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)"},LegendMarkerSize->10,LabelStyle->Directive[FontSize->8]],{0.5,0.15}]]

(*Now we repeat for the uniform distribution on S2*)
SeedRandom[123];
UniformDistS2= Table[
Module[{rand1,rand2,rand3},
 rand1 = RandomVariate[NormalDistribution[0,1]];
 rand2 = RandomVariate[NormalDistribution[0,1]];
 rand3 = RandomVariate[NormalDistribution[0,1]];
{rand1,rand2,rand3}/Sqrt[rand1^2+rand2^2+rand3^2]], 
{i,1,1000}];
dataUS2= UniformDistS2;
n=Length[dataUS2];
S2UnifPlotdata = Show[SphericalPlot3D[1,{\[Theta],0,\[Pi]},{\[Phi],-\[Pi],\[Pi]}, PlotStyle->{Opacity[1], White}, Mesh -> None, Axes-> False, Boxed -> False, PlotRangePadding->None,ImagePadding->0,PlotRange->{{-1.2,1.2},{-1.2,1.2},{-1.2,1.2}}],
ListPointPlot3D[dataUS2, PlotStyle->{Red,PointSize[0.01]}]]

(*KDE on S^2 for s=2. For the s=0.5,1 cases change the s parameter below.*)
s=2;
n=1000;
NewCeilFunc[x_]:= If[Ceiling[x]==x,x+1,Ceiling[x]];
NewFloorFunc[x_]:=If[Floor[x]==x,x-1,Floor[x]];
r=4+NewCeilFunc[s]+1;
TruncPointS2= NewFloorFunc[(1/(2*\[Pi]*(r-2)) n^((s+r)/(2s+2)))^(1/(r-2))]+1;

gfunc[x_]:=1/(1+Abs[x]^r);
h = n^(-1/(2*s+2));

KDES2[{x_,y_,z_},data_]:=1/Length[data]*Sum[(2l+1)/(4\[Pi]) gfunc[h*Sqrt[l*(l+1)]]*Sum[LegendreP[l,Dot[{x,y,z},data[[j]]]],{j,1,Length[data]}],{l,0,TruncPointS2}];

(*Compute KDE values on a grid over the sphere for plotting*)
KDES2data = Table[KDES2[{Cos[\[Phi]]Sin[\[Theta]],Sin[\[Phi]]Sin[\[Theta]],Cos[\[Theta]]},dataUS2],{\[Phi],-\[Pi],\[Pi],\[Pi]/32},{\[Theta],0,\[Pi],\[Pi]/32}];

DensityKDES2U= ListDensityPlot[KDES2data, Axes-> False, AxesLabel-> None,Frame->False,BoundaryStyle->None,ImageMargins->None,PlotRangePadding->None,ColorFunction->"Rainbow", PlotLegends->Placed[BarLegend[Automatic,LegendLayout->"Row", LegendMarkerSize->250,LabelStyle->Directive[FontSize->8]],Below], PlotRange->All];
TextureS2= Image[DensityKDES2U[[1]]];
(*This is the plot of the sphere with the density texture overlayed*)
UKDEdensityS2Plot1 = Show[SphericalPlot3D[1,{\[Theta],0,\[Pi]},{\[Phi],-\[Pi],\[Pi]}, PlotStyle->{Texture[TextureS2]}, Mesh -> None,Lighting->"Neutral", Axes-> False, Boxed -> False,PlotRange->All]];
legend1S2 = DensityKDES2U[[2,1]];
KDES2PlotsU1= Column[{UKDEdensityS2Plot1,legend1S2},Alignment->Center]

(*MISE on S^2. For the s=1,0.5 cases just change the s value below.*)
MISES2U=ConstantArray[0,30];
UniDistS2 = 1/(4\[Pi]);

n=1000;
s=2;
NewCeilFunc[x_]:= If[Ceiling[x]==x,x+1,Ceiling[x]];
NewFloorFunc[x_]:=If[Floor[x]==x,x-1,Floor[x]];
r=5+NewCeilFunc[s]+1; (*r increased by one still works, as this still satisfies the strict greater than condition*)
TruncPointS2= NewFloorFunc[(1/(2\[Pi]*(r-2)) n^((s+r)/(2s+2)))^(1/(r-2))]+1;
gfunc[x_]:=1/(1+Abs[x]^r);
h = n^(-1/(2*s+2));

KDES2[{x_(*?NumericQ*),y_(*?NumericQ*),z_(*?NumericQ*)},data_]:=1/Length[data]*Sum[((2 l+1)/(4*Pi)) gfunc[h*Sqrt[l (l+1)]]*Sum[LegendreP[l,Dot[{x,y,z},data[[j]]]],{j,1,Length[data]}],{l,0,TruncPointS2}];

SetSharedVariable[MISES2U];
ParallelDo[
(*setting seed each time for reproducibility*)
SeedRandom[ti];
    X1=RandomVariate[NormalDistribution[0,1],n];
	X2=RandomVariate[NormalDistribution[0,1],n];
	X3=RandomVariate[NormalDistribution[0,1],n];
Uniformdatas2=Normalize[#]&/@Transpose[{X1,X2,X3}];
IntfhatSquared=(1/n^2)*Sum[((2 l+1)/(4 \[Pi]))*gfunc[h*Sqrt[l*(l+1)]]^2*Sum[LegendreP[l,Dot[Uniformdatas2[[i]],Uniformdatas2[[j]]]],{i,1,n},{j,1,n}],{l,0,TruncPointS2}];
MISES2U[[ti]]=
IntfhatSquared -UniDistS2 (*Note the remark in Section 5.1 of the thesis that allows us to write it like this*)
,{ti,1,30}];
Print[Mean[MISES2U]];

(*Probability Estimation on S^2. We switch to polar coordinates to use our integration theory in probability estimation*)
SphereToCartesian[\[Theta]_,\[Phi]_]:={Sin[\[Theta]] Cos[\[Phi]],Sin[\[Theta]] Sin[\[Phi]],Cos[\[Theta]]};
KDES2Angles[\[Theta]_,\[Phi]_,data_]:=KDES2[SphereToCartesian[\[Theta],\[Phi]],data];
(*Integration region function and estimated probabilities*)
ProbRegionS2[{\[Theta]min_,\[Theta]max_},{\[Phi]min_,\[Phi]max_},data_]:=NIntegrate[KDES2Angles[\[Theta],\[Phi],data] Sin[\[Theta]],{\[Theta],\[Theta]min,\[Theta]max},{\[Phi],\[Phi]min,\[Phi]max},Method->"QuasiMonteCarlo"];
EPtotal=ProbRegionS2[{0,Pi},{-Pi,Pi},dataUS2];
EPhalf1S2= ProbRegionS2[{0,Pi},{-Pi,0},dataUS2];
EPhalf2S2=ProbRegionS2[{0,Pi},{0,Pi},dataUS2];

EPquarter1S2 = ProbRegionS2[{0,Pi/2},{-Pi,0},dataUS2];
EPquarter2S2 =ProbRegionS2[{0,Pi/2},{0,Pi},dataUS2];
EPquarter3S2 =ProbRegionS2[{Pi/2,Pi},{-Pi,0},dataUS2];
EPquarter4S2 =ProbRegionS2[{Pi/2,Pi},{0,Pi},dataUS2];

EPquarterTot = Total[{EPquarter1S2,EPquarter2S2,EPquarter3S2,EPquarter4S2}];
EPq={EPquarter1S2,EPquarter2S2,EPquarter3S2,EPquarter4S2}/EPquarterTot;

