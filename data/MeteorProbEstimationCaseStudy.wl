(* ::Package:: *)

(*Note the use of the dataset consisting of the XYZ data, which can be found in the data folder. This csv may need to be downloaded, and the Import path redefined to match where it is downloaded to*)
MeteorDataXYZ=Import["data\meteor1kgXYZ.csv"];
(*Cleaning the data*)
MeteorDataXYZ=MeteorDataXYZ[[All,3;;5]];
MeteorDataXYZ=Rest[MeteorDataXYZ];
n=Length[MeteorDataXYZ];
Count[MeteorDataXYZ[[All,3]],_?(#>0&)]/Length[MeteorDataXYZ]

(*KDE definition*)
s=2;
NewCeilFunc[x_]:= If[Ceiling[x]==x,x+1,Ceiling[x]];
NewFloorFunc[x_]:=If[Floor[x]==x,x-1,Floor[x]];
r=4+NewCeilFunc[s]+1;
TruncPointS2= NewFloorFunc[(1/(\[Pi]*(r-2)) n^((s+r)/(2s+2)))^(1/(r-2))]+1;

gfunc[x_]:=1/(1+Abs[x]^r);
h = n^(-1/(2*s+2));

KDES2[{x_,y_,z_},data_]:=1/Length[data]*Sum[(2l+1)/(4\[Pi]) gfunc[h*Sqrt[l*(l+1)]]*Sum[LegendreP[l,Dot[{x,y,z},data[[j]]]],{j,1,Length[data]}],{l,0,TruncPointS2}];

(*Probability Estimation for Northern and Southern hemisphere*)
NorthProb=NIntegrate[KDES2[{Sin[theta] Cos[phi],Sin[theta] Sin[phi],Cos[theta]},MeteorDataXYZ]*Sin[theta],{theta,0,Pi/2},{phi,0,2 Pi}]
SouthProb=NIntegrate[KDES2[{Sin[theta] Cos[phi],Sin[theta] Sin[phi],Cos[theta]},MeteorDataXYZ]*Sin[theta],{theta,Pi/2,Pi},{phi,0,2 Pi}]
