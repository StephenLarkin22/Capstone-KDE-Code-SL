(* ::Package:: *)

(*Cycling Data Dublin City S^1 Case Study*)
(*Again, CycleData2025.csv is attached in the repository under 'data' branch. If the following import doesn't work, resolve by downloading the dataset and changing the file path in Import[] below *)
CycleData =  Import["data/CycleData2025.csv"];
(*Cleaning data*)
times = CycleData[[All,1]]; 
times = Rest[times]; 

counts = CycleData[[All,5]]; 
counts = Rest[counts]; 

goodRows = Not @* MissingQ /@ counts; 

times = Pick[times, goodRows]; 
counts = Pick[counts, goodRows]; 

months = ToExpression[StringTake[times, {4, 5}]]; 
summerRows = (MemberQ[{6, 7, 8}, #1] & ) /@ months; 
winterRows = (MemberQ[{12, 1, 11}, #1] & ) /@ months; 

summerTimes = Pick[times, summerRows]; 
summerCounts = Pick[counts, summerRows]; 

winterTimes = Pick[times, winterRows]; 
winterCounts = Pick[counts, winterRows]; 

summerHours = (ToExpression[StringSplit[StringSplit[#1, Whitespace][[-1]], ":"][[1]]] & ) /@ summerTimes; 
winterHours = (ToExpression[StringSplit[StringSplit[#1, Whitespace][[-1]], ":"][[1]]] & ) /@ winterTimes; 

summerWeights = Table[Total[Pick[summerCounts, summerHours, h]], {h, 0, 23}]; 
winterWeights = Table[Total[Pick[winterCounts, winterHours, h]], {h, 0, 23}]; 

CyclistAngles = Pi/2 - (2*(Pi/24))*Range[0, 23]; 

(*Winter KDE on S^1*)
nW = Total[winterWeights]; 
s = 2; 
NewCeilFunc[x_] := If[Ceiling[x] == x, x + 1, Ceiling[x]]; 
NewFloorFunc[x_] := If[Floor[x] == x, x - 1, Floor[x]]; 
r = 2 + NewCeilFunc[s] + 1; 
TruncPointS1W = NewFloorFunc[((1/(Pi*(r - 1)))*nW^((s + r)/(2*s + 1)))^(1/(r - 1))] + 1; 
gfunc[x_] := 1/(1 + Abs[x]^r); 
h = nW^(-(2*s + 1)^(-1));
(*Weighted KDE*)
KDES1W[t_] := (1/(2*Pi*nW))*(nW + 2*Sum[gfunc[h*l]*Sum[winterWeights[[j]]*Cos[l*(CyclistAngles[[j]] - t)], {j, 1, 24}], 
       {l, 1, TruncPointS1W}]); 
min1W = Min[KDES1W /@ Range[-Pi, Pi, 0.1]]; 
max1W = Max[KDES1W /@ Range[-Pi, Pi, 0.1]]; 
UniS1Plot2 = Show[PolarPlot[1.05, {\[Theta], 0, 2*Pi}, PlotStyle -> {Thickness[0.15], CapForm["Butt"]}, 
     ColorFunction -> Function[{x, y, \[Theta], r}, ColorData["Rainbow"][Rescale[KDES1W[\[Theta]], {min1W, max1W}]]], 
     ColorFunctionScaling -> False, Axes -> False, Frame -> False, PlotPoints -> 3000, MaxRecursion -> 5, 
     PlotRange -> {{-1.25, 1.25}, {-1.25, 1.25}}], PolarPlot[1.4, {t, 0, 2*Pi}, PlotStyle -> {Thickness[0.2], White}], 
    Ticks -> None]; 
(*Hour lines for the graph *)
hourMarks = Graphics[Table[With[{ang = Pi/2 - 2*Pi*(h/24)}, {Black, Thin, Line[{{0, 0}, 1.12*{Cos[ang], Sin[ang]}}], 
       Black, Text[Style[ToString[h], 10], 1.22*{Cos[ang], Sin[ang]}]}], {h, 0, 23}]]; 
(*Plot of the winter cycle KDE*)
WinterCyclePlot = Show[UniS1Plot2, hourMarks, PlotRange -> {{-1.35, 1.35}, {-1.35, 1.35}}, ImagePadding -> 30] 
legend2 = BarLegend[{"Rainbow", {min1W, max1W}}, LegendLayout -> "Column", LabelStyle -> Directive[Black], 
    LegendMarkerSize -> {15, 150}, LegendMargins -> 0, Method -> {"ShrinkWrap" -> True}, LabelStyle -> 11] 

(*Summer KDE on S^1*)
nS = Total[summerWeights]; 
s = 2; 
NewCeilFunc[x_] := If[Ceiling[x] == x, x + 1, Ceiling[x]]; 
NewFloorFunc[x_] := If[Floor[x] == x, x - 1, Floor[x]]; 
r = 2 + NewCeilFunc[s] + 1; 
TruncPointS1S = NewFloorFunc[((1/(Pi*(r - 1)))*nS^((s + r)/(2*s + 1)))^(1/(r - 1))] + 1; 
gfunc[x_] := 1/(1 + Abs[x]^r); 
h = nS^(-(2*s + 1)^(-1));
(*Weighted KDE*)
KDES1S[t_] := (1/(2*Pi*nS))*(nS + 2*Sum[gfunc[h*l]*Sum[summerWeights[[j]]*Cos[l*(CyclistAngles[[j]] - t)], {j, 1, 24}], 
       {l, 1, TruncPointS1S}]); 
min1S = Min[KDES1S /@ Range[-Pi, Pi, 0.1]]; 
max1S = Max[KDES1S /@ Range[-Pi, Pi, 0.1]]; 
UniS1Plot2 = Show[PolarPlot[1.05, {\[Theta], 0, 2*Pi}, PlotStyle -> {Thickness[0.15], CapForm["Butt"]}, 
     ColorFunction -> Function[{x, y, \[Theta], r}, ColorData["Rainbow"][Rescale[KDES1S[\[Theta]], {min1S, max1S}]]], 
     ColorFunctionScaling -> False, Axes -> False, Frame -> False, PlotPoints -> 3000, MaxRecursion -> 5, 
     PlotRange -> {{-1.25, 1.25}, {-1.25, 1.25}}], PolarPlot[1.4, {t, 0, 2*Pi}, PlotStyle -> {Thickness[0.2], White}], 
    Ticks -> None]; 
(*Hour lines on the graph*)
hourMarks = Graphics[Table[With[{ang = Pi/2 - 2*Pi*(h/24)}, {Black, Thin, Line[{{0, 0}, 1.12*{Cos[ang], Sin[ang]}}], 
       Black, Text[Style[ToString[h], 10], 1.22*{Cos[ang], Sin[ang]}]}], {h, 0, 23}]]; 
(*Plot of the summer cycle KDE*)
SummerCyclePlot = Show[UniS1Plot2, hourMarks, PlotRange -> {{-1.35, 1.35}, {-1.35, 1.35}}, ImagePadding -> 30] 
legend2 = BarLegend[{"Rainbow", {min1S, max1S}}, LegendLayout -> "Column", LabelStyle -> Directive[Black], 
    LegendMarkerSize -> {15, 150}, LegendMargins -> 0, Method -> {"ShrinkWrap" -> True}, LabelStyle -> 11] 

(*Probability Estimation, thetas are the respective hour angles*)
theta8 = Pi/2 - 2*Pi*(8/24);
theta9 = Pi/2 - 2*Pi*(9/24);
theta17 = Pi/2 - 2*Pi*(17/24);
theta18 = Pi/2 - 2*Pi*(18/24);
theta0 = Pi/2;
theta5 = Pi/2 - 10*(Pi/24);
(*Estimated probabilities*)
CommuteProbS89 = NIntegrate[KDES1S[t], {t, theta9, theta8}]
CommuteProbS56 = NIntegrate[KDES1S[t], {t, theta18, theta17}]
CommuteProbS50 = NIntegrate[KDES1S[t], {t, theta5, theta0}]
CommuteProbW89 = NIntegrate[KDES1W[t], {t, theta9, theta8}]
CommuteProbW56 = NIntegrate[KDES1W[t], {t, theta18, theta17}]
CommuteProbW50 = NIntegrate[KDES1W[t], {t, theta5, theta0}]



