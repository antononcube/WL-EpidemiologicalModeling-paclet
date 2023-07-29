BeginPackage["AntonAntonov`EpidemiologicalModeling`VisualizationFunctions`"];

Begin["`Private`"];

Needs["AntonAntonov`EpidemiologicalModeling`"];

(**************************************************************)
(* EvaluateSolutionsByModelIDs                         *)
(**************************************************************)

Clear[EvaluateSolutionsByModelIDs];

EvaluateSolutionsByModelIDs::"ntr" = "The forth argument is expected to be a valid time range specification.";

EvaluateSolutionsByModelIDs::"nst" = "No model stocks are found with the specification given as the third argument.";

EvaluateSolutionsByModelIDs::"ket" = "The computation times are not added to the result because there is stock named \"Times\".";

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  maxTimeArg : (Automatic | _?NumberQ) ] :=
    EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {1, maxTimeArg, 1}];

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  {minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ)} ] :=
    EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {minTime, maxTimeArg, 1}];

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  { minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ), step_?NumberQ } ] :=

    Block[{maxTime = maxTimeArg, stockSymbols, stockValues, lsTimes},

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      If[ step == 0 || (step > 0 && minTime > maxTime) || (step < 0 && minTime < maxTime),
        Message[EvaluateSolutionsOverGraph::"ntr"];
        Return[$Failed]
      ];

      stockSymbols = Union @ Flatten @ Map[ Cases[GetStockSymbols[model, #], p_[id_] :> p]&, Flatten[{stockNames}] ];

      If[ Length[stockSymbols] == 0,
        Message[EvaluateSolutionsByModelIDs::"nst"];
        Return[$Failed]
      ];

      lsTimes = Range[minTime, maxTime, step];

      stockValues = Map[ #[ lsTimes ]&, KeyTake[ aSol, Union @ Flatten @ Map[ GetStockSymbols[model, #]&, Flatten[{stockNames}] ] ] ];

      stockValues = GroupBy[ Normal[stockValues], #[[1, 1]]&, Total @ #[[All, 2]] & ];

      If[ KeyExistsQ[stockValues, "Times" ],
        Message[EvaluateSolutionsByModelIDs::"ket"],
        (* ELSE *)
        stockValues = Append[ stockValues, "Times" -> lsTimes ]
      ];

      stockValues
    ];

EvaluateSolutionsByModelIDs[gr_Graph, args___] := EvaluateSolutionsByModelIDs[args];


(**************************************************************)
(* EvaluateSolutionsOverGraphVertexes                         *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraphVertexes];

EvaluateSolutionsOverGraphVertexes = EvaluateSolutionsByModelIDs;


(**************************************************************)
(* EvaluateSolutionsOverGraph                                 *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraph];

EvaluateSolutionsOverGraph::"ncs" = "The value of the option \"ColorScheme\" is expected to be a string.";

EvaluateSolutionsOverGraph::"nnsf" = "The value of the option \"NodeSizeFactor\" is expected to be a positive number.";

EvaluateSolutionsOverGraph::"nnorm" = "The value of the option \"Normalization\" is expected to be one of `1`.";

EvaluateSolutionsOverGraph::"ntr" = "The fifth argument is expected to be a valid time range specification.";

EvaluateSolutionsOverGraph::"nst" = "No model stocks are found with the specification given as the third argument.";

Options[EvaluateSolutionsOverGraph] =
    Join[
      {"ColorScheme" -> "TemperatureMap", "NodeSizeFactor" -> 1, "TimePlotLabels" -> True, "Normalization" -> Automatic, "Legended" -> False },
      Options[GraphPlot]
    ];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  maxTimeArg : (Automatic | _?NumberQ),
  opts : OptionsPattern[]] :=
    EvaluateSolutionsOverGraph[ gr, model, stockNames, aSol, {1, maxTimeArg, 1}, opts];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  {minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ)},
  opts : OptionsPattern[]] :=
    EvaluateSolutionsOverGraph[ gr, model, stockNames, aSol, {minTime, maxTimeArg, 1}, opts];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  { minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ), step_?NumberQ },
  opts : OptionsPattern[]] :=

    Block[{cf, nodeSizeFactor, timeLabelsQ, normalization, legendedQ, maxTime = maxTimeArg, expected,
      stockSymbols, vf, stockValues, maxStockValue, res},

      cf = OptionValue[EvaluateSolutionsOverGraph, "ColorScheme"];
      If[! StringQ[cf],
        Message[EvaluateSolutionsOverGraph::"ncs"];
        Return[$Failed]
      ];

      nodeSizeFactor = OptionValue[EvaluateSolutionsOverGraph, "NodeSizeFactor"];
      If[! (NumberQ[nodeSizeFactor] && nodeSizeFactor > 0),
        Message[EvaluateSolutionsOverGraph::"nnsf"];
        Return[$Failed]
      ];

      timeLabelsQ = TrueQ[ OptionValue[EvaluateSolutionsOverGraph, "TimePlotLabels"] ];

      normalization = OptionValue[EvaluateSolutionsOverGraph, "Normalization"];
      expected = {Automatic, "Global", "ByVertex", "byNode"};
      If[ ! MemberQ[ expected, normalization ] ,
        Message[EvaluateSolutionsOverGraph::"nnorm", ToString[InputForm[expected]] ];
        Return[$Failed]
      ];

      legendedQ = TrueQ[ OptionValue[EvaluateSolutionsOverGraph, "Legended"] ];

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      If[ step == 0 || (step > 0 && minTime > maxTime) || (step < 0 && minTime < maxTime),
        Message[EvaluateSolutionsOverGraph::"ntr"];
        Return[$Failed]
      ];

      stockSymbols = Union @ Flatten @ Map[ Cases[GetStockSymbols[model, #], p_[id_] :> p]&, Flatten[{stockNames}] ];

      If[ Length[stockSymbols] == 0,
        Message[EvaluateSolutionsOverGraph::"nst"];
        Return[$Failed]
      ];

      (* Some of the work above is repeated in EvaluateSolutionsByModelIDs. *)
      stockValues = EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {minTime, maxTime, step} ];

      If[ MemberQ[ {Automatic, "Global" }, normalization],

        maxStockValue = Max[Values[stockValues]];

        vf[time_][{xc_, yc_}, name_, {w_, h_}] :=
            {
              ColorData[cf, "ColorFunction"][Total@Rescale[Map[aSol[#[name]][time] &, stockSymbols], {0, maxStockValue}, {0, 1}]],
              Rectangle[{xc - nodeSizeFactor * w, yc - nodeSizeFactor * h}, {xc + nodeSizeFactor * w, yc + nodeSizeFactor * h}]
            },
        (* ELSE *)

        maxStockValue = Max /@ stockValues;

        vf[time_][{xc_, yc_}, name_, {w_, h_}] :=
            {
              ColorData[cf, "ColorFunction"][Total@Rescale[Map[aSol[#[name]][time] &, stockSymbols], {0, maxStockValue[name]}, {0, 1}]],
              Rectangle[{xc - nodeSizeFactor * w, yc - nodeSizeFactor * h}, {xc + nodeSizeFactor * w, yc + nodeSizeFactor * h}]
            }
      ];

      res =
          Table[
            GraphPlot[gr, VertexShapeFunction -> vf[t], FilterRules[ Join[ If[timeLabelsQ, {PlotLabel -> t}, {}], {opts} ], Options[GraphPlot]]],
            {t, Range[minTime, maxTime, step]}
          ];

      Which[
        legendedQ && MemberQ[ {Automatic, "Global" }, normalization],
        Legended[ res, BarLegend[{cf, MinMax[stockValues]}]],

        legendedQ,
        Legended[ res, BarLegend[cf]],

        True,
        res
      ]
    ];


(**************************************************************)
(* MakeVertexShapeFunction                                    *)
(**************************************************************)

Clear[MakeVertexShapeFunction];

SetAttributes[MakeVertexShapeFunction, HoldAll];

MakeVertexShapeFunction[vfName_Symbol, stockArg_Symbol, timeArg_, aSolArg_, maxPopulationArg_, colorScheme_, factorArg_] :=
    With[{vf = vfName, stock = stockArg, aSol = HoldForm[aSolArg],
      time = timeArg, maxPopulation = maxPopulationArg,
      factor = factorArg, cf = colorScheme},
      vf[{xc_, yc_}, name_, {w_, h_}] := {
        ColorData[cf, "ColorFunction"][Rescale[aSol[[1]][stock[name]][time], {0, maxPopulation}, {0, 1}]],
        Rectangle[{xc - factor w, yc - factor h}, {xc + factor w, yc + factor h}]
      };
    ];

MakeVertexShapeFunction[___] := $Failed;


(**************************************************************)
(* ConvertSolutions                                           *)
(**************************************************************)

Clear[ConvertSolutions];

ConvertSolutions::"ntype" = "Unknown conversion type.";

ConvertSolutions[ aStockSolutionValues : Association[(_String -> Association[(_ -> _?VectorQ) ..]) ..], type_String : "Dataset" ] :=
    Block[{res},

      res =
          Join @@
              KeyValueMap[
                Function[{k, v},
                  Join @@ KeyValueMap[ Thread[{k, #1, Range[Length[#2]], #2 }]&, v]
                ],
                aStockSolutionValues
              ];

      Which[
        type == "Dataset",
        Dataset[Dataset[res][All, AssociationThread[#, {"Stock", "Node", "Time", "Value"}]& ]],

        type == "Array",
        res,

        True,
        Message[ConvertSolutions::"ntype"];
        $Failed
      ]

    ];


(**************************************************************)
(* CellValues3D                                               *)
(**************************************************************)

Clear[CellValues3D];

CellValues3D[
  aCells_?AssociationQ,
  model_?EpidemiologyFullModelQ,
  focusStocksArg_ : (_String | {_String ..}),
  aSolHexGermany_?AssociationQ, maxTime_?NumberQ,
  normalizeQ : (True | False)] :=

    Block[{focusStocks = Flatten[{focusStocksArg}], aSolVertexVals,

      aSolVertexVals2, aSolVertexVals3},
      aSolVertexVals = EvaluateSolutionsOverGraphVertexes[model, focusStocks, aSolHexGermany, {0, maxTime}];

      If[normalizeQ,
        aSolVertexVals2 = Map[# / Max[#] &, aSolVertexVals],
        aSolVertexVals2 = aSolVertexVals
      ];

      aSolVertexVals3 = Association[KeyValueMap[Function[{k, v}, k -> Map[Append[aCells[k]["Center"], #] &, v]], aSolVertexVals2]]
    ];


(**************************************************************)
(* MultiSiteModelStocksPlot                                   *)
(**************************************************************)

Clear[MultiSiteModelStocksPlot];

SyntaxInformation[MultiSiteModelStocksPlot] = { "ArgumentsPattern" -> { _, _, _, _, OptionsPattern[] } };

MultiSiteModelStocksPlot::"nargs" = "The first argument is expected to be an epidemiology model. \
The second argument is expected to be a stock description, a list of stock descriptions, or All. \
The third argument is expected to be an association of multi-site solutions. \
The fourth argument is expected to be a number.";

Options[MultiSiteModelStocksPlot] = Join[ {"FocusTime" -> None}, Options[ListLinePlot] ];

MultiSiteModelStocksPlot[
  model_?EpidemiologyModelQ,
  focusStocksArg : ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ),
  aSol_?AssociationQ,
  maxTimeArg : (Automatic | _?NumericQ),
  opts : OptionsPattern[] ] :=

    Block[{focusStocks = Flatten[{focusStocksArg}], maxTime = maxTimeArg, focusTime, a3DVals, a2DVals, epilog = {} },

      focusTime = OptionValue[ MultiSiteModelStocksPlot, "FocusTime"];

      (*
            stockRules = Normal[model["Stocks"]];
            stockRules[[All, 1]] = stockRules[[All, 1]] /. {x_Symbol[id_][v_Symbol] :> x[id]["t"], x_Symbol[v_Symbol] :> x["t"]};
      *)

      Which[
        TrueQ[focusStocksArg === All],
        focusStocks = Union[ Values[model["Stocks"]] ],

        MatchQ[focusStocks, {_StringExpression..}],
        focusStocks = Flatten[ StringCases[ Union[ Values[model["Stocks"]] ], #]& /@ focusStocks ]
      ];

      a3DVals = Association @ Map[ # -> EvaluateSolutionsByModelIDs[model, #, aSol, {0, maxTime}]&, focusStocks];

      a2DVals = Total[Values[#]] & /@ a3DVals;

      If[ NumericQ[focusTime],
        epilog = {Red, Dashed, Line[{{focusTime, -0.1 * Max[a2DVals]}, {focusTime, 1.3 * Max[a2DVals]}}]}
      ];

      If[ TrueQ[maxTime === Automatic],
        maxTime = Max[Cases[aSol, _InterpolatingFunction, Infinity][[1]]["Domain"]]
      ];

      ListLinePlot[
        a2DVals,
        PlotLabel -> "Aggregated over all sites",
        PlotLegends -> Keys[a2DVals],
        Epilog -> epilog, FilterRules[{opts}, Options[ListLinePlot]],
        PlotRange -> All, PlotTheme -> "Detailed"
      ]
    ];

MultiSiteModelStocksPlot[___] :=
    Block[{},
      Message[MultiSiteModelStocksPlot::"nargs"];
      $Failed
    ];

(**************************************************************)
(* PopulationStockPlots                                       *)
(**************************************************************)

Clear[PopulationStockPlots];

PopulationStockPlots::"nst" = "At least one of the specified stocks is not known.";

Options[PopulationStockPlots] = Options[EvaluateSolutionsByModelIDs];

PopulationStockPlots[grHexagonCellsDummy_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stocksArg : (_String | {_String ..}), maxTime_?NumericQ, opts : OptionsPattern[]] :=
    PopulationStockPlots[modelMultiSite, aSolMultiSite, stocksArg, maxTime, opts];

PopulationStockPlots[modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stocksArg : (_String | {_String ..}), maxTime_?NumericQ, opts : OptionsPattern[]] :=
    Block[{lsLocalOpts = {PlotTheme -> "Detailed", PlotRange -> All, ImageSize -> Medium}, stocks = Flatten[{stocksArg}], vals},

      lsLocalOpts = Join[{opts}, lsLocalOpts];

      If[Apply[And, MemberQ[Union[Values[modelMultiSite["Stocks"]]], #] & /@ stocks],

        vals = Total@Map[Total@Values[EvaluateSolutionsByModelIDs[modelMultiSite, #, aSolMultiSite, {1, maxTime, 1}]] &, stocks];

        Row[{
          ListLinePlot[
            vals,
            lsLocalOpts,
            PlotLabel -> StringRiffle[stocks, " + "]
          ],
          Spacer[10],
          ListLinePlot[
            vals / Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, "Total Population", aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> Row[{"Ratio of", Spacer[2], StringRiffle[stocks, " + "], Spacer[2], "with Total Population"}]
          ]
        }],
        (*ELSE*)
        Message[PopulationStockPlots::"nst"];
        $Failed
      ]
    ];

PopulationStockPlots[___] := $Failed;

(**************************************************************)
(* EconomicsStockPlots                                        *)
(**************************************************************)

Clear[EconomicsStockPlots];

EconomicsStockPlots::"nst" = "The specified stock is not known.";

Options[EconomicsStockPlots] = Options[EvaluateSolutionsByModelIDs];

EconomicsStockPlots[grHexagonCellsDummmy_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stock_String, maxTime_?NumericQ, opts : OptionsPattern[] ] :=
    EconomicsStockPlots[modelMultiSite, aSolMultiSite, stock, maxTime, opts];

EconomicsStockPlots[modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stock_String, maxTime_?NumericQ, opts : OptionsPattern[] ] :=
    Block[{lsLocalOpts = {PlotTheme -> "Detailed", PlotRange -> All, ImageSize -> Medium}},

      lsLocalOpts = Join[{opts}, lsLocalOpts];

      If[ MemberQ[Union[Values[modelMultiSite["Stocks"]]], stock],
        Row[{
          ListLinePlot[
            Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, stock, aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> stock
          ],
          Spacer[10],
          ListLinePlot[
            Differences@Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, stock, aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> Row[{"\[CapitalDelta]", Spacer[1], stock}]
          ]
        }],
        (* ELSE *)
        Message[EconomicsStockPlots::"nst"];
        $Failed
      ]

    ];

EconomicsStockPlots[___] := $Failed;


(**************************************************************)
(* SiteIndexSolutionsPlot                                     *)
(**************************************************************)

Clear[SiteIndexSolutionsPlot];

SiteIndexSolutionsPlot::"nst" = "No model stocks are found with the specification given as the third argument.";

Options[SiteIndexSolutionsPlot] = Options[Plot];

SiteIndexSolutionsPlot[
  siteIndex_Integer,
  modelMultiSite_?EpidemiologyModelQ,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSolMultiSite_?AssociationQ,
  maxTimeArg : (Automatic | _?NumberQ),
  opts : OptionsPattern[]] :=

    Block[{maxTime = maxTimeArg, aSol, stockSymbols},

      stockSymbols = Union @ Flatten @ Map[ Cases[GetStocks[modelMultiSite, #], p_[id_] :> p]&, Flatten[{stockNames}] ];

      If[ Length[stockSymbols] == 0,
        Message[SiteIndexSolutionsPlot::"nst"];
        Return[$Failed]
      ];

      aSol = KeySelect[KeyTake[aSolMultiSite, stockSymbols], #[[1]] == siteIndex &];

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      Plot[
        Evaluate[Map[#[t] &, Values[aSol]]], {t, 0, maxTime},
        opts,
        PlotRange -> All, PlotTheme -> "Detailed",
        PlotLegends -> Keys[aSol], ImageSize -> Medium]
    ];


(**************************************************************)
(* Add time axis                                              *)
(**************************************************************)

Clear[ToTimeSeries];

ToTimeSeries::"nargs" = "The first argument is expected to be a numeric vector or a list or a association \
of numeric vectors. The second argument is expected to be a date specification, a number, or a date object.";

ToTimeSeries[ data_?VectorQ, startDate : (_List | _DateObject | _?NumberQ )] :=
    Block[{},
      TimeSeries @ Transpose[{DatePlus[startDate, #] & /@ Range[0, Length[data] - 1], data}]
    ] /; VectorQ[data, NumericQ];

ToTimeSeries[ data : Association[ (_ -> _?VectorQ) ..], startDate : (_List | _DateObject | _?NumberQ )] :=
    Map[ ToTimeSeries[#, startDate]&, data ];

ToTimeSeries[ data : List[ (_ -> _?VectorQ) ..], startDate : (_List | _DateObject | _?NumberQ )] :=
    Map[ #[[1]] -> ToTimeSeries[#[[2]], startDate] &, data ];

ToTimeSeries[___] :=
    Block[{},
      Message[ToTimeSeries::"nargs"];
      $Failed
    ];


(**************************************************************)
(* ToPrefixGroupsSolutions                                    *)
(**************************************************************)

Clear[ToPrefixGroupsSolutions];

ToPrefixGroupsSolutions[model_?EpidemiologyModelQ, aSolVals : Association[ (_ -> {_?NumericQ ..}) .. ] ] :=
    Block[{aSolStocks, aSolVals2},

      aSolStocks =
          GroupBy[
            Normal[model["Stocks"]],
            StringCases[SymbolName[Head[#[[1]]]], StartOfString ~~ x : ( LetterCharacter ..) :> x][[1]] &,
            StringRiffle[Fold[LongestCommonSubsequence, First[#], Rest[#]] & @ StringSplit[#[[All, 2]]], " "] &
          ];

      aSolVals2 =
          GroupBy[
            Normal[aSolVals],
            StringCases[SymbolName[#[[1]]], StartOfString ~~ x : ( LetterCharacter ..) :> x][[1]] &,
            Total[#[[All, 2]]] &
          ];

      KeyMap[StringRiffle[{#, # /. aSolStocks}, ", "] &, aSolVals2]
    ];


(**************************************************************)
(* PrefixGroupsSolutionsListPlot                              *)
(**************************************************************)

Clear[PrefixGroupsSolutionsListPlot];

PrefixGroupsSolutionsListPlot::"nargs" =
    "The expected signatures are \
PrefixGroupsSolutionsListPlot[model_?EpidemiologyModelQ, aSolVals : Association[ (_ -> {_?NumericQ ..}) .. ], opts : OptionsPattern[]] \
or \
PrefixGroupsSolutionsListPlot[ aSolCurvesArg : Association[ (_ -> (_?VectorQ | _?MatrixQ)) .. ], opts : OptionsPattern[]] .";

SyntaxInformation[PrefixGroupsSolutionsListPlot] = { "ArgumentsPattern" -> {_, _., OptionsPattern[]} };

Options[PrefixGroupsSolutionsListPlot] =
    Join[
      { "DateListPlot" -> False, "StartDate" -> Automatic },
      Union @ Join[ Options[ListPlot], Options[DateListPlot] ]
    ];

PrefixGroupsSolutionsListPlot[model_?EpidemiologyModelQ, aSolVals : Association[ (_ -> {_?NumericQ ..}) .. ], opts : OptionsPattern[]] :=
    Block[{aSolCurves},
      aSolCurves = ToPrefixGroupsSolutions[ model, aSolVals ];

      PrefixGroupsSolutionsListPlot[aSolCurves, opts]
    ];

PrefixGroupsSolutionsListPlot[ aSolCurvesArg : Association[ (_ -> (_?VectorQ | _?MatrixQ)) .. ], opts : OptionsPattern[]] :=
    Block[{ aSolCurves = aSolCurvesArg, dateListPlotQ, startDate, listPlotFunc = ListPlot, listPlotFuncOpts},

      dateListPlotQ = TrueQ[OptionValue[PrefixGroupsSolutionsListPlot, "DateListPlot"]];

      startDate = OptionValue[PrefixGroupsSolutionsListPlot, "StartDate"];
      If[ TrueQ[startDate === Automatic], startDate = AbsoluteTime @ Take[Date[], 3] ];

      If[ dateListPlotQ, listPlotFunc = DateListPlot ];

      listPlotFuncOpts = FilterRules[{opts}, Options[listPlotFunc]];

      If[ dateListPlotQ && MatchQ[aSolCurves, Association[(_ -> _?VectorQ) ..] ],
        aSolCurves = Map[Transpose[{DatePlus[startDate, #] & /@ Range[0, Length[#] - 1], #}] &, aSolCurves]
      ];

      listPlotFunc[
        Association @ KeyValueMap[#1 -> Tooltip[#2, #1, FilterRules[{opts}, Options[Tooltip]] ] &, aSolCurves],
        listPlotFuncOpts,
        PlotRange -> All, PlotTheme -> "Detailed",
        PlotLegends -> Keys[aSolCurves], ImageSize -> Medium
      ]
    ];

PrefixGroupsSolutionsListPlot[___] :=
    Block[{},
      Message[PrefixGroupsSolutionsListPlot::"nargs"];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]