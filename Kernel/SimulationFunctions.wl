(* :Title: EpidemiologyModelingSimulationFunctions *)
(* :Context: EpidemiologyModelingSimulationFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-24 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["AntonAntonov`EpidemiologicalModeling`SimulationFunctions`"];

Begin["`Private`"];

Needs["AntonAntonov`EpidemiologicalModeling`"];
Needs["AntonAntonov`TileStats`"];

(***********************************************************)
(* ModelNDSolveEquations                                   *)
(***********************************************************)

Clear[ModelNDSolveEquations];

SyntaxInformation[ModelNDSolveEquations] = { "ArgumentsPattern" -> { _, _. } };

ModelNDSolveEquations::"nargs" = "The first argument is expected to be a model.";

ModelNDSolveEquations[ model_?EpidemiologyFullModelQ ] := ModelNDSolveEquations[ model, model["RateRules"] ];

ModelNDSolveEquations[ model_?EpidemiologyFullModelQ, rateRules_Association ] :=
    Block[{lsActualEquations, lsInitialConditions},

      lsInitialConditions =
          Thread[
            Equal[
              model["InitialConditions"][[All, 1]],
              model["InitialConditions"][[All, 2]] //. Join[ Association[ Rule @@@ model["InitialConditions"] ], rateRules]
            ]
          ];

      lsActualEquations =
          Join[
            model["Equations"] //. Join[ Association[ Rule @@@ model["InitialConditions"] ], rateRules],
            lsInitialConditions
          ];

      lsActualEquations
    ];

ModelNDSolveEquations[___] :=
    Block[{},
      Message[ModelNDSolveEquations::"nargs"];
      $Failed
    ];


(***********************************************************)
(* ModelNDSolve                                            *)
(***********************************************************)

Clear[ModelNDSolve];

SyntaxInformation[ModelNDSolve] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

ModelNDSolve::"nargs" = "The first argument is expected to be a model. \
The second argument is expected to be a time range specification: {var, maxTime}, \
where var is a symbol and maxTime is a positive number.";

ModelNDSolve[ model_?EpidemiologyFullModelQ, {var_, maxTime_?NumericQ}, opts : OptionsPattern[] ] :=
    ModelNDSolve[ model, {var, 0, maxTime}, opts];

ModelNDSolve[ model_?EpidemiologyFullModelQ, {var_, 0, maxTime_?NumericQ}, opts : OptionsPattern[] ] :=
    Block[{lsActualEquations},

      lsActualEquations = ModelNDSolveEquations[ model ];

      NDSolve[lsActualEquations, GetStockSymbols[model], {var, 0, maxTime}, FilterRules[{opts}, NDSolve] ]

    ] /; TrueQ[ Head[var] === Symbol] && maxTime > 0;

ModelNDSolve[___] :=
    Block[{},
      Message[ModelNDSolve::"nargs"];
      $Failed
    ];


(***********************************************************)
(* MakePolygonGrid                                         *)
(***********************************************************)

Clear[GridObjectQ];
GridObjectQ[a_] := AssociationQ[a] && Length[ Intersection[ Keys[a], {"Cells", "AdjacencyMatrix", "Range", "CellRadius"} ] ] == 4;

Clear[MakePolygonGrid];

MakePolygonGrid::nargs = "The first argument is expected to be a numerical matrix with two columns (list of 2D coordinates). \
The second argument is expected to be a number (grid cell size).
The third argument is expected to be a list of two numerical pairs (2D range specification).";

MakePolygonGrid::nbf = "The value of the option \"BinningFunction\" is expected to be one of
\"HextileBins\", \"TileBins\", or Automatic.";

Options[MakePolygonGrid] :=
    Join[
      {"RemoveLoneCells" -> False, "BinningFunction" -> Automatic },
      Options[HextileBins],
      Options[NearestNeighborGraph]
    ];

MakePolygonGridDataQ[d_] := MatrixQ[ d, NumberQ ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, Automatic, opts ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, Automatic, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, MinMax /@ Transpose[lsLonLat], opts ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, range : { {_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ} }, opts : OptionsPattern[] ] :=
    Block[{removeLoneCellsQ, binningFunction, aPolygonValues, lsCells, aCells,
      nc, lsDistances, pos, grHexagonCellsNetwork, matHexGrid},

      removeLoneCellsQ = TrueQ[OptionValue[MakePolygonGrid, "RemoveLoneCells"]];

      binningFunction = OptionValue[ MakePolygonGrid, "BinningFunction" ];

      Which[

        MemberQ[ {Automatic, "HextileBins", HextileBins}, binningFunction ],
        binningFunction = HextileBins,

        MemberQ[ {"TileBins", TileBins}, binningFunction ],
        binningFunction = TileBins,

        True,
        Message[MakePolygonGrid::nbf];
        Return[$Failed]
      ];

      aPolygonValues = binningFunction[lsLonLat, cellRadius, range, FilterRules[{opts}, Options[binningFunction]]];

      (* Make cell objects *)
      lsCells = KeyValueMap[<|"Value" -> #2, "Cell" -> #1, "Center" -> Mean[PolygonCoordinates[#1]]|> &, aPolygonValues];
      lsCells = SortBy[lsCells, #["Center"] &];
      aCells = AssociationThread[Range[Length[lsCells]], lsCells];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Create a function to find the nearest cell to a given position *)
      nc = Nearest[Values[aCells] -> Keys[aCells], DistanceFunction -> (EuclideanDistance[#1["Center"], #2["Center"]] &)];

      lsDistances = Select[Flatten@DistanceMatrix[Values[#["Center"] & /@ aCells]], # > 0 &];

      (* Identify outlier(s) and drop them *)
      If[removeLoneCellsQ,
        pos = Select[nc[#, {6, 1.1 * Min[lsDistances] / Cos[\[Pi] / 6.]}] & /@ aCells, Length[#] == 1 &];
        aCells = KeyDrop[aCells, Keys[pos]];
      ];

      (* Reassign cell ID's *)
      aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Make neighbors graph *)
      (* The NN's radius should fit both hex-tiles and tiles. *)
      grHexagonCellsNetwork =
          NearestNeighborGraph[
            Keys[aCells], {7, Min[lsDistances] / Cos[\[Pi] / 6.]},
            DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], aCells[#2]["Center"]] &),
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[NearestNeighborGraph]]
          ];

      (* Make final graph matrix *)
      matHexGrid =
          SparseArray[
            Thread[ Rule[ List @@@ Join[EdgeList[grHexagonCellsNetwork], Reverse /@ EdgeList[grHexagonCellsNetwork]], 1 ] ],
            {Length[aCells], Length[aCells]}
          ];

      (* Result *)
      <| "Cells" -> aCells, "AdjacencyMatrix" -> matHexGrid, "Range" -> range, "CellRadius" -> cellRadius |>

    ];

MakePolygonGrid[___] :=
    Block[{},
      Message[MakePolygonGrid::nargs];
      $Failed
    ];


(***********************************************************)
(* MakeHexagonGrid                                         *)
(***********************************************************)

Clear[MakeHexagonGrid];

Options[MakeHexagonGrid] :=
    Join[
      {"RemoveLoneCells" -> False },
      Options[HextileBins],
      Options[NearestNeighborGraph]
    ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, opts : OptionsPattern[] ] :=
    MakeHexagonGrid[ lsLonLat, cellRadius, Automatic, opts ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, Automatic, opts : OptionsPattern[] ] :=
    MakeHexagonGrid[ lsLonLat, cellRadius, MinMax /@ Transpose[lsLonLat], opts ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, range : { {_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ} }, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, range, "BinningFunction" -> HextileBins, opts];


(***********************************************************)
(* ToGraph                                                 *)
(***********************************************************)

Clear[ToGraph];

Options[ToGraph] = Options[Graph];

ToGraph[ aGrid_?GridObjectQ, opts : OptionsPattern[] ] :=
    AdjacencyGraph[
      aGrid["AdjacencyMatrix"],
      opts,
      DirectedEdges -> True,
      VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aGrid["Cells"]],
      VertexLabels -> Placed[Automatic, Center],
      VertexSize -> 0.6
    ];


(***********************************************************)
(* AggregateForCellIDs                                     *)
(***********************************************************)

Clear[AggregateForCellIDs];

AggregateForCellIDs::"narg" = "The first argument is expected to be a grid object. \
The second argument is expected to be an association with keys that are coordinates and values that are atoms.";

Options[AggregateForCellIDs] = { "AggregationFunction" -> Total };

AggregateForCellIDs[ aGrid_?GridObjectQ, aLonLatPopulation : Association[ ( {_?NumberQ, _?NumberQ} -> _?AtomQ ) .. ], opts : OptionsPattern[] ] :=
    Block[{nc, aDataIDs, aggrFunc},

      aggrFunc = OptionValue[ AggregateForCellIDs, "AggregationFunction" ];

      nc = Nearest[Values[#["Center"] & /@ aGrid["Cells"]] -> Keys[aGrid["Cells"]]];

      aDataIDs = KeyValueMap[ First[nc[#1, 1]] -> #2 &, aLonLatPopulation ];

      GroupBy[ aDataIDs, First, aggrFunc[ #[[All, 2]] ]& ]
    ];

AggregateForCellIDs[___] :=
    Block[{},
      Message[AggregateForCellIDs::"narg"];
      $Failed
    ];


(***********************************************************)
(* ApproximateField                                        *)
(***********************************************************)

Clear[ApproximateField];
ApproximateField[
  aStateToValue : Association[ ( _String -> _?NumberQ ) .. ],
  aStateCityToPopulation : Association[ ( { _String, _String} -> _?NumberQ ) .. ],
  opts : OptionsPattern[] ] :=
    Block[{aStatePopulations},

      aStatePopulations = GroupBy[ Normal[aStateCityToPopulation], #[[1, 1]] &, Total[Map[#[[2]]&, #]] &];

      Association @
          KeyValueMap[
            #1 ->
                If[NumberQ @ aStateToValue[#1[[1]]],
                  N[ #2 / aStatePopulations[#1[[1]]] * aStateToValue[#1[[1]]] ],
                  0
                ]&,
            aStateCityToPopulation
          ]
    ];

ApproximateField[
  aStateToValue : Association[ ( _String -> _?NumberQ ) .. ],
  aStateCityToPopulation : Association[ ( { _String, _String} -> _?NumberQ ) .. ],
  aStateCityToCoords : Association[ ( { _String, _String } -> {_?NumberQ, _?NumberQ} ) .. ],
  opts : OptionsPattern[] ] :=
    Block[{aStateCityToValue},

      aStateCityToValue = ApproximateField[ aStateToValue, aStateCityToPopulation, opts];

      Map[ aStateCityToValue[#]&, Association[ Reverse /@ Normal[ aStateCityToCoords ] ] ]
    ];

End[]; (* `Private` *)

EndPackage[]