(* :Title: EpidemiologyModelModifications *)
(* :Context: EpidemiologyModelModifications` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-08 *)

(* :Package Version: 0.3 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["AntonAntonov`EpidemiologicalModeling`ModelModifications`"];
(* Exported symbols added here with SymbolName::usage *)


Begin["`Private`"];

Needs["AntonAntonov`EpidemiologicalModeling`"];

(***********************************************************)
(* AddModelIdentifier                                      *)
(***********************************************************)

Clear[AddModelIdentifier];

AddModelIdentifier[ model_?EpidemiologyModelQ, id_ ] :=
    Block[{modelSymbols, rules},
      modelSymbols = Union @ Cases[ Normal /@ Values[ KeyTake[model, {"Stocks", "Rates"}]], HoldPattern[ (x_Symbol | x_[args__]) -> descr_String ] :> x , Infinity ];

      rules = {
        (x_Symbol[args__] /; MemberQ[modelSymbols, x]) :> x[id][args],
        Derivative[d_][x_Symbol][args__] :> Derivative[d][x[id]][args],
        (x_Symbol /; MemberQ[modelSymbols, x]) :> x[id]
      };

      Association @ KeyValueMap[ #1 -> If[ AssociationQ[#2], Association[ Normal[#2] /. rules], #2 /. rules ] &, model ]
    ];


(***********************************************************)
(* JoinModels                                              *)
(***********************************************************)

Clear[JoinModels];

JoinModels::"nargs" = "The arguments are expected to be valid model associations.";

JoinModels[ m1_?EpidemiologyModelQ, args__ ] :=
    JoinModels[ {m1, args} ];

JoinModels[ models : {_?EpidemiologyModelQ ..} ] :=
    MapThread[Join, models];

JoinModels[___] :=
    Block[{},
      Message[JoinModels::"nargs"];
      $Failed
    ];


(***********************************************************)
(* MakeCoreMultiCellModel                                  *)
(***********************************************************)

Clear[MakeCoreMultiSiteModel];

MakeCoreMultiSiteModel::"nargs" = "The first argument is expected to be a model association or a model making function. \
The second argument is expected to be an integer or a list of ID's. \
If the first argument is model making function the third argument is expected to be a time variable, \
and the fourth (optional) argument is expected to be a context.";

MakeCoreMultiSiteModel[model : (_Symbol | _?EpidemiologyModelQ), n_Integer, args___] :=
    MakeCoreMultiSiteModel[model, Range[n], args];

MakeCoreMultiSiteModel[model_Symbol, cellIDs_List, t_Symbol, context_ : "Global`"] :=
    MapThread[Join, Map[AddModelIdentifier[model[t, context], #] &, cellIDs]];

MakeCoreMultiSiteModel[model_?EpidemiologyModelQ, cellIDs_List, args___] :=
    MapThread[Join, Map[AddModelIdentifier[model, #] &, cellIDs]];

MakeCoreMultiSiteModel[___] :=
    Block[{},
      Message[MakeCoreMultiSiteModel::"nargs"];
      $Failed
    ];

(***********************************************************)
(* MakeMigrationTerms                                      *)
(***********************************************************)

Clear[MakeMigrationTerms];

SyntaxInformation[MakeMigrationTerms] = { "ArgumentsPattern" -> { _, _, _, _., OptionsPattern[] } };

Options[MakeMigrationTerms] = {"MinGuard" -> True};

MakeMigrationTerms::"nargs" = "The first argument is expected to be a matrix. \
If there is no fourth argument then the second and third argument are expected to be lists of stock functions. \
If a fourth argument is given then the second and third arguments are expected to be lists of stock symbols.";

MakeMigrationTerms[mat_?MatrixQ, TPs_List, Ps_List, t_Symbol, opts : OptionsPattern[]] :=
    MakeMigrationTerms[mat, Through[TPs, t], Through[Ps[t]], opts ];

MakeMigrationTerms[mat_?MatrixQ, TPs_List, Ps_List, opts : OptionsPattern[]] :=
    Block[{n = Dimensions[mat][[2]], res, minGuardFunc = Min},

      If[ !TrueQ[OptionValue[MakeMigrationTerms, "MinGuard"]],
        minGuardFunc = #1&
      ];

      res =
          Total /@
              Association[
                Table[
                  Ps[[i]] ->
                      Table[
                        Which[

                          TrueQ[mat[[j, i]] == 0] && TrueQ[mat[[i, j]] == 0],
                          0,

                          TrueQ[mat[[j, i]] == 0],
                          - minGuardFunc[ Ps[[i]] / TPs[[i]] * mat[[i, j]], Ps[[i]] ],

                          TrueQ[mat[[i, j]] == 0],
                          minGuardFunc[ Ps[[j]] / TPs[[j]] * mat[[j, i]], Ps[[j]] ],

                          True,
                          minGuardFunc[ Ps[[j]] / TPs[[j]] * mat[[j, i]], Ps[[j]] ] - minGuardFunc[ Ps[[i]] / TPs[[i]] * mat[[i, j]], Ps[[i]] ]
                        ],
                        {j, n}],
                  {i, n}]
              ];

      res = AssociationThread[ Keys[res] /. p_[id_][__] :> p[id], Values[res]];

      res

    ] /; TrueQ[Head[mat] =!= SparseArray] && Dimensions[mat][[1]] == Dimensions[mat][[2]] == Length[TPs] == Length[Ps];

MakeMigrationTerms[mat_SparseArray, TPs_List, Ps_List, opts : OptionsPattern[]] :=
    Block[{res, lsRules, aRules, aGroups1, aGroups2, aGroups, NewTermCoeff, minGuardFunc = Min},

      If[ !TrueQ[OptionValue[MakeMigrationTerms, "MinGuard"]],
        minGuardFunc = #1&
      ];

      lsRules = Most @ ArrayRules[mat];
      aRules = Association @ lsRules;

      aGroups1 = GroupBy[lsRules, #[[1, 1]]&, #[[All, 1, 2]]& ];
      aGroups2 = GroupBy[lsRules, #[[1, 2]]&, #[[All, 1, 1]]& ];
      aGroups = Merge[ {aGroups1, aGroups2}, Union[Join @@ ##] & ];

      NewTermCoeff[ i_, j_ ] :=
          Block[{mij = Lookup[aRules, Key@{i, j}, 0], mji = Lookup[aRules, Key@{j, i}, 0]},
            Which[

              TrueQ[mji == 0] && TrueQ[mij == 0],
              0,

              TrueQ[mji == 0],
              - minGuardFunc[ Ps[[i]] / TPs[[i]] * mij, Ps[[i]] ],

              TrueQ[mij == 0],
              minGuardFunc[ Ps[[j]] / TPs[[j]] * mji, Ps[[j]] ],

              True,
              minGuardFunc[ Ps[[j]] / TPs[[j]] * mji, Ps[[j]] ] - minGuardFunc[ Ps[[i]] / TPs[[i]] * mij, Ps[[i]] ]
            ]
          ];

      res = Total /@ Association[ KeyValueMap[ Function[{k, v}, Ps[[k]] -> Map[ NewTermCoeff[k, #]&, v]], aGroups ] ];

      res = AssociationThread[ Keys[res] /. p_[id_][__] :> p[id], Values[res]];

      res

    ] /; MatrixQ[mat] && Dimensions[mat][[1]] == Dimensions[mat][[2]] == Length[TPs] == Length[Ps];

MakeMigrationTerms[___] :=
    Block[{},
      Message[ MakeMigrationTerms::"nargs" ];
      $Failed
    ];


(***********************************************************)
(* MakeAgeGroupMortalityTerms                              *)
(***********************************************************)

Clear[MakeAgeGroupMortalityTerms];

Options[MakeAgeGroupMortalityTerms] = {"AgeGroupNames" -> Automatic};

MakeAgeGroupMortalityTerms::"nargs" = "The first argument is expected to be a vector. \
If there is no third argument then the second argument is expected to be a list \
of stock functions. If a third argument is given then the second  \
arguments is expected to a stock symbol.";

MakeAgeGroupMortalityTerms[vecMortality_?VectorQ, SP_Symbol, t_Symbol, opts : OptionsPattern[]] :=
    Block[{agn, SPs},
      agn = OptionValue[MakeAgeGroupMortalityTerms, "AgeGroupNames"];

      If[TrueQ[agn === Automatic],
        agn = Range[Dimensions[vecMortality][[1]]]
      ];

      SPs = SP /@ agn;
      MakeAgeGroupMortalityTerms[vecMortality, Through[SPs[t]], opts]
    ];

MakeAgeGroupMortalityTerms[vecMortality_?VectorQ, SPs_List, opts : OptionsPattern[]] :=
    Block[{},
      AssociationThread[SPs -> Thread[-SPs * vecMortality]]
    ] /; Length[vecMortality] == Length[SPs];

MakeAgeGroupMortalityTerms[___] :=
    Block[{},
      Message[MakeAgeGroupMortalityTerms::"nargs"];
      $Failed
    ];


(***********************************************************)
(* MakeAgeGroupMixingTerms                                 *)
(***********************************************************)

Clear[MakeAgeGroupMixingTerms];

Options[MakeAgeGroupMixingTerms] = {"AgeGroupNames" -> Automatic};

MakeAgeGroupMixingTerms::"nargs" = "The first argument is expected to be a symmetric matrix. \
If there is no fourth argument then the second and third argument are expected to be lists \
of stock functions. If a fourth argument is given then the second and third \
arguments are expected to be lists of stock symbols.";

MakeAgeGroupMixingTerms[matMixing_SparseArray, matContactRates_SparseArray, args__] :=
    MakeAgeGroupMixingTerms[Normal[matMixing], Normal[matContactRates], args] /; MatrixQ[matMixing] && MatrixQ[matContactRates];

MakeAgeGroupMixingTerms[matMixing_?MatrixQ, matContactRates_?MatrixQ, TP_Symbol, SP_Symbol, IP_Symbol, t_Symbol, opts : OptionsPattern[]] :=
    Block[{agn, SPs, IPs},

      agn = OptionValue[MakeAgeGroupMixingTerms, "AgeGroupNames"];

      If[TrueQ[agn === Automatic],
        agn = Range[Dimensions[matMixing][[1]]]
      ];

      SPs = SP /@ agn;
      IPs = IP /@ agn;

      MakeAgeGroupMixingTerms[matMixing, matContactRates, TP[t], Through[SPs[t]], Through[IPs[t]], opts]
    ];

MakeAgeGroupMixingTerms[matMixing_?MatrixQ, matContactRates_?MatrixQ, TP_Symbol, SPs_List, IPs_List, t_Symbol] :=
    MakeAgeGroupMixingTerms[matMixing, matContactRates, TP[t], Through[SPs[t]], Through[IPs[t]]];

MakeAgeGroupMixingTerms[matMixing_?MatrixQ, matContactRates_?MatrixQ, TP_, SPs_List, IPs_List, opts : OptionsPattern[]] :=
    Block[{n = Dimensions[matMixing][[2]], res},

      res =
          Total /@ Association[
            Table[SPs[[i]] ->
                Table[
                  Which[
                    TrueQ[matMixing[[j, i]] == 0] && TrueQ[matMixing[[i, j]] == 0], 0,

                    True,
                    -((SPs[[i]] IPs[[j]] matMixing[[i, j]] matContactRates[[i, j]]) / TP)
                  ], {j, n}], {i, n}]
          ];

      res

    ] /; (Dimensions[matMixing] == Dimensions[matContactRates] &&
        SymmetricMatrixQ[matMixing] &&
        Dimensions[matMixing][[1]] == Length[SPs] == Length[IPs]);

MakeAgeGroupMixingTerms[___] :=
    Block[{},
      Message[MakeAgeGroupMixingTerms::"nargs"];
      $Failed
    ];


(***********************************************************)
(* Locate equation                                         *)
(***********************************************************)

Clear[EquationPosition];

EquationPosition[model_Association, lhsSpec_] := EquationPosition[model["Equations"], lhsSpec];

EquationPosition[equations : {_Equal ..}, lhsSpec_] :=
    Block[{pos},
      pos = Flatten[Position[equations[[All, 1]], lhsSpec]];
      If[Length[pos] == 0, $Failed, First[pos]]
    ];

EquationPosition[___] := $Failed;


(***********************************************************)
(* Add terms to equation                                   *)
(***********************************************************)

Clear[AddTermsToEquations];

AddTermsToEquations[equations : {_Equal ..}, lhsSpec_ -> terms_] :=
    Block[{pos = EquationPosition[equations, lhsSpec]},

      If[TrueQ[pos === $Failed], Return[$Failed]];

      ReplacePart[equations, pos -> equations[[pos, 1]] == equations[[pos, 2]] + terms]
    ];

AddTermsToEquations[equations : {_Equal ..}, newTerms_Association] :=
    Fold[AddTermsToEquations, equations, Normal@newTerms];

AddTermsToEquations[___] := $Failed;


(***********************************************************)
(* Get model entity symbols                                *)
(***********************************************************)

Clear[RetrieveModelEntities];

SyntaxInformation[RetrieveModelEntities] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[RetrieveModelEntities] = Options[StringMatchQ];

RetrieveModelEntities[model_Association, key_, opts : OptionsPattern[]] :=
    RetrieveModelEntities[model, key, __ ~~ __, opts];

RetrieveModelEntities[model_Association, key_, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    Keys[Select[model[key], StringMatchQ[#, descr, opts]&]];

RetrieveModelEntities[model_Association, key_, descr : (_Symbol | {_Symbol..}), opts : OptionsPattern[] ] :=
    Select[ Keys[model[key]], MemberQ[Flatten[{descr}], #] || MemberQ[Flatten[{descr}], Head[#]]& ];


Clear[RetrieveModelEntitySymbols];

SyntaxInformation[RetrieveModelEntitySymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[RetrieveModelEntitySymbols] = Options[RetrieveModelEntities];

RetrieveModelEntitySymbols[model_Association, key_, opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[model, key, __ ~~ __, opts];

RetrieveModelEntitySymbols[model_Association, key_, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    Join[
      Cases[RetrieveModelEntities[model, key, descr, opts], p_Symbol[id_][_] :> p[id] ],
      Cases[RetrieveModelEntities[model, key, descr, opts], p_Symbol[_] :> p ]
    ];

RetrieveModelEntitySymbols[model_Association, key_, dSym_Symbol, opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[model, key, {dSym}, opts];

RetrieveModelEntitySymbols[model_Association, key_, dSym : {_Symbol..}, opts : OptionsPattern[] ] :=
    Join[
      Cases[RetrieveModelEntities[model, key, dSym, opts], ( p_Symbol[id_][_] /; MemberQ[dSym, p] ) :> p[id] ],
      Cases[RetrieveModelEntities[model, key, dSym, opts], ( p_Symbol[_] /; MemberQ[dSym, p] ) :> p ]
    ];


(***********************************************************)
(* Get stocks symbols                                      *)
(***********************************************************)

Clear[GetStocks];

SyntaxInformation[GetStocks] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[GetStocks] = Options[StringMatchQ];

GetStocks[ model_?EpidemiologyModelQ, opts : OptionsPattern[] ] := GetStocks[ model, __ ~~ __, opts ];

GetStocks[ model_?EpidemiologyModelQ, descr : (_String | _StringExpression | _Symbol | {_Symbol..}), opts : OptionsPattern[] ] :=
    RetrieveModelEntities[ model, "Stocks", descr, opts];

GetStocks[ model_?EpidemiologyModelQ, descr : ( {_String..} | {_StringExpression..} ), opts : OptionsPattern[] ] :=
    DeleteDuplicates[ Join @@ Map[ RetrieveModelEntities[ model, "Stocks", #, opts]&, descr ] ];

Clear[GetStockSymbols];

SyntaxInformation[GetStockSymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

GetStockSymbols[ model_?EpidemiologyModelQ, opts : OptionsPattern[] ] := GetStockSymbols[ model, __ ~~ __, opts ];

GetStockSymbols[ model_?EpidemiologyModelQ, descr : (_String | _StringExpression | _Symbol | {_Symbol..}), opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[ model, "Stocks", descr, opts];

GetStockSymbols[ model_?EpidemiologyModelQ, descr : ( {_String..} | {_StringExpression..} ), opts : OptionsPattern[] ] :=
    DeleteDuplicates[ Join @@ Map[ RetrieveModelEntitySymbols[ model, "Stocks", #, opts]&, descr ] ];

Clear[GetPopulations, GetPopulationSymbols];

GetPopulations = GetStocks;
GetPopulationSymbols = GetStockSymbols;


(***********************************************************)
(* Get rates symbols                                       *)
(***********************************************************)

Clear[GetRates];

SyntaxInformation[GetRates] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[GetRates] = Options[StringMatchQ];

GetRates[ model_?EpidemiologyModelQ, opts : OptionsPattern[] ] := GetRates[ model, __ ~~ __, opts ];

GetRates[ model_?EpidemiologyModelQ, descr : (_String | _StringExpression | _Symbol | {_Symbol..}), opts : OptionsPattern[] ] :=
    RetrieveModelEntities[ model, "Rates", descr, opts];


Clear[GetRateSymbols];

SyntaxInformation[GetRateSymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

GetRateSymbols[ model_?EpidemiologyModelQ, opts : OptionsPattern[] ] := GetRateSymbols[ model, __ ~~ __, opts ];

GetRateSymbols[ model_?EpidemiologyModelQ, descr : (_String | _StringExpression | _Symbol | {_Symbol..}), opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[ model, "Rates", descr, opts];


(***********************************************************)
(* ToSiteCompartmentsModel                                 *)
(***********************************************************)

(* matMigration - constant matrix *)
(* matMigration - time dependent *)
(* matMigration -  *)

Clear[ToSiteCompartmentsModel];

SyntaxInformation[ToSiteCompartmentsModel] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

ToSiteCompartmentsModel::"nargs" = "The first argument is expected to be a single cell model association. \
The second argument is expected to be a square matrix. \
The third optional argument is expected to be list of ID's with length that corresponds to number of rows of the first argument.";

ToSiteCompartmentsModel::"nmgrpop" = "The values of the option \"MigratingPopulations\" is expected to be
a subset of `1` or one of Automatic, All, or None.";

Options[ToSiteCompartmentsModel] = {
  "MigratingPopulations" -> Automatic,
  "TotalPopulation" -> Automatic,
  "PopulationPattern" -> Automatic,
  "MinGuard" -> True};

ToSiteCompartmentsModel[model_Association, matMigration_?MatrixQ, opts : OptionsPattern[] ] :=
    Block[{ids},
      ids = Range @ Dimensions[matMigration][[1]];
      ToSiteCompartmentsModel[model, matMigration, ids, opts]
    ] /; (Equal @@ Dimensions[matMigration]);

(*ToGeoCompartmentsModel[matMigration_?SSparseMatrixQ] :=*)
(*    Block[{},*)
(*    ];*)

ToSiteCompartmentsModel[model_?EpidemiologyModelQ, matMigration_?MatrixQ, cellIDs_List, opts : OptionsPattern[] ] :=
    Block[{allPops, migrPops, totalPop, popPat, coreModel, eqs, newTerms},

      migrPops = OptionValue[ToSiteCompartmentsModel, "MigratingPopulations"];
      totalPop = OptionValue[ToSiteCompartmentsModel, "TotalPopulation"];
      If[TrueQ[totalPop === Automatic], totalPop = "Total Population"];

      popPat = OptionValue[ToSiteCompartmentsModel, "PopulationPattern"];
      If[TrueQ[popPat === Automatic], popPat = __ ~~ "Population" ~~ EndOfString];

      allPops = Values @ Select[ model["Stocks"], StringMatchQ[#, popPat ]& ];
      allPops = Complement[allPops, {totalPop} ];

      Which[
        TrueQ[migrPops === None],
        migrPops = {},

        TrueQ[migrPops === Automatic],
        migrPops = Complement[ allPops, {"Infected Severely Symptomatic Population"} ],

        TrueQ[migrPops === Automatic] || TrueQ[migrPops === All],
        migrPops = allPops,

        VectorQ[migrPops, StringQ] && Length[ Intersection[allPops, migrPops] ] == Length[ Union[migrPops] ],
        migrPops = Union[migrPops],

        True,
        Message[ToSiteCompartmentsModel::"nmgrpop", ToString[InputForm[allPops]] ];
        Return[$Failed]
      ];

      coreModel = MakeCoreMultiSiteModel[model, cellIDs];

      If[ Length[migrPops] == 0,
        Return[coreModel]
      ];

      eqs = coreModel["Equations"];

      eqs =
          Fold[(
            newTerms =
                MakeMigrationTerms[
                  matMigration,
                  GetStocks[coreModel, totalPop],
                  GetStocks[coreModel, #2],
                  FilterRules[{opts}, Options[MakeMigrationTerms]]
                ];
            AddTermsToEquations[#1, newTerms])&,
            eqs,
            migrPops
          ];

      Join[ coreModel, <| "Equations" -> eqs |> ]

    ] /; Dimensions[matMigration][[1]] == Dimensions[matMigration][[2]] == Length[cellIDs];

ToSiteCompartmentsModel[___] :=
    Block[{},
      Message[ToSiteCompartmentsModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* Initial conditions setter                               *)
(***********************************************************)

Clear[AssignInitialConditions];

AssignInitialConditions::"nargs" = "The first argument is expected to be a model association. \
The second argument is expected to be an associations of initial condition rules.";

AssignInitialConditions::"ninit" = "The model does not have initial conditions.";

AssignInitialConditions[model_?EpidemiologyModelQ, lsInitConds : { _Equal .. } ] :=
    AssignInitialConditions[ model, Association @ ReplaceAll[ lsInitConds, Equal[x_, y_] :> Rule[x, y] ] ];

AssignInitialConditions[model_?EpidemiologyModelQ, aInitConds_Association] :=
    Block[{lsInitConds, pos},

      If[ !KeyExistsQ[model, "InitialConditions"],

        lsInitConds = KeyValueMap[ Equal[#1, #2]&, aInitConds],
        (* ELSE *)

        lsInitConds = model["InitialConditions"];

        lsInitConds =
            Fold[
              Function[{ics, icRule},
                pos = EquationPosition[lsInitConds, icRule[[1]]];
                If[IntegerQ[pos],
                  ReplacePart[ics, pos -> icRule[[1]] == icRule[[2]]],
                  (*ELSE*)
                  ics
                ]
              ],
              lsInitConds,
              Normal[aInitConds]
            ]
      ];

      Join[model, <|"InitialConditions" -> lsInitConds|>]
    ];

AssignInitialConditions[___] :=
    Block[{},
      Message[AssignInitialConditions::"nargs"];
      $Failed
    ];

(*---------------------------------------------------------*)
Clear[SetInitialConditions];
SetInitialConditions = AssignInitialConditions;


(***********************************************************)
(* Rate rules setter                                       *)
(***********************************************************)

Clear[AssignRateRules];

AssignRateRules::"nargs" = "The first argument is expected to be a model association. \
The second argument is expected to be an association of rate rules.";

AssignRateRules::"nrrs" = "The model does not have rate rules.";

AssignRateRules[model_?EpidemiologyModelQ, aRateRules_Association] :=
    Block[{lsRateRules},

      If[ !KeyExistsQ[model, "RateRules"],
        lsRateRules = aRateRules,
        (* ELSE *)
        lsRateRules = Join[ model["RateRules"], aRateRules ];
        (* There is no need to do the following because it is done in ModelNDSolve. *)
        (* lsRateRules = AssociationThread[ Keys[model["RateRules"]], Values[model["RateRules"]] //. aRateRules]; *)
        (* lsRateRules = Join[ lsRateRules, aRateRules ] *)
      ];

      Join[model, <|"RateRules" -> lsRateRules|>]
    ];

AssignRateRules[___] :=
    Block[{},
      Message[AssignRateRules::"nargs"];
      $Failed
    ];

(*---------------------------------------------------------*)
Clear[SetRateRules];
SetRateRules = AssignRateRules;


(***********************************************************)
(* To associations                                         *)
(***********************************************************)

Clear[ToAssociation];

ToAssociation::"nargs" = "The first argument is expected to be a list of equations.";

(*ToAssociation[ eqs:{ _Equal ..} ] := Association@ToRules[And @@ eqs];*)
ToAssociation[ eqs : { _Equal ..} ] := Association[ Rule @@@ eqs ];

ToAssociation[___] :=
    Block[{},
      Message[ToAssociation::"nargs"];
      $Failed
    ];


(***********************************************************)
(* CoerceAnnotatedSymbols                                  *)
(***********************************************************)

Clear[CoerceAnnotatedSymbols];

CoerceAnnotatedSymbols[x : (Subscript[s_Symbol, j_])] := ToExpression[SymbolName[s] <> ToString[j]];
CoerceAnnotatedSymbols[x : (Subscript[s_Symbol, j__])] := ToExpression[SymbolName[s] <> StringRiffle[ToString /@ {j}, ""]];

CoerceAnnotatedSymbols[ model_?EpidemiologyModelQ ] :=
    Block[{aAnnotatedNameToName},
      aAnnotatedNameToName = Association @ Map[# -> CoerceAnnotatedSymbols[#] &, Union @ Join[ Head /@ Keys[model["Stocks"]], Keys[model["RateRules"]], Keys[model["Rates"]] ] ];
      Map[ If[ AssociationQ[#], Association[ Normal[#] /. aAnnotatedNameToName], # /. aAnnotatedNameToName ]&, model ]
    ];

CoerceAnnotatedSymbols[x : (_List | _Association) ] := CoerceAnnotatedSymbols /@ x;

CoerceAnnotatedSymbols[x_] := x;

End[]; (* `Private` *)

EndPackage[]