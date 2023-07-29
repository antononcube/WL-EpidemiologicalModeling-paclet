BeginPackage["AntonAntonov`EpidemiologicalModeling`"];

(*---------------------------------------------------------*)
(* Models                                                  *)
(*---------------------------------------------------------*)

EpidemiologicalModelQ::usage = "Is the argument an association with stocks, rates, and equations?";

EpidemiologyModelQ::usage = "Synonym of EpidemiologicalModelQ.";

EpidemiologicalFullModelQ::usage = "Is the argument an association with \
stocks, rates, equations, initial conditions, and rate rules ?";

EpidemiologyFullModelQ::usage = "Synonym of EpidemiologicalFullModelQ.";

MalariaModel::usage = "MalariaModel[var, con] generates malaria model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SIRModel::usage = "SIRModel[var, con] generates SIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SI2RModel::usage = "SI2RModel[var, con] generates SI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEIRModel::usage = "SEIRModel[var, con] generates SEIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2RModel::usage = "SEI2RModel[var, con] generates SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2HRModel::usage = "SEI2HRModel[var, con] generates hospitalization SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2HREconModel::usage = "SEI2HREconModel[var, con] generates economics SEI2HR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI4RModel::usage = "SEI4RModel[var, con] generates SEI4R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

ModelGridTableForm::usage = "Displays the model legibly.";

(*---------------------------------------------------------*)
(* Model modifications                                     *)
(*---------------------------------------------------------*)

(*AddDeceasedPopulation::usage "AddDeceasedPopulation[model] add a deceased equation and stock to the model argument.";*)

AddModelIdentifier::usage = "AddModelIdentifier[m, id] adds a specified identifier id to all stocks and rates on the model m.";

JoinModels::usage = "JoinModels[m1_Association, ..] or JoinModels[ {_Association ..} ] joins models.";

MakeCoreMultiSiteModel::usage = "MakeCoreMultiSiteModel[coreModel_, n_Integer | ids_List, t_Symbol, context_] \
makes core multi-site model.";

EquationPosition::usage = "EquationPosition[eqs:{_Equal..}, lhs_] finds the element of eqs that has \
the specified left hand side lhs.";

AddTermsToEquations::usage = "AddTermsToEquations[eqs:{_Equal..}, nt_Association] adds the corresponding \
new terms in specified with nt to the right hand side of the equations eqs.";

MakeMigrationTerms::usage = "MakeMigrationTerms[m_?MatrixQ, TPs_List, Ps_List] gives an association with \
the migration terms for the total populations TPs and populations Ps.";

MakeAgeGroupMortalityTerms::usage = "MakeAgeGroupMortalityTerms[m_?VectorQ, SP_List, opts___] gives an association \
of age group mortality terms.";

MakeAgeGroupMixingTerms::usage = "MakeAgeGroupMixingTerms[matMixing_?MatrixQ, matContactRates_?MatrixQ, TP_, SPs_List, IPs_List, opts___] \
gives an association of age groups infection terms.";

GetStocks::usage = "GetStocks[m_Association, d:(_String | _StringExpression)] get stocks \
in the model m that correspond to the descriptions d.";

GetStockSymbols::usage = "GetStockSymbols[m_Association, d:(_String | _StringExpression)] get \
stock symbols in the model m that correspond to the descriptions d.";

GetPopulations::usage = "GetPopulations[m_Association, d:(_String | _StringExpression)] get populations \
in the model m that correspond to the descriptions d. A synonym of GetStocks.";

GetPopulationSymbols::usage = "GetPopulationSymbols[m_Association, d:(_String | _StringExpression)] get \
population symbols in the model m that correspond to the descriptions d. A synonym of GetStockSymbols.";

GetRates::usage = "GetRates[m_Association, d:(_String | _StringExpression)] get rates in the model m \
that correspond to the descriptions d.";

GetRateSymbols::usage = "GetRateSymbols[m_Association, d:(_String | _StringExpression)] get rates symbols in \
the model m that correspond to the descriptions d.";

ToSiteCompartmentsModel::usage = "ToSiteCompartmentsModel[singleCellModel_Association, mat_?MatrixQ, opts___] \
makes a multi-cell model based on singleCellModel using the population migration matrix mat.";

AssignInitialConditions::usage = "AssignInitialConditions[ m_Association, ics_Associations ] changes the initial \
conditions of the model m according to the rules ics.";

SetInitialConditions::usage = "Synonym of AssignInitialConditions.";

AssignRateRules::usage = "AssignRateRules[ m_Association, rrs_Associations ] changes the rate rules \
of the model m according to the rules rrs.";

SetRateRules::usage = "Synonym of AssignRateRules.";

ToAssociation::usage = "ToAssociation[ eqs : { _Equal..} ] converts a list equations into an association.";

CoerceAnnotatedSymbols::usage = "CoerceAnnotatedSymbols[x] coverts subscript symbols to non-subscript ones.";

(*---------------------------------------------------------*)
(* Simulation functions                                    *)
(*---------------------------------------------------------*)

ModelNDSolveEquations::usage = "ModelNDSolveEquations[model_, rateRules_.] combines the model equations,
initial conditions, and rate rules into a list equations to be given to NDSolve. \
If no rate rules are given the model[\"RateRules\"] is used.";

ModelNDSolve::usage = "ModelNDSolve[model, {t, maxTime}, opts] simulates the model from 0 to maxTime using NDSolve";

MakePolygonGrid::usage = "MakePolygonGrid[ coords_List, cellSize_?NumberQ, range : ( Automatic | _?MatrixQ), opts___ ] \
makes a polygonal tiling grid for specified data.";

MakeHexagonGrid::usage = "MakeHexagonGrid[ coords_List, cellRadius_?NumberQ, range : ( Automatic | _?MatrixQ), opts___ ] \
makes a hexagonal tiling grid for specified data. \
Shortcut for MakePolygonGrid[ coords, cellSize, \"BinningFunction\" -> HextileBins ]";

ToGraph::usage = "ToGraph[ grid_Association ] makes a graph for a given grid.";

AggregateForCellIDs::usage = "AggregateForCellIDs[ aGrid, aLonLatVal] aggregated the values of aLonLatVal around
the ID's of aGrid[\"Cells\"].";

GridObjectQ::usage = "GridObjectQ[arg] checks is the argument a grid object";

ApproximateField::usage = "ApproximateField[aStateToValue, aStateCityToPopulation] \
approximates a state-granularity field to a state-city-granularity field proportionally to cities populations. \
ApproximateField[aStateToValue, aStateCityToPopulation, aStateCityToCoords] approximates the field over the \
state-city coordinates.";

(*---------------------------------------------------------*)
(* Visualization functions                                 *)
(*---------------------------------------------------------*)

EvaluateSolutionsByModelIDs::usage = "EvaluateSolutionsOverModelIDs[mdl, sns, aSol, trng] \
evaluates the solutions in aSol for the stock names sns for each ID in the model mdl over the specified time range trng.";

EvaluateSolutionsOverGraphVertexes::usage = "EvaluateSolutionsOverGraphVertexes[gr, mdl, sns, aSol, trng] \
evaluates the solutions in aSol for the stock names sns for each (vertex) ID in the model mdl over the specified time range trng. \
(Legacy function.)";

EvaluateSolutionsOverGraph::usage = "EvaluateSolutionsOverGraph[gr, model, stockNames, aSol, timeRange, opts] \
makes a sequence of graph plots of the graph gr with the graph nodes colored according solution functions aSol.";

MakeVertexShapeFunction::usage = "MakeVertexShapeFunction makes a vertex shape function.";

ConvertSolutions::usage = "ConvertSolutions[ aSolEvals, type] converts an association of solution evaluations into
a dataset.";

MultiSiteModelStocksPlot::usage = "MultiSiteModelStocksPlot[model, focusStocks, aSol, maxTime, opts___] \
plots aggregated by stock solution values for a multi-site model.";

PopulationStockPlots::usage = "PopulationStockPlots[grHexagonCells_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_Association, stocksArg : (_String | {_String ..}), maxTime_?NumberQ, addOpts : OptionsPattern[]]";

EconomicsStockPlots::usage = "EconomicsStockPlots[grHexagonCells_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_Association, stock_String, maxTime_?NumberQ, opts : OptionsPattern[] ]";

SiteIndexSolutionsPlot::usage = "SiteIndexSolutionsPlot[siteIndex_Integer, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, maxTime_?NumberQ, opts : OptionsPattern[]]";

ToTimeSeries::usage = "Makes data into time series.";

ToPrefixGroupsSolutions::usage = "ToPrefixGroupsSolutions[model_?EpidemiologyModelQ, aSolVals_Association].";

PrefixGroupsSolutionsListPlot::usage = "PrefixGroupsSolutionsListPlot[model_?EpidemiologyModelQ, aSolVals_Association, opts : OptionsPattern[] ].";


(*---------------------------------------------------------*)
(* Dependencies                                            *)
(*---------------------------------------------------------*)

PacletInstall["AntonAntonov/TileStats", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`EpidemiologicalModeling`Models`"];
Needs["AntonAntonov`EpidemiologicalModeling`ModelModifications`"];
Needs["AntonAntonov`EpidemiologicalModeling`SimulationFunctions`"];
Needs["AntonAntonov`EpidemiologicalModeling`VisualizationFunctions`"];


End[];
EndPackage[];