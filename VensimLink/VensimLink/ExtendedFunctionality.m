(* Mathematica Package *)

BeginPackage["ExtendedFunctionality`",{"VensimLink`","BSLUtilities`"}]
(* Exported symbols added here with SymbolName::usage *) 

findInGraph::usage = "findInGraph[ modelGraph, vertex ] returns the subgraph that contains all vertices connected by a directed path to the vertex.\n\
findInGraph[ modelGraph, {v1, v2, ...} ] returns the subgraph that contains all vertices connected by a directed path to any of the v1, v2, ..."

findOutGraph::usage = "findOutGraph[ modelGraph, vertex ] returns the subgraph that contains all vertices to which vertex is connected to by a directed path.\n\
findOutGraph[ modelGraph, {v1, v2, ...} ] returns the subgraph that contains all vertices to which any of the v1, v2, ... is connected to by a directed path."

findAggregateGraph::usage = "findAggregateGraph[ modelGraph, {v1, v2, ...} ] returns an aggregated Graph containing only the given vertices. Any two vertices {vi,vj} will be shown as connected if and only if there is a directed path from vi to vj."

getData::usage = "getData[ modelGraph, vertex, varName, runName ] returns the TemporalData object for the given variable. If [PEERS] is added to the varName then Peergroup-Values will be given, in all other cases the base company is returned."

listPropertyValues::usage = "listPropertyValues[ modelGraph, vertex ] returns all Properties of the vertex as a list of tuples {property, propertyValue}."

(* Higher Functionality - customized functions that build upon the DLL functions *)

VensimLoadModel::usage = "\
VensimLoadModel[ modelFileName ] loads the model given by a file-string."

VensimListVariables::usage="\
VensimListVariables[modelName, {VarTypes}] gibt eine Liste der relevanten Modell-Variablen zurueck, d.h. Konstanten, DatenVariabeln, Bestandsgroessen, Hilfsgroessen, InitialGroessen."

VensimModelGraph::usage = "VensimModelGraph[ modelName ] erzeugt ein Graph-Objekt mit saemtlichen relevanten Informationen des Modells."	

VensimStoreRunData::usage = "VensimStoreRunData[ modelGraph, runName] liest fuer die Variablen des Modell-Graphen die jeweiligen Zeitreihen des angegebenen runs ein und legt sie unter den Knoten-Properties Values ab." 

(* output *)
(*bslKennzahlenSystem::usage = "bslKennzahlenSystem[ modelGraph, kennzahlenGraph ] plottet ein Kennzahlensystem auf Basis eines Strukturgraphen und der Daten eines Modell-Graphen."
Options[bslKennzahlenSystem] = {
	
	edgeStyle 			-> Thick,
	runName 			-> "Basislauf.vdf",
	edgeShapeFunction 	-> ({bslGray[],Thick,Line[#1]}&), 
	graphLayout 		-> {"LayeredDigraphEmbedding", "Orientation" -> Bottom},
	imageSize 			-> Large,
	imagePadding 		-> All,
	timeRange 			-> All, (* All or {StartTime,EndTime } *)
	benchmarkFunction 	-> (Median[#]&),
	vertexChartFunction -> ChartDefinitions`bslBenchmarkGauge,
	vertexChartSize 	-> {200,200},
	showPeergroup 		-> True (* Or False to indicate whether Peergroup-Values should be shown *)	
}*)

Begin["`Private`"] (* Begin Private Context *) 

findInGraph[ modelGraph_?GraphQ, vertices_List, opts:OptionsPattern[] ] /; VectorQ[vertices, MemberQ[VertexList@modelGraph, #]& ] := Subgraph[ modelGraph, VertexInComponent[ modelGraph, vertices], opts ]
findInGraph[ modelGraph_?GraphQ, vertex_, opts:OptionsPattern[]] /; MemberQ[VertexList@modelGraph, vertex] := findInGraph[modelGraph, {vertex}, opts]
findInGraph[___] := Return[ Message[findInGraph::funcargs]; $Failed ]

findOutGraph[ modelGraph_?GraphQ, vertices_List, opts:OptionsPattern[] ] /; VectorQ[vertices, MemberQ[VertexList@modelGraph, #]& ] := Subgraph[ modelGraph, VertexOutComponent[ modelGraph, vertices ], opts]
findOutGraph[ modelGraph_?GraphQ, vertex_, opts:OptionsPattern[] ] /; MemberQ[VertexList@modelGraph, vertex] := findOutGraph[ modelGraph, {vertex}, opts ]
findOutGraph[___] := Return[ Message[ findInGraph::funcargs ]; $Failed ]

findAggregateGraph[ modelGraph_?GraphQ, vertices_List, opts:OptionsPattern[] ] /; VectorQ[vertices, MemberQ[VertexList@modelGraph, #]& ] := Module[
	{
		tuples,
		edgeList
	},
	
	(* find all possible combinations vertices x vertices *)
	
	tuples = Tuples[ vertices, 2]//DeleteCases[#,{v_,v_}]&;
	
	(* generate an aggregated edgeList *)
	
	edgeList = 
		If[
			VertexConnectivity[modelGraph, #1, #2] > 0,
			(* then *) #1~DirectedEdge~#2
		]& @@@ tuples//DeleteCases[#,Null]& ;
		
	Graph[ vertices, edgeList, opts ]
]	
findAggregateGraph[___] := Return[ Message[ findAggregateGraph::funcargs]; $Failed ]

getData::notfound = "Data not found in the model graph"
getData[ modelGraph_?GraphQ, vertex_ , varName_String, runName_String ] := Module[
	{
		result
	},
	
	(* Get the appropriate data depending on the varName String *)
	(* if values for the peergroup shall be obtained the varName is to be given *)
	(* with the subscripts given as: [PEERS, sub1, sub2]  *)
	
	Which[ 
		StringFreeQ[ varName, "PEERS" ], result = Cases[ PropertyValue[{modelGraph, vertex}, "Values" ], data_TemporalData /; data["RunName"] == runName, Infinity ],
		True, result = Cases[ PropertyValue[ {modelGraph, vertex}, "PeergroupValues" ], data_TemporalData /; data["RunName"] == runName, Infinity ]
	];
	 
	(* In a later Version the subscript range of the data to be fetched might be specified *)
	
	If[
		result == {},
		Return[ Message[getData::notfound]; $Failed]
	];
	First@Flatten@result
]
getData[___] := Return[ Message[ getData::funcargs ]; $Failed ]

listPropertyValues[ modelGraph_?GraphQ, vertex_ ] /; MemberQ[ VertexList@modelGraph, vertex] := {
	#,
	PropertyValue[{modelGraph, vertex},#]
}& /@ PropertyList[{modelGraph, vertex}]
listPropertyValues[___] /; Message[ listPropertyValues::funcargs ] := "Never happens"

VensimLoadModel::fnf="The model `1` could not be found"
VensimLoadModel[model_String] := Module[
	{
		strCurrentModel,
		strFileName
	},
	
	strCurrentModel = First@VensimGetInfo[5];
	
	If[ strCurrentModel == model, Return["Model loaded."] ]; (* return if model is already loaded *)
		
	If[ 
		Not@FileExistsQ[ model ],
		(* then *) Return[ Message[VensimLoadModel::fnf, model]; $Failed],
		(* else *) strFileName = FindFile@model 
	];	 
	
	If[
		StringFreeQ[strFileName, strCurrentModel] || strCurrentModel == "?",
		(* then *) 
		If[
			Quiet@VensimCommand["SPECIAL>LOADMODEL|" <> strFileName] == 0,				
			(* then *) Return[ Message[VensimLoadModel::fnf, model] ]
		]
	];
	"Model loaded."
]
VensimLoadModel[___] /; Message[VensimLoadModel::funcargs] := "Never happens"

(* Liste der Modellvariablen*)

VensimListVariables::noload = "No model loaded."
Default[VensimListVariables] = {1,2,3,4,5,6} (* Levels, Auxiliary, Data, Initial, Constant, Lookup *)
VensimListVariables[model_String, types_.] /; (VectorQ[types, IntegerQ] && Min[types] >= 1 && Max[types]<= 13) := Module[
	{
		listVarModelSetup = { "SAVEPER","FINAL TIME","INITIAL TIME","TIME STEP","RC START TIME", (s_ /; StringMatchQ[s, StringExpression["#", __, "#"]]) }, (* Simulation Constants and Macros should not be included *)
		listVariables
	},
	If[
		VensimLoadModel[model] != "Model loaded.",
		(* then *) Return[Message[VensimListVariables::noload]]
	];
	listVariables = VensimGetVarNames["*",#]& /@ types//DeleteCases[#,""]&//Union//Flatten;
	DeleteCases[listVariables,Alternatives@@listVarModelSetup] (* returns the result *)
]

VensimListVariables[ model_String, type_Integer] := VensimListVariables[model, {type}]

VensimListVariables[___] /; Message[VensimListVariables::funcargs] := "Never happens"


(* Erzeugen eines Modell-Graphen *)

vertexShape[type_String] := Which[
	type == $variableTypes[[1]] (* levels *) , Graphics[{bslBlue[], Rectangle[]}],
	True,	Automatic (* all other types will be standard, e.g. circles *)
] (* define special shapes according to variable type *)
vertexShape[___] := Return[ Message[vertexShape::funcargs]; $Failed ]

vertexStyle[type_String] := Which[
	type == $variableTypes[[1]] (* levels *) , bslBlue[],
	type == $variableTypes[[3]] (* data *) , bslRed[],
	type == $variableTypes[[4]] (* initial *) , bslRed[],
	type == $variableTypes[[5]] (* constant *) , bslRed[],
	True,	Automatic
] (* define special shapes according to variable type *)
vertexStyle[___] := Return[ Message[ vertexStyle::funcargs ]; $Failed ]

vertexSize[type_String] := Which[
	type == $variableTypes[[1]] (* levels *) , Automatic,
	True,	Automatic
] (* define special shapes according to variable type *)
vertexSize[___] := Return[ Message[ vertexSize::funcargs ]; $Failed ]

(* determine the number of PEERS *)
getNumberOfPeers[___] := Module[
	{
		numPeerSubs,
		numPeers
	},
		numPeerSubs = Quiet@VensimGetVarAttrib["PEERS", 9];
	If[
		MatchQ[ numPeerSubs,{Null}|Null ],
		(* then *) numPeers = 0,
		(* else *) numPeers = Length@VensimGetVarAttrib["PEERS",9] - 1; (* by definition PG0 is the base company and has to be subtracted *)
	];
	numPeers
]

VensimModelGraph::noload= "No model loaded."
VensimModelGraph[ model_String, opts:OptionsPattern[] ]:= Module[
	{
		listVerticeVariableTypes = {1,2,3,4,5,6}, (* Levels, Auxiliaries, DataVars, InitialVars, Constants, Lookups *)
		listVertexVariableTypesStr = $variableTypes[[1;;6]] ,
		listVertexVariableNames,
		listEdgeRules,
		nachfolger,
		modelGraph,
		modelName
	},
	If[
		VensimLoadModel[model] != "Model loaded.",
		(* then *) Return[Message[VensimListVariables::noload]]
	];
	modelName = First@VensimGetInfo[5];
	
	nachfolger = Function[ 
		vorgaenger, 
		DeleteCases[
			VensimGetVarAttrib[vorgaenger,5], 
			vertex_String /; Intersection[ listVertexVariableTypesStr, {VensimGetVarAttrib[vertex, 14]}//Flatten ] == {} (* eliminate those that are not the proper type *) 
		]
	]; (* this gives a list of the vertices of variable types 1,2,3,4,5,6 that lead into the given vertex *)
	listVertexVariableNames = VensimListVariables[model, listVerticeVariableTypes];
	
	(* generate the edge rules connecting the vertices *)
	listEdgeRules = Table[
			(vorgaenger~DirectedEdge~#)& /@ nachfolger[vorgaenger],
			{vorgaenger, listVertexVariableNames}
		]//Flatten//DeleteCases[#, _~DirectedEdge~Null]&;
	(* generate the basic model graph *)
	modelGraph = Graph[
		listVertexVariableNames,
		listEdgeRules	
	];
	
	(* Set global properties for the Graph *)
	
	PropertyValue[modelGraph,"ModelName"] = modelName;
	PropertyValue[modelGraph,"RunNames"] = {};
	
	(* Set properties for the vertices *)
	
	Do[
		modelGraph = Fold[
			SetProperty[{#1, #2}, 
				{
					VertexLabels -> bslText[#2, Background -> White],
					VertexShape -> vertexShape[ $variableTypes[[type]] ],
					VertexStyle -> vertexStyle[ $variableTypes[[type]] ],
					VertexSize -> vertexSize[ $variableTypes[[type]] ],
					"Type" -> First@Quiet@VensimGetVarAttrib[#2, 14],
					"Units" -> First@Quiet@VensimGetVarAttrib[#2, 1],
					"Comment" -> First@Quiet@VensimGetVarAttrib[#2, 2], (* ggf. zu parsen *)
					"Equation" -> First@Quiet@VensimGetVarAttrib[#2, 3],
					"Values" -> {}, (* Initialization to indicate absence of values *)
					"Range" -> ({ Quiet@VensimGetVarAttrib[#2, 11], Quiet@VensimGetVarAttrib[#2, 12] }//Flatten//ToExpression) ,
					"SubscriptRanges" -> Quiet@VensimGetVarAttrib[#2, 8],
					"SubscriptElements" -> Quiet@VensimGetVarAttrib[#2, 9],
					"PeergroupValues" -> {}
				}]&,
			modelGraph,
			VensimListVariables[model,type]
		],
		{type, listVerticeVariableTypes}
	];
	modelGraph		(* returns a model-graph *)
]

VensimModelGraph[___] /; Message[VensimModelGraph::funcargs] := "Never happens"

SetAttributes[VensimStoreRunData, HoldFirst]

VensimStoreRunData[modelGraph_?GraphQ, runName_String] := Module[
	{
		listVertexNames,
		subscriptElements,
		subscriptElementsPG,
		subscriptElementsPeer,
		numPeers,
		baseResults,
		peerResults
	},
	
	(*
		Das Ablegen der Daten erfolgt jeweils in Listen unter "Values" und "PeergroupValues". Im Falle weiterer Subscripts werden die Werte hintereinanderweg abgelegt, wobei
		jeweils RunName und genaue Variablenbezeichnung mit Subscripts als MetaInformation der TemporalData-Objekte abgelegt werden.
	
	*)
	
	If[ Not@FileExistsQ@runName, Return[ Message[VensimStoreRunData::fnf] ]];
	listVertexNames = VertexList@modelGraph;
	numPeers = getNumberOfPeers[];
	Function[
		vertex,
			subscriptElements = PropertyValue[{modelGraph, vertex},"SubscriptElements"];
			Which[
				(* case 1:  there are no subscripts at all *)
				
				MatchQ[ subscriptElements, {Null} | {}| Null],
					(
						PropertyValue[{modelGraph, vertex},"Values"] = AppendTo[ PropertyValue[{modelGraph,vertex},"Values"], VensimGetData[runName, vertex , "Time"] ];					
					),
				
				(* case 2: there are subscripts, but no PEERS *)
				numPeers == 0,
					(
						baseResults= VensimGetData[runName, vertex <> #, "Time" ]& /@ subscriptElements;
						PropertyValue[{modelGraph,vertex},"Values"] = AppendTo[ PropertyValue[{modelGraph,vertex},"Values"], baseResults ];
					),
				
				(* general case:  there are subscripts and at least one peer *)
				True,
					(
						subscriptElementsPG = Cases[ subscriptElements, sub_ /; StringFreeQ[sub,"PG0"]]; (* all elements without "PG0" *)
						subscriptElements = Complement[ subscriptElements, subscriptElementsPG]; (* thus for [PG0] the complement lists all relevant elements *)
						
						(* base company *)
						baseResults= VensimGetData[runName, vertex<>#, "Time"]& /@ subscriptElements;
						PropertyValue[{modelGraph,vertex},"Values"] = AppendTo[ PropertyValue[{modelGraph,vertex},"Values"], baseResults];
						
						(* peergroup *)
						peerResults = 
							Table[
								(* Die relevanten Subscript Elemente sind das Komplement aller Peergroup-Elemente und derjenigen Teilmenge daraus, die
							   	nicht den jeweiligen Peer enthaelt *)
								subscriptElementsPeer = Cases[
									subscriptElementsPG, sub_ 
									/; 	StringMatchQ[sub, "[PG"<>ToString[peer]<>"]"]
										||	StringMatchQ[ sub, ___~~"PG"<>ToString[peer]<>","~~___]
								];
								VensimGetData[ runName, vertex<># , "Time" ]& /@ subscriptElementsPeer,
								{peer, numPeers}
							];
						(* nunmehr muessen die Werte zusammengefasst werten *) 
						peerResults = 
							Table[
								TemporalData[
									peerResults[[All,sub]],
									MetaInformation-> {
										"VarName" -> StringReplace[ peerResults[[1,sub]]["VarName"], "PG"~~NumberString -> "PEERS"],
										"RunName" -> runName
									}
								],
								{sub, Length@peerResults[[1]] }
							];									
						PropertyValue[{modelGraph,vertex},"PeergroupValues"] = AppendTo[PropertyValue[{modelGraph,vertex},"PeergroupValues"], peerResults];
					)
			];	 	
	] /@ listVertexNames;
	PropertyValue[modelGraph, "RunNames"] = AppendTo[ PropertyValue[modelGraph,"RunNames"], runName];
	"Done."
]	

VensimStoreRunData[___] /; Message[VensimStoreRunData::funcargs] := "Never happens"

(*bslKennzahlenSystem::runname = "The model run `1` does not exist."
bslKennzahlenSystem::data = "No data found."
bslKennzahlenSystem[ modelGraph_?GraphQ, kennzahlenGraph_?GraphQ, opts:OptionsPattern[]] := Module[
	{
		vertexLabels,
		listVertices,
		timeSeries,
		rgTime,
		times,
		varRange,
		varLabel,
		benchFunc,
		peerGroupData,
		strUnits,
		runName = OptionValue[runName]	
	},
	
	If[
		StringFreeQ[ PropertyValue[modelGraph,"RunNames"], runName ],
		Return[ Message[bslKennzahlenSystem::runname, runName] ]
	]; (* check if runname exists *)
	
	listVertices = VertexList@kennzahlenGraph;
	rgTime = OptionValue[timeRange];
	
	(* definiere die Charts fuer die Knoten *)
	vertexLabels = Function[
		vertex,
		
		timeSeries = Quiet@getData[ modelGraph, vertex, vertex, runName ]; (* Time Series for the Base Company as TemporalData *)
				If[
			timeSeries == $Failed,
			Return[bslKennzahlenSystem::data]
		];
		
		peerGroupData = Quiet@getData[ modelGraph, vertex, vertex<>"[PEERS]", runName ]; (* TemporalData for the PeerGroup *)
				If[
			peerGroupData === $Failed || OptionValue[showPeergroup] == False ,
			peerGroupData = {}
		];
		
		benchFunc = getBenchmarkFunction[ modelGraph, vertex ];
		If[ benchFunc === Null, benchFunc = OptionValue[benchmarkFunction] ]; (* if no BenchmarkFunction is found, the default setting is used *)
		
		times = Switch[ rgTime,
				All, timeValues@timeSeries,
				___, listClip[ # , timeValues@timeSeries ]& /@ rgTime (* stellt sicher, dass range nicht ausserhalb der Modell-Zeitspanne liegt *)
			];
					
		varRange = PropertyValue[{modelGraph, vertex},"Range"];
		strUnits = PropertyValue[{modelGraph, vertex},"Units"] // unitsToLabel ;
		varLabel = PropertyValue[{modelGraph, vertex},VertexLabels];
		
		(* Here follows the rule for the specific vertex to be used *)
		
		vertex -> Placed[
			OptionValue[vertexChartFunction][
				(* [timeSeries, peeerGroupData, timeRange, varRange, BenchmarkFunction] *)
				timeSeries, 
				peerGroupData,
				times,
				varRange,
				benchFunc,
				{
					units -> strUnits,
					chartLabel -> varLabel,
					chartSize -> OptionValue[vertexChartSize]
				}
			],
			Center
		]
	] /@  listVertices;
	
	(* plotte das Kennzahlensystem als Graphen *)
	Graph[
		listVertices,
		EdgeList@kennzahlenGraph,
		EdgeStyle -> OptionValue[edgeStyle],
		EdgeShapeFunction -> OptionValue[edgeShapeFunction],
		VertexLabels -> vertexLabels,
		GraphLayout -> OptionValue[graphLayout],
		ImagePadding -> OptionValue[imagePadding],
		ImageSize -> OptionValue[imageSize]
	]
]

bslKennzahlenSystem[ modelGraph_?GraphQ, opts:OptionsPattern[] ]:= bslKennzahlenSystem[ modelGraph, modelGraph, opts]

bslKennzahlenSystem[___] := "never happens" /; Message[bslKennzahlenSystem::funcargs]*)

(* ChartDefinitions`bslBarChart[ dummy_String ] := "This extends the BSLUtilities-Package" *)

End[] (* End Private Context *)

EndPackage[]