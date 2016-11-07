(* Mathematica Package *)

(* Created by the Wolfram Workbench 30.09.2014 *)

BeginPackage["VensimLink`",{"NETLink`"}]
(* Exported symbols added here with SymbolName::usage *) 

(*General::fargs = "False Argument(s)."*)
(*General::fnf="File not found."*)

(* DLL Functions *)

VensimBeQuiet::usage = "\
VensimBeQuiet[quietFlag] will turn off certain work in progress dialog; the quietFlag may be set to 0,1 or 2.\nUse 0 for normal interaction, 1 to prevent the appearance of any work in progress windows, and 2 to also prevent the appearance of any interrogative dialogs."

VensimCheckStatus::usage = "\
VensimCheckStatus[] is used to check the current status of Vensim DLL. The two main purposes are to check whether Vensim is still in gaming mode, and to be sure Vensim is in a consistent internal state.\n\nA return value of 0 indicates that Vensim is idle and awaiting activity. This is the normal value."

VensimCommand::usage = "VensimCommand[ command ] is the main function to interact with Vensim. The commands supported are a subset of the commands supported when running Venapps as provided in chapter 5 of the DSS Supplement."

VensimContinueSimulation::usage = "\
VensimContinueSimulation[numTimeSteps] will execute numTimeSteps iterations. To be used between VensimStartSimulation and VensimFinishSimulation."

VensimFinishSimulation::usage = "\
VensimFinishSimulation[] completes a simulation started with VensimStartSimulation."

VensimGetData::usage = "\
VensimGetData[FileName,VarName,tName,maxNum] returns the time series for the variable VarName read in from the given vdf-file as a TemporalData Object.\n The names of the run and the variable are stored as MetaInformation."

VensimGetDPVal::usage = "\
VensimGetDPVal[VarName] returns the value of the variable during the simulation or during simulation setup."

VensimGetInfo::usage = "\
VensimGetInfo[InfoWanted] is used to get information about Vensim that will be returned as a list of strings."

VensimGetVarAttrib::usage = "\
VensimGetVarAttrib[VarName, IntAttrib] will return the requested attribute as a list of strings."

VensimGetVarNames::usage = "\
VensimGetVarnames[strFilter, intVarType, buffer, maxBufLen] will return the requested variable names as a list of strings."

(* 
VensimShowSketch::usage="VensimShowSketch[ViewNr,WantScroll,ZoomPercent,pWindow]"

can currently not be realized in Mathematica due to problems with the WindowHandle

*)

VensimStartSimulation::usage = "\
VensimStartSimulation[loadfirst, game, overwrite] starts a simulation that will be performed a bit at a time.\
loadfirst use 1 to indicate that the run resulting from the simulation should be loaded first in the list of runs.\ 
game use 0 to indicate that this is a regular simulation.\ 
overwrite use 1 to suppress any query about overwriting an existing file."

(* Hilfsfunktion fuer StringKonversion *)
VensimStringConversion::usage = "\
VensimStringConversion[string] converts a string so that special characters may be used also."

(* global variables for Vensim Link Package *)
$vensimDLL = "vdpdll32.dll" ; (* 64-Bit Dll:  "vendll64.dll" 32-Bit Double Precision DLL: "vdpdll32.dll" 32Bit-Single Precision DLL:  "Vendll32.dll" *)
$characterEncoding = "UTF8";
$maxNumber = 10000; (* Maximum Number of Values for Arrays *)
$maxBufLen = 20000; (* Maximum Buffer Length for Strings given as [out] arrays *)
$variableTypes = {"Level", "Auxiliary", "Data", "Initial", "Constant", "Lookup", "Group","SubscriptRange", "Constraint", "TestInput", "TimeBase", "Gaming", "SubscriptConstant" }

Begin["`Private`"]
(* Implementation of the package *)

(* force .NET to work with 32Bit applications and load needed objects *)

ReinstallNET["Force32Bit"->True] (* this may not be needed with the new Vensim 64bit release *)
LoadNETType["System.Runtime.InteropServices.Marshal"] (* needed to read multiple null terminated strings cf. stackexchange *)

(* special containers for vector or string output *)
vecVarValues = NETNew["System.Single[]",$maxNumber] (* vecVarValuesDP=NETNew["System.Double[]",$maxNumber]; *)
vecTimeValues = NETNew["System.Single[]",$maxNumber]
strOutput = NETNew["System.Text.StringBuilder",$maxBufLen]
buffer = Marshal`AllocHGlobal[$maxBufLen] (* needed to read in a list of integers using "IntPtr" (pass-by-reference) *)
managedArray=NETNew["System.Byte[]",$maxBufLen]

(* DLL Function Declarations *)
vensimBeQuiet = DefineDLLFunction[ "vensim_be_quiet", $vensimDLL, "int", {"int"} ]
vensimCheckStatus = DefineDLLFunction[ "vensim_check_status",$vensimDLL, "int", {} ] 
vensimCommand = DefineDLLFunction[ "vensim_command", $vensimDLL, "int", {"char*"} ] (* null-terminated string representation *)
vensimContinueSimulation = DefineDLLFunction[ "vensim_continue_simulation", $vensimDLL, "int", {"int"} ]
vensimFinishSimulation = DefineDLLFunction[ "vensim_finish_simulation", $vensimDLL, "int", {} ]
vensimGetData = DefineDLLFunction[ "vensim_get_data", $vensimDLL, "int", {"const char*","const char*","const char*","float[]","float[]","int"} ] (* vecVarValues and vecTimeValues must be used when calling the function *)
vensimGetDPVal = DefineDLLFunction[ "vensim_get_dpval", $vensimDLL, "int", {"char*","out double"} ]
(* vensimGetDPVecVals *)
(* vensimGetInfo=DefineDLLFunction["vensim_get_info",vensimDLL,"int",{"int","System.Text.StringBuilder","int"}]; *)
vensimGetInfo = DefineDLLFunction[ "vensim_get_info", $vensimDLL, "int", {"int","IntPtr","int"} ]
(* vensim_get_sens_at_time *)
vensimGetSubstring = DefineDLLFunction[ "vensim_get_substring", $vensimDLL, "int", {"System.Text.StringBuilder","int","System.Text.StringBuilder","int"} ]
(* vensim_get_val *)
vensimGetVarAttrib = DefineDLLFunction[ "vensim_get_varattrib", $vensimDLL, "int", {"const char*","int","IntPtr","int"} ]
vensimGetVarNames = DefineDLLFunction[ "vensim_get_varnames", $vensimDLL, "int", {"const char*","int","IntPtr","int"} ]
(* vensim_get_varoff *)
(* vensim_get_vecvals *)
(* vensim_set_parent_window *)
vensimShowSketch = DefineDLLFunction[ "vensim_show_sketch", $vensimDLL, {"int","int","int","int"} ]
vensimStartSimulation = DefineDLLFunction[ "vensim_start_simulation", $vensimDLL, "int", {"int","int","int"} ]
(* vensim_tool_command *)
(* VensimContextAdd *)
(* VensimContextDrop *)


(* Public Funcion Implementation *)
VensimStringConversion[ str_String ] := FromCharacterCode[ ToCharacterCode[ str, $CharacterEncoding ], "ASCII" ] (* Tipp von Additive *)
VensimStringConversion[ ___ ] := Return[ Message[ VensimStringConversion::fargs ]; $Failed ]

VensimBeQuiet::invalidarg = "Only the numbers 0, 1 or 2 are allowed as arguments."
VensimBeQuiet[ quietFlag_Integer:0 ] /; 0<= quietFlag <= 2 := vensimBeQuiet[quietFlag]
VensimBeQuiet[___] /; Message[VensimBeQuiet::invalidarg] := "Never happens"

VensimCheckStatus::error = "VensimCheckStatus reported an error that should be forwarded to Ventana Systems Inc."
VensimCheckStatus[___] := Module[
	{funcVal},
	funcVal = vensimCheckStatus[];
	If[
		Not@MemberQ[ {0,1,5,32,33}, funcVal ],
		Message[VensimCheckStatus::error]
	];
	funcVal
	]
	
VensimCommand::failed = "The command or test given failed."
VensimCommand[ command_String ] := Module[
	{funcVal},
	funcVal = vensimCommand[command];
	If[
		funcVal == 0,
		(* then *) Message[VensimCommand::failed]
	];
	funcVal
]
VensimCommand[___] /; Message[VensimCommand::fargs] := "Never happens"
	
VensimContinueSimulation::fperror = "A floating point error occured during the simulation and VensimFinishSimulation has been called."
VensimContinueSimulation[ numTimeSteps_Integer] := Module[
	{funcVal},
	funcVal=vensimContinueSimulation[numTimeSteps];
	Which[
		funcVal==-1, Message[VensimContinueSimulation::fperror]; VensimFinishSimulation[] ,
		funcVal== 0, VensimFinishSimulation[];
	];
	funcVal
]
VensimContinueSimulation[___] /; Message[VensimContinueSimulation::fargs] := "Never happens"

VensimFinishSimulation::failed = "The simulation could not be finished properly."
VensimFinishSimulation[___] := Module[
	{funcVal},
	funcVal = vensimFinishSimulation[];
	If[
		funcVal== 0,Message[VensimFinishSimulation::failed]
	];
	funcVal
]

VensimGetData::notfound = "The Variable `1` was not found in the dataset."
VensimGetData::insufficient="There is insufficient room to write the values; increase $maxNumber."
VensimGetData[
	fileName_String,
	varName_String,
	tName_String,
	0
	] :=	vensimGetData[
			  fileName,
			  varName,
			  tName,
			  vecVarValues,
			  vecTimeValues,
			  0
] (* returns the required buffer size to write the requested values *)

VensimGetData[
	fileName_String,
	varName_String,
	tName_String,
	maxNum_Integer:$maxNumber
	] := Module[
	
	{vecVals,vecTimes,numValuesRetrieved, presentDirectory },
	
	If[ Not@FileExistsQ@fileName, Return[ Message[VensimGetData::fnf]; $Failed ]];
	presentDirectory = Directory[];
	SetDirectory[ DirectoryName@fileName ];
	
	numValuesRetrieved = vensimGetData[ 
		ToString[ FileNameTake@fileName, CharacterEncoding -> $characterEncoding] , 
		ToString[ varName, CharacterEncoding -> $characterEncoding  ],
		ToString[ tName, CharacterEncoding -> $characterEncoding ], 
		vecVarValues, 
		vecTimeValues,
		maxNum
	];
	Which[
		numValuesRetrieved == 0, Return[ Message[VensimGetData::notfound, varName ] ],
		numValuesRetrieved == maxNum, Return[ Message[VensimGetData::insufficient] ]
	];
	vecVals=Take[ NETObjectToExpression[vecVarValues] , numValuesRetrieved ];
	vecTimes=Take[ NETObjectToExpression[vecTimeValues], numValuesRetrieved];
	
	SetDirectory[ presentDirectory ]; (* reset the directory *)
	
	(* return the value as a TemporalData object *)
	TemporalData[
		vecVals,
		{vecTimes}, 
		MetaInformation -> {
			"VarName" -> varName,
			"RunName" -> fileName
		}
	] 
]

VensimGetData[___] /; Message[VensimGetData::fargs] := "Never happens"


VensimGetDPVal::notfound = "The Variable `1` was not found in the dataset."
VensimGetDPVal[varName_String] := Module[
	{varVal,funcVal},
	funcVal = vensimGetDPVal[ varName, varVal ];
	If[
		funcVal== 0,
		(* then *) Return[ Message[VensimGetData::notfound, varName] ]
	];
	varVal
]
VensimGetDPVal[___] /; Message[VensimGetDPVal::fargs] := "Never happens"

VensimGetInfo::args = "VensimGetInfo called with no or false arguments. The argument given must be an integer in the range 1 to 23."
VensimGetInfo[
	infoWanted_Integer,
	bufLen_Integer:$maxBufLen
	] /; (1<=  infoWanted <= 23 && bufLen > 0 ) := Module[
		{
			numBytes,bytes
		},
		numBytes=vensimGetInfo[infoWanted,buffer,bufLen];
		Marshal`Copy[buffer,managedArray,0,numBytes];
		bytes = NETObjectToExpression[managedArray][[ 1;;numBytes ]];(* gives an array of characters with `0` being the Null String *)
		FromCharacterCode[DeleteCases[SplitBy[bytes,# != 0 &],{0..}], $characterEncoding] (* returns a list of strings *)
]

VensimGetInfo[
	infoWanted_Integer,
	0
	] /; 1<= infoWanted <= 23 := vensimGetInfo[infoWanted,buffer,0] (* returns the size of the buffer needed to store the information *)

VensimGetInfo[___] /; Message[VensimGetInfo::args] := "Never happens"

VensimGetVarAttrib::error = "VensimGetVarAttrib returned an error; it may be that the variable could not be found."
VensimGetVarAttrib[
	strVarName_String,
	intAttribute_Integer,
	maxBufLen_Integer:$maxBufLen
	]/; 1 <= intAttribute <= 15 := Module[
		{
			numBytes,
			bytes,
			attribute
		},

		numBytes = vensimGetVarAttrib[ ToString[strVarName, CharacterEncoding -> $characterEncoding], intAttribute, buffer, maxBufLen ] ;
		If[
			numBytes == -1,
			(* then *) Return[ Message[VensimGetVarAttrib::error] ]
		];
		Marshal`Copy[buffer,managedArray,0,numBytes];
		bytes=NETObjectToExpression[managedArray][[1;;numBytes]];(* gives an array of characters with `0` being the Null String *)
		attribute=FromCharacterCode[DeleteCases[SplitBy[bytes,# != 0 &],{0..}], $characterEncoding];
		If[
			attribute == "" || attribute == {},
			(* then *) Return[ {Null} ]
		];
		attribute (* returns a list of strings or {NULL} *)
]

VensimGetVarAttrib[___] := Return[ Message[VensimGetVarAttrib::fargs]; $Failed ]

VensimGetVarNames::error = "VensimGetVarnames returned an error"
VensimGetVarNames[
	strFilter_String,
	intVarType_Integer,
	maxBufLen_Integer:$maxBufLen
	]/; 0 <= intVarType <= 13 := Module[
		{
			numBytes,
			bytes
		},
		numBytes = vensimGetVarNames[ToString[strFilter, CharacterEncoding->$characterEncoding],intVarType,buffer,maxBufLen];
		If[
			numBytes == -1,
			(* then *) Return[ Message[VensimGetVarNames::error] ]
		];
		Marshal`Copy[buffer,managedArray,0,numBytes];
		bytes=NETObjectToExpression[managedArray][[1;;numBytes]];(* gives an array of characters with `0` being the Null String *)
		FromCharacterCode[DeleteCases[SplitBy[bytes, # != 0 & ],{0..}],$characterEncoding] (* returns a list of strings *)
		]

VensimGetVarNames[___] /; Message[VensimGetVarNames::fargs] := "Never happens"

(*
VensimShowSketch::fail = "Vensim was not able to show the model in a window."
VensimShowSketch::args = "VensimShowSketch was called with incorrect arguments."
VensimShowSketch[
	sketchnum_Integer,
	wantscroll_Integer,
	zoompercent_Integer,
	pwindow_Integer
	] /; (sketchnum > 0 && (wantscroll == 0 || wantscroll == 1) && (zoompercent == 0 || zoompercent == 5 || 20 <= zoompercent <= 500) ) := Module[
	{funcVal},
	funcVal=vensimShowSketch[sketchnum,wantscroll,zoompercent,pwindow];
	If[
		funcVal != 1,
		(* then *) Return[Message[vensimShowSketch::fail]]
	];
	funcVal
	] 

VensimShowSketch[___] /; Message[VensimShowSketch::args]
*)

VensimStartSimulation::unsuccessful = "The simulation could not be started."
VensimStartSimulation[
	loadfirst_Integer,
	game_Integer,
	overwrite_Integer
	] /; (0 <= loadfirst <= 1 && 0 <= game <= 2 && 0 <= overwrite <= 1) := Module[
		{funcVal},
		funcVal = vensimStartSimulation[ loadfirst, game, overwrite ];
		If [
			funcVal == 0, 
			(* then *) Message[VensimStartSimulation::unsuccessful]
		];
		funcVal
]

VensimStartSimulation[___] /; Message[VensimStartSimulation::fargs] := "Never happens"

End[]

EndPackage[]

