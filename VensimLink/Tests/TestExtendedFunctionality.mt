(* Mathematica Test File *)

(* linearScore *)
Test[
	linearScore[5,{5,5},{0,10}]
	,
	0.
	,
	TestID->"TestCoreFunctions-20140221-C4P3U6"
]

Test[
	linearScore[5,{0,10},{5,5}]
	,
	1.
	,
	TestID->"TestCoreFunctions-20140221-J6N3S0"
]

Test[
	linearScore[4,{3,7},{0,5}]
	,
	0.5
	,
	TestID->"TestCoreFunctions-20140221-T9Q1Z4"
]

Test[
	linearScore[5,{3,4},{6,7}]
	,
	0.5
	,
	TestID->"TestCoreFunctions-20140221-T9D3X6"
]

(* ratingScore *)
Test[
	rateScore[0.5, {0.8,0.6,0.4,0.2}]
	,
	3
	,
	TestID->"BSLKennzahlenTool-20140221-R9V8T2"
]

Test[
	rateScore[0.5, {0.8},ratings->{1}]
	,
	1
	,
	TestID->"BSLKennzahlenTool-20140221-U6E2X1"
]

Test[
	toPercentInteger[0.5]
	,
	50
	,
	TestID->"TestCoreFunctions-20140224-I9A3T6"
]


Test[
	listClip[-1, {1,2,3,4}]
	,
	1
	,
	TestID->"TestCoreFunctions-20140306-L0U2X0"
]

Test[
	listClip[10, {1}]
	,
	1
	,
	TestID->"TestCoreFunctions-20140306-J9O1F0"
]