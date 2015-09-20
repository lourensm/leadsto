:- module(pareto,
	  [
	   handle_pareto_options/0
	  ]).
:- use_module(util).
:- use_module(satgenut).
:- use_module(ttlchecker).
:- use_module(trace_manager).

:- dynamic dyn_pareto_info/1, dyn_pareto_info/2, dyn_discr_configuration/5.
handle_pareto_options :-
	(	get_option(pareto(What)),
		handle_pareto_option(What),
		fail
	;	true
	).
handle_pareto_option(show_curves) :-
	!,
	get_curves.

handle_pareto_option(What) :-
	error('Unrecognised pareto option ~w', [What]).


local_option(pareto, arg('-pareto', Arg,set_option(pareto(Arg)),'PARETO'),
	     'PARETO in  {show_curves}',[]).
invalidate_traces :-
	retractall(dyn_pareto_info(_)),
	retractall(dyn_pareto_info(_,_)),
	retractall(dyn_attr_util_point(_Trace1, _, _, _, _, _)),
	retractall(dyn_discr_configuration(_, _, _, _, _)).

ensure_pareto_info(Trace,ParetoInfo) :-
	dyn_pareto_info(Trace,ParetoInfo),
	!.
ensure_pareto_info(Trace,ParetoInfo) :-
	get_pareto_info(Trace,ParetoInfo),
	assertz(dyn_pareto_info(Trace,ParetoInfo)).

ensure_pareto_info(ParetoInfo) :-
	dyn_pareto_info(ParetoInfo),
	!.
ensure_pareto_info(ParetoInfo) :-
	(get_pareto_info(ParetoInfo)
	->	assertz(dyn_pareto_info(ParetoInfo))
	;	ParetoInfo = error
	).



get_single_trace(TraceName1) :-
	(holds:atom_trace(TraceName1, _, _, _)
	->	(holds:atom_trace(TraceName2, _, _, _),
			TraceName1 \= TraceName2
		->	error('Multiple traces, ParetoInfo needs trace argument'),
			fail
		;	true
		)
	;	error('Missing pareto trace contents, no trace loaded?'),
		fail
	).
get_pareto_info(ParetoInfo) :-
	(get_single_trace(TraceName1)
	->	ensure_pareto_info(TraceName1, ParetoInfo)
	;	error('Missing pareto trace contents, no trace loaded?'),
		fail
	).

get_pareto_info(TraceName1, ParetoInfo) :-
	flag(paretoinfo, I, I + 1),
	(I = 0
	->	user:add_invalidate_client(pareto:invalidate_traces)
	;	true
	),
	(	atom_trace_constant(TraceName1,
				    buyer_price_utility_info(PriceMax,BUMax,BUFact,
							     PriceBUZero)),
		atom_trace_constant(TraceName1,
				    seller_price_utility_info(BCost, SFactor))
	->	complete_pareto_data(TraceName1, PriceMax,BUMax,BUFact,
				     PriceBUZero,BCost,SFactor,ParetoInfo)
	;	error('Missing utility info for ParetoInfo'),
		fail
	).
atom_trace_constant(TraceName1, Term) :-
	holds:atom_trace(TraceName1, _Key, Term, Ranges),
	valid_data_ranges(Ranges).
valid_data_ranges([range(1,maxinf, true)]) :-
	!.
valid_data_ranges(Ranges) :-
	warning('Constant nego trace value suspect ranges:~w', [Ranges]).


discr_attr(cd_player).
discr_attr(extra_speakers).
discr_attr(airco).
discr_attr(drawing_hook).
discr_value(good).
discr_value('fairly good').
discr_value(standard).
discr_value(meager).
discr_value(none).
/*
BUMax   *************
        |             *
Ubprice |               *
        |                 *
        |                   *
   0    |---------------------*****************
        0           SUA      SUB


  Distance:
  Filter non optimum contributing discrete utility points:

BUdiscr1  *************
                        *
BUdiscr2  ++++++++++++++++*
                            +
                              *
Then point 2 need not be considered because it will never lead to a
smaller pareto_optimum_distance.

So:
if BU1 >= BU2 and SU1 >= SU2 - (BU1 - BU2)*BUMax/(SFactor*(PriceBUZero - PriceMax))
then point 2 not relevant: probably BUMax/(SFactor*(PriceBUZero - PriceMax))
equivalent to - BUFactor/SFactor

Or store BU1, SU1 + BU1*BUMax/(SFactor*(PriceBUZero - PriceMax))
*/

:- dynamic dyn_attr_util_point/6, dyn_discr_envelope_config/4.

rm_irrelevant :-
	fail.


get_attr_points(Trace1, BUF) :-
	retractall(dyn_attr_util_point(Trace1, _, _, _, _, _)),
	(	discr_attr(DA),
		discr_value(DV),
		(	atom_trace_constant(Trace1,
					    attr_utility(buyer,DA, DV, BU)),
			atom_trace_constant(Trace1,
					    attr_utility(seller,DA, DV, SU))
		->	SUZ is SU + BU/BUF,
			(	rm_irrelevant,
				dyn_attr_util_point(Trace1, BU1, SU1, SUZ1,
						    DA, DV1),
				BU1 >= BU,
				SUZ1 >= SUZ
			->	local_format('Pareto irrelevant attribute combination:~w:~w(wrt ~w)~n', [DA,DV, DV1])
			;	assertz(dyn_attr_util_point(Trace1, BU, SU,
							    SUZ, DA, DV)),
				(	rm_irrelevant,
					dyn_attr_util_point(Trace1, BU1, SU1,
							    SUZ1, DA, DV1),
					BU >= BU1,
					SUZ > SUZ1,
					retract(dyn_attr_util_point(Trace1,
								    BU1, SU1,
								    SUZ1,DA,
								    DV1)),
						local_format('Pareto irrelevant attribute combination:~w:~w~n', [DA,DV1]),
					fail
				;	true
				)
			)
		;	error('Missing attribute info for ~w ~w', [DA, DV])
		),
		fail
	;	true
	).

attr_util(Trace1, Role, Attr, V, Ut) :-
	atom_trace_constant(Trace1, attr_utility(Role, Attr, V, Ut)).
max_attr_u(Trace1, Role, Attr, ABU) :-
	bagof(U-V, attr_util(Trace1, Role, Attr, V, U), UVs),
	max_list_uv(UVs, ABU).
max_attr_sum(Trace1, Role, U) :-
	bagof(DA, discr_attr(DA), DAs),
	max_attr_suml(DAs, Trace1, Role, U).
max_attr_suml([], _Trace1, _Role, 0).
max_attr_suml([DA|DAs], Trace1, Role, U) :-
	max_attr_u(Trace1, Role, DA, ABU-Vs),
	local_format('Attr ~w must have value in ~w~n', [DA, Vs]),
	max_attr_suml(DAs, Trace1, Role, U1),
	U is ABU + U1.

max_list_uv([U-V], U-[V]) :-
	!.
max_list_uv([U-V|R], UM) :-
	max_list_uv(R, U1-L),
	(U > U1
	->	UM = U-[V]
	;	U =:= U1
	->	UM = U1-[V|L]
	;	UM = U1-L
	).



complete_pareto_data(Trace1, PriceMax,BUMax,BUFact,
				    PriceBUZero,
				    BCost,SFactor,
				    ParetoInfo) :-
	SUA is (PriceMax-BCost)*SFactor,
	SUB is (PriceBUZero-BCost)*SFactor,
	BUF is BUMax/(SFactor*(PriceBUZero - PriceMax)),
	BUF2 is - BUFact/SFactor,
	BUF3 is 1/((SUB - SUA)/BUMax),
	format('all:~w ~w ~w~n',[BUF,BUF2,BUF3]),
	(abs(BUF-BUF2)/BUF < 1e-8
	->	true
	;	local_format('Expected identity:~w = ~w ~n', [BUF, BUF2])
	),
	get_attr_points(Trace1, BUF),
	max_attr_sum(Trace1, buyer, BSum),
	BUTotalMax is BSum + BUMax,
	local_format('Maximal Buyer utility is ~w (should be 1.0)~n', [BUTotalMax]),
	get_attr_configs(Trace1),
	get_attr_envelope(Trace1,Points),
	setof(du(BU, SU, SUZ), DAVs^dyn_discr_configuration(Trace1, BU, SU, SUZ, DAVs),
	      DiscreteData),
	DistFact is BUMax/sqrt((SUB-SUA)*(SUB-SUA)),
	local_format('SUA:~w  SUB:~w  BUMax:~w~n', [SUA, SUB, BUMax]),
	% Probably related to or equal to inverse of BFact/SFact
	(member(BULs-SULs-_SUZLs, Points),
		local_format('PT0: ~w  ~w~n', [BULs, SULs]),
		fail
	;	true
	),
	% Points are setof sorted: BU-SU-SUZ
	get_envelope_path(Points, BUMax, SUA, SUB, Path),
	get_nash_path_point(Path, BUNash, SUNash),
	local_trace(epp),
	get_epp_path_point(Path, BUEPP),
	ParetoInfo = ds_pi(ui(BUMax, SUA, SUB, BUF, DistFact), Points, Path,
			BUNash-SUNash, BUEPP, DiscreteData).
local_test_nash(BU, SU, Path, CoordInfo, P) :-
	(Path = [BU1-SU1,BU2-SU2|_]
	->	Alpha is (BU2 - BU1)/(SU2-SU1),
		SUm is SU1 - BU1/Alpha,
		BUm is BU1 + Alpha*(SUm - SU1),
		assert_debug(abs(BUm) < 0.0001),
		SUa is (SUm - 0.0)/2.0,
		BUa is BU1 + Alpha*(SUa - SU1),
		(	abs(BUa - BU) < 0.01,
			abs(SUa - SU) < 0.01
		->	true
		;	get_point(CoordInfo, BUa, SUa, X1, Y1),
			R is 4,
			send(P, display, new(C,circle(2*R + 1)),
			     point(X1-R,Y1-R)),
			send(C, colour, red)
		)
	;	fatal_error('1 data point')
	).
chk_bu_su_near(BUL-SUL-SUZL,BUL2-SUL2-SUZL2,BUL3-SUL3-SUZL3) :-
	abs(BUL - BUL2) < 1e-7,
	(	abs(SUL - SUL2) < 1e-5,
		abs(SUZL - SUZL2) < 1e-5
	->	local_inform('Thrown away neighbour'),
		BUL3-SUL3-SUZL3 = BUL-SUL-SUZL
	;	(SUL >= SUL2
		->	BUL3 = BUL,
			SUL3 = SUL,
			SUZL3 = SUZL
		;	BUL3 = BUL2,
			SUL3 = SUL2,
			SUZL3 = SUZL2
		),
		warning('Pareto optimum curve contains two points with almost identical BU, small chance that the wrong envelope point got thrown away')
	;	impl_error('envelope mismatch ~w  - ~w',[BUL-SUL-SUZL,BUL2-SUL2-SUZL2])
	).

get_envelope_path([BSZ1,BSZ2|Points], BUMax, SUA, SUB,
		  Path) :-
	chk_bu_su_near(BSZ1,BSZ2,BSZ3),
	!,
	get_envelope_path([BSZ3|Points], BUMax, SUA, SUB, Path).


get_envelope_path(Points, BUMax, SUA, SUB, Path) :-
	(Points = [BUL-SUL-SUZL|R]
	->	last(Points, BULl-_SULl-_SUZLl),
		SUL1 is SUA + SUL + (BULl-BUMax-BUL)*(SUB-SUA)/(0.0 - BUMax),
		%BUL1 is min(BUL + BUMax - 0.1, BUMin),
		
		BU1 is BUL + BUMax,
		SU1 is SUL + SUA,
		Path = [BULl-SUL1,BU1-SU1|Path1],
		assert_debug(BU1 > BULl, lastenv),
		get_envelope_rest(R, BU1,SU1,SUZL, BUMax, SUA, Path1)
	;	Path = []
	).
get_envelope_rest([], _BUL,_SUL,_SUZL, _BUMax, _SUA, []) :-
	!.
get_envelope_rest([BSZ1,BSZ2|R], BUL,SUL,SUZL, BUMax, SUA,Path) :-
	chk_bu_su_near(BSZ1,BSZ2,BSZ3),
	!,
	get_envelope_rest([BSZ3|R], BUL,SUL,SUZL, BUMax, SUA,Path).


get_envelope_rest([BUL1-SUL1-SUZ1|R], BUL,SUL,SUZL, BUMax, SUA,
		  [BUL-SULA,BUL1A-SUL1A|Path]) :-
	BUL1A is BUL1 + BUMax,
	SUL1A is SUL1 + SUA,
	SULA is SUL  - (SUZL - SUZ1),
	assert_debug(BUL1A > BUL, envrest),
	get_envelope_rest(R, BUL1A,SUL1A,SUZ1, BUMax, SUA, Path).


	
get_attr_configs(Trace1) :-
	retractall(dyn_discr_configuration(Trace1, BU, SU, SUZ, DAVs)),
	bagof(DA, discr_attr(DA), DAs),
	(	some_discr_configuration(DAs, Trace1, BU, SU, SUZ, DAVs),
		(	(       dyn_discr_configuration(Trace1, BU1, SU1, SUZ1, DAVs1),
				BU1 >= BU,
				SUZ1 >= SUZ,
				rm_irrelevant
			->	local_format('Pareto irrelevant attribute combination:~w~n',
				       [DAVs])
			;	maplist(trfloat,[BU,SU,SUZ],[BUf,SUf,SUZf]),
				assertz(dyn_discr_configuration(Trace1, BUf,
								SUf,
								SUZf,DAVs)),
				dyn_discr_configuration(Trace1, BU1, SU1, SUZ1,
							DAVs1),
				(	BU >= BU1,
					SUZ > SUZ1,
					rm_irrelevant
				->	retract(dyn_discr_configuration(Trace1,
									BU1,
									SU1, SUZ1,
									DAVs1)),
					local_format('Pareto irrelevant attribute combination:~w~n', [DAVs1])
				;	true
				)
			),
			fail
		),
		fail
	;	true
	).
get_attr_envelope(Trace1,Points) :-
	retractall(dyn_discr_envelope_config(Trace1, _, _, _, _)),
	(	dyn_discr_configuration(Trace1, BU1, SU1, SUZ1, _DAVs1),
		chk_single_envelope_config(Trace1, BU1, SU1, SUZ1),
		fail
	;	true
	),
	(	dyn_discr_envelope_config(Trace1, BU1, SU1, _SUZ1),
		dyn_discr_envelope_config(Trace1, BU1, SU2, _SUZ2),
		SU1 < SU2
	->	format('ERROR:unexpected max config~n')
	;	true
	),
	setof(BU-SU-SUZ, dyn_discr_envelope_config(Trace1, BU, SU, SUZ),
	      Points),
	(is_local
	->	tst_single_bu(Points)
	;	true
	).
tst_single_bu([]).
tst_single_bu([F|R]) :-
	tst_single_bu(F, R).
tst_single_bu(BU-SU-SUZ, R) :-
	(	member(BU1-SU1-SUZ1, R),
		Dif is abs(BU - BU1),
		Dif < 1e-5
	->	local_inform('TR:(~w)~w  <-> ~w~n', [Dif,BU-SU-SUZ, BU1-SU1-SUZ1])
	;	tst_single_bu(R)
	).


	
chk_single_envelope_config(Trace1, BU, SU, SUZ) :-
	(	dyn_discr_envelope_config(Trace1, BU1, _SU1, SUZ1),
		BU1 >= BU,
		SUZ1 >= SUZ
	->	local_format('Pareto irrelevant configuration:~w,~w~n', [BU, SU])
	;	assertz(dyn_discr_envelope_config(Trace1, BU, SU, SUZ)),
		(	dyn_discr_envelope_config(Trace1, BU2, SU2, SUZ2),
			(	BU >= BU2,
				SUZ > SUZ2
			->	local_format('Pareto irrelevant configuration:~w,~w~n',
					     [BU2,SU2]),
				retract(dyn_discr_envelope_config(Trace1, BU2,
									 SU2, SUZ2))
			;	true
			),
			fail
		;	true
		)
	).

some_discr_configuration([], _Trace, 0, 0, 0, []).

some_discr_configuration([DA|DAs], Trace1, BU, SU, SUZ, [DA-DV|DAVs]) :-
	dyn_attr_util_point(Trace1, BU1, SU1, SUZ1, DA, DV),
	some_discr_configuration(DAs, Trace1, BU2, SU2, SUZ2, DAVs),
	BU is BU1 + BU2,
	SU is SU1 + SU2,
	SUZ is SUZ1 + SUZ2.



/*
  For each candidate discrete utility point, calculate the distance.
  The minimum value is the result
  */
background_function(pareto_distance(BU,SU), Res) :-
	ensure_pareto_info(ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, Path, _NashPt, _BUEPP, _DiscreteData)
	->	get_min_distance(Path, BU,SU, Res),
		local_format('~w~n', [pareto_distance(BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).

background_function(pareto_distance(Trace, BU,SU), Res) :-
	ensure_pareto_info(Trace, ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, Path, _NashPt, _BUEPP, _DiscreteData)
	->	get_min_distance(Path, BU,SU, Res),
		local_format('~w~n', [pareto_distance(Trace,BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).

background_function(nash_distance(BU,SU), Res) :-
	ensure_pareto_info(ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, _Path, NashPt, _BUEpp, _DiscreteData)
	->	get_nash_distance(NashPt, BU,SU, Res),
		local_format('~w~n', [nash_distance(BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).

background_function(nash_distance(Trace, BU,SU), Res) :-
	ensure_pareto_info(Trace, ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, _Path, NashPt, _BUEpp, _DiscreteData)
	->	get_nash_distance(NashPt, BU,SU, Res),
		local_format('~w~n', [nash_distance(Trace,BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).




background_function(epp_distance(BU,SU), Res) :-
	ensure_pareto_info(ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, _Path, _NashPt, BUEPP, _DiscreteData)
	->	get_epp_distance(BUEPP, BU,SU, Res),
		local_format('~w~n', [epp_distance(BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).

background_function(epp_distance(Trace, BU,SU), Res) :-
	ensure_pareto_info(Trace, ParetoInfo),
	(ParetoInfo = ds_pi(_UI, _Points, _Path, _NashPt, BUEPP, _DiscreteData)
	->	get_epp_distance(BUEPP, BU,SU, Res),
		local_format('~w~n', [epp_distance(Trace,BU,SU)= Res])
	;	error('Cannot handle pareto function, incomplete info'),
		fail
	).
get_nash_distance(BUN-SUN, BU,SU, Res) :-
	sqrtdist(BUN,SUN, BU, SU, Res).

get_epp_distance(BUEPP, BU, SU, Res) :-
	sqrtdist(BUEPP,BUEPP, BU, SU, Res).



get_min_distance(Path, BU,SU,Res) :-
	get_min_distance(Path, BU,SU,Res, _BUPT, _SUPT).

get_min_distance(Path, BU,SU,Res, BUPT, SUPT) :-
	get_min_distance(Path, BU,SU,Res, BUPT, SUPT, [norm]).
get_min_distance(_Path, BU,SU,_Res, _BUPT, _SUPT, [F|R]) :-
	memberchk(norm, [F|R]),
	(	BU =< 1.0,
		SU =< 1.0,
		BU >= 0.0,
		SU >= 0.0
	->	fail
	;	fatal_error('Normalised pareto distance requires point normalised')
	).

get_min_distance(Path, BU,SU,Res, BUPT, SUPT, Opts) :-
	Path = [BU1-SU1,BU2-SU2|Path1],
	% largest SU, BU near zero
	% first test special case
	(	memberchk(norm,Opts),
		(SU1 =< 0
		->	Res1a is BU1 - BU,
			(Res1a < 0
			->	(SU =:= 0.0
				->	Res1 = 0.0,
					BUPT1 = BU,
					SUPT1 = 0.0
				;	fatal_error('point outside pareto area(BU1=~w BU = ~w, SU=~w SU1 = ~w', [BU1, BU, SU, SU1])
				)
			;	Res1 = Res1a
			),
			Res2 is 1.0 - SU,
			(Res1 =< Res2
			->	BUPT = BU1,
				SUPT = SU,
				Res = Res1
			;	Res = Res2,
				BUPT = BU,
				SUPT = 1.0
			)
		;	SU >= SU1
		->	fail
		;	check_norm_su0(BU1, SU1, BU2, SU2, BU, SU, _Res, _BUPT, _SUPT,
				       Res, BUPT, SUPT, Opts)
		)
	->	true
	;	SU >= SU1
	->	(BU =< BU1
		->	true
		;	fatal_error('utility point outside pareto area')
		),
		Res1 is abs(BU1 - BU),
		Res2 is 1.0 - SU,
		(Res1 < Res2
		->	BUPT = BU1,
			SUPT = SU,
			Res = Res1
		;	BUPT = BU,
			SUPT = 1.0,
			Res = Res2
		)
	;	norm_line_dist(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT1, SUPT1,
			       Opts),
		assert_debug(SU2 >= 0.0, at1),
		get_rest_dist([BU2-SU2|Path1], BU, SU, Res1, BUPT1, SUPT1,
			      Res, BUPT, SUPT,Opts)
	).

check_norm_su0(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT1, SUPT1, Res, BUPT,SUPT,Opts) :-
	SU2 < 0,
	memberchk(norm, Opts),
	BU2a is BU1 + (0 - SU1)*(BU2 - BU1)/(SU2 - SU1),
	assert_debug(SU >= 0),
	(	BU >= BU2a
	->	Res = 0.0,
		BUPT = BU,
		SUPT = 0.0,
		(	SU > 0.0
		->	fatal_error('SU outside pareto area(SU=~w)',[SU])
		;	true
		)
	;	SU2a = 0.0,
		norm_line_dist(BU1, SU1, BU2a, SU2a, BU, SU, Res2,
			       BUPT2, SUPT2, Opts),
		(	(var(Res1);Res2 < Res1)
		->	BUPT = BUPT2,
			SUPT = SUPT2,
			Res = Res2
		;	BUPT = BUPT1,
			SUPT = SUPT1,
			Res = Res1
		)
	).

norm_line_dist(BU1, SU1, BU2, SU2, BU, SU, Res3, BUPT3, SUPT3, Opts) :-
	line_dist(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT1, SUPT1),
	(	memberchk(norm, Opts),
		Res2 is 1.0 - SU,
		assert_debug(SU =< 1.0),
		Res2 < Res1
	->	Res3 = Res2,
		BUPT3 = BU,
		SUPT3 = 1.0
	;	Res3 = Res1,
		BUPT3 = BUPT1,
		SUPT3 = SUPT1
	).

/*
get_min_distance(_DiscreteData, Path, BU,SU,_UI, Res) :-
	true,
	!,
	get_min_distance_new(Path, BU,SU, Res, _BUPT, _SUPT).
get_min_distance(DiscreteData, _Path, BU,SU,UI, Res) :-
	get_min_distance(DiscreteData, BU,SU, initial,UI, Res),
	(Res == invalid
	->	format('ERROR:utility point outside of pareto curve~n'),
		fail
	;	true
	).
*/
ens_res(BU, SU, Res1, BUPT1, SUPT1) :-
	sqrtdist(BUPT1,SUPT1, BU, SU, Res2),
	(abs(Res1 - Res2) < 1e-8
	->	true
	;	L is (Res1 - Res2)*1000,
		local_format('WARNING:res mismatch:~w - ~w (~w/1000)~n',[Res1,Res2,L]),
		trace
	).


/*
  Improve performance?

  *
   *
    *******
           *
            *
             **********
                       *
                        *******
                               *
                                *
  There is a fast way of determining a sub range of line segments
  that define the minimal distance:
  There are two slopes, determine the perpendiculars to all endpoints.
  BU,SU will more or less intersect one perpendicular of both slopes.
  The nearest point will only depend on one of the line segments that are
  between both segments defined by both perpendiculars.
  */
get_rest_dist([BU1-SU1], BU, SU, Res1, BUPT1, SUPT1, Res,BUPT, SUPT,_Opts) :-
	ens_res(BU, SU, Res1, BUPT1, SUPT1),
	assert_debug(SU1 >= 0),
	%assert_debug(SU1 =< 1.0),
	sqrtdist(BU1,SU1, BU, SU, Res3),
	(SU < SU1
	->	Res21 is BU1 - BU,
		(Res21 < 0
		->	local_format('WARNING:res < 0 first:(~w - ~w)  bid:(~w - ~w)~n',[BU1,SU1,BU,SU]),
			(abs(Res21) < 1e-6
			->	Res2a is abs(Res21)
			;	format('ERROR:~w:res < 0 first:(~w - ~w)  bid:(~w - ~w)~n',[Res21,BU1,SU1,BU,SU]),
				Res2a is abs(Res21)
			)
		;	Res2a = Res21
		),
		BUPT2a = BU1,
		SUPT2a = SU,
		(Res3 =< Res2a
		->	BUPT2 = BU1,
			SUPT2 = SU1,
			Res2 = Res3
		;	BUPT2 = BUPT2a,
			SUPT2 = SUPT2a,
			Res2 = Res2a
		)
	;	BUPT2 = BU1,
		SUPT2 = SU1,
		Res2 = Res3
	),
	(Res2 < Res1
	->	Res = Res2,
		BUPT = BUPT2,
		SUPT = SUPT2
	;	Res = Res1,
		BUPT = BUPT1,
		SUPT = SUPT1
	),
	ens_res(BU, SU, Res, BUPT, SUPT).


get_rest_dist([BU1-SU1,BU2-SU2|Path1], BU, SU, Res1, BUPT1, SUPT1, Res,
	      BUPT, SUPT,Opts) :-
	ens_res(BU, SU, Res1, BUPT1, SUPT1),
	assert_debug((\+ memberchk(norm, Opts);SU1 >= 0)),
	(check_norm_su0(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT1, SUPT1, Res, BUPT, SUPT, Opts)
	->	true
	;	norm_line_dist(BU1, SU1, BU2, SU2, BU, SU, Res2, BUPT2, SUPT2,
			       Opts),
		assert_debug(SU2 >= 0.0, at2),
		(	Res2 < Res1
		->	get_rest_dist([BU2-SU2|Path1], BU, SU, Res2, BUPT2,
				      SUPT2,Res,BUPT, SUPT,Opts)
		;	get_rest_dist([BU2-SU2|Path1], BU, SU, Res1, BUPT1,
				      SUPT1,Res, BUPT, SUPT,Opts)
		)
	).



line_dist(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT, SUPT) :-
	Lambda1 is (BU2 - BU1)*(BU - BU1) + (SU2 - SU1)*(SU - SU1),
	R2 is (BU2 - BU1)*(BU2 - BU1) + (SU2 - SU1)*(SU2 - SU1),
	Lambda is Lambda1/R2,
	(	Lambda < 0
	->	BUPT = BU1,
		SUPT = SU1
	;	Lambda > 1
	->	BUPT = BU2,
		SUPT = SU2
	;	BUPT is BU1 + Lambda*(BU2 - BU1),
		SUPT is SU1 + Lambda*(SU2 - SU1)
	),
	sqrtdist(BUPT,SUPT, BU, SU, Res1).


/*
  r1, r2, r3, r4


          r3
          |
          |
  r1 ---- r4 ----- r2

  r41 = l*r21
  (r3 - r4)*(r2 -r4) = 0
  (r31 - r41)*(r21 - r41) = 0
  (r1 -r4)*(R3 - r4) = 0
  r41*(r31 -r41) = 0
  r21*(r31 - l*r21) = 0
  r21*r31 =l*r21*r21
  l = r21*r31/(r21*r21) = x21*x31 +
  (r31 - l*r21)*(r21 - l*r21) = 0
  */








/*

  *
     *
        *
  +        *
     +
        +
           +
  Distance lines:sqrt(deltaBU, deltaSUZ)

  We calculate vertical distance for buyer utility
  The other distance we calculate difference in intersection with SellerUtilty
  axis. Then:
  */
/*
get_min_distance([], _BU,_SU, OldRes, _UI, OldRes).

get_min_distance([BUD - _SUD - SUZ|DiscreteData], BU,SU,OldRes,UI, Res) :-
	UI = ui(BUMax,_SUBUMax,SUZPrice,BUF,DistFact),
	BUO is (BUMax + BUD),
	Res1 is BU - BUO,
	SUZ1 is SUZPrice + SUZ,
	SUZBid is SU + BU/BUF,
	Res3 is (SUZBid - SUZ1)*DistFact,
	(	Res1 >= 0,
		Res3 =< 0
	->	Res2 = Res1
	;	Res1 < 0,
		Res3 >= 0
	->	Res2 = Res3
	;	Res1 < 0
	->	(abs(Res1) =< abs(Res3)
		->	Res2 = Res1
		;	Res2 = Res3
		)
	;	Res2 = invalid
	),
	min_special(OldRes, Res2, Res4),
	get_min_distance(DiscreteData,BU,SU,Res4,UI, Res).
*/
/*
get_min_distance([du(BUD, SUD, SUZ)|DiscreteData], BU,SU,OldRes,UI, Res) :-
	BU1 is BU - BUD,
	SU1 is SU - SUD,
	get_distance_norm(BU1, SU1, UI, Dist),
	min_special(OldRes, Dist, Res4),
	get_min_distance(DiscreteData,BU,SU,Res4,UI, Res).
*/

/*
                  SUBUMax
                   |  /H  %
      A            | /  %
                   | / %                 /
                90 |/%                   /
********************  90      G         /      ...............BUMax
                  /  *                 /
                 /     *               /
        B        /       *            /       F
                /          *         /
                /            *       /
               /               *    /
               /                 * /
              /                90  *************************    0
              /        C          /% 90
             /                   %/%
             /                 % / %        E
            /                 %  / %
            /               %   /  %
           /               %  D /  %
                                  SUZPrice
*/

sqrtdist(X1,Y1, X2, Y2, R) :-
	R is sqrt((X1-X2)*(X1-X2)+(Y1 -Y2)*(Y1-Y2)).

/*
  C,D distinction: GDist < 0,
  %% line: BUp = SUp*(SUZPrice - SUBUMax)/BUMax + bx
         0 = SUZPrice*(SUZPrice - SUBUMax)/BUMax + bx
  bx = - SUZPrice*(SUZPrice - SUBUMax)/BUMax
  BUp = (SUp - SUZPrice)*(SUZPrice - SUBUMax)/BUMax

  The point perpendicular to the C,G line through BU,SU
  1)line:
         BU1-BU = (SU1-SU)*(SUZPrice - SUBUMax)/BUMax
     must intersect with BU1 = (SUZPrice-SU1)*(BUMax)/(SUZPrice - SUBUMax)
  (SUZPrice-SU1)*(BUMax)/(SUZPrice - SUBUMax) - BU =
            (SU1-SU)*(SUZPrice - SUBUMax)/BUMax
  SU1*((SUZPrice - SUBUMax)/BUMax + (BUMax)/(SUZPrice - SUBUMax)) =
      SUZPrice*(BUMax)/(SUZPrice - SUBUMax) - BU + SU*(SUZPrice - SUBUMax)/BUMax
  SU1 = SUZPrice*(BUMax)/(SUZPrice - SUBUMax) - BU + SU*(SUZPrice - SUBUMax)/BUMax
        --------------------------------------------------------------------------
       ((SUZPrice - SUBUMax)/BUMax + (BUMax)/(SUZPrice - SUBUMax))
       SUZPrice*BUMax^2 - BU*(BUMax*(SUZPrice - SUBUMax) + SU*(SUZPrice - SUBUMax)^2
  SU1 = ----------------------------------------------------------------------------
       (SUZPrice - SUBUMax)^2 + BUMax^2
        
  */
/*
get_distance_norm(BU, SU, UI, Dist) :-
	UI = ui(BUMax,SUBUMax,SUZPrice,BUF,DistFact),
	(BU >= BUMax
	->	(SU =< SUBUMax
		->	% A
			Dist is BU - BUMax
		;	sqrtdist(BU,SU,BUMax, SUBUMax, DistH),
			GDist is (SU + BU*BUF  -SUZPrice)*DistFact,
			FDist is BU,
			((BU - BUMax)/(SU - SUBUMax) > (SUZPrice - SUBUMax)/BUMax
			->	% NOT G
				Dist is min(BU, DistH)
			;	Dist is min(GDist, FDist)
			)
		)
	;	BU =< 0,
		SU >= SUZPrice
	->	%E
		Dist is BU
	;	% not E, not A, not H
		GDist is (SU + BU*BUF  -SUZPrice)*DistFact,
		(GDist > 0
		->	% G or F or E
			(BU =< 0
			->	%E
				Dist = BU
			;	% G or F
				FDist = BU,
				Dist is min(FDist, GDist)
			)
		;	% B or C or D
			((0 - BU)/(SUZPrice - SU) > (SUZPrice - SUBUMax)/BUMax
			->	
		)
	).
*/
min_special(initial, invalid, invalid) :-
	!.
min_special(invalid, invalid, invalid) :-
	!.

min_special(OldRes, NewRes, Res) :-
	(memberchk(OldRes, [initial,invalid])
	->	Res = NewRes
	;NewRes == invalid
	->	Res = OldRes
	;	NewRes =< 0,
		OldRes > 0
	->	Res = NewRes
	;	OldRes < 0,
		NewRes > 0
	->	Res = OldRes
	;abs(NewRes) < abs(OldRes)
	->	Res = NewRes
	;	Res = OldRes
	).


help_curve :-
	format('get_curve(Trace, Options)~n'),
	format('get_curve(Options)~n'),
	format('get_curves(Options)~n'),
	format('Options is [Option1,Option2,..]:~n'),
	format('    normutil:normalise utilities in picture between 0 and 1~n'),
	format('    Lines have kind area, buyer, seller, axis, util1, pareto, distance~n'),
	format('    color(Kind)=color: color of Kind line~n'),
	format('    width(Kind)=int:(default 1) width(pixels) Kind line~n'),
	format('    texture(Kind)=int:(default none)none,dotted,dashed,dashdot.dashdotted,longdash Kind line~n').


defopts([noclip, normutil, width(buyer)=2,width(seller)=2]).


get_curve(Trace) :-
	defopts(Opts),
	get_curve(Trace, Opts).


get_curve(Trace, Opts) :-
	reload_holds,
	get_curve1(Trace, Opts).

get_curve1(Trace,Opts) :-
	(	ensure_pareto_info(Trace, PI),
		get_curve_pi(Trace,PI,Opts)
	->	invalidate_holds
	;	invalidate_holds,
		fail
	).

get_curves :-
	defopts(Opts),
	get_curves(Opts).

get_curves(Opts) :-
	invalidate_holds,
	reload_holds,
	spec:sortdef('TRACE', Traces),
	(	member(Trace, Traces),
		ensure_pareto_info(Trace, PI),
		get_curve_pi(Trace,PI,Opts),
		fail
	;	true
	),
	invalidate_holds.

get_curve :-
	invalidate_holds,
	reload_holds,
	(get_single_trace(TraceName1)
	->	true
	;	error('Missing pareto trace contents, no trace loaded?'),
		invalidate_holds,
		fail
	),
	get_curve(TraceName1).

add_axis_info(P, CoordInfo) :-
	%CoordInfo = ds_ci(_MinUtilCoor, MaxUtilCoor, _, _Opts),
	%MaxUtilCoor = ds_maxu(MaxCBU, MaxCSU),
	add_line(P, CoordInfo, 0, 0, 1.0, 0, axis),
	add_line(P, CoordInfo, 0, 0, 0, 1.0, axis),
	add_line(P, CoordInfo, 1.0, 0, 1.0, 1.0, util1),
	add_line(P, CoordInfo, 0, 1.0, 1.0, 1.0, util1),
	add_extra_axis_info(P, CoordInfo).

add_extra_axis_info(P, CoordInfo) :-
	CoordInfo = ds_ci(_MinUtilCoor, _MaxUtilCoor, _, Opts),
	(memberchk(axisinfo, Opts)
	->	get_point(CoordInfo, 1.0, 0.0, X1, Y1),
		new(T, text('1.0')),
		send(P, display, T, point(X1 - T?width -T?height/3, Y1 - T?height/2)),
		send(P, display, line(X1, Y1, X1, Y1 + 5)),
		get_point(CoordInfo, 0.0, 0.0, X0, Y0),
		new(T0, text('0.0')),
		send(P, display, T0, point(X0 - T0?width -T0?height/3, Y0 - T0?height/2)),
		send(P, display, line(X0, Y0, X0, Y0 + 5)),
		get_point(CoordInfo, 0.9, 0.0, XB, YB),
		new(TB, text('BU')),
		send(TB, font, bold),
		send(P, display, TB, point(XB - TB?width -TB?height, YB - TB?height/2)),
		get_point(CoordInfo, 0.0, 1.0, X01, Y01),
		new(T01, text('1.0')),
		send(P, display, T01, point(X01 - T01?width/2, Y01 + T01?height/2)),
		get_point(CoordInfo, 0.0, 0.0, X00, Y00),
		new(T00, text('0.0')),
		send(P, display, T00, point(X00 - T00?width/2, Y00 + T00?height/2)),
		get_point(CoordInfo, 0.0, 0.9, X09, Y09),
		new(T09, text('SU')),
		send(T09, font, bold),
		send(P, display, T09, point(X09 - T09?width/2, Y09 + T09?height/2))
	;	true
	).

get_curve_pi(Trace,PI,Opts1) :-
	Opts = [normutil|Opts1],
/*	holds:atom_trace(Trace, _Key, bid_utility(Round, BuyerSeller, buyer, BU),
			 Ranges),
	*/
	format('Curve for trace ~w~n', [Trace]),
	MaxCSU =1.1,
	MaxCBU = 1.05,
	MinUtilCoor = ds_minu(0.0,0.0),
	MaxUtilCoor = ds_maxu(MaxCBU, MaxCSU),
	X = 500,
	Y = 500,
	Size = size(X, Y),
	new(P, picture(pareto_area, Size)),
	send(P, popup, @postscript_popup),
	CoordInfo = ds_ci(MinUtilCoor, MaxUtilCoor, ds_pict(X,Y), Opts),
	add_axis_info(P, CoordInfo),
	PI = ds_pi(UI, _Points, Path, NashPt, _BUEpp, DiscreteData),
	uchecklist(get_curve_line(P,CoordInfo, UI), DiscreteData),
	add_envelope(P, PI, CoordInfo),
	unique_trace_manager(M),
	(get(M, trace_info, Trace, TI)
	->	get(TI, file, File)
	;	File = 'UNDEFINED'
	),
	new(S, string('Trace %s File %s', Trace, File)),
	new(T, text(S, @default,bold)),
	get(T, width, W),
	Co is max(0, X/2 - W/2),
	send(P, display, T, point(Co, 0)),
	add_bids(Trace, P, buyer, CoordInfo, Path, NashPt),
	add_bids(Trace, P, seller, CoordInfo, Path, NashPt),
	send(P, open).
/* sorted rising BU */


add_envelope_path([], _P, _PI) :-
	!.
add_envelope_path([_], _P, _PI) :-
	!.
add_envelope_path([BU1-SU1,BU2-SU2|R], P, CoordInfo) :-
	local_format('PT1:~w  - ~w~n', [BU1, SU1]),
	add_line(P, CoordInfo, BU1 , SU1, BU2, SU2, pareto,clip),
	add_envelope_path([BU2-SU2|R], P, CoordInfo).
add_envelope(P, PI, CoordInfo) :-
	PI = ds_pi(_UI, _Points, Path, NashBU-NashSU, BUEPP, _DiscreteData),
	add_envelope_path(Path, P, CoordInfo),
	get_point(CoordInfo, NashBU, NashSU, X1, Y1),
	R is 5,
	send(P, display, new(C,circle(2*R + 1)), point(X1-R,Y1-R)),
	send(C, colour, purple),
	send(C, fill_pattern, colour(purple)),
	get_point(CoordInfo, BUEPP, BUEPP, X2, Y2),
	R2 is R - 2,
	send(P, display, new(C2,circle(2*R2 + 1)), point(X2-R2,Y2-R2)),
	send(C2, colour, orange),
	send(C2, fill_pattern, colour(orange)),
	(	is_local
	->	local_test_nash(NashBU, NashSU, Path, CoordInfo, P)
	;	true
	).




trace_bid_util_point(Trace, BuyerSeller, Round-ds_bid(BUf, SUf)) :-
	holds:atom_trace(Trace, _Key, bid_utility(Round, BuyerSeller, buyer, BU),
			 Ranges),
	memberchk(range(_, _, true), Ranges),
	holds:atom_trace(Trace, _Key2, bid_utility(Round, BuyerSeller, seller,
						   SU),Ranges2),
	maplist(trfloat,[BU,SU],[BUf,SUf]),
	%assert_debug(SUf =< 1.0, trace_bid_util_point),
	memberchk(range(_, _, true), Ranges2).



add_bids(Trace, P, BuyerSeller, CoordInfo, Path, NashPt) :-
	(setof(Bid, trace_bid_util_point(Trace, BuyerSeller, Bid), Bids)
	->	add_bids_list(Bids, P, BuyerSeller, CoordInfo, Path, NashPt)
	;	format('WARNING no bids for ~w~n', [BuyerSeller])
	).
add_bids_list([], _P, BuyerSeller,_CoordInfo, _Path, _NashPt) :-
	format('WARNING no ~w bid~n', [BuyerSeller]),
	!.
add_bids_list([B], P, BuyerSeller,CoordInfo, Path, NashPt) :-
	add_bid_point(B,P, BuyerSeller,CoordInfo, Path, NashPt),
	format('WARNING one ~w bid~n', [BuyerSeller]),
	!.
add_bids_list([B1,B2], P, BuyerSeller,CoordInfo, Path, NashPt) :-
	!,
	add_bid_point(B1,P, BuyerSeller,CoordInfo, Path, NashPt),
	add_bid_line(B1, B2, P, BuyerSeller,CoordInfo),
	add_bid_point(B2,P, BuyerSeller,CoordInfo, Path, NashPt).

add_bids_list([B1,B2|R], P, BuyerSeller,CoordInfo, Path, NashPt) :-
	add_bid_point(B1,P, BuyerSeller,CoordInfo, Path, NashPt),
	add_bid_line(B1, B2, P, BuyerSeller,CoordInfo),
	add_bids_list([B2|R], P, BuyerSeller,CoordInfo, Path, NashPt).
add_bid_line(_R1-ds_bid(BU1,SU1), _R2-ds_bid(BU2,SU2), P, BuyerSeller,CoordInfo) :-
	norm_util_point(BU1,SU1, CoordInfo, BU1a, SU1a),
	norm_util_point(BU2,SU2, CoordInfo, BU2a, SU2a),
	add_line(P, CoordInfo, BU1a, SU1a, BU2a, SU2a, BuyerSeller).
add_bid_point(R1-ds_bid(BU1,SU1), P, BuyerSeller,CoordInfo, Path, NashPt) :-
	norm_util_point(BU1,SU1, CoordInfo, BU1a, SU1a),
	get_point(CoordInfo, BU1a, SU1a, X1, Y1),
	R is 2,
	send(P, display, new(C,circle(2*R + 1)), point(X1-R,Y1-R)),
	send(C, colour, red),
	send(C, fill_pattern, colour(red)),
	get_min_distance(Path, BU1a,SU1a,Res, BUPT, SUPT,[norm]),
	get_nash_distance(NashPt, BU1,SU1,ResN),
	local_format('round ~w ~w:pareto = ~w  nash = ~w~n',
		     [R1, BuyerSeller, Res, ResN]),
	add_line(P, CoordInfo, BU1a, SU1a, BUPT, SUPT, distance).
norm1(U, U1) :-
	(U < 0
	->	U1 = 0.0
	;U >= 1
	->	U1 = 1.0
	;	U1 = U
	).

norm_util_point(BU1,SU1, CoordInfo, BU2, SU2) :-
	(	CoordInfo = ds_ci(_MinUtilCoor, _MaxUtilCoor, _Size, Opts),
		memberchk(normutil,Opts)
	->	norm1(BU1, BU2),
		norm1(SU1, SU2)
	;	BU2 = BU1,
		SU2 = SU1
	).


opt_value(Options, Element, Default, Value) :-
	(	copy_term(Options, Options1),
		memberchk(Element= Value1,Options1)
	->	Value = Value1
	;	member(E, Options),
		atom(E),
		atom_to_term(E, Element = Value1, _)
	->	Value = Value1
	;	Value = Default
	).

	
get_curve_line(Pict, CoordInfo, UI, du(BUD, SUD, _SUZ)) :-
	UI = ui(BUMax, SUA, SUB, _BUF, _DistFact),
	CoordInfo = ds_ci(MinUtilCoor, MaxUtilCoor, _Size, _Opts),
	MinUtilCoor = ds_minu(_BUCMin,SUCMin),
	MaxUtilCoor = ds_maxu(_BUCMax, SUCMax),
	BUMax1 is BUMax + BUD,
	(BUMax1 > 1.0
	->	BUDiff is BUMax1 - 1.0,
		warning('BUMax > 0: 1.0 + ~w', [BUDiff]),
		%assert_debug(BUDiff =< 1.0e-6),
		BUMax11 = 1.0
	;	BUMax11 = BUMax1
	),
	assert_debug(BUMax11 =< 1.0),
	SUA1 is SUA + SUD,
	MM is min(SUCMin, 0),
	add_line(Pict, CoordInfo, BUMax11, MM, BUMax11, SUA1,area, clip),
	SUB1 is SUB + SUD,
	EL is max(SUCMax, SUB1 + 0.1),
	add_line(Pict, CoordInfo, BUD, SUB1, BUD, EL,area, clip),
	add_line(Pict, CoordInfo, BUMax11, SUA1, BUD, SUB1,area, clip).

/* Utility clipping:
   noclip,clip
   */
add_line(Pict, CoordInfo, BU1, SU1, BU2, SU2, Kind) :-
	add_line(Pict, CoordInfo, BU1, SU1, BU2, SU2, Kind, noclip).

add_line(Pict, CoordInfo, BU1, SU1, BU2, SU2, Kind, UtilClip) :-
	get_line_vals(Kind, CoordInfo, Width, Colour, Texture),
	add_line(Pict, CoordInfo, BU1, SU1, BU2, SU2, Colour, Texture, Width,UtilClip).

line_defaults(axis, 1, black, none).
line_defaults(util1, 1, grey, none).
line_defaults(pareto, 2, black, dashed).
line_defaults(buyer, 2, red, none).
line_defaults(distance, 2, blue, none).
line_defaults(area, 1, lightblue, none).
line_defaults(seller, 2, red, none).
line_defaults(buyer, 2, red, none).
get_line_vals(Kind, CoordInfo, Width, Colour, Texture) :-
	CoordInfo = ds_ci(_, _, _,Opts),
	(line_defaults(Kind, Width2, Colour2, Texture2)
	->	true
	;	pr_error('Missing defaults for ~w', [Kind])
	),
	opt_value(Opts, width(Kind), Width2, Width),
	opt_value(Opts, color(Kind), Colour2, Colour),
	opt_value(Opts, texture(Kind), Texture2, Texture).

add_line(Pict, CoordInfo, BU1, SU1, BU2, SU2,Colour,Texture,Width,UtilClip) :-
	(	(	UtilClip==clip,
			CoordInfo = ds_ci(_MinUtilCoor, _MaxUtilCoor, _Size, Opts),
			memberchk(normutil,Opts)
		->	clip_line(1.0,1.0,BU1, SU1, BU2, SU2, BU1a, SU1a, BU2a, SU2a)
		;	BU1a = BU1, SU1a = SU1, BU2a = BU2, SU2a = SU2
		),
		get_point(CoordInfo, BU1a, SU1a, X1, Y1),
		get_point(CoordInfo, BU2a, SU2a, X2, Y2),
		cut_line(CoordInfo, X1, Y1, X2, Y2, X11, Y11, X21, Y21)
	->	new(L, line(X11, Y11, X21, Y21)),
		send(L, colour, Colour),
		send(Pict, display, L),
		(Texture = none
		->	true
		;	send(L, texture, Texture)
		),
		send(L, pen, Width)
	;	true
	).
cut_line(CoordInfo, X1, Y1, X2, Y2, X11, Y11, X21, Y21) :-
	CoordInfo = ds_ci(_MinUtilCoor, _MaxUtilCoor, ds_pict(X,Y),Opts),
	memberchk(clip, Opts),
	!,
	clip_line(X,Y,X1, Y1, X2, Y2, X11, Y11, X21, Y21).
cut_line(_CoordInfo, X1, Y1, X2, Y2, X1, Y1, X2, Y2).
clip_line(X,Y,X1, Y1, X2, Y2, X11, Y11, X21, Y21) :-
	clip_range(x,X, X1, Y1, X2, Y2, X13, Y13, X23, Y23),
	clip_range(y,Y, X13, Y13, X23, Y23, X11, Y11, X21, Y21).

clip_range(y, Y, X1, Y1, X2, Y2, X11, Y11, X21, Y21) :-
	!,
	clip_range(x, Y, Y1, X1, Y2, X2, Y11, X11, Y21, X21).

clip_range(x, X, X1, Y1, X2, Y2, X11, Y11, X21, Y21) :-
	!,
	clip_point(X, X1, Y1, X1, Y1, X2, Y2, X11, Y11),
	clip_point(X, X2, Y2, X1, Y1, X2, Y2, X21, Y21).
clip_point(XM, XP, YP, X1, _Y1, X2, _Y2, XP, YP) :-
	X1 = X2,
	!,
	X1 >= 0,
	X1 < XM.

clip_point(XM, XP, YP, X1, Y1, X2, Y2, X11, Y11) :-
	(XP >= 0
	->	X13 = XP,
		Y13 = YP
	;	X13 = 0,
		% Y11-Y1 = (X11-X1)*(Y2-Y1)/(X2 - X1)
		Y13 is Y1 + (X13-X1)*(Y2-Y1)/(X2 - X1),
		inside_range(Y1, Y2, Y13)
	),
	(XP < XM
	->	X11 = X13,
		Y11 = Y13
	;	X11 = XM,
		Y11 is Y1 + (X11-X1)*(Y2-Y1)/(X2 - X1),
		inside_range(Y1, Y2, Y11)
	).


inside_range(Y1, Y2, Y) :-
	(Y1 =< Y2
	->	Y >= Y1,
		Y =< Y2
	;	Y >= Y2,
		Y =< Y1
	).




get_point(CoordInfo, BU1, SU1, X1, Y1) :-
	CoordInfo = ds_ci(MinUtilCoor, MaxUtilCoor, ds_pict(X,Y),_Opts),
	MinUtilCoor = ds_minu(BUCMin,SUCMin),
	MaxUtilCoor = ds_maxu(BUCMax, SUCMax),
	X1 is X* (SU1 - SUCMin)/(SUCMax - SUCMin),
	Y1 is Y* (1.0 - (BU1 - BUCMin)/(BUCMax - BUCMin)).

/* linesegment */
/* parellel to SU axis:
   result is point with SU furthest from origin

   Decision needed for first point.

   We need the intersection of BU1-SU1,BU2-SU2 and 0,1,1,0
   BU = BU1 + alpha *(SU -SU1)
   intersection with SU = 1,
   BUsu1 = BU1 + (1 - SU1)*(BU2-BU1)/(Su2 - SU1)
   (BUsu1, 1) if > 1 => special if < 0 ??

   intersection with BU = 1:
   SU = SU1 + (1 - BU1)/alpha = SU1 + (1-BU1)*(SU2-SU1)/BU2 - BU1)

   If zwaartelijn:
   Middle between:
   SUbu0 = SU1 + (0 - BU1)/alpha
   BUsu0 = BU1 + alpha*(0 - SU1)

   BUz = 0.5*BUsu0
   SUz = 0.5*SUbuz

   nash_max_rest([BU2-SU2|Path], BU, SU) :
   if SU > 1.0, we proceed until either BU also becomes >= 1.0
   or SU <= 0, then as long as BU < 1.0 proceed unless we have a real maximum.
   If the nash max is not BU2-SU2, we must be in BU < 1.0, SU > 1.0 and we already have a
   local maximum of (BU,SU)

   
   */
get_nash_path_point([BU1-SU1|Path], BU, SU) :-
	assert_debug(BU1 =< 1.0),
	(SU1 >= 1.0
	->	get_nash_path_su_gt1([BU1-SU1|Path], BU, SU)
	;	% first point at BU1, 1.0
		get_nash_path_su_lt1([BU1-SU1|Path], BU1, 1.0, BU, SU)
	).


get_nash_path_su_gt1([BU1-SU1,BU2-SU2|Path], BU, SU):-
	assert_debug(BU2 > BU1),
	assert_debug(SU1 >= 1.0),
	(BU1 >= 1.0
	->	BU = 1.0,
		SU = 1.0
	;	SU2 =< 1.0
	->	BUsu1 is BU1 + (1.0 - SU1)*(BU2-BU1)/(SU2-SU1),
		(BUsu1 >= 1.0
		->	BU = 1.0, SU = 1.0
		;	get_nash_segment_point(BUsu1, 1.0, BU2, SU2, BUN, SUN,
					       Lambda),
			(Lambda < 1.0
			->	BU = BUN,
				SU = SUN
			;	get_nash_path_su_lt1(Path, BU2, SU2, BU, SU)
			)
		)
		% get part of segment SU <= 1.0
		% get local maximum
		% if lambda < 1.0 then that is the point
	;	Path == []
	->	BU = BU2,
		SU = 1.0,
		(BU >= 1.0 - 1e-2
		->	true
		;	warning('BUmax seems too low:~w', [BU])
		)
	;	Path = [BU3-SU3|_],
		assert_debug(BU3 =:= BU2),
		(SU3 >= 1.0
		->	get_nash_path_su_gt1(Path, BU, SU)
		;	get_nash_path_su_lt1(Path, BU3, 1.0, BU, SU)
		)
	).
/*
  The slope is so steep that maximum is to the left, we have
  a maximum candidate of BU2-SU2
  */
get_nash_path_su_lt1([], BU2, SU2, BU2, SU2).

get_nash_path_su_lt1([BU3-SU3,BU1-SU1|Path], BU2, SU2, BU, SU) :-
	flag(gnp, J, J + 1),
	local_inform('GNP:~w~n', [J]),
	(BU1 > 1.0
	->	BUDiff is BU1 - 1.0,
		warning('BU above 1.0: 1.0 + ~w', [BUDiff]),
		assert_debug(BUDiff < 1e-6),
		BU11 = 1.0
	;	BU11 = BU1
	),
	assert_debug(BU11 =< 1.0),
	(is_local,
		BU11 =< BU3
	->	local_inform('SETTING:~w~n', [[BU3-SU3,BU1-SU1]])
	;	true
	),
	assert_debug(BU11 > BU3),
	%assert_debug(SU1 >= 0.0, 'su1 gt 0'),
	(BU11 =:= 1.0
	->	get_nash_segment_point(BU3, SU3, BU11, SU1, BUN, SUN, Lambda),
		(BUN*SUN >= BU2*SU2
		->	BU = BUN,
			SU = SUN
		;	BU = BU2,SU = SU2
		)
	;	get_nash_segment_point(BU3, SU3, BU11, SU1, BUN, SUN, Lambda),
		(BUN*SUN >= BU2*SU2
		->	(Lambda < 1.0
			->	BU = BUN,
				SU = SUN
			;	get_nash_path_su_lt1(Path, BUN, SUN, BU, SU)
			)
		;	get_nash_path_su_lt1(Path, BU2, SU2, BU, SU)
		        % UNCHECKED
		)
	).



	


get_nash_segment_point(BU1, SU1, BU2, SU2, BUN, SUN) :-
	get_nash_segment_point(BU1, SU1, BU2, SU2, BUN, SUN, _Lambda).
get_nash_segment_point(BU1, SU1, BU1, SU2, BU1, SUN, Lambda) :-
	!,
	(	SU1 >= 0,
		SU2 >= 0
	->	SUN is max(SU1, SU2),
		Lambda is 1.0*sign(SU2 - SU1)
	;	pr_error('illegal nash point')
	).
get_nash_segment_point(BU1, SU1, BU2, SU2, BUN, SUN, Lambda) :-
	Alpha is (BU2 - BU1)/(SU2-SU1),
	SUm is SU1 - BU1/Alpha,
	BUm is BU1 + Alpha*(SUm - SU1),
	assert_debug(abs(BUm) < 0.0001),
	SUa is (SUm - 0.0)/2.0,
	BUa is BU1 + Alpha*(SUa - SU1),
	Lambda is (SUa - SU1)/(SU2 - SU1),
	(Lambda < 0
	->	BUN = BU1,
		SUN = SU1
	;Lambda >= 1
	->	BUN = BU2,
		SUN = SU2
	;	BUN = BUa,
		SUN = SUa
	).


/* Nash distance:
   nash_point: BU*SU = maximal

BU - BU1 = alpha*(SU - SU1)
P = BU*SU = (BU1 + alpha*(SU - SU1))*SU
dP/dSU = BU1 + alpha*(SU - SU1) + alpha*SU
BU1 + alpha*(2SU - SU1) = 0
2SUm = SU1 - BU1/alpha

alpha = (BUm - BU1)/(SUM-SU1)

2SUm = SU1 - BU1*(SUm-SU1)/(BUm - BU1)
2BUm = BU1 - SU1*(


   
BU*SU = constant

Nu BU = 0, SU = 0

   Dan is X = BU1 + (BU2 - BU1)*L

Lambda1 is (BU2 - BU1)*(0 - BU1) + (SU2 - SU1)*(0 - SU1) =
            -BU1(BU2 - BU1) -SU1(SU2-SU1)/(BU2 - BU1)*(BU2 - BU1) + (SU2 - SU1)*(SU2 - SU1)
   

   SUp = (SU2 - SU1)*lambda + SU1
   SUp = (SUm - SU1)*{ -BU1(BUm - BU1) -SU1(SUm-SU1)}
       (BU2 - BU1)*(BU2 - BU1) + (SU2 - SU1)*(SU2 - SU1)       +SU1
 SUp = 
   
line_dist(BU1, SU1, BU2, SU2, BU, SU, Res1, BUPT, SUPT) :-
	Lambda1 is (BU2 - BU1)*(BU - BU1) + (SU2 - SU1)*(SU - SU1),
	R2 is (BU2 - BU1)*(BU2 - BU1) + (SU2 - SU1)*(SU2 - SU1),
	Lambda is Lambda1/R2,
	(	Lambda < 0
	->	BUPT = BU1,
		SUPT = SU1
	;	Lambda > 1
	->	BUPT = BU2,
		SUPT = SU2
	;	BUPT is BU1 + Lambda*(BU2 - BU1),
		SUPT is SU1 + Lambda*(SU2 - SU1)
	),
	sqrtdist(BUPT,SUPT, BU, SU, Res1).
*/


/*
  First point is right lower, probably point represents horizontal line
  extending inf horizontal.
  First point defines horizontal line. If BU = SU is on that line then that
  is epp point.
  BU = BU1, SU >= SU1

  BU = SU => BU1 >= SU1

  Determine whether line crosses BU = SU:
  BU >= BU2, SU <= SU2

  BU = BU1 + (SU - SU1)*(BU2 - BU1)/(SU2 - SU1)
  BU = SU
  BU = BU1 + (BU - SU1)*(BU2 - BU1)/(SU2 - SU1)
  BU*(SU2 - SU1) = BU1*(SU2 - SU1) + (BU - SU1)*(BU2 - BU1)

  BU*[SU2 - SU1 - BU2 + BU1] = BU1*(SU2 - SU1) - SU1*(BU2 - BU1)
  BU = (BU1*SU2 - BU1*SU1 - SU1*BU2 + SU1*BU1)/[SU2 - SU1 - BU2 + BU1]
  BU = (BU1*SU2 - SU1*BU2)/[SU2 - SU1 - BU2 + BU1]
  */
get_epp_path_point([BU1-SU1|Path], BU) :-
	assert_debug(BU1 =< 1.0),	
	(BU1 >= SU1
	->	BU = BU1
	;	Path == []
	->	BU = BU1
	;	Path = [BU2-SU2|Path1],
		(BU2  >= SU2
		->	BU is (BU1*SU2 - SU1*BU2)/(SU2 - SU1 - BU2 + BU1)
		;	get_epp_path_point(Path1, BU)
		)
	).


