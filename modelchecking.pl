:- module(modelchecking,
	  [
	   set_model_checking_p_rule/6,
	   model_checking_stop/0,
	   model_checking/0,
	   set_state/0,
	   setup_atom_state_boundaries/0
	  ]
	   ).
:- use_module(util).
:- use_module(algo).

model_checking :-
	get_option(modelchecking).

set_state :-
	(model_checking
	->	do_set_state
	;	true
	).

/*
  0: minimal
  > 0 : print identity, print cycle stop
  > 1 : print_states id
  > 2 : print_traces id
  */
print_model_checking(1).


model_checking_stop :-
	model_checking,
	\+ continue_states,
	flag(state_cycle, P, P),
	P > 0,
	!,
	print_model_checking(Level),
	(Level > 0
	->	format('Trace cut off in model_checking mode: CYCLE detected~n')
	;	true
	).

/*
  Up to handled_time rules fired.
  There must be a previous state (or not).
  First check whether current state is available already.
  If not, create new state. States are not necessarily explicitly
  available? If we store handled_time and of all rules that have fired
  store their lambda.
  We may first simplify things by only dealing with e = f. Then fired rules
  need no extra data.
  */
do_set_state :-
	\+ continue_states,
	flag(state_cycle, P, P),
	P > 0,
	!.




do_set_state :-
	print_model_checking(Level),
	flag(state_no, States, States),
	S1 is States - 1,
	(	between(0, S1, I),
		cmp_to_state(I, Result),
		Result == same
	->	(Level > 0
		->	format('State identity ~w - ~w => stop~n',
			       [I, States])
		;	true
		),
		(Level > 2
		->	print_traces
		;	true
		),
		(Level > 1
		->	print_state(I),
			print_state(current)
		;	true
		),
		flag(state_cycle, _, 1),
		(continue_states
		->	flag(state_no, K, K + 1),
			set_new_state(K)
		;	true
		)
	;	flag(state_cycle, P, P),
		P > 0
	->	pr_error('state mismatch'),
		local_trace(where),
		fail
	;	flag(state_no, K, K + 1),
		set_new_state(K)
	).
continue_states :-
	fail.


:- dynamic dyn_state_prop/3, dyn_atom_trace_state/4, dyn_initial_atom_trace/3.

reset_states :-
	retractall(dyn_state_prop(_,_,_)),
	retractall(dyn_atom_trace_state(_AtomKey, _Atoma, _K, _AtomTrace)),
	retractall(dyn_initial_atom_trace(_,_,_)).


set_state_prop(I, Attr, Val) :-
	assert_debug(\+ dyn_state_prop(I, Attr, _)),
	assertz(dyn_state_prop(I, Attr, Val)).
get_state_prop(I, Attr, Val) :-
	(dyn_state_prop(I, Attr, Val)
	->	true
	;	pr_error('Missing state_prop'),
		fail
	).

set_old_future_trace(K, AtomKey, Atoma, AtomTrace) :-
	assert_debug(ground(AtomKey)),
	assert_debug(ground(Atoma)),
	(dyn_atom_trace_state(AtomKey, Atoma, K, _AtomTrace)
	->	pr_error('Duplicate atom_trace_state')
	;	assertz(dyn_atom_trace_state(AtomKey, Atoma, K, AtomTrace))
	).
get_old_future_trace(I, AtomKey, Atoma, AtomTrace) :-
	check_atom_key(AtomKey, Atoma),
	(dyn_atom_trace_state(AtomKey, Atoma, I, AtomTrace)
	->	true
	;	AtomTrace = []
	).
check_atom_key(AtomKey, Atoma) :-
	assert_debug(ground(AtomKey)),
	assert_debug(ground(Atoma)).

unimplemented(X) :-
	format('Unimplemented:~w~n', [X]),
	local_trace(unimplemented),
	pr_error('Unimplemented:~w', [X]),
	fail.



/* First of all, lambda rules, i.e. intervals could influence
   what part of the future may be present and so relevant. But it serves
   probably only the purpose of checking whether atom does not propagate too
   far into the future.
   We need to maintain per atom what has been derived wrt the future!

   Lets be more specific about what is possible: Probably explicitly model
   all start rules. And maintain absolute time until
   T >= max({{L in startrules}:eL + lambdaL(fL - eL) + hL}
   after that only maintain relative time.

   For simplicities sake, lets simply store whole trace as oldfuture.
   */

atom_state_boundaries(AtomKey, Atoma,
		      ds_occ(Intervals,Antecedents,Consequents)) :-
	start_rule_intervals(AtomKey, Atoma, Intervals),
	leadsto_antecedent_occurrences(AtomKey, Atoma, Antecedents),
	leadsto_consequent_occurrences(AtomKey, Atoma, Consequents).
leadsto_antecedent_occurrences(AtomKey, Atoma,
			       ds_ant(Antecedents,PosTime,NegTime)) :-
	setof1(ds_ao(PosNeg, G), rule_ante_pn_g(AtomKey, Atoma, PosNeg, G),
	      Antecedents),
	get_past_max_boundaries(Antecedents, 0, PosTime, 0, NegTime).
get_past_max_boundaries([], PosTime, PosTime, NegTime, NegTime).
get_past_max_boundaries([DsAo|Antecedents], PIn, PosTime, NIn,NegTime) :-
	adjust_past_max(DsAo, PIn, P1, NIn, N1),
	get_past_max_boundaries(Antecedents, P1, PosTime, N1,NegTime).
adjust_past_max(ds_ao(pos, G), PIn, P1, NIn, NIn) :-
	!,
	P1 is max(G, PIn).
adjust_past_max(ds_ao(neg, G), PIn, PIn, NIn, NOut) :-
	!,
	NOut is max(G, NIn).

set_model_checking_p_rule(Atom, TF, T1, T2, P1, VarsInst) :-
	tr_basic_element(Atom, VarsInst, Atom1),
	construct_periodic_rule(Atom1, TF, T1, T2, P1).
construct_periodic_rule(Atom1, TF, T1, T2, P1) :-
	(	lt_rule(_Id, _AnteLits, ConseLits, PVOutC, Delay, RId),
		efgh0(Delay, E, F, _G, H),
		ensure_set_once(ConseLits=ds_cr(CL, ds_ri(RId))),
		atom_in_conse(Atoma, CL, PVOutC, PosNeg),
		pntf(PosNeg, TF)
	->	impl_error('email lourens unimplemented1')
	;	impl_error('email lourens unimplemented2')
	).

rule_ante_pn_g(_AtomKey, Atoma, PosNeg, G) :-
	lt_rule(_Id, AnteLits, _ConseLits, PVOutC, Delay, _RId),
	efgh0(Delay, _E, _F, G, _H),
	atom_in_ante(Atoma, AnteLits, PVOutC, PosNeg).
atom_in_ante(Atoma, AnteLits, PVOutC, PosNeg) :-
	member(DSLitD, AnteLits),
	atom_ds_litd(DSLitD, PVOutC, Atoma, PosNeg).



leadsto_consequent_occurrences(AtomKey, Atoma, Consequents) :-
	setof1(ds_co(PosNeg, E, F, H),
	       rule_conse_pn_efh(AtomKey, Atoma, PosNeg, E, F, H),
	       Consequents).
rule_conse_pn_efh(_AtomKey, Atoma, PosNeg, E, F, H) :-
	lt_rule(_Id, _AnteLits, ConseLits, PVOutC, Delay, RId),
	efgh0(Delay, E, F, _G, H),
	ensure_set_once(ConseLits=ds_cr(CL, ds_ri(RId))),
	atom_in_conse(Atoma, CL, PVOutC, PosNeg).
atom_in_conse(Atoma, ConseLits, PVOutC, PosNeg) :-
	member(DSLitD, ConseLits),
	atom_ds_litd(DSLitD, PVOutC, Atoma, PosNeg).

atom_ds_litd(DSLitD, PVOutC, Atoma, PosNeg) :-
	(	DSLitD = ds_litd(Atom,PN,PreOpsOut,PostOpsOut,PostCondsOut)
	->	true
	;	pr_error('Illegal ds_litd:~w', [DSLitD]),
		fail
	),
	(	PVOutC == [],
		ground(DSLitD),
		PreOpsOut == [],
		PostOpsOut == [],
		PostCondsOut == []
	->	Atom = Atoma,
		PosNeg = PN
	;	Atom \= Atoma
	->	fail
	;	PreOpsOut == [],
		PostOpsOut == [],
		Atom = Atoma,
		ground(PostCondsOut)
	->	(run_ops(PostCondsOut)
		->	PosNeg = PN
		;	warning('atom outside of sort?'),
			fail
		)
	;	unimplemented(atom_ds_litd(DSLitD, PVOutC, Atoma, PosNeg))
	).




setup_atom_state_boundaries :-
	\+ model_checking,!.
setup_atom_state_boundaries :-
	(	atom_trace(AtomKey, Atoma, AtomTrace),
		assertz(dyn_initial_atom_trace(AtomKey, Atoma, AtomTrace)),
		fail
;	true
	).


start_rule_intervals(AtomKey, Atoma, Intervals) :-
	check_atom_key(AtomKey, Atoma),
	(dyn_initial_atom_trace(AtomKey, Atoma, Intervals)
	->	true
	;	Intervals = []
	).

set_new_state(K) :-
	ensure_handled_time(HT),
	set_state_prop(K, ht, HT),
	(	atom_trace(AtomKey, Atoma, AtomTrace),
		atom_state_boundaries(AtomKey, Atoma, Boundaries),
		check_future_ok(AtomTrace, Atoma, HT, Boundaries),
		set_old_future_trace(K, AtomKey, Atoma, AtomTrace),
		fail
	;	true
	).
check_future_ok([], _Atom, _HT, _Boundaries).

check_future_ok([Int|AtomTrace], Atom, HT, Boundaries) :-
	check_future_ok_interval(Int, Atom, HT, Boundaries),
	check_future_ok(AtomTrace, Atom, HT, Boundaries).
check_future_ok_interval(Int, Atom, HT, Boundaries) :-
	(Int = range(Lo, Hi, TV)
	->	(cmp_le(Hi, HT)
		->	true
		;	check_future_ok_interval1(Atom,Lo, Hi, TV, HT, Boundaries)
		)
	;	pr_error('Wrong range')
	).
check_future_ok_interval1(Atom,Lo, Hi,TV,HT,
			  ds_occ(Intervals,_Antecedents,Consequents)) :-
	(memberchk(range(Lo, Hi, TV),Intervals)
	->	true
	;	(   member(Conse, Consequents),
			in_conse_future(Conse, Lo, Hi, TV, HT)
		->	true
		;	is_local
		->	local_trace(test_future),
			warning('Atom ~w future ~w not within boundaries',
				[Atom, range(Lo, Hi, TV)]),
			check_future_ok_interval1(Atom,Lo, Hi,TV,HT,
			  ds_occ(Intervals,_Antecedents,Consequents))
		;	pr_error('Range ~w not within future boundaries',
				 [range(Lo, Hi, TV)])
		)
	).
in_conse_future(Conse, Lo, Hi, TV, HT) :-
	ensure_set_once(Conse = ds_co(PosNeg, _E, F, H)),
	Hi - Lo >= H,
	ensure_set_once(pntf(PosNeg, TF)),
	TF = TV,
	Hi < HT + F + H.



cmp_to_state(I, Result1) :-
	get_state_prop(I, ht, HTI),
	ensure_handled_time(HT),
	(	atom_trace(AtomKey, Atoma, AtomTrace),
		cmp_state_trace(I,HT,AtomKey,Atoma,AtomTrace, HTI, HT, Result),
		(Result \= same
		->	true
		;	fail
		)
	->	Result1 = Result
	;	Result1 = same
	).


print_traces :-
	ensure_handled_time(HT),
	format('Current trace values at htime=~w~n', [HT]),
	AtomNameSize = 45,
	format(' ~w~t~*|  ~w~n', ['Atom', AtomNameSize, 'Intervals']),
	(	atom_trace(_AtomKey, Atoma, AtomTrace),
		print_trace(Atoma, AtomTrace, AtomNameSize),
		fail
	;	true
	).
print_trace(Atoma, AtomTrace, AtomNameSize) :-
	format(' ~w~t~*|  ~w~n', [Atoma, AtomNameSize, AtomTrace]).

print_state(I) :-
	(I == current
	->	ensure_handled_time(HT)
	;	get_state_prop(I, ht, HT)
	),
	format('State ~w, htime=~w~n', [I, HT]),
	AtomNameSize = 37,
	PastFutureTab is AtomNameSize + 20,
	format(' ~w~t~*|  ~w~*|~w~n', ['Atom', AtomNameSize, 'Past(relative)',PastFutureTab,'Future(relative)']),
	(	atom_trace(AtomKey, Atoma, AtomTrace),
		print_atom_state(I, AtomKey, Atoma, AtomTrace, HT, AtomNameSize,PastFutureTab),
		fail
	;	true
	).
print_atom_state(I, AtomKey, Atoma, AtomTrace, HT, AtomNameSize,PastFutureTab) :-
	atom_state_boundaries(AtomKey, Atoma, Boundaries),
	(I == current
	->	FutureTrace = AtomTrace
	;	get_old_future_trace(I, AtomKey, Atoma, FutureTrace)
	),
	default_value(Atoma, DefVal),
	present_past_range_result(AtomTrace, DefVal,HT, Boundaries, PastResult),
	future_range_result(FutureTrace, HT, Boundaries, FutureResult),
	format(' ~w~t~*|  ~w~*|~w~n', [Atoma, AtomNameSize, PastResult,PastFutureTab,
				       FutureResult]).



cmp_state_trace(I, HT, AtomKey, Atoma, AtomTrace, HTI, HT, Result) :-
	assert_debug(var(Result)),
	atom_state_boundaries(AtomKey, Atoma, Boundaries),
	get_old_future_trace(I, AtomKey, Atoma, OldFuture),
	default_value(Atoma, DefVal),
	cmp_state_trace_boundaries(AtomTrace,DefVal,OldFuture,HTI, HT,
				   Boundaries,Result),
	!.
cmp_state_trace(I, HT, AtomKey, Atoma, AtomTrace, HTI, HT, Result) :-
	pr_error('~w failed',[cmp_state_trace(I, HT, AtomKey, Atoma, AtomTrace, HTI, HT,Result)]).

present_past_range_result(AtomTrace, DefVal, HT1, Boundaries, Result) :-
	past_max_boundaries(Boundaries, PosTime, NegTime),
	present_past_range(AtomTrace,DefVal, HT1, Low1, TV1),
	(TV1 == unknown
	->	Result = unknown
	;	(TV1 == true
		->	Time is min(PosTime, Low1)
		;TV1 == false
		->	Time is min(NegTime, Low1)
		;	impl_error(tvf)
		),
		Result = tf(TV1, Time)
	).


/* Find interval at ending at T <= HT1 if present,
   otherwise fill in unknown
   */
present_past_range([],DefVal,HT1, HT1, DefVal) :-
	!.
present_past_range([range(Lo, Hi, TF)|Rest],DefVal,HT1, Low1, TV1) :-
	(	Lo >= HT1
	->	present_past_range(Rest, DefVal,HT1, Low1, TV1)
	;	(Hi >= HT1
		->	Low1 is min(HT1, Hi) - Lo,
			TV1 = TF
		;	TV1 = DefVal,
			Low1 is HT1 - Hi
		)
	).
past_max_boundaries(Boundaries,PosTime, NegTime) :-
	ensure_set_once(Boundaries =ds_occ(_Intervals,Antecedents,_Conses)),
	ensure_set_once(Antecedents = ds_ant(_Occ,PosTime,NegTime)).



cmp_past_result(Result, Result, same) :-
	!.
cmp_past_result(_, _, different).

future_range_result([], _HT1, _Boundaries, []).

future_range_result([range(Lo,Hi, TV)|Ranges], HT1, Boundaries, Result3) :-
	(	Hi =< HT1
	->	future_range_result(Ranges,  HT1, Boundaries, Result3)
	;	Lo1 is max(Lo, HT1) - HT1,
		Hi1 is Hi - HT1,
		Result3 = [range(Lo1,Hi1,TV)|Result4],
		future_range_result(Ranges, HT1, Boundaries, Result4)
	).


cmp_future_result(Result, Result, same) :-
	!.
cmp_future_result(_, _, different).

cmp_state_trace_boundaries(AtomTrace,DefVal,OldFuture,HT1,HT,Boundaries,
			   Result) :-
	present_past_range_result(AtomTrace, DefVal,HT1, Boundaries, Result1),
	present_past_range_result(AtomTrace, DefVal,HT, Boundaries, Result2),
	cmp_past_result(Result1, Result2, Result3),
	(Result3 == same
	->	future_range_result(OldFuture, HT1, Boundaries, Result4),
		future_range_result(AtomTrace, HT, Boundaries, Result5),
		cmp_future_result(Result4, Result5, Result)
	;	Result = Result3
	).
