:- module(satgenutil,
	  [
	   tr_holds_state1/2,
	   isStateTerm/6,
	   normaliseDenotes/5,
	   normDArgs/4,
	   rmSatVars/3,
	   is_implies/3,
	   generateNextExAllL/4,
	   contains_var/1,
	   is_var/1,
	   holds/3,
	   %wr_sat/2,
	   %wr_not_sat/2,
	   %wr_element/5,
	   sattfneg/2,
	   exop/1,
	   allop/1,
	   exallop/2,
	   isFAsat/2,
	   sort_element/3,
	   generateNextExAll/3,
	   tr_role_io/2,
	   test_recursive_denotes/1,
	   free_recursive_denotes/1,
	   reset_recursive_denotes/0
	  ]).
:- use_module(util).
:- use_module(varutil).

:- use_module(library(lists)).

%:- ensure_loaded(satmacros).

isStateTerm(state(Trace,Time,IOI), Time, [Trace, IOI], Time1,
	    [Trace1, IOI1], state(Trace1,Time1,IOI1)).
isStateTerm(state(Trace,Time), Time, [Trace], Time1,
	    [Trace1], state(Trace1,Time1)).
test_recursive_denotes(Term) :-
	functor(Term, F, A),
	(dynInDenotes(F, A)
	->	error('Recursive application of denotes not allowed:~w',
		      [Term]),
		fail
	;	asserta(dynInDenotes(F, A))
	).
free_recursive_denotes(Term) :-
	functor(Term, F, A),
	retract(dynInDenotes(F, A)).
:- dynamic dynInDenotes/2.
reset_recursive_denotes :-
	retractall(dynInDenotes(_,_)).


:- dynamic holds:holds/3.
:- dynamic holds:interval/3.

:- dynamic holds:last_time/1.
holds(State, Atom, TF) :-
	holds:holds(State, Atom, TF).


not_yet(P) :-
	fatal_error('not implemented yet : ~w~n', [P]).

is_var(X) :-
	assert_debug(ground(X)),
	X = (_ : _).
contains_var(Term) :-
	assert_debug(ground(Term)),
	is_var(Term),
	!.
contains_var(Term) :-
	Term =.. [_|Args],
	member(Arg, Args),
	contains_var(Arg).

is_var(X, X, time) :-
	var(X),!.
is_var(X:Sort, X, Sort) :-
	(	var(X),
		ground(Sort)
	->	true
	;	fatal_error('variable incorrect binding:~w',[X:Sort])
	).

traceLHolds1(Atom, TF, Time, TI) :-
	memberchk(lholds(Atom, TF1, Time), TI),
	!,
	TF = TF1.
traceHolds1(SA,  F, TF, TI) :-
	memberchk(holds(SA,  F, TF1), TI),
	!,
	TF = TF1.
traceSetHolds(pos, SA,  F, TF, TI, [holds(SA,  F, TF)|TI]).
traceSetHolds(neg, SA,  F, TF, TI, [holds(SA,  F, TF1)|TI]) :-
	not_same_tf_tfu(TF, TF1).

not_same_tf_tfu(true, false).
not_same_tf_tfu(true, unknown).
not_same_tf_tfu(false, true).
not_same_tf_tfu(false, unknown).
not_same_tf_tfu(unknown, true).
not_same_tf_tfu(unknown, false).

traceSetHolds1(SA,  F, TF, TI, [holds(SA,  F, TF)|TI]).
traceSetLHolds1(Atom, TF, Time, TI, [lholds(Atom, TF, Time)|TI]).

traceSetLHolds(pos,Atom,TF,Time,LTI,[lholds(Atom,TF,Time)|LTI]) :-
	(ground((Atom,TF,Time))
	->	true
	;	format('Nonground setLHolds~n'),
		trace
	).

traceSetLHolds(neg,Atom,TF,Time, LTI,[lholds(Atom, TF1, Time)|LTI]) :-
	not_same_tf_tfu(TF, TF1).

testUniqueLHolds(Atom, Time, TI) :-
	(memberchk(lholds(Atom, _TF, Time), TI)
	->	warning('NOT understood problem:Multiset~n'),
		local_trace(multiset)
	;	true
	).

traceAddHoldsList([], TI, TI).
traceAddHoldsList([Fact|R], TI, TO) :-
	Fact = holds(SA,  F, TF),
	traceAddHoldsList(R, TI, T1),
	(traceHolds1(SA,  F, TF1, TI)
	->	(TF1 = TF
		->	warning('multiple holds facts ~w', [Fact]),
			TO = T1
		;	fatal_error('conflicting holds facts ~w', [Fact])
		)
	;	traceSetHolds1(SA,  F, TF, T1, TO)
	).


traceAddLHoldsList([], TI, TI).
traceAddLHoldsList([Fact|R], TI, TO) :-
	Fact = lholds(Atom, TF, Time),
	traceAddLHoldsList(R, TI, T1),
	(traceLHolds1(Atom, TF1, Time, TI)
	->	(TF1 = TF
		->	warning('multiple holds facts ~w', [Fact]),
			TO = T1
		;	fatal_error('conflicting holds facts ~w',[Fact])
		)
	;	traceSetLHolds1(Atom, TF, Time, T1, TO)
	).

mkvardata(TI, VL, t(TI, v(VL,_))).
varsFromTI(t(_TI, v(VL,_)), VL).
varsToTI(TI, VL, TO) :-
	varsToTI(TI, _VO, VL, TO).

varsToTI(t(TI, v(VO,R)), VO, VL, t(TI, v(VL,R))).
lholdsToTI(t(t(L,TI),R), TI, TO, t(t(L,TO),R)).
holdsToTI(t(t(TI,L),R), TI, TO, t(t(TO,L),R)).



tiData(t(t(H,LH),v(VB,R)), H, LH, VB, R).

/* Atom, TF, Time must be ground */
satlholdsgenground(PosNeg, Atom, TF, Time, AIn, AOut) :-
	lholdsToTI(AIn, TI, TO, AOut),
	(memberchk(lholds(Atom, TF1, Time), TI)
	->	(PosNeg == pos
		->	TF = TF1
		;	TF \= TF1
		),
		TO = TI
	;	traceSetLHolds(PosNeg, Atom, TF, Time, TI, TO)
	).

satholdsgenground(PosNeg, S, F, TF, AIn, AOut) :-
	tr_holds_state(S, SA),
	holdsToTI(AIn, TI, TO, AOut),
	(memberchk(holds(SA,  F, TF1), TI)
	->	(PosNeg == pos
		->	TF = TF1
		;	TF \= TF1
		)
	;	traceSetHolds(PosNeg, SA,  F, TF, TI, TO)
	).



satlholdsgenset(PosNeg, Atom, TF, Time, TI, TO) :-
	not_yet(satlholdsgenset(PosNeg,Atom, TF, Time, TI, TO)).


/*========================*/

tr_holds_state1(S1, state(S, M, T, IO)) :-
	nonvar(S1),
	S1 = state(S, M, T, IORole),
	nonvar(IORole),
	tr_role_io(IORole, IO),!.

tr_holds_state1(S1, S1).

tr_holds_state(state(S, M, T, IORole), state(S, M, T, IO)) :-
	!,tr_role_io(IORole, IO).
tr_holds_state(State, State).


tr_role_io(input(E), input(A)) :-
	tr_role(E, A).
tr_role_io(output(E), output(A)) :-
	tr_role(E, A).

:- dynamic spec:is_agent/1.
:- dynamic spec:has_role/2.

tr_role(Element, Agent) :-
	(spec:is_agent(Element)
	->	Agent = Element
	;spec:has_role(Agent, Element)
	->	true
	;sort_element1(role(RoleName, Group), true, Element)
	->	fatal_error('no agent fulfills role ~p in group ~p for ~p',
		       [RoleName, Group, Element])
	;	Agent = Element
	).

exop(ex).
exop(exists).
allop(all).
allop(forall).
setdebugging :-
	flag(debuggingl, _, 1).

debuggingl :-
	flag(debuggingl, I, I),
	I > 0.


is_implies(imp(F1, F2), F1, F2).
is_implies(implies(F1, F2), F1, F2).
exallop(ex, exists).
exallop(exists, exists).
exallop(all,forall).
exallop(forall,forall).

sort_element(Sort, Call, Var) :-
	(	var(Var),
		ground(Sort)
	->	true
	;	fatal_error('non var Var:~p:~p', [Var,Sort])
	),
	sort_element1(Sort, Call, Var).

:- dynamic dynwse/2.
checkinsort(Sort, Var, Expr) :-
	(	sort_element(Sort, Var)
	->	true
	;	dynwse(Sort,Var)
	->	true
	;	warning('assignment of ~w:~w = ~w fails because ~w not in sort "~w"',
			[Var,Sort,Expr,Var,Sort]),
		assertz(dynwse(Sort,Var)),
		fail
	).

sort_element1(Sort, (Var1 is Expr), Var) :-
	!,
	(	Var1 == Var,
		ground(Expr)
	->	Var is Expr,
		checkinsort(Sort, Var, Expr)
	;	fatal_error('Unexpected range equality')
	).
sort_element1(Sort, Var1 = Expr, Var) :-
	!,
	(	Var1 == Var,
		ground(Expr)
	->	Expr = Var,
		checkinsort(Sort, Var, Expr)
	;	fatal_error('Unexpected range equality')
	).

sort_element1(Sort, Call, Var) :-
	(memberchk(Sort, [[], time])
	->	holds:last_time(LT),
		between(0,LT,T),
		Var = T
	;Sort = time_range(From, To)
	->	between(From, To, T),
		Var = T
	;	sort_element(Sort, Var)
	),
	(Call == true
	->	true
	;	call(Call)
	).



isFAsat(and, _) :- !.
isFAsat(or, _) :- !.
isFAsat(Op, N) :- exop(Op),!,N > 1.
isFAsat(Op, N) :- allop(Op),!, N > 1.

cmpcopy(Cmp, X1, X2, X1s, X2s, Cmp2) :-
	Cmp =.. [F, X11, X22],
	X11 == X1,
	X22 == X2,
	Cmp2 =.. [F, X1s, X2s],!.

cmpcopy(Cmp, X1, X2, X1s, X2s, Cmp2) :-
	fatal_error('Lawrence: ~w failed',
		   [cmpcopy(Cmp, X1, X2, X1s, X2s, Cmp2)]).

/* langzaam maar helder */
generateNextExAll([F], _ExAll, F) :-
	!.
generateNextExAll(Args, ExAll, F) :-
	F =.. [ExAll|Args].

generateNextExAllL([], R, _ExAll, F) :-
	!,
	(R = [F]
	->	true
	;	fatal_error('Unhandled quantifier construct')
	).

generateNextExAllL(Vars, R, ExAll, F) :-
	F =.. [ExAll, Vars|R].




sattfneg(true, false).
sattfneg(true, undefined).
sattfneg(false, true).
sattfneg(false, undefined).
sattfneg(undefined, true).
sattfneg(undefined, false).



/*
  Remove all formal variables
  */
normaliseDenotes(TermD, Body, VL, DArgsR, Term1) :-
	TermD =.. [_|DArgs],
	normDArgs(DArgs, [], DArgsR, VL),
	rmSatVars(VL, Body, Term1).
/*
	% the second list will be used for q-vars
	rmCallArgsCheckOnlyLocals(Body, VL, [], Term1).
*/
normDArgs([], VL, [], VL).
normDArgs([Arg|Args], VLIn, [NArg|NArgs], VLOut) :-
	normDArg(Arg, VLIn, NArg, VL1),
	normDArgs(Args, VL1, NArgs, VLOut).
normDArg(Arg, VLIn, Var, VLOut) :-
	is_var_then_ensure_addv(Arg, _X, Var, VLIn, VLOut, _SortName, Status),
	!,
	var(Status).

normDArg(Arg, VLIn, Arg, VLIn) :-
	ground(Arg),
	!.
normDArg(Arg, VLIn, Arg, VLIn) :-
	error('denotes should only contain ground or variable parameters, not ~w', [Arg]),
	fail.


rmSatVars(_VL, TermIn, TermIn) :-
	var(TermIn),
	!.
rmSatVars(VL, VarName:SortName, TermOut) :-
	!,
	(	varFromVarList(VL, VarName, Sort1, TermOut, _)
	->	(Sort1 == SortName
		->	true
		;	error('Inconsistent sort name in var:~w -> ~w',
				   [VarName:SortName, Sort1]),
			fail
		)
	;	TermOut = (VarName:SortName)
	).
rmSatVars(VL, VarName, TermOut) :-
	atom(VarName),
	varFromVarList(VL, VarName, _Sort1, TermOut, _),
	!.
rmSatVars(VL, TermIn, TermOut) :-
	TermIn =.. [F|ArgsIn],
	maplist(rmSatVars(VL), ArgsIn, ArgsOut),
	TermOut =.. [F|ArgsOut].