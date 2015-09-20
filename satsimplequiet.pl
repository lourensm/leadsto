:- module(satsimplequiet,
	  [
	   sats_quiet/1
	  ]).

:- use_module(util).
:- use_module(satgenut).
:- use_module(library(lists)).
:- multifile user:io_level/2.

user:io_level(satsimplequiet, 10).


sats_quiet(Formula) :-
	sats(Formula).

:- ensure_loaded(satmacros).


/*
  All variables have been replaced by prolog variables
  exists(VarName, Var, Sort, Call, Form)
  forall(VarName, Var, Sort, Call, Form)
  and(Form, Form)
  or(Form, Form)
  not(Form)
  trace
  true
  false
  callassign(Var, Sort, Expr)
  holds(S2, F1, TF1) S2 = state(S, M, T, Agent)
  substituted(Denotes, Form)
  cmp(Call)
*/

/* moved to satmacros 
wr_element(Txt, Id, X, Sort, VarName) :-
	  inform(20,'~p:~p NEXT    ~p:~p = ~w~n', [Id, Txt, VarName, Sort,X]).
wr_sat(Txt, Id) :-
	wr_sat(Txt, Id, '').
wr_not_sat(Txt, Id) :-
	wr_sat(Txt, Id, 'NOT').
wr_sat(Txt, Id, NOT) :-
	inform(20,'~p:~p ~p SATISFIED~n',[Id, Txt, NOT]).
*/

sats(true) :-
	!.

sats(false) :-
	!,
	fail.
sats(and(F1, F2)) :-
	!,
	sats(F1),
	sats(F2).
sats(or(F1, F2)) :-
	!,
	(sats(F1)
	->	true
	;	sats(F2)
	).
sats(implies(F, G)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'~p:IMPLIES  ~p  ->  ~p~nFIRST sat(RHS):~n', [Id, F, G]),
	(sats(G)
	->	inform(20,'~p:IMPLIES SATISFIED~n', [Id])
	;	inform(20,'~p:IMPLIES TRY sat(neg(LHS))~n', [Id]),
		(satsneg(F)
		->	inform(20,'~p:IMPLIES SATISFIED~n', [Id])
		;	inform(20,'~p:IMPLIES NOT SATISFIED~n', [Id]),
			fail
		)
	).
sats(equi(F, G)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'~p:EQUIVALENCE  ~p  <->  ~p~nFIRST sat(LHS):~n', [Id,F,G]),
	(sats(F)
	->	inform(20,'~p:EQUIVALENCE  LHS SATISFIED:~n', [Id]),
		(sats(G)
		->	inform(20,'~p:EQUIVALENCE SATISFIED~n', [Id])
		;	inform(20,'~p:EQUIVALENCE NOT SATISFIED~n', [Id]),
			fail
		)
	;	inform(20,'~p:EQUIVALENCE LHS NOT SATISFIED:satneg(RHS)~n',
		       [Id]),
		(satsneg(G)
		->	inform(20,'~p:EQUIVALENCE SATISFIED~n', [Id])
		;	inform(20,'~p:EQUIVALENCE NOT SATISFIED~n', [Id]),
			fail
		)
	).
sats(exists(VarName, Var, Sort, Call, Form)) :-
	!,satsq(exists, VarName, Var, Sort, Call, Form).
sats(call(OrigCall, PLCall)) :-
	!,(call(PLCall)
	->	inform(20, 'CALL ~p succeeded:~p~n', [OrigCall,PLCall])
	;	inform(20, 'CALL ~p failed:~p~n', [OrigCall, PLCall]),
		fail
	).

sats(forall(VarName, Var, Sort, Call, Form)) :-
	!,satsq(forall, VarName, Var, Sort, Call, Form).

sats(not(Form)) :-
	!,satsneg(Form).
sats(trace) :-
	format('Starting trace~n'),
	trace.  %OK:sats(trace) :- trace.
sats(callassign(Var, _Sort, Expr)) :-
	!,Var is Expr.
sats(holds(S2, F1, TF1)) :-
	!,
	(	holds(S2,  F1, TF1)
	->	true
	;	inform(20,'NEED UNFULFILLED:~p~n', [holds(S2,  F1, TF1)]),
		fail
	).
sats(subscode(Code, Form)) :-
	!,(	call(Code)
	->	true
	;	impl_error('arg eval failed')
	),
	sats(Form).

sats(substituted(Denotes, Form)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'SUB ~p : ~p~n', [Id, Denotes]),
	(	sats(Form)
	->	inform(20,'SUB ~p : ~p SATISFIED~n', [Id, Denotes])
	;	inform(20,'SUB ~p : ~p NOT SATISFIED~n', [Id, Denotes]),
		fail
	).
sats(var(Formula, Sort)) :-
	!,inform(20, 'Formula ~w is instantiated variable of sort ~w~n',
	       [Formula, Sort]),
	sats(Formula).
sats(call(Call)) :-
	!,
	(call(Call)
	->	true
	;	inform(20, 'Simple call ~w failed~n', [Call]),
		fail
	).


sats(F) :-
	fatal_error('Unrecognised formula ~p~n', [F]).


satsq(Which, VarName, Var, Sort, Call, Form) :-
	flag(allf, Id, Id + 1),
	inform(20,'~p:~p ~p:~p', [Id, Which, VarName, Sort]),
	(Call == true
	->	true
	;	inform(20,' WHEN ~p', [Call])
	),
	(is_local
	->	(var(Var)
		->	true
		;	format('nonvar qvar ~w~n', [VarName]),
			trace
		)
	;	true
	),
	inform(20,'~n     ~p~n', [Form]),
	(satsq_do(Which, Id, VarName, Var, Sort, Call, Form)
	->	wr_sat(Which, Id)
	;	wr_not_sat(Which, Id),
		fail
	).


satsq_do(Which, Id, VarName, Var, Sort, Call, Form) :-
	allop(Which),
	!,
	forall(sort_element(Sort, Call, Var),
	       (formframe:sync_interrupted
	       ->      !,
		       fail
	       ;       sat_one_s(Id, Which, VarName, Var, Sort, Form)
	       )
	      ).
satsq_do(Which, Id, VarName, Var, Sort, Call, Form) :-
	exop(Which),
	!,
	sort_element(Sort, Call, Var),
	(formframe:sync_interrupted
	->	!,
		fail
	;	sat_one_s(Id, Which, VarName, Var, Sort, Form)
	),
	!.
satsq_do(Which, Id, VarName, Var, Sort, Call, Form) :-
	fatal_error('IMPL ERROR: unrecognised Q:~w',
		   [satsq_do(Which, Id, VarName, Var, Sort, Call, Form)]).

sat_one_s(Id, Which, VarName, Val, Sort, F) :-
	wr_element(Which, Id, Val, Sort, VarName),
	(sats(F)
	->	inform(20,'~p:~p CURRENT ~p:~p = ~p SATISFIED~n',
		       [Id, Which, VarName, Sort, Val])
	;       inform(20,'~p:~p CURRENT ~p:~p = ~p NOT SATISFIED~n',
		       [Id,Which,VarName,Sort, Val]),
		fail
	).


/*********************************/
satsneg(true) :-
	!,
	fail.

satsneg(false) :-
	!.
satsneg(or(F1, F2)) :-
	!,
	satsneg(F1),
	satsneg(F2).
satsneg(and(F1, F2)) :-
	!,
	(satsneg(F1)
	->	true
	;	satsneg(F2)
	).
satsneg(exists(VarName, Var, Sort, Call, Form)) :-
	!,sats(forall(VarName, Var, Sort, Call, not(Form))).
satsneg(forall(VarName, Var, Sort, Call, Form)) :-
	!,sats(exists(VarName, Var, Sort, Call, not(Form))).

satsneg(not(Form)) :-
	!,sats(Form).
satsneg(trace) :-
	!,format('Starting trace~n'),
	trace, % OK:satsneg(trace)
	fail.
satsneg(callassign(Var, Sort, Expr)) :-
	!,fatal_error('Do not like negative assignments yet ~w',[callassign(Var, Sort, Expr)]).


satsneg(holds(S2, F1, TF1)) :-
	!,
	(ground((S2, F1, TF1))
	->	true
	;	fatal_error('~p all arguments should be bound',
		       [not(holds(S2, F1, TF1))])
	),
	!,
	(	(holds(S2, F1, TFU1)
		->	sattfneg(TF1, TFU1)
		;	memberchk(TF1, [true, false])
		->	true
		;	inform(5,'WARNING: subtle neg(holds(..,undefined)) while no entry: is undefined equiv to no entry in ~p? Assume YES~n',
			       [neg(holds(S2, F1, TF1))]),
			fail
		)
	->	true
	;	inform(20,'NEED UNFULFILLED:~p~n', [not(holds(S2,  F1, TF1))]),
		fail
	).
satsneg(substituted(Denotes, Form)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'SUB ~p : ~p~n', [Id, not(Denotes)]),
	(satsneg(Form)
	->	inform(20,'SUB ~p : ~p SATISFIED~n', [Id, not(Denotes)])
	;	inform(20,'SUB ~p : ~p NOT SATISFIED~n', [Id, not(Denotes)]),
		fail
	).
satsneg(subscode(Code, Form)) :-
	!,
	(	call(Code)
	->	true
	;	impl_error('arg eval failed')
	),
	satsneg(Form).
satsneg(equi(F, G)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'~p:NOT EQUIVALENCE  ~p  <->  ~p~nFIRST sat(LHS):~n', [Id, F, G]),
	(sats(F)
	->	inform(20,'~p:NOT EQUIVALENCE  LHS SATISFIED:~n', [Id]),
		(satsneg(G)
		->	inform(20,'~p:NOT EQUIVALENCE SATISFIED~n', [Id])
		;	inform(20,'~p:NOT EQUIVALENCE NOT SATISFIED~n', [Id]),
			fail
		)
	;	inform(20,'~p:EQUIVALENCE LHS NOT SATISFIED:satneg(RHS)~n',
		       [Id]),
		(sats(G)
		->	inform(20,'~p:NOT EQUIVALENCE SATISFIED~n', [Id])
		;	inform(20,'~p:NOT EQUIVALENCE NOT SATISFIED~n', [Id]),
			fail
		)
	).

satsneg(implies(F, G)) :-
	!,flag(allf, Id, Id + 1),
	inform(20,'~p:NOT IMPLIES  ~p  ->  ~p~nFIRST sat(LHS):~n', [Id, F, G]),
	(sats(F)
	->	inform(20,'~p:IMPLIES LHS SATISFIED try not(RHS)~n', [Id]),
		(satsneg(G)
		->	inform(20,'~p:NOT IMPLIES SATISFIED~n', [Id])
		;	inform(20,'~p:NOT IMPLIES NOT SATISFIED~n', [Id]),
			fail
		)
	;	inform(20,'~p:NOT IMPLIES NOT SATISFIED(LHS NOT SATISFIED)~n',
		       [Id]),
		fail
	).

satsneg(var(Formula, Sort)) :-
	!,
	inform(20,
	       'Formula not(~w) is negated instantiated variable of sort ~w~n',
	       [Formula, Sort]),
	satsneg(Formula).
satsneg(call(OrigCall,PlCall)) :-
	!,
	(call(PlCall)
	->	inform(20, 'Simple call not(~w) failed~n', [OrigCall]),
		fail
	;	true
	).
satsneg(call(Call)) :-
	!,
	(call(Call)
	->	inform(20, 'Simple call not(~w) failed~n', [Call]),
		fail
	;	true
	).

satsneg(F) :-
	fatal_error('Unrecognised formula ~p(neg)~n', [not(F)]).













