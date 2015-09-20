:- module(transutil,
	  [plVarDFromVL/5,
	   tr_form_den_const/2,
	   addHoldsTerm/6,
	   coded_holds/6,
	   resetcholds6/0,
	   qvars_in_term/6,
	   qvars_in_terms/6,
	   vd_toq/4,
	   vd_fromq/4,
	   intervalTerm/3,
	   holdsbound/2,
	   correct_negated_q_vars/4,
	   getExpr/4
	   ]).

:- use_module(util).
:- use_module(satgenut).
:- use_module(varutil).


number_kind(Val, Kind) :-
	assert_debug(number(Val)),
	(integer(Val)
	->	Kind = integer
	;	Kind = real
	).

holdsbound(pos, bound).
holdsbound(neg, needs_bound).


% tr_form_den_const(FormulaIn, FormulaOut)
%   * remove denotes subformulae
%   * remove constant definitions
%   * normalise quantifiers: exists(Args, SubFormula)
%                            forall(Args, SubFormula)
%   * normalise connectors and, or: only and/2, or/2
%   * remove implication
tr_form_den_const(FormulaIn, FormulaOut) :-
	tr_form_den_const(FormulaIn, [], VLOut, FormulaOut),
	assert_debug(VLOut == []).

tr_form_den_const(Formula, VLIn, _VLOut, _Formula1) :-
	isVarOccS(Formula, VLIn, Var, Sort, _Data,Status),
	!,
	var(Status),
	var_error(Var:Sort),
	fail.
tr_form_den_const(Formula, VLIn, VLOut, Formula1) :-
	spec_constant(Formula, Formula2),
	test_recursive_constant(Formula, Formula2),
	!,
	(tr_form_den_const(Formula2, VLIn, VLOut, Formula1)
	->	free_recursive_constant(Formula)
	;	free_recursive_constant(Formula),
		fail
	).
tr_form_den_const(Formula, VLIn, VLOut, Formula1) :-
	trans_denotes_new(Formula, VLIn, Formula2),
	!,
	(	test_recursive_denotes(Formula),
		tr_form_den_const(Formula2, VLIn, VLOut, Formula1)
	->	free_recursive_denotes(Formula)
	;	free_recursive_denotes(Formula),
		fail
	).
tr_form_den_const(Formula, VLIn, VLIn, Formula) :-
	memberchk(Formula, [true,false]).
tr_form_den_const(not(Formula), VLIn, VLOut, not(Formula1)) :-
	tr_form_den_const(Formula, VLIn, VLOut, Formula1).

tr_form_den_const(Formula,  VLIn, VLOut, Formula1) :-
	tr_formula_code_prolog_f(Formula, PN, Formula2, PN),
	!,
	tr_form_den_const(Formula2, VLIn, VLOut, Formula1).
tr_form_den_const(holds(S, A, TF),  VLIn, VLIn, holds(S, A, TF)) :-
	!.
tr_form_den_const(Formula,  VLIn, VLIn, Formula) :-
	iscmpoploc(Formula, _NumCall,_LHS,RHS),
	!,
	(	iscmpoploc(RHS, _)
	->	error('multirange comparison not supported'),
		fail
	;	true
	).
tr_form_den_const(Formula,  VLIn, VLOut, Formula1) :-
	functor(Formula, F, A),
	isFAsat(F,A),
	A > 0,
	!,
	Formula =.. [_|Args],
	tr_form_den_constFA(F, Args, VLIn, VLOut, Formula1).

tr_form_den_const(Formula,  _VLIn, _VLOut, _Formula1) :-
	error('Unrecognised formula ~w', [Formula]),!,
	fail.
simplify_neg(true, false) :-
	!.
simplify_neg(false, true) :-
	!.
simplify_neg(F, not(F)).


tr_form_den_constFA(F, Args, VLIn, VLOut, Formula1) :-
	memberchk(F, [and,or]),
	!,
	length(Args, L),
	(L = 0
	->	(F == and
		->	Formula1 = true
		;	Formula1 = false
		)
	;L = 1
	->	Args  = [Formula2],
		tr_form_den_const(Formula2, VLIn, VLOut, Formula1)
	;L = 2
	->	Args  = [F1,F2],
		tr_form_den_const(F1, VLIn, VL1, FF1),
		tr_form_den_const(F2, VL1, VLOut, FF2),
		Formula1 =.. [F, FF1, FF2]
	;	Args  = [F1|Args1],
		tr_form_den_const(F1, VLIn, VL1, FF1),
		tr_form_den_constFA(F, Args1, VL1, VLOut, FF2),
		Formula1 =.. [F, FF1, FF2]
	).
tr_form_den_constFA(F, Args, VLIn, VLOut, Formula1) :-
	exallop(F,ExAll1),
	!,
	isolate_qvars(Args, ExAll1, VLIn, VL1, QVars, QArgs, Formula, _Code),
	tr_form_den_const(Formula, VL1, VL2, Formula2),
	Formula1 =.. [ExAll1, QArgs, Formula2],
	rm_local_qvars(QVars, VL2, VLOut).

resetcholds6 :-
	clear_module(cholds),
	retractall(dyn_coded_fact(_, _, _, _,_,_)).

:- dynamic dyn_coded_fact/6.

coded_holds(Holds1, PreBoundVars1,Holds,PreBoundVars,BindingVars,
			CHolds) :-
	dyn_coded_fact(Holds1, PreBoundVars1,Holds,PreBoundVars,BindingVars,
			CHolds).

addHoldsTerm(Holds, PreCode, PostCode, VLPre, VLPost, Code) :-
	free_variables(Holds, FV),
	split_bound_var(FV, PreCode, PostCode, VLPre, VLPost, PreBoundVars,
			BindingVars, TestCode),
	copy_term((Holds, PreBoundVars),
		  (	  Holds1, PreBoundVars1)),
	numbervars(Holds1, 0, _),
	(dyn_coded_fact(Holds1, PreBoundVars1,Holds,PreBoundVars,BindingVars,
			CHolds)
	->	true
	;	compile_holds(Holds1, PreBoundVars1, Holds,
			      PreBoundVars, TestCode,
			      BindingVars,CHolds),
		assertz(dyn_coded_fact(Holds1,PreBoundVars1,Holds,PreBoundVars,
				       BindingVars, CHolds))
	),
	combine_and([PreCode, CHolds, PostCode], Code),
	!.
addHoldsTerm(Holds, PreCode, PostCode, VLIn, CHolds) :-
	pr_error('~w failed',
		 [addHoldsTerm(Holds, PreCode, PostCode, VLIn, CHolds)]),
	fail.

callholds(Holds1, TestCode1) :-
	call(Holds1),
	call(TestCode1).

%compile_holds(Holds1, PreBoundVars1, Holds, PreBoundVars, BindingVars, Code)
% Holds1, PreBoundVars are numbervars versions
compile_holds(Holds1, _, Holds, _, TestCode, _BindingVars,fail) :-
	\+ callholds(Holds,TestCode),
	!,
	warning('holds term ~w always false', [Holds1]).
compile_holds(Holds1, _PreBoundVars1, Holds, PreBoundVars, TestCode, BindingVars,
	      Code) :-
	copy_term((Holds,PreBoundVars, TestCode, BindingVars),
		  (Holds2,PreBoundVars2, TestCode2, BindingVars2)),
	copy_term((Holds, TestCode), (Holds3,TestCode3)),
	callholds(Holds2,TestCode2),
	(	callholds(Holds3,TestCode3),
		Holds3 \= Holds2
	->	fail
	;	(PreBoundVars2 == []
		->	true
		;	warning('Reordering single ~w occurrence would increase performance',
				[Holds1])
		),
		assert_debug(uchecklist(var,PreBoundVars)),
		mk_assignments(PreBoundVars, PreBoundVars2, Code),
		warning('Holds term ~w has only one value ~w(compiled away)',
			[Holds, Holds2]),
		/*
		(BindingVars2 == []
		->	true
		;	warning('Untested code for ~w, please check your result(and warn lourens)',
				[Holds1]),
			trace
		),
		  */
		assert_debug(uchecklist(var,BindingVars)),
		BindingVars = BindingVars2
	).

compile_holds(Holds1, [], Holds, [], TestCode, BindingVars,cholds:Call) :-
	term_to_atom(Holds1, Holds1A),
	concat_atom([Holds1A,'|',[]], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF| BindingVars],
	(	callholds(Holds,TestCode),
		cholds:assertz(Call),
		fail
	;	true
	).

compile_holds(Holds1, PreBoundVars1, Holds, [PreBoundVar], TestCode, BindingVars,
	      (term_to_atom(PreBoundVar, Arg),cholds:Call)) :-
	term_to_atom(Holds1, Holds1A),
	term_to_atom(PreBoundVars1, PreBoundVars1A),
	concat_atom([Holds1A,'|',PreBoundVars1A], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF, Arg| BindingVars],
	(	callholds(Holds,TestCode),
		term_to_atom(PreBoundVar, Arg),
		cholds:assertz(Call),
		fail
	;	true
	).
compile_holds(Holds1, PreBoundVars1, Holds, PreBoundVars, TestCode, BindingVars,
	      (term_to_atom(PreBoundVars, Arg),cholds:Call)) :-
	term_to_atom(Holds1, Holds1A),
	term_to_atom(PreBoundVars1, PreBoundVars1A),
	concat_atom([Holds1A,'|',PreBoundVars1A], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF, Arg| BindingVars],
	(	callholds(Holds, TestCode),
		term_to_atom(PreBoundVars, Arg),
		cholds:assertz(Call),
		fail
	;	true
	).

compile_holds(Holds1, [], Holds, [], TestCode, BindingVars,cholds:Call) :-
	term_to_atom(Holds1, Holds1A),
	concat_atom([Holds1A,'|',[]], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF| BindingVars],
	(	callholds(Holds, TestCode),
		cholds:assertz(Call),
		fail
	;	true
	).
compile_holds(Holds1, PreBoundVars1, Holds, [PreBoundVar], TestCode, BindingVars,
	      (term_to_atom(PreBoundVar, Arg),cholds:Call)) :-
	term_to_atom(Holds1, Holds1A),
	term_to_atom(PreBoundVars1, PreBoundVars1A),
	concat_atom([Holds1A,'|',PreBoundVars1A], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF, Arg| BindingVars],
	(	callholds(Holds,TestCode),
		term_to_atom(PreBoundVar, Arg),
		cholds:assertz(Call),
		fail
	;	true
	).
compile_holds(Holds1, PreBoundVars1, Holds, PreBoundVars, TestCode, BindingVars,
	      (term_to_atom(PreBoundVars, Arg),cholds:Call)) :-
	term_to_atom(Holds1, Holds1A),
	term_to_atom(PreBoundVars1, PreBoundVars1A),
	concat_atom([Holds1A,'|',PreBoundVars1A], Fn),
	concat_atom([Fn,1], NF),
	Call =.. [NF, Arg| BindingVars],
	(	callholds(Holds,TestCode),
		term_to_atom(PreBoundVars, Arg),
		cholds:assertz(Call),
		fail
	;	true
	).
mk_assignments([], [], true) :-
	!.

mk_assignments([Var|Vars], [Val|Vals], Code) :-
	mk_assignments(Vars, Vals, Code1),
	combine_and([Var=Val, Code1], Code).

user:check_holds_type(Var, VarName, Sort) :-
	(check_ground_sort_value(Sort, _Kind, Var)
	->	true
	;	warning('holds value ~w for variable ~w not in ~w sortrange',
			[Var, VarName, Sort]),
		fail
	).

split_bound_var([], _PreCode, _PostCode, _VLPre, _VLPost, [], [], true).
split_bound_var([V1|FV], PreCode, PostCode, VLPre, VLPost, PreBoundVars,
		BindingVars, TestCode) :-
	split_bound_var(FV, PreCode, PostCode, VLPre, VLPost, PreBoundVars1,
			BindingVars1, TestCode1),
	(plVarDFromVL(VLPre, V1, VarName)
	->	ensureVarDInVarList(VLPre, VarName, Sort, Data),
		(Data = bound(PVar1, _Sort1)
		->	assert_debug(PVar1 == V1),
			PreBoundVars = [V1|PreBoundVars1],
			BindingVars = BindingVars1
		;	PreBoundVars = PreBoundVars1,
			BindingVars = [V1|BindingVars1]
		),
		combine_and([user:check_holds_type(V1, VarName, Sort),
			     TestCode1],
			    TestCode)
	;	(plVarDFromVL(VLPost, V1, VarName, Sort, _Data)
		->	PreBoundVars = PreBoundVars1,
			BindingVars = [V1|BindingVars1],
			combine_and([user:check_holds_type(V1, VarName, Sort),
				    TestCode1],TestCode)
		;	var_occurs_in(V1, PreCode)
		->	PreBoundVars = [V1|PreBoundVars1],
			BindingVars = BindingVars1,
			TestCode = TestCode1
		;	var_occurs_in(V1, PostCode)
		->	PreBoundVars = PreBoundVars1,
			BindingVars = [V1|BindingVars1],
			TestCode = TestCode1
		;	pr_error('Missing variable ~w', [V1]),
			local_trace(mies),
			fail
		)
	).


plVarDFromVL(VLIn, PVar, Var) :-
	plVarDFromVL(VLIn, PVar, Var, _Sort, _Data).

plVarDFromVL([ds_vd(Var1, Sort1, Data1)|_VLDIn], PVar, Var, Sort, Data) :-
	Data1 = bound(PVar1, Sort1),
	PVar1 == PVar,
	!,
	Var = Var1,
	Sort = Sort1,
	Data = Data1.

plVarDFromVL([_|VLDIn], PVar, Var, Sort, Data) :-
	plVarDFromVL(VLDIn, PVar, Var, Sort, Data).


/* Subtilities:
   correct_negated_q_vars(PN, QInt1, QIn1, Q3):
   Q1 has hidden variables removed.
   We need to analyse QVars, in QInt1: They should occur in QIn1.
   The set should be identical in a way?
   */
correct_negated_q_vars(pos, QOut1, _QIn1, QOut1).
correct_negated_q_vars(neg, QOut1, QIn1, QOut) :-
	correct_neg_bound(QOut1, QIn1, QOut).
/* Possibly sub analysis might reorder Q QHidden */
correct_neg_bound([], QIn, []) :-
	!,assert_debug(QIn == []).

correct_neg_bound([q(Var, VarData)|QInt1], QIn1, QOut) :-
	!,
	(	vd_fromq(QIn1, Var, VarDataIn, QIn2)
	->	correct_neg_bound_var_data(VarData, VarDataIn, VarDataOut)
	;	impl_error('Unexpected case, emali lourens'),
		correct_neg_bound_var_data(VarData, local, VarDataOut),
		QIn2 = QIn1
	),
	QOut = [q(Var, VarDataOut)|QOut1],
	correct_neg_bound(QInt1, QIn2, QOut1).

correct_neg_bound(QInt1, QIn1, QOut1) :-
	impl_error('Failed ~w', [correct_neg_bound(QInt1, QIn1, QOut1)]).
correct_neg_bound_var_data(needs_bound, VarDataIn, needs_bound) :-
	assert_debug(memberchk(VarDataIn, [needs_bound, initial, bound_lost])).

correct_neg_bound_var_data(initial, VarDataIn, initial) :-
	!,
	assert_debug(VarDataIn == initial).

correct_neg_bound_var_data(VarData, bound, bound) :-
	!,
	assert_debug(VarData == bound).
correct_neg_bound_var_data(bound, initial, needs_bound) :-
	!.

correct_neg_bound_var_data(bound_lost, initial, needs_bound) :-
	!.

correct_neg_bound_var_data(VarData, VarDataIn, VarDataOut) :-
	impl_error('Unimplemented ~w',
		   [correct_neg_bound_var_data(VarData, VarDataIn,
					       VarDataOut)]).
%qvars_in_expr_may_fail(var(Var, Sort, Data, NewData, VLOut), Which, Bound, VLIn, QIn, QOut) :-
%qvars_in_expr_may_fail(number(Term,Kind), Which, Bound, VLIn, QIn, QOut) :-
%qvars_in_expr_may_fail(arop(Ts, TSs, Term1), Which, Bound, VLIn, QIn, QOut) :-
qvars_in_expr_may_fail(case(TTL, ThenExpr, ElseExpr), Which, Bound, VLIn, QIn,
		       QOut) :-
	assert_debug(Which == 1),
	assert_debug(Bound == needs_bound),
	transnew:vars_in_formula(TTL, pos, VLIn, QIn, Q1),
	correct_negated_q_vars(neg, Q1, QIn, QIn1),
	transnew:vars_in_formula(or(ThenExpr<1,ElseExpr<1),pos,VLIn,QIn1,
				  QOut).
qvars_in_expr_may_fail(closure(Ranges, InitialValue, ClosureVar, Function),
		       Which, Bound, VLIn, QIn,QOut) :-
	assert_debug(Which == 1),
	assert_debug(Bound == needs_bound),
	qvars_in_term(InitialValue, Which, needs_bound, VLIn, QIn, Q1),
	(Ranges == []
	->	warning('Closure contains no range variables'),
		QOut = Q1
	;	is_list(Ranges)
	->	qvars_in_closure(Ranges, ClosureVar, Function, Which, VLIn, Q1,
				 QOut)
	;	qvars_in_closure([Ranges], ClosureVar, Function, Which, VLIn,
				 Q1, QOut)
	;	error('Expected range variable or list of range variables, got ~w',
		      Ranges),
		fail
	).

qvars_in_closure(Ranges, ClosureVar, Function, Which, VLIn, Q1, QOut) :-
	transnew:add_q_vars(Ranges, VLIn, VL1, Q1, Q2, [], QH1, _Vars),
	(isVar(ClosureVar, VarName, Sort)
	->	qvars_in_term(Sort, Which, needs_bound, VLIn, Q2, Q3),
		(vd_fromq(Q2, VarName, VarData1, Q3)
		->	(vd_fromq(Q1, VarName, _VarData2, _Q4)
			->	warning('Variable ~w hides previous quantifier variable',
					[Var:Sort]),
				QHidden = [q(Var, VarData1)|QH1]
			;	error('Closure variable should be distinct from range variable'),
				!,
				fail
			)
		;	QHidden = QH1,
			Q3 = Q2
		)
	;	error('Closure Variable is not a variable ~w', [ClosureVar]),
		!,
		fail
	),
	ensure_sort_kinde(Sort, _, Status),
	var(Status),
	varDToVarList(VarName, Sort, bound, VL1, VL2),
	qvars_in_term(Function, Which, needs_bound, VL2, Q3, Q4),
	append(QHidden, Q4, QOut).

%qvars_in_expr_may_fail(interval(Term), Which, Bound, VLIn, QIn, QOut) :-
%qvars_in_expr_may_fail(interval(What, T1), Which, Bound, VLIn, QIn, QOut) :-
%qvars_in_expr_may_fail(background(Term,Data), Which, Bound, VLIn, QIn, QOut) :-
getExpr(Term, VLIn, var(Var, Sort, Data, NewData, VLOut),Status) :-
       isVarDNewDataS(Term, VLIn, Var, Sort, Data, NewData, VLOut,Status),
       !.
getExpr(Term, VLIn, What, Status) :-
       spec_constant(Term, Term2),
       test_recursive_constant(Term, Term2),
       !,
	(getExpr(Term2, VLIn, What, Status)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).
getExpr(Term, _VLIn, number(Term,Kind), _Status) :-
       number(Term),
       number_kind(Term, Kind).
getExpr(last_interval, _VLIn, number(Val,integer), _) :-
        !,
	last_interval(Val).


getExpr(Term, _VLIn, number(_Res,real), _) :-
	warn_epi(Term),
	fail.


getExpr(Term, _VLIn, arop(Ts, TSs, Term1), _) :-
        arop(Term, Ts, TSs, Term1),
	!.
getExpr(interval(Term), _VLIn, interval(Term), _) :-
        !.

getExpr(Term, _VLIn, interval(What, T1), _) :-
        intervalTerm(Term, What, T1),
        !.
getExpr(sum(Ranges, Expr), _VLIn,
	closure(Ranges, 0.0, Variable, Function),_) :-
	!,
	Variable = _NewVar:real,
	Function = Variable + Expr.

getExpr(closure(Ranges, InitialValue, Variable, Function), _VLIn,
	closure(Ranges, InitialValue, Variable, Function),_) :-
	!.

getExpr(case(TTL, Then, Else), _VLIn, case(TTL, Then, Else),_) :-
	!.
getExpr(Term, _, background(Term,Data), _) :-
	background_ar_fn(Term,Data),
	!.

warn_epi(e) :-
	flag(warn_e, I, I + 1),
	I = 0,
	warning('Arithmetic constant "e" is no longer recognised, use exp(1)').
warn_epi(pi) :-
	flag(warn_pi, I, I + 1),
	I = 0,
	warning('Arithmetic constant "pi" is no longer recognised, use 4*atan(1) for pi').

qvars_in_term(Term, Which, Bound, VLIn, QIn, QOut) :-
	getExpr(Term, VLIn, What, Status),
	(var(Status)
	->	qvars_in_expr_may_fail(What, Which, Bound, VLIn, QIn, QOut),
		!
	;	error('Some error in expression'),
		fail
	).

qvars_in_term(Term, Which, Bound, VLIn, QIn, QOut) :-
	some_var_termSD(Term, VLIn, Var, Sort, _Data, Status),
	!,
	(var(Status)
	->	qvar_in_term(Var, Which, Sort, Bound, QIn, QOut)
	;	fail
	).

qvars_in_term(Term, Which, _Bound, VLIn, QIn, QOut) :-
	intervalTerm(Term, _What, I),
	!,
	qvars_in_term(I, Which, needs_bound, VLIn, QIn, QOut).
qvars_in_term(Term, Which, Bound, VLIn, QIn, QOut) :-
	background_ar_fn(Term, _),
	Bound \= needs_bound,
	!,
	qvars_in_term(Term, Which, needs_bound, VLIn, QIn, QOut).


qvars_in_term(Term, Which, Bound, VLIn, QIn, QOut) :-
	Term =.. [_|Terms],
	qvars_in_terms(Terms, Which, Bound, VLIn, QIn, QOut).
qvars_in_terms([], _Which, _Bound, _VLIn, QIn, QIn).

qvars_in_terms([Term|Terms], Which, Bound, VLIn, QIn, QOut) :-
	qvars_in_term(Term, Which, Bound, VLIn, QIn, Q1),
	qvars_in_terms(Terms, Which, Bound, VLIn, Q1, QOut).

/*
  adjust qvar occurrence:
  1)If no entry => no interest
  2)If already bound => possibly add bound/used
  3)
  */
qvar_in_term(Var, Which, Sort, Bound, QIn, QOut) :-
	(Which = 1
	->	transnew:qvar_in_term(Var, Bound, QIn, QOut)
	;Which = 2
	->	transnew2:qvar_in_term(Var, Sort, Bound, QIn, QOut)
	;	impl_error('Missing qvar_in_term(~w) implementation', [Which]),
		fail
	).


vd_toq(Var, VarData, QIn, [q(Var, VarData)|QIn]).

vd_fromq([q(Var1, VarData1)|QIn], Var, VarData, Q1) :-
	(Var1 == Var
	->	Q1 = QIn,
		VarData = VarData1
	;	vd_fromq(QIn, Var, VarData, Q2),
		Q1 = [q(Var1, VarData1)|Q2]
	).

intervalTerm(begin(I), begin, I).
intervalTerm(end(I), end, I).
