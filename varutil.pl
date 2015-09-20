:- module(varutil,
	  [
	   padd_q_vars/7,
	   isVarDNewDataS/8,
	   ensureVarDFromVarList/5,
	   isVarDRmS/7,
	   isVar/3,
	   %setupqvars/6,
	   rm_local_vars/3,
	   rm_local_qvars/3,
	   some_var_termSD/6,
	   ensureVarDInVarListS/5,
	   isGround/3,
	   varDInVarListS/5,
	   some_var_termS/5,
	   varDFromVarList/5,
	   varDFromVarListS/6,
	   varDToVarList/5,
	   plvarVarFromVarList/4,
	   isVarThenEnsureAddD/5,
	   isVarThenEnsureAddD/6,
	   is_var_then_ensure_addv/7,
	   varDInVarList/4,
	   ensureVarDInVarList/4,
	   is_var_then_ensure_add/5,
	   is_var_then_ensure_add/4,
	   is_var_then_ensure_add/3,
	   var_in_var_list/3,
	   var_to_var_list/4,
	   var_from_var_list/4,
	   ensure_var_in_var_list/4,
	   same_var_from_var_list/3,
	   ensure_new_var/3,
	   varFromVarList/5,
	   ensureVarFromVarList/4,
	   addVarToVarList/5,
	   addVarToVarListE/6,
	   isVarOccS/6,
	   var_error/1,
	   isolate_qvars/7,
	   isolate_qvars/8
	   ]).
:- use_module(util).
:- use_module(library(lists)).
:- use_module(transnew).
rm_local_vars([], VLIn, VLIn).

rm_local_vars([Var|Vars], VLIn, VLOut) :-
	(varDFromVarListS(VLIn, Var, _, _, VL1, Status)
	->	var(Status),
		rm_local_vars(Vars, VL1, VLOut)
	;	pr_error('Missing local variable ~w', [Var]),
		fail
	).
rm_local_qvars([], VLIn, VLIn).

rm_local_qvars([q(Var, _VarData)|QVars], VLIn, VLOut) :-
	(varDFromVarListS(VLIn, Var, _, _, VL1, Status)
	->	var(Status),
		rm_local_qvars(QVars, VL1, VLOut)
	;	pr_error('Missing local variable ~w', [Var]),
		fail
	).
/* New model: No restrictions on order of quantifier variables,
   conditions will be simply interpreted as extra conditions.

setupqvars2(Vars, Initial, VLIn, VLOut, QVars, Code) :-
	(Vars == []
	->	warning('quantifier without variables'),
		QVars = [],
		VLOut = VLIn
	;	padd_q_varsl(Vars, Initial, VLIn, VLOut, [], QVars, Code)
	).
*/
	
isolate_qvars(Args, ExOp, VLIn, VLOut, QVars, Formula, Code) :-
	isolate_qvars(Args, ExOp, VLIn, VLOut, QVars, _QArgs, initial, Formula,
		      Code).
isolate_qvars(Args, ExOp, VLIn, VLOut, QVars, QArgs, Formula, Code) :-
	isolate_qvars(Args, ExOp, VLIn, VLOut, QVars, QArgs, initial, Formula,
		      Code).

isolate_qvars([Arg|Args], ExOp, VLIn, VLOut, QVars, QArgs, Initial, Formula,
	      Code) :-
	assert_debug(memberchk(ExOp, [exists,forall])),
	(is_list(Arg)
	->	(Arg = [Arg1|Args1]
		->	QArgs = [Arg1],
			padd_q_var(Arg1, Initial, VLIn, VLOut, [], QVars, Code)
		;	VLOut = VLIn,
			QVars = [],
			Code = true,
			QArgs = []
		),
		(Args = [Formula1]
		->	(Args1 == []
			->	Formula = Formula1
			;	Formula =.. [ExOp, Args1, Formula1]
			)
		;	error('Wrong quantifier formula'),
			fail
		)
	;Args == []
	->	warning('quantifier without variables'),
		VLOut = VLIn,
		QVars = [],
		QArgs = [],
		Formula = Arg,
		Code = true
	;	padd_q_var(Arg, Initial, VLIn, VLOut, [], QVars, Code),
		QArgs = [Arg],
		(Args = [Formula]
		->	true
		;	Formula =.. [ExOp|Args]
		)
	).
/* SO
   adds entry q(Var, initial(Initial,Sort1)) to QIn->QOut with
   member(Initial,  [true, 
                     call2(Op1, Op2, CmpTerm11,CmpTerm12),
                     call(Op, CmpTerm1)
                    ])
   */
padd_q_vars([], _Initial, VLIn, VLIn, QIn, QIn, true).

padd_q_vars([RangeArg|RangeArgs], Initial, VLIn, VLOut, QIn, QOut, Code) :-
	padd_q_var(RangeArg, Initial, VLIn, VL1, QIn, Q1, Code1),
	padd_q_vars(RangeArgs, Initial, VL1, VLOut, Q1, QOut, Code2),
	combine_and([Code1, Code2], Code).

	
padd_q_var(RangeArg, Initial, VLIn, VLOut, QIn, [q(Var, Initial)|QIn], Code) :-
	(isVarThenEnsureAddDs(RangeArg, Var, Sort, initial(true,Sort1), VLIn,
			     VLOut,Status)
	->      var(Status),
	        transTerm(Sort, VLIn, Code, Sort1)
	;	iscmpoploc(RangeArg, Call1, LHS, RHS)
	->	(iscmpoploc(RHS, Call2, LHS1, _RHS1)
		->	isVarThenEnsureAddD(LHS1,Var, Sort,
					    initial(call2(Op1, Op2, CmpTerm11,
							  CmpTerm12),Sort1),
					    VLIn, VLOut),
			transTerm(Sort, VLIn, CodeS, Sort1),
			ensure_sort_kinde(Sort1, Kind, Status),
			var(Status),
			Call1 =.. [Op1, CmpTerm1, _T1],
			Call2 =.. [Op2, _T2, CmpTerm2],
			(memberchk(Kind, [integer,real])
			->	transExprs([CmpTerm1,CmpTerm2], VLIn, Code2,
				   [CmpTerm11,CmpTerm12])
			;	error(['multi restriction range only defined ',
				      'for numbers']),
				fail
			),
			combine_and([CodeS, Code2], Code)
		;	isVarThenEnsureAddD(LHS, Var, Sort,
					    initial(call(Op, CmpTerm1), Sort1),
					    VLIn, VLOut),
			transTerm(Sort, VLIn, CodeS, Sort1),
			ensure_sort_kinde(Sort1, Kind, Status),
			var(Status),
			Call1 =.. [Op, _T1, CmpTerm],
			(memberchk(Kind, [integer,real])
			->	transExpr(CmpTerm, VLIn, Code2,CmpTerm1)
			;memberchk(Op, [=,\=])
			->	transTerm(CmpTerm, VLIn, Code2, CmpTerm1)
			;	error(['Operation ~w not defined for',
				       ' non numeric arguments'], [Op]),
				fail
			),
			combine_and([CodeS, Code2], Code)
		)
	;	error('Unrecognised quantifier range ~w(2)', [RangeArg]),
		fail
	).

var_error(Var) :-
	error('Cannot handle variable formulae ~w', [Var]).

isVarDRmS(Term, VLIn, Var, Sort, Data, VLOut, Status) :-
	isVarDRmS(Term, VLIn, Var, Sort, Data, _VLData,_NewData,VLOut,Status).
isVarDNewDataS(Term, VLIn, Var, Sort, Data, NewData, VLData, Status) :-
	isVarDRmS(Term, VLIn, Var, Sort, Data, VLData,NewData,_VLOut,Status).

isVarDRmS(Term, VLIn, Var, Sort, Data,VLData,NewData, VLOut, Status) :-
	var(Term),
	!,
	assert_debug(var(Var)),
	Var = Term,
	ensureVarDFromVarListS(VLIn, VLOut, Term, Sort, Data,VLData,NewData,
			       Status).
isVarDRmS(Var:Sort, VLIn, Var, Sort, Data, VLData,NewData,VLOut, Status) :-
	!,
	ensureVarDFromVarListS(VLIn, VLOut, Var, Sort, Data, VLData,NewData,
			       Status).
isVarDRmS(Term, VLIn, Term, Sort, Data, VLData,NewData,VLOut, Status) :-
	atom(Term),
	varDFromVarListS(VLIn, Term, Sort, Data,VLData,NewData,VLOut, Status),
	!.
isVarOccS(Term, VLIn, Var, Sort, Data, Status) :-
	isVarDRmS(Term, VLIn, Var, Sort, Data, _VLOut, Status).


some_var_termS(VarTerm, VLIn, Var, Sort, Status) :-
	some_var_termSD(VarTerm, VLIn, Var, Sort, _Data, Status).

some_var_termSD(Var, VLIn, Var, Sort, Data, Status) :-
	var(Var),
	!,
	(varDInVarListS(VLIn, Var, Sort,Data,Status)
	->	true
	;	error('Isolated variable ~w',[Var]),
		Status = error('Isolated variable')
	).

some_var_termSD(Var:Sort, VLIn, Var, Sort, Data, Status) :-
	!,
	(varDInVarListS(VLIn, Var, Sort, Data, Status)
	->	true
	;	error('Isolated variable ~w in term', [Var:Sort]),
		Status = error('Isolated variable ~w in term', [Var:Sort])
	).
some_var_termSD(Var, VLIn, Var, Sort, Data, Status) :-
	atom(Var),
	varDInVarListS(VLIn, Var, Sort,Data,Status),
	!.

isGround(Term, VLD, Status) :-
	ground(Term),
	isGround1(Term, VLD, Status).
isGround1(Term, VLD, Status) :-
	some_var_termS(Term,VLD,_,_Sort, Status),
	!,
	fail.
isGround1(Term, _VLD, _Status) :-
	atomic(Term),
	!.
isGround1(Term, VLD, Status) :-
	Term =..[_|Args],
	isGround1L(Args, VLD, Status).
isGround1L([], _VLD, _Status) :-
	!.
isGround1L(_Args, _VLD, Status) :-
	nonvar(Status),
	!.
isGround1L([Arg|Args], VLD, Status) :-
	isGround1(Arg, VLD, Status),
	isGround1L(Args, VLD, Status).


addVarToVarListE(X, Sort, Var, VLIn, [v(X, Sort,Var,_NotInstantiated)|VLIn],
		 Status) :-
	ensure_sort_kinde(Sort, _, Status),
	(is_list(VLIn)
	->	true
	;	fatal_error('addVarToVarList')
	).
addVarToVarList(X, Sort, Var, VLIn, [v(X, Sort,Var,_NotInstantiated)|VLIn]) :-
	ensure_sort_kinde(Sort, _, Status),
	(var(Status)
	->	true
	;	fail
	),
	(is_list(VLIn)
	->	true
	;	fatal_error('addVarToVarList')
	).
varFromVarList(VLIn,VarName,Sort,Var, VLOut) :-
	varFromVarList(VLIn,VarName,Sort,Var, _Inst, VLOut).

varFromVarList([v(VarName1, Sort1, Var1, Inst1)|VLIn],VarName,Sort,Var,
	       Inst, VLIn) :-
	(is_list(VLIn)
	->	true
	;	fatal_error('IMPL ERROR var Varlist?')
	),
	VarName == VarName1,
	!,
	Var = Var1,
	Inst = Inst1,
	Sort = Sort1.
varFromVarList([E1|VLIn],VarName,Sort,Var, Inst, [E1|VLOut]) :-
	varFromVarList(VLIn,VarName,Sort,Var,Inst, VLOut).

ensureVarFromVarList(VarName, Sort, VLIn, TermOut) :-
	(varFromVarList(VLIn, VarName, Sort1, TermOut, _)
	->	(Sort == Sort1
		->	true
		;	fatal_error('IMPL ERROR: expected var ~w  sort ~w, got ~w',
				   [VarName,Sort, Sort1])
		)
	;var(VarName)
	->	fatal_error('IMPL ERROR:Unknown var?')
	;	fatal_error('Variable not bound by quantor: ~w',[VarName:Sort])
	).



warn_duplicate_do(Var, Sort, SortWas) :-
	warning('duplicate quantor range variable ~w:~w (sort was ~w)',
		[Var, Sort,SortWas]),
	flag(warn_duplicate, I, I + 1),
	(I = 0
	->	format(' It seems to be a bad idea to use variable hiding in SAT formulae~n'),
		format(' Even reusing quantifier variable names in subproperties expressions:~n'),
		format(' SAT properties are simply MACROs, not "recursive" "calls"~n')
	;	true
	).
/* var lists as Var:Sort */
warn_duplicate(Var, Sort, VLIn) :-
	var_in_var_list(VLIn, Var, SortWas),!,
	warn_duplicate_do(Var, Sort, SortWas).
warn_duplicate(_X, _Sort, _VLIn).


is_var_then_ensure_add(Term, VLIn, Sort, VLOut) :-
	is_var_then_ensure_add(Term, VLIn, Sort, VLOut, _Var).

is_var_then_ensure_add(X, VLIn, VLOut) :-
	is_var_then_ensure_add(X, VLIn, _Sort, VLOut).

is_var_then_ensure_add(X, VLIn, time, VLOut, X) :-
	var(X),!,
	warn_duplicateD(X, time, VLIn),
	varDToVarList(X, time, qvar, VLIn, VLOut).
is_var_then_ensure_add(X:Sort, VLIn, Sort, VLOut, X) :-
	warn_duplicateD(X, Sort, VLIn),
	varDToVarList(X, Sort, qvar, VLIn, VLOut).

is_var_then_ensure_addv(X, X, Var, VLIn, VLOut, time, Status) :-
	var(X),!,
	warn_duplicatev(X, time, VLIn),
	addVarToVarListE(X, time, Var, VLIn, VLOut, Status).

is_var_then_ensure_addv(X:Sort, X, Var, VLIn, VLOut, Sort,Status) :-
	warn_duplicatev(X, Sort, VLIn),
	addVarToVarListE(X, Sort, Var, VLIn, VLOut,Status).


var_to_var_list(Var, Sort, VIn, [Var:Sort|VIn]).

var_in_var_list([Var1:Sort1|VIn], Var, Sort) :-
	(Var == Var1
	->	(	Sort = Sort1
		->	true
		;	impl_error('Use of var_from_var_list')
		)
	;	var_in_var_list(VIn, Var, Sort)
	),!.
var_in_var_list([Element|_], _, _) :-
	impl_error('Wrong varlist type ~w', [Element]).

var_from_var_list([VE|VIn], Var, Sort, VOut) :-
	var_from_var_list1(VE, VIn, Var, Sort, VOut).
var_from_var_list1(Var1:Sort1, VIn, Var, Sort, VIn) :-
	same_var(Var1, Sort1, Var, Sort),
	!.
var_from_var_list1(VE, VIn, Var, Sort, [VE|VOut]) :-
	assert_debug(VE = _ : _),
	var_from_var_list(VIn, Var, Sort, VOut).

same_varS(Var1, Sort1, Var, Sort, Status) :-
	Var == Var1,
	!,
	(	Sort = Sort1
	->	true
	;	error('Variable ~w:~w conflicts with ~w:~w',
		      [Var,Sort,Var,Sort1]),
		Status = error('Variable ~w:~w conflicts with ~w:~w',
			       [Var,Sort,Var,Sort1])
	).
same_var(Var1, Sort1, Var, Sort) :-
	Var == Var1,
	!,
	(	Sort = Sort1
	->	true
	;	impl_error('Use of var_from_var_list')
	).

ensure_var_in_var_list(VIn, V, Sort, VOut) :-
	(same_var_from_var_list(VIn, V, Sort)
	->	VOut = VIn
	;	var_to_var_list(V, Sort, VIn, VOut)
	).

same_var_from_var_list([Var1:Sort1|VIn], Var, Sort) :-
	!,
	(same_var(Var1, Sort1, Var, Sort)
	->	true
	;	same_var_from_var_list(VIn, Var, Sort)
	).


ensure_new_var(Var, Vars, Msg) :-
	(var_from_var_list(Vars, Var, Sort, _)
	->	fatal_error('Reuse of variable ~w:~w   :~w',
			    [Var, Sort, Msg])
	;	true
	).
ensureVarDInVarListS(VLDIn, Var, Sort, Data, Status) :-
	ensureVarDFromVarListS(VLDIn, _VLDOut, Var, Sort, Data, Status).

ensureVarDFromVarListS(VLDIn, VLDOut, Var, Sort, Data, Status) :-
	ensureVarDFromVarListS(VLDIn, VLDOut, Var, Sort, Data, _VLDData,
			       _NewData, Status).

ensureVarDFromVarListS(VLDIn, VLDOut, Var,Sort,Data,VLDData,NewData,Status) :-
	(varDFromVarListS(VLDIn, Var,Sort,Data,VLDData,NewData,VLDOut,Status)
	->	true
	;	(ground(Sort)
		->	error('Missing variable ~w', [Var:Sort]),
			local_trace(missing_var)
		;	error('Missing variable ~w', [Var])
		),
		Status = missing_variable
	).

ensureVarDInVarList(VLDIn, Var, Sort, Data) :-
	ensureVarDFromVarList(VLDIn, _VLDOut, Var, Sort, Data).

ensureVarDFromVarList(VLDIn, VLDOut, Var, Sort, Data) :-
	(varDFromVarList(VLDIn, Var, Sort, Data, VLDOut)
	->	true
	;	fatal_error('Missing variable ~w', [Var:Sort])
	).
varDInVarList(VLDIn, Var, Sort, Data) :-
	varDFromVarList(VLDIn, Var, Sort, Data, _VLDOut).
varDInVarListS(VLDIn, Var, Sort, Data,Status) :-
	varDFromVarListS(VLDIn, Var, Sort, Data, _VLDOut,Status).


varDFromVarListS(VLDIn, Var, Sort, Data, VLDOut,Status) :-
	varDFromVarListS(VLDIn, Var, Sort, Data, _VLDData, _NewData, VLDOut,Status).




/* varDFromVarListS(VLIn, Var, Sort, Data, VLDData, NewData, VLDOut,Status)
   Data is Data found VLDOut is varlist with entry removed, VLDData is varlist with entry remaining but
   NewData instead of Data as data value
   */
varDFromVarListS([VE|VLDIn], Var, Sort, Data, VLDData, NewData, VLDOut,Status) :-
	varDFromVarList1S(VE, VLDIn, Var, Sort, Data,  VLDData, NewData, VLDOut,Status).
varDFromVarList1S(ds_vd(Var1, Sort1, Data1), VLDIn, Var, Sort, Data,  [ds_vd(Var1, Sort1, NewData)|VLDIn], NewData,
		  VLDIn,Status) :-
	same_varS(Var1, Sort1, Var, Sort, Status),
	!,
	(var(Status)
	->	Data = Data1
	;	true
	).

varDFromVarList1S(VE, VLDIn, Var, Sort, Data, [VE|VLDData],NewData,[VE|VLDOut],
		  Status) :-
	assert_debug(nonvar(VE)),
	assert_debug(VE = ds_vd(_, _, _)),
	varDFromVarListS(VLDIn, Var, Sort, Data,VLDData,NewData,VLDOut,Status).








varDFromVarList([VE|VLDIn], Var, Sort, Data, VLDOut) :-
	varDFromVarList1(VE, VLDIn, Var, Sort, Data, VLDOut).
varDFromVarList1(ds_vd(Var1, Sort1, Data1), VLDIn, Var, Sort, Data, VLDIn) :-
	same_var(Var1, Sort1, Var, Sort),
	!,
	Data = Data1.

varDFromVarList1(VE, VLDIn, Var, Sort, Data, [VE|VLDOut]) :-
	assert_debug(VE = ds_vd(_, _, _)),
	varDFromVarList(VLDIn, Var, Sort, Data, VLDOut).
varDToVarList(Var, Sort, Data, VLDIn, [ds_vd(Var, Sort, Data)|VLDIn]).


isVarThenEnsureAddD(Term, Var, Data, VLIn, VLOut) :-
	isVarThenEnsureAddD(Term, Var, _Sort, Data, VLIn, VLOut).

isVar(Var, Var, time) :-
	var(Var),!.
isVar(VarName:Sort, VarName, Sort).

isVarThenEnsureAddD(Term, VarName, Sort, Data, VLIn, VLOut) :-
	isVarThenEnsureAddDs(Term, VarName, Sort, Data, VLIn, VLOut,Status),
	var(Status).

isVarThenEnsureAddDs(Term, VarName, Sort, Data, VLIn, VLOut,Status) :-
	isVar(Term, VarName, Sort),
	!,
	ensure_sort_kinde(Sort, _, Status),
	warn_duplicateD(VarName, Sort, VLIn),
	varDToVarList(VarName, Sort, Data, VLIn, VLOut).


warn_duplicateD(_Var, _Sort, _VLIn).


warn_duplicatev(Var, Sort, VLIn) :-
	varFromVarList(VLIn, Var, SortWas, _, _),!,
	warn_duplicate_do(Var, Sort, SortWas).
warn_duplicatev(_Var, _Sort, _VLIn).


plvarVarFromVarList([v(VarName1, Sort1,Var1,_Inst)|VLIn],Var,VarName,Sort) :-
	(is_list(VLIn)
	->	true
	;	fatal_error('IMPL ERROR var Varlist?')
	),
	Var == Var1,
	!,
	VarName = VarName1,
	Sort = Sort1.
plvarVarFromVarList([_|VLIn],Var,VarName,Sort) :-
	plvarVarFromVarList(VLIn,Var,VarName,Sort).










