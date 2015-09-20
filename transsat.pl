:- module(transsat,
	  [
	   resettrans/0,
	   adaptcholds/0,
	   transTermVarsProlog/4
	  ]).

:- use_module(satgenut).

:- use_module(util).
:- use_module(library(lists)).
:- use_module(library(pce)).
:- use_module(varutil).

/*
  <H1>TODO</H1>
  <OL>
  <LI>Test whether exists(x:s1 = y:s1 + 3,formula) is allowed and what it implies.
  <LI>Investigate sorts in terms, but especially in ranges. Allow
  variables in sorts if useful.
  </OL>
  <H1>Development</H1>

  Variable handling during transformation is sloppy:
  We could replace sat variables by prolog variables as soon as possible:
  In a quantor, substitute the variable wherever it occurs. In
  a denotes: replace the formal argument by the actual argument.
  The actual argument should never be a sat variable.
  But, now as soon as we add a variable to the varlist, it should become
  invisible.
  
  Recognizing common simple sub formulae. first inspect strange implies:
  and(implies(i = 0, F0),implies(i = 1, F1))
  strange: in a way complexity could be lower if:
  and(implies(i = 0, and(i = 0, F0))

  and( or(F0,not(i = 0)), or(F1,not(i = 1)))
  or(and(F0, F1), and(F0, not(i = 1)), and(not(i = 0), F1), and(not(i=0),not(i=1)))
  will remain very strange.
  Would the more natural verion be better subject to optimization?
  or(and(i = 0, F0), and(i = 1, F1), and(i = 2, F2), i > 2)
  (Do disjunctions have simple exclusive partial functions?)
  Optimizations:
  Relationships between range variables into quantification.
  Possibly add a construct x:s1 not in list (internally)

  We could compile time sort and, or, implies formulae wrt complexity
  of sub calls.
*/
:- ensure_loaded(satgenut).
/*
  Complexity:
  forall(y < z,forall(x < y,
  complexity would be 1/n
  */
:- ensure_loaded(satmacros).

transTermVarsProlog(Term, VLIn, Term1, CTerm) :-
	transTermVarsPrologC(Term, VLIn, Term1, CTerm, Complexity),
	(get_option(prcompl)
	->	prComplexity(Complexity)
	;	true
	).


prComplexity(Complexity) :-
	sat_format('COMPLEXITY:~w~n', Complexity),
	simplifyComplexity(Complexity, X),
	reorderComplexity(X, X1),
	sat_format('Simplified count:~w~n', [X1]),
	calcComplexity(X1, Y),
	sat_format('basic steps:~w~n', [Y]).

simplifyComplexity(F, F1) :-
	(simplifyComplexity1(F, F2)
	->	simplifyComplexity(F2, F1)
	;	F1 = F
	).

reorderComplexity(I, O) :-
	(reorderComplexity1(I, I1)
	->	reorderComplexity(I1, O)
	;	O = I
	).

reorderComplexity1(A * B, A1 * B) :-
	reorderComplexity1(A, A1).
reorderComplexity1(A * B, A * B1) :-
	reorderComplexity1(B, B1).

reorderComplexity1(A + B, A1 + B) :-
	reorderComplexity1(A, A1).
reorderComplexity1(A + B, A + B1) :-
	reorderComplexity1(B, B1).
reorderComplexity1(A + B, C) :-
	\+ op(A),
	\+ op(B),
	(	number(A),
		number(B)
	->	C is A + B
	;	B @< A,
		C = B + A
	).
reorderComplexity1((A + B) + C, R) :-
	\+ op(B),
	\+ op(C),
	(	number(B),
		number(C)
	->	D is B  + C,
		R = A + D
	;	C  @< B,
		R = A + C + B
	).
reorderComplexity1(A * B, C) :-
	\+ op(A),
	\+ op(B),
	(	number(A),
		number(B)
	->	C is A * B
	;	B @< A,
		C = B * A
	).
reorderComplexity1((A * B) * C, R) :-
	\+ op(B),
	\+ op(C),
	(	number(B),
		number(C)
	->	D is B  * C,
		R = A * D
	;	C  @< B,
		R = A * C * B
	).

op(_*_).
op(_+_).

/*
simplifyComplexity(call, call) :-
	!.
*/
simplifyComplexity1(q(_ExAll, _Sort, _Arg, SubComplexity,Call), Res) :-
	Call = (_ = _),
	!,simplifyComplexity(SubComplexity, Res).
simplifyComplexity1(q(_ExAll, Sort, _Arg, SubComplexity,_Call), Sort*Res1) :-
	!,simplifyComplexity(SubComplexity, Res1).
/*
simplifyComplexity(holds, holds) :-
	!.
*/


simplifyComplexity1(A + B, A1 + B) :-
	simplifyComplexity1(A, A1).
simplifyComplexity1(A + B, A + B1) :-
	simplifyComplexity1(B, B1).
simplifyComplexity1(A * B, A1 * B) :-
	simplifyComplexity1(A, A1).
simplifyComplexity1(A * B, A * B1) :-
	simplifyComplexity1(B, B1).

simplifyComplexity1(A + (B + C), A + B + C).

simplifyComplexity1(A * (B * C), A * B * C).
/*
simplifyComplexity(X, X) :-
	functor(X, F, A),
	format('Not simplified:~w (~w)~n', [X,F/A]).
*/
calcComplexity(X, _) :-
	is_local,
	format('calculating ~w~n', [X]),
	fail.

calcComplexity(N, N) :-
	number(N),
	!.

calcComplexity(call, 2) :-
	!.
calcComplexity(holds, C) :-
	!,
	(is_local
	->	sat_format('    calculating holds size~n')
	;	true
	),
	C = C1,
	flag(holds_size, H, H),
	(H =:= -1
	->	H1 = 0
	;H =:= 0
	->	(flag(holds_size, _, 0),
			holds(_,_,_),
			flag(holds_size, I, I + 1),
			fail
		;	flag(holds_size, H1, H1),
			(H1 =:= 0
			->	flag(holds_size, _, -1)
			;	true
			)
		),
		sat_format('HOLDS SIZE = ~w~n', [H1])
	;	H1 = H
	),
	(H1 =:= 0
	->	C1 = 1
	;	C1 = H1
	).
calcComplexity(X + Y, R) :-
	!,calcComplexity(X, X1),
	calcComplexity(Y, Y1),
	R is X1 + Y1.
calcComplexity(X * Y, R) :-
	!,calcComplexity(X, X1),
	calcComplexity(Y, Y1),
	R is X1 * Y1.
calcComplexity(S, C) :-
	(dynSE(S, C)
	->	!
	;	(	predicate_property(spec:sort_complexity(_,_), interpreted),
			spec:sort_complexity(S, C)
		->	true
		;	ensure_sort_kind(S, _),
			!,
			flag(se, O, 0),
			(	sort_element(S, _),
				flag(se, I, I + 1),
				fail
			;	flag(se, K, O)
			),
			C = K
		),
		assertz(dynSE(S, C)),
			sat_format(' Size of ~w = ~w~n', [S, C])
	).


:- dynamic dynWarned/1.

calcComplexity(S, 0) :-
	(dynWarned(S)
	->	true
	;S == empty
	->	true
	;	warning('Probably empty sort ~w', [S]),
		assertz(dynWarned(S))
	).

:- dynamic dynSE/2.

transTermVarsPrologC(Term, VLIn, TermOut, CTerm, Complexity) :-
	(	transTermVarsProlog0(Term, VLIn, TermOut, CTerm, Complexity)
	->	true
	;	fail,
		fatal_error('Translation of (sub)formula ~w failed',[Term])
	).


checkNotExprKind(Kind) :-
	(Kind = expr(_)
	->	error('Cannot handle expressions here yet'),
		fail
	;	true
	).


transTermVarsProlog0(Term, _VLIn, _Term1, _CTerm, _) :-
	var(Term),
	!,
	error('unexpected variable sub-formula~n'),
	fail.


transTermVarsProlog0(Term:Sort1, VLIn, var(TermOut, Sort),call(TermOut),var) :-
	!,
	warning('you seem to define a variable formula ~w',[Term:Sort1]),
	transExprEvalExpr(Sort1, VLIn, Kind, Code1, Sort),
	(Code1 == []
	->	true
	;	error('Sort Term ~w not allowed here', [Sort1]),
		fail
	),
	checkNotExprKind(Kind),
	!,ensureVarFromVarList(Term, Sort, VLIn, TermOut),
	(memberchk(Sort, [time, 'TIME'])
	->	fatal_error('No variables of sort time allowed as (sub)formula')
	;	true
	).
transTermVarsProlog0(trace, _VLIn, trace, trace, 0) :-
	!.
transTermVarsProlog0(false, _VLIn, false, fail, 1) :-
	!.
transTermVarsProlog0(true, _VLIn, true, true, 1) :-
	!.


transTermVarsProlog0(equi(F, G), VLIn, FG, Code, C) :-
	!,
	(get_option(compile(_))
	->	fatal_error('not implemented equi compile'),
		Code = niks
	%makeEquiCode(F, G, VLIn, FG, Code, C)
	;	transTermVarsPrologC(F, VLIn, F1, _Code1, C1),
		transTermVarsPrologC(G, VLIn, G1, _Code2, C2),
		FG = equi(F1, G1),
		C = C1 + C2
	).

transTermVarsProlog0(imp(F, G), VLIn, Form, Code, C) :-
	!,transImplies(F,G, VLIn, Form, Code, C).
transTermVarsProlog0(implies(F, G), VLIn, Form, Code, C) :-
	!,transImplies(F,G, VLIn, Form, Code, C).


transTermVarsProlog0(not(Term), VLIn, Term2, Code, C):-
	!,transTermVarsPrologC(Term, VLIn, Term1, Code1, C1),
	(get_option(compile(_))
	->	fatal_error('not implemented yet compile not'),
		Code = some_fn(Code1)
	% need to instantiate all vars here
	;	true
	),
	(Term1 == true
	->	Term2 = false,
		C = 1
	;Term1 == false
	->	Term2 = true,
		C = 1
	;	Term2 = not(Term1),
		C = C1 + 1
	).

transTermVarsProlog0(holds(S, F, TF), VLIn, Result,_Code,holds):-
	!,
	transTerms([S, F, TF], VLIn, [], COut, [S1, F1, TF1]),
	tr_holds_state1(S1, S2),
	addHoldsTerm(holds(S2, F1, TF1), COut, Result).

transTermVarsProlog0(lholds(Atom, TF, Time), VLIn, Res, _Code, lholds):-
	!,
	transTerms([Atom, Time, TF], VLIn, [], COut, [Atom1, Time1, TF1]),
	addLHoldsTerm(lholds(Atom1, TF1, Time1), COut, Res).

transTermVarsProlog0(call0(Call), _VLIn, call(Call), _Code, call):-
	!.
transTermVarsProlog0(call(Call), VLIn, call(Call,CallOut2), _Code, call):-
	!,transExpr(Call, VLIn, _, Code1, CallOut),
	append(Code1, [CallOut], CallOut1),
	list_to_commas(CallOut1, CallOut2).

transTermVarsProlog0(Term, VLIn, call(Call), Call, call):-
	(cmpop(Term, _, NegNumCall, OtherCall, _NegOtherCall, X1, X2)
	->	NumCall = Term
	;	cmpop(NumCall,Term,NegNumCall, OtherCall, _NegOtherCall, X1, X2)
	),
	!,
	trace,
	transExpr(X1, VLIn, K1, [], C1, X1s),
	transExpr(X2, VLIn, K2, C1, C2, X2s),
	(	is_number_kind(K1),
		is_number_kind(K2)
	->	(C2 == []
		->	true
		;	impl_error('arithm expr with code')
		), 
		(	NumCall = (_ = _),
			(K1 = expr(_)
			;K2 = expr(_)
			)
		->	Call = (X1s =:= X2s)
		;	functor(NumCall, F, 2),
			Call =.. [F,X1s,X2s]
		)
	;NumCall = (_ = _)
	->	append(C2, [X1s = X2s], Call2),
		list_to_commas(Call2, Call)
	;NumCall = (_ \= _)
	->	append(C2, [X1s \= X2s], Call2),
		list_to_commas(Call2, Call)
	;	error('Cannot compare non numeric vars yet(~w,~w)',
			   [K1, K2]),
		fail
	).








transTermVarsProlog0(TermIn, VLIn, TermOut, Code, C):-
	functor(TermIn, F, A),
	isFAsat(F,A),
	!,
	transFA(F, A, TermIn, VLIn, TermOut, Code, C).
transTermVarsProlog0(TermIn, VLIn, Res, Code, C):-
	transDenotes(TermIn, VLIn, VL1, Code1, Term1),
	!,
	% Ugly hack:
	test_recursive_denotes(TermIn),
	transTermVarsPrologC(Term1, VL1, TermOut, Code, C),
	(Code1 == true
	->	Res = substituted(TermIn,TermOut)
	;	Res = subscode(Code1, substituted(TermIn,TermOut))
	),
	free_recursive_denotes(TermIn).






transTermVarsProlog0(TermIn, _VLIn, _TermOut, _Code, _C):-
	error('Unrecognised element ~w in formula', [TermIn]),!,
	fail.


subsargs(Call, Arg1, Arg2) :-
	(	is_local
	->	(Call =.. [_,B1, B2],
			var(B1),
			var(B2)
		->	true
		;	impl_error('B1B2')
		)
	;	true
	),
	arg(1, Call, Arg1),
	arg(2, Call, Arg2).




/* de denotes moet algemener of gelijk zijn aan de caller of
   zero overlap hebben

transDenotes doet aan vereenvoudigen van expressies. Hierdoor kunnen nieuwe
   variabelen geinitroduceerd worden. Deze kunne dan weer voorkomen in sub formules.
   Ze moeten dus op enige wijze in een output varlist voor gaan komen voor de latere analyse.
*/
transDenotes(TermIn, VLIn, VLOut, Code1, Term1) :-
	functor(TermIn, F, A),
	functor(TermD, F, A),
	flag(den, Old, 0),
	(	denotes(TermD, Body),
		flag(den, N, N + 1),
		normaliseDenotes(TermD, Body, DenotesVL, DArgs, Term1),
		TermIn =.. [F|Args],
		unifyCallHeader(Args, DArgs, VLIn, VLOut, DenotesVL, TermIn, TermD, [], Code),
		(Code == []
		->	Code1 = true
		;	list_to_commas(Code, Code1)
		)
	->	flag(den, _, Old)
	;	flag(den, M, M),
		M > 0,
		numbervars(TermIn, 23,_),
		(M = 1
		->	error('denotes ~w definition does not match for ~w',
			   [F/A, TermIn])
		;	error('~w denotes ~w definitions, none match for ~w',
			   [M, F/A, TermIn])
		),
		fail
	).
list_to_commas([A], A) :-
	!.

list_to_commas([A,B|L], (A, R)):-
	list_to_commas([B|L], R).







rmSatVarFormula(F1, _VarName, _Sort, _Var, _F) :-
	var(F1),
	!,
	error('unexpected variable sub-formula~n').

rmSatVarFormula(F1, VarName, _Sort, _Var, _F) :-
	F1 = VarName,
	!,
	error('unexpected variable sub-formula~n'),
	fail.

rmSatVarFormula(F1, _VarName, _Sort, _Var, F1) :-
	atom(F1),
	!.
rmSatVarFormula(F1, VarName, Sort, Var, F2) :-
	functor(F1, F, A),
	is_simple_rmsat_formula_sub(F, A),
	!,
	F1 =.. [F|Args1],
	maplist(rmSatVarFormulaM(VarName, Sort, Var), Args1, Args2),
	F2 =.. [F|Args2].

rmSatVarFormula(F1, VarName, Sort, Var, F2) :-
	functor(F1, F, A),
	is_simple_rmsat_formula_term(F, A),
	!,
	F1 =.. [F|Args1],
	maplist(rmSatVarTermM(VarName, Sort, Var), Args1, Args2),
	F2 =.. [F|Args2].
rmSatVarFormula(F1, VarName, Sort, Var, F2) :-
	functor(F1, F, A),
	isFAsat(F,A),
	!,
	F1 =.. [ExAll|R],
	assert_debug(exallop(F,_ExAll1)),
	rmSatVarEx(R, VarName, Sort, Var, R2),
	F2 =.. [ExAll|R2].
rmSatVarEx(R, VarName, Sort, Var, R2) :-
	pr_error('~w noot', [rmSatVarEx(R, VarName, Sort, Var, R2)]).

rmSatVarFormulaM(VarName, Sort, Var, F1, F2) :-
	rmSatVarFormula(F1, VarName, Sort, Var, F2).
rmSatVarTermM(VarName, Sort, Var) :-
	pr_error('~w aap', [rmSatVarTermM(VarName, Sort, Var)]).

is_simple_rmsat_formula_sub(F, 0) :-
	memberchk(F, [true,false]).
is_simple_rmsat_formula_sub(F, _) :-
	memberchk(F, [and, or]).
is_simple_rmsat_formula_sub(equi,2).
is_simple_rmsat_formula_sub(imp, 2).
is_simple_rmsat_formula_sub(implies, 2).
is_simple_rmsat_formula_sub(not, 1).

is_simple_rmsat_formula_term(F, A) :-
	cmpop(Term, _, _NegNumCall, _OtherCall, _NegOtherCall, _X1, _X2),
	functor(Term, F, A).
is_simple_rmsat_formula_term(holds, 3).
is_simple_rmsat_formula_term(lholds, 3).
is_simple_rmsat_formula_term(call, 1).



	
/*
rmSatVar(TermIn, VarName, SortName, Var, TermOut) :-
*/
rmSatVar(TermIn, _VarName, _SortName, _Var, TermIn) :-
	var(TermIn),
	!.
rmSatVar(VarName:SortName1, VarName, SortName, Var, Var) :-
	!,
	(	SortName == SortName1
	->	true
	;	error('variable ~w reused with sort ~w',
			   [VarName:SortName1, SortName]),
		fail
	).
rmSatVar(VarName, VarName, _SortName, Var, Var) :-
	!.
rmSatVar(TermIn, VarName, SortName, Var, TermOut) :-
	TermIn =.. [F|ArgsIn],
	maplist(rmSatVar1(VarName, SortName, Var), ArgsIn, ArgsOut),
	TermOut =.. [F|ArgsOut].

rmSatVar1(VarName, SortName, Var, TermIn, TermOut) :-
	rmSatVar(TermIn, VarName, SortName, Var, TermOut).


/*
  Args are actual, DArgs are formal parameters (D=denotes)
unifyCallHeader(Args, DArgs, VLIn, VLOut, DenotesVL, TermIn, TermD, [], Code)
*/
unifyCallHeader([], [], VL, VL, _DVL, _TermIn, _TermD, CodeIn, CodeIn).

unifyCallHeader([Arg|Args], [DArg|DArgs], VL, VLOut, DVL, TermIn, TermD, CodeIn, CodeOut) :-
	unifyArg(Arg, DArg, VL, VL1, DVL, TermIn, TermD, CodeIn, Code1),
	unifyCallHeader(Args, DArgs, VL1, VLOut, DVL, TermIn, TermD, Code1, CodeOut).
ens_id_sortsunifyArgs(Name1, Sort1, Name2, Sort2, TermIn, TermD) :-
	(Sort1 == Sort2
	->	true
	;sort_kind(Sort1, Sort2)
	->	true
	;sort_kind(Sort2, Sort1)
	->	true
	;	sort_kind(Sort1, Kind1),
		sort_kind(Sort2, Kind2),
		Kind1 = Kind2
	->	true
	;	error('only identical type variable allowed in denotes call, not ~w <-> ~w~nActual:~w~nFormal:~w', [Name1:Sort1, Name2:Sort2,TermIn,TermD]),
		fail
	).

/*
is0(X, Y) :-
	(ground(Y)
	->	X is Y
	;	error('expression insufficiently bound ~w',Y),
		fail
	).
*/
ens_id_sorts(VarName, SortName, DVarName, DSortName,TermIn, TermD) :-
	impl_error('Unimplemented:~w',
		   [ens_id_sorts(VarName, SortName, DVarName, DSortName,TermIn, TermD)]).

/*
  the actual parameter is Arg, the formal parameter is DArg
  DArg will become part of the transformed formula, so if a variable remains in a subterm,
  that variable should be in the VarList.
*/
unifyArg(Arg, DArg, VL, VLOut, DVL, TermIn, TermD, CodeIn, CodeOut) :-
	(var(DArg)
	->	plvarVarFromVarList(DVL,DArg,DVarName,DSortName),
		(var(Arg)
		->	plvarVarFromVarList(VL,Arg,VarName,SortName),
			ens_id_sorts(VarName, SortName, DVarName, DSortName,TermIn, TermD),
			Arg = DArg,
			CodeOut = CodeIn,
			VLOut = VL
		;	transExpr(Arg, VL, Kind, CodeIn, Code1, TrArg),
			(memberchk(Kind, [atom, compound])
			->	(sort_element(DSortName, TrArg)
				->	DArg = TrArg,
					CodeOut = Code1,
					VLOut = VL
				;	error('Actual argument ~w does not match formal argument ~w', [Arg, DVarName:DSortName]),
					fail
				)
			;memberchk(Kind, [number, integer])
			->	(sort_element(DSortName, TrArg)
				->	DArg = TrArg,
					CodeOut = Code1,
					VLOut = VL
				;	error('Actual argument ~w does not match formal argument ~w', [Arg, DVarName:DSortName]),
					fail
				)
			;Kind = var(Sort)
			->	ens_id_sorts('Some Var', Sort, DVarName, DSortName, TermIn, TermD),
				DArg = Arg,
				CodeOut = Code1,
				VLOut = VL
			;Kind = expr(ExprSort)
			->	CodeOut = [is(DArg,TrArg)|Code1],
				(varFromVarList(VL, DArg, _SN1, _, _)
				->	fatal_error('IMPL ERROR dupl var')
				;	true
				),
				(	sort_kind(DSortName, Kind1),
					ExprSort == Kind1
				->	true
				;	error('Cannot determine expression sort, cannot be ~w, should be of sort_kind ~w', [DSortName, ExprSort]),
					fail
				),
				addVarToVarList(DArg, DSortName, DArg, VL, VLOut)
			;	impl_error('Unimplemented kind ~2w', [Kind]),
				fail
			)
		)
	;ground(DArg)
	->	(var(Arg)
		->	plvarVarFromVarList(VL,Arg,VarName,SortName),
			error('denotes formal argument ~w must be at least as general as actual argument ~w', [DArg, VarName:SortName]),
			fail
		;ground(Arg)
		->	Arg = DArg, % allow matching although it is ugly
			CodeOut = CodeIn,
			VLOut = VL
		;	impl_error('actual argument should be ground here'),
			fail
		)
	;	error('expected ground or var actual arguments'),
		fail
	).







transImplies(F,G, VLIn, implies(F1, G1), _Code, C) :-
	(get_option(compile(_))
	->	fatal_error('not implemented codeImples')
%		makeImpliesCode(F, G, VLIn, Code, C)
	;	transTermVarsPrologC(F, VLIn, F1, _Code1, C1),
		transTermVarsPrologC(G, VLIn, G1, _Code2, C2),
		C = C1 + C2
	).

checksameVL(VLIn, VLOut, Where) :-
	(VLIn == VLOut
	->	true
	;	fatal_error('Do not understand added vars at ~w', [Where])
	).


%is_number_kind(time).
is_number_kind(var(integer)).
is_number_kind(var(number)).

is_number_kind(expr(number)).
is_number_kind(expr(integer)).

is_number_kind(integer).
is_number_kind(number).



%	fatal_error('cannot handle this term: ~w',[S1]).


	
transFA(AndOr, Arity, TermIn, VLIn, TermOut, Code, C) :-
	memberchk(AndOr, [and, or]),
	Arity > 0,
	!,transAndOr(AndOr, 1, Arity, TermIn, VLIn, TermOut, Code, C).
transFA(ExAll, Arity, TermIn, VLIn, TermOut, Code, C) :-
	!,
	exallop(ExAll,ExAll1),
	Arity > 1,
	!,
	TermIn =.. [ExAll|Args],
	transq(ExAll1,Args, VLIn, TermOut, Code, C).


chkVL(Where, VLIn) :-
	(is_list(VLIn)
	->	true
	;	fatal_error('chkVL(~w):not VL1',[Where])
	).

transq(ExAll, Args, VLIn, TermOut, Code, q(ExAll, Sort, Arg, C1, Call)) :-
	Args = [Arg|_],
	analyse_ex_all_trans(Args, ExAll,VarName,Var,Sort, VLIn, VL1, Call, F),
	TermOut =.. [ExAll, VarName, Var, Sort, Call, Term1],
	Code = some_q_fn(Code1),
	transTermVarsPrologC(F, VL1, Term1, Code1, C1).


transAndOr(AndOr, I, Arity, TermIn, VLIn, TermOut, Code, C) :-
	arg(I, TermIn, ArgIn),
	(I = Arity
	->	transTermVarsPrologC(ArgIn, VLIn, TermOut, Code, C)
	;	I1 is I + 1,
		transTermVarsPrologC(ArgIn, VLIn, TermOut1, Code1, C1),
		(TermOut1 == true
		->	(AndOr == and
			->	transAndOr(AndOr, I1, Arity,TermIn,VLIn,
					   TermOut, Code, C)
			;	TermOut = true,
				Code = true,
				C = 0
			)
		;TermOut1 == false  
		->	(AndOr == or
			->	transAndOr(AndOr, I1, Arity,TermIn,VLIn,
					   TermOut, Code, C)
			;	TermOut = false,
				C = 0,
				Code = fail	
			)
		;	transAndOr(AndOr,I1,Arity,TermIn,VLIn,TermOut2,Code2,
				   C2),
			(TermOut2 == true
			->	(AndOr == and
				->	TermOut = TermOut1,
					C = C1,
					Code = Code1
				;	TermOut = true,
					Code = true,
					C = 0
				)
			;TermOut2 == false
			->	(AndOr == or
				->	TermOut = TermOut1,
					Code = Code1,
					C = C1
				;	TermOut = false,
					Code = fail,
					C = 0
				)
			;	functor(TermOut, AndOr, 2),
				arg(1, TermOut, TermOut1),
				arg(2, TermOut, TermOut2),
				memberchk(c(AndOr, Prolog),[c(and, ','),
							 c(or, ';')]),
				Code =.. [Prolog, Code1, Code2],
				C = C1 + C2
			)
		)
			
	).
/*
is_var_then_add(X, Var, VLIn, VLOut, SortName) :-
	var(X),
	!,(varFromVarList(VLIn, X, _SN1, Var, _)
	->	!,fail
	;	!,addVarToVarList(X, SortName, Var, VLIn, VLOut)
	).
is_var_then_add(X:SortName, Var, VLIn, VLOut, SortName) :-
	(	varFromVarList(VLIn, X, _SN1, Var, _)
	->	!,fail
	;	!,addVarToVarList(X, SortName, Var, VLIn, VLOut)
	).
*/

/*
is_var(X:Sort, X, VL, Sort1) :-
	(	var(X),
		ground(Sort)
	->	Sort = Sort1
	;ground(Sort)
	->	Sort = Sort1
	;	transExpr(Sort, VL, SKind, SCode, Sort1),
		(	SCode == [],
			memberchk(SKind, [atom, compound])
		->	true
		;	fatal_error('Did not expect complex sort term ~w', [Sort])
		),
		ground(Sort1)
	->	true
	;	is_var(X:Sort, X, VL, Sort1),
		fatal_error('variable incorrect binding:~w',[X:Sort])
	).
*/

analyse_ea_var(X, VarName, Var, VLIn, VLOut, Sort, Call) :-
	(	is_var_then_ensure_addv(X, VarName, Var, VLIn, VLOut, Sort,
				       Status)
	->	Call = true,
		var(Status)
	;	iscmpoploc(X, Call1),
		arg(1, X, X1),
		is_var_then_ensure_addv(X1, VarName, Var, VLIn, VLOut, Sort, Status)
	->	var(Status),
		(	Call1 =.. [Op, X2, X3],
			X2 == X1
		->	rmSatVar(X3, VarName, Sort, Var, X33),
			transExpr(X33, VLIn, K1, Code1, X4),
			(	Op = =,
				K1 = expr(_Number)
			->	Call2 = (Var is X4),
				(Code1 == []
				->	true
				;	warning('complex equality will not be optimized:~w',
						 [X])
				)
			;	Call2 =.. [Op, Var, X4]
			),
			append(Code1, [Call2], Code2),
			list_to_commas(Code2, Call)
		;	impl_error('IMPL ERROR aea'),
			fail
		)
	;	error('expected range of quantor, got:~w',[X]),
		fail
	).
analyse_ex_all_trans([X|R], ExAll, VarName, Var, Sort, VLIn, VLOut, Call, F) :-
	(is_list(X)
	->	(X == []
		->	warning('quantifier without variables'),
			generateNextExAll(R, ExAll, F1)
		;	X = [X1|X2],
			analyse_ea_var(X1,VarName, Var, VLIn, VLOut,Sort,Call),
			generateNextExAllL(X2, R, ExAll, F1)
		)
	;	analyse_ea_var(X, VarName, Var, VLIn, VLOut, Sort, Call),
		generateNextExAll(R, ExAll, F1)
	),
	rmSatVar(F1, VarName, Sort, Var, F).






% Let user do this : sort_kind(time, integer).

lsort_kind(Sort, Kind) :-
	(sort_kind(Sort, Kind)
	->	true
	;	Kind = Sort
	).

/*
  transExpr(Term, VLIn, ResultKind, Code, TermOut)
  transform a term. The term may be an argument of a holds or denotes
  actual argument list. Variables are replaced by their prolog equivalent.
  Internal arithmetic expressions are replaced by a variable and code
  is generated for the evaluation. A top level arithmetic expression is not
  replaced by code.
  Kind:
  var(Kind) where Kind is either a SortName or if lsort_kind is defined a kind
  expr(Kind)
  integer
  number
  atom
  
  */
transExprEvalExpr(Term, VLIn, ResultKind, Code, TermOut) :-
	transExpr(Term, VLIn, Kind1, Code1, TermOut1),
	(Kind1 = expr(ResultKind)
	->	Code = [TermOut is TermOut1|Code1]
	;	Code = Code1,
		TermOut = TermOut1
	).
transExpr(Term, VLIn, Kind, Code, TermOut) :-
	transExpr(Term, VLIn, Kind, [], Code, TermOut).

transExpr(Term, VLIn, var(Kind), CIn, CIn, Term) :-
	var(Term),
	!,
	(plvarVarFromVarList(VLIn,Term,_VarName,SortName)
	->	lsort_kind(SortName, Kind)
	;	impl_error('unsupported var')
	).
transExpr(Term:Sort1, VLIn, var(Kind), CIn, CIn, _TermOut) :-
	!,
	(atom(Sort1)
	->	true
	;arop(Sort1, _, _, _)
	->	warning('Possibly forgot to put () around var:sort in ~w',
		       [Term:Sort1])
	;	true
	),
	% Below is plainly wrong: useless here
	transExpr(Sort1, VLIn, SKind, SCode, Sort),
	(	SCode == [],
		memberchk(SKind, [atom, compound])
	->	true
	;	fatal_error('Did not expect complex sort term ~w', [Sort1])
	),
	lsort_kind(Sort, Kind),
	!,
	error('Probably isolated variable ~w', [Term:Sort1]),
	fail.


/*
  arithmetic operator may only have arithmetic sub expressions
  */
transExpr(Term, VLIn, expr(Kind), CIn, CIn, TermOut) :-
	arop(Term, Ts, TSs, TermOut),
	!,
	transExprList(Ts, VLIn, Kind, TSs).
transExpr(TermIn, _VLIn, integer, CIn, CIn, TermIn) :-
	integer(TermIn),!.
transExpr(TermIn, _VLIn, number, CIn, CIn, TermIn) :-
	number(TermIn),!.
transExpr(TermIn, _VLIn, atom, CIn, CIn, TermIn) :-
	atom(TermIn),
	!.

transExpr(TermIn, VLIn, compound, CIn, COut, TermOut) :-
	TermIn =.. [F|Args],
	transTerms(Args, VLIn, CIn, COut, ArgsOut),
	TermOut =.. [F|ArgsOut],!.
/*transExpr(TermIn, VLIn, Kind, CIn, COut, TermOut) :-
	fatal_error('IMPLEMENTATION ERROR:transExpr failed:~w',
		   [transExpr(TermIn, VLIn, Kind, CIn, COut, TermOut)]).
*/
transTerms([], _VLIn, Codes, Codes, []).

transTerms([Arg|Args], VLIn, CIn, COut, [ArgOut|ArgsOut]) :-
	transExpr(Arg, VLIn, Kind, CIn, C1, ArgOut1),
	(Kind = expr(_Type)
	->	C2 = [is(ArgOut, ArgOut1)|C1]
	;	C2 = C1,
		ArgOut = ArgOut1
	),
	transTerms(Args, VLIn, C2, COut, ArgsOut).

transExprKind(TermIn, VLIn, Kind2, TermOut) :-
	transExpr(TermIn, VLIn, Kind1, Code, TermOut),
	(Kind1 = expr(Kind2)
	->	true
	;Kind1 = var(Kind2)
	->	true
	;memberchk(Kind1, [atom, compound])
	->	fatal_error('Arithmetic expression contains unrecognised sub term ~w', [TermIn])
	;	Kind2 = Kind1
	),
	(Code == []
	->	true
	;	fatal_error('IMPL ERROR:code in expr')
	).

transExprList([TermIn], VLIn, Kind, [TermOut]) :-
	!,transExprKind(TermIn, VLIn, Kind, TermOut).

transExprList([TermIn|TermsIn], VLIn, Kind, [TermOut|TermsOut]) :-
	transExprKind(TermIn, VLIn, Kind1, TermOut),
	transExprList(TermsIn, VLIn, Kind2, TermsOut),
	(compatible_kinds(Kind1, Kind2, Kind)
	->	true
	;	fatal_error('mixed sorts in expression ~w - ~w',[Kind1,Kind2])
	).

compatible_kinds(K1, K1, K1) :-
	!.
compatible_kinds(K1, K2, K) :-
	compatible_kinds(Ks, K),
	memberchk(K1, Ks),
	memberchk(K2, Ks),
	!.
compatible_kinds([time, integer], time).

	
addHoldsTerm(Fact,Codes,Res) :-
	addHTerm(Fact,Codes,Res).
addLHoldsTerm(Fact,Codes,Res) :-
	addHTerm(Fact, Codes,Res).

:- dynamic dyncoded_fact/4.

do_compile_holds :-
	get_option(cholds).
do_compile_holds :-
	get_option(compile(_)).
/* ONLY for interpreted version, i.e. everything ground */
coded_fact(F1, Fact, Status, Res) :-
	dyncoded_fact(F1, Fact, Status, Res).
code_fact(F1, Fact, Status, Res) :-
	(do_compile_holds
	->	compile_holds(F1, Fact, Status, Res)
	;	Status = not_compiled,
		Res = error
	),
	assertz(dyncoded_fact(F1, Fact, Status, Res)).

adaptcholds :-
	use_cholds_mod,
	!.

adaptcholds :-
	close(choldsfile),
	choldsfile(HF),
	format('LOADING optimizations.~n'),
	set_prolog_flag(unknown,fail),
	satsimpleverbose:ensure_loaded(HF),
	satsimplequiet:ensure_loaded(HF),
	make.

resetcholds1 :-
	retractall(dyncoded_fact(_, _, _, _)).

resettrans :-
	reset_recursive_denotes,
	retractall(dynWarned(_)),
	(get_option(cholds)
	->	resetcholds
	;	true
	).


resetcholds :-
	use_cholds_mod,
	!,
	resetcholds1,
	clear_module(cholds).

resetcholds :-
	resetcholds1,
	clearcholds,
	appendcholds.

clearcholds :-
	% make empty
	choldsfile(HF),
	open(HF, write, _, [alias(choldsfile)]),
	close(choldsfile).
appendcholds :-
	(current_stream(choldsfile, _, _)
	->	true
	;	choldsfile(HF),
		open(HF, append, _, [alias(choldsfile)])
	).
choldsfile('generatedholds.pl').
choldsalias(Alias) :-
	assert_debug(\+ use_cholds_mod),
	(get_option(compile(_))
	->	Alias = compilefile
	;	get_option(cholds),
		Alias = choldsfile
	).

use_cholds_mod.


compile_holds(F1, Fact, coded, fail) :-
	\+ call(Fact),
	!,
	warning('holds subformula ~w never succeeds in loaded trace(s)',
		[F1]).

compile_holds(F1, Fact, coded, Header) :-
	term_to_atom(F1, Name),
	free_variables(Fact, FV),
	Header1 =.. [Name|FV],
	concat_atom([Name,1], NF),
	Call =.. [NF,Arg],
	(use_cholds_mod
	->	cholds:assertz((
			Header1 :-
		       concat_atom(FV, '|', Arg),
			Call
		       )),
		(	call(Fact),
			concat_atom(FV, '|', Arg),
			cholds:assertz(Call),
			fail
		;	true
		),
		Header = cholds:Header1
	;	Header = Header1,
		current_output(Old),
		choldsalias(Alias),
		set_output(Alias),
		portray_clause((
				Header :-
			       concat_atom(FV, '|', Arg),
				Call
			       )),
		fill_holds(Fact, FV, Arg, Call),
		set_output(Old)
	).

fill_holds(Fact, FV, Arg, Call) :-
	(	call(Fact),
		concat_atom(FV, '|', Arg),
		portray_clause(Call),
		fail
	;	true
	).



			
addHTerm(Fact,Codes,Result) :-
	copy_term(Fact, F1),
	numbervars(F1, 0, _),
	(coded_fact(F1, Fact, Status, Res1)
	->	local_inform('Reuse of code ~w~n', [F1])
	;	code_fact(F1, Fact, Status, Res1),
		local_inform('New code ~w~n', [F1])
	),
	(Status = coded
	->	(Codes == []
		->	(Res1 == fail
			->	Result = false
			;	Result = call(Res1)
			)
		;	fatal_error('Unimplemented compiled arg eval')
		)
	;	(Codes == []
		->	Result = Fact
		;	list_to_commas(Codes, Code1),
			Result = subscode(Code1, Fact)
		)
	).

end_of_file.

getUninstantiatedGlobalVars(Term, VLIn, FGlobalVars) :-
	/* independent of where, simply gather them ?? */
	/* Wat te doen met de vars? Zijn er al vars omgezet? Blijf
	   van lokale vars af? Op zich: stel we vervangen alle global
	   vars door echte prolog vars, dan free_variables en opzoeken
	   of ze geinstantieerd zijn of niet.
	   */

makeImpliesCode(F, G, VLIn, Code, C) :-
	getUninstantiatedGlobalVars(F, VLIn, FGlobalVars), 
	getUninstantiatedGlobalVars(G, VLIn, GGlobalVars),
	varsSize(FGlobalVars, FSz),
	varsSize(GGlobalVars, GSz),
	(GSz < FSz
	->	makeInstantiateCode(GGlobalVars, InstantiateG, VLIn, VLG),
		transTermVarsPrologC(G, VLG, GOut, CodeG, ComplexityG),
		transTermVarsPrologC(F, VLG, FOut, CodeF, ComplexityF),
		Code = (InstantiateG,
			       (CodeG
			       ->      true
			       ;       CodeF
			       )
		       )
	;	makeInstantiateCode(FGlobalVars, InstantiateF, VLIn, VLF),
		transTermVarsPrologC(F, VLF, FOut, CodeF, ComplexityF),
		transTermVarsPrologC(G, VLF, GOut, CodeG, ComplexityG),
		Code = (InstantiateF,
			       (CodeF
			       ->      CodeG
			       ;       true
			       )
		       )
	).

makeEquiCode(F, G, VLIn, ImplInterpreted, Code, InterprComplexity) :-
	getUninstantiatedGlobalVars(F, VLIn, FGlobalVars), 
	getUninstantiatedGlobalVars(G, VLIn, GGlobalVars),
	varsSize(FGlobalVars, FSz),
	varsSize(GGlobalVars, GSz),
	(GSz < FSz
	->	makeEquiCode1(G, F, GGlobalVars, FGlobalVars, VLIn, 
		ImplInterpreted, Code, InterprComplexity)
	;	makeEquiCode1(F, G, FGlobalVars, GGlobalVars, VLIn, 
			ImplInterpreted, Code, InterprComplexity)
	).



makeEquiCode1(F, G, FGlobalVars, GGlobalVars, VLIn, equi(FOut, GOut), Code,
	      InterprComplexity) :-
	makeInstantiateCode(FGlobalVars, InstantiateF, VLIn, VLF),
	transTermVarsPrologC(F, VLF, FOut, CodeF, ComplexityF),
	transTermVarsPrologC(G, VLF, GOut, CodeG, ComplexityG),
	transTermVarsPrologC(not(G), VLF, NotGOut, NotCodeG, ComplexityNotG),
	InterprComplexity is ComplexityF + ComplexityG,
	Code = (InstantiateF,
		(CodeF
		->	CodeG
		;	NotCodeG
		)).





	































