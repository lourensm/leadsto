/*
vars_in_formula(Formula, PN, VLIn, QVL, QVOOIn, QVOOut)
Formula: logical formula
PN: pos, neg whether we expect code to be pos or neg.
VLIN: variables defined in the context, also containing those
      in QVL
QIn:  variables we wish to investigate occurring in Formula,
      may have occurred before, update result into
QOut
called by code-formula, when encountering

forall(QVars, implies(F1, F2)) then
does vars_in_formula of F1 and
vars_in_formula of F2.
Those vars that are instantiated in F1 and needed in F2 are nicely
dealt with.
Instantiated in F1, not used in F2, no problem.
Not instantiated in F1, ...

forall(QVars, implies(F1, F2)) variables not occurring in F1
forall(x:sx, y:sy, implies(f(x:sx), g(x:sx, y:sy)))
forall(x:sx, implies(f(x:sx), forall(y:sy, g(x:sx, y:sy))))
want
forall(x:sx, y:sy, or(not(f(x:sx)), g(x:sx, y:sy)))
forall(x:sx, or(not(f(x:sx)), forall(y:sy, g(x:sx, y:sy))))
forall(x:sx, y:sy, or(not(f(x:sx)), and(f(x:sx),g(x:sx, y:sy))))
(f(x:sx)
->   (
  */
:- module(transnew,
	  [
	   transTerm/4,
	   transExprs/4,
	   transExpr/4,
	   codePrologNew/3,
	   setup_verbose/1,
	   cleanup_verbose_advanced/0
	  ]).
:- use_module(transutil).

:- use_module(varutil).
:- use_module(util).
:- use_module(satgenut).
:- use_module(library(lists)).

tfpnft(true, pos, true).
tfpnft(true, neg, false).
tfpnft(false, pos, false).
tfpnft(false, neg, true).
tfcode(true, true).
tfcode(false,fail).
/*
  First, we remove meta-expressions, i.e., implies, equiv & xor which are substituted by or, and & non.

ex2basic(X implies Y, R):-ex2basic(Y or non X,R).
ex2basic(X equiv Y,R):-ex2basic(X implies Y and Y implies X, R).
ex2basic(X xor Y,R):-ex2basic((X and non Y) or (Y and non X), R).
ex2basic(X or Y, XB or YB):-ex2basic(X,XB),ex2basic(Y,YB).
ex2basic(X and Y, XB and YB):-ex2basic(X,XB),ex2basic(Y,YB).
ex2basic(non X, non XB):-ex2basic(X,XB).
ex2basic(X,X):-atom(X).
Second, we move negation non to atomic formulas.

non2basic(non (X and Y),XN or YN):-non2basic(non X,XN),non2basic(non Y,YN).
non2basic(non (X or Y),XN and YN):-non2basic(non X,XN),non2basic(non Y,YN).
non2basic(non non X, XB):-non2basic(X,XB)
non2basic(non X,non X):-atom(X).
non2basic(X and Y,XB and YB):-non2basic(X,XB),non2basic(Y,YB).
non2basic(X or Y,XB or YB):-non2basic(X,XB),non2basic(Y,YB).
non2basic(X,X):-atom(X).
Finally, we can construct a conjunctive normal form.

ex2conj(X and Y,XC and YC):-ex2conj(X,XC),ex2conj(Y,YC).
ex2conj(X or Y, R):-ex2conj(X,XC),ex2conj(Y,YC),join_disj(XC,YC,R).
ex2conj(non X,non X).
ex2conj(X,X):-atom(X).

join_disj(X and Y,Z,XZ and YZ):-join_disj(X,Z,XZ),join_disj(Y,Z,YZ).
join_disj(X,Y and Z,XY and XZ):-X\=(_ and _),join_disj(X,Y,XY),join_disj(X,Z,XZ).
join_disj(X,Y,X or Y):-X\=(_ and _),Y\=(_ and _).
Now, we join above three procedures into compact form which transforms arbitrary expression into its conjuctive normal form. We call this process normalization.

normalize(Ex,Norm):-
   ex2basic(Ex,Bas),
   non2basic(Bas,NonBas),
   ex2conj(NonBas,Norm).

convertToCnf(X,Y) :- cnf(X,Z), convertToCnf(Z,Y).

convertToCnf(X,X).
% Remove if's and iff's
cnf(A iff B, (not(A)or B) and (not(B) or A)).
cnf(A implies B, not(A) or B).

% Remove double negation
cnf(not(not(B)),B).

% Apply the De Morgan Rules
cnf(not(A and B),not(A) or not(B)).
cnf(not(A or B),not(A) and not(B)).

% Apply the Distribution rules
cnf(A or(B and C),(A or B) and (A or C)).
cnf((A and B)or C,(A or C) and (B or C)).


% Recursively break down and subformulas which are not in cnf
cnf(A1 or B, A2 or B) :- cnf(A1,A2).
cnf(A or B1, A or B2) :- cnf(B1,B2).

cnf(A1 and B, A2 and B) :- cnf(A1,A2).
cnf(A and B1, A and B2) :- cnf(B1,B2).

cnf(not(A1),not(A2)) :- cnf(A1,A2).


%------------------- PRENEX NORMAL FORM ----------------------------------------

% pnf takes one argument which is the wff that you want to change to
% prenex normal form. The PNF variable returns the transformed prenex wff
% and prints it on the output screen in Prolog, using the write command. A list, the
% second argument in pnf/3, holds a list of the bound variables in the formula.

pnf(F) :- pnf(F, [], PNF), write(PNF).

% pnf is satisfied if a sub-formula can be found in the wff which
% allows it to be transformed into an equivalent formula which is nearer
% to pnf. pnf requires:
%(1) all its quantifiers clustered at the left,
%(2) no quantifier is negated,
%(3) the scope of each quantifier extends to the end of the wff,
%(4) no two quantifiers quantify the same variable,
%(5) every quantified variable occurs in the matrix of the wff.
%
% Hence we start of by getting rid of the iff (if and only if) and if (implies)
% operators, by converting them into formulas with only or's, and's and negations.
% This is the reasoning behind the precedence of the operators defined above.
%
% Then we remove the double negations and open up any formulas which may be negated
% using the elimination rules and De Morgan's Rules.
%
% The rules are recursively applied until no more can be done and the formula is
% in pnf.


% Remove if's and iff's first, because these may affect the quantifiers in the formula.
% For example, ex(x, p(x) implies q(y)) would transform to all(x, not p(x) or q(y)).
*/
pnf(all(X, iff(A,B)), Variables, all(X, PNF)) :-
	!, copy_term((X, A, Variables), (Y, Ay, Variables)),
	pnf(and(or(not(Ay),B),or(Ay,not(B))),	[Y | Variables], PNF).


pnf(all(X, implies(A,B)), Variables, all(X, PNF)) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(or(not(Ay),B), [Y | Variables], PNF).

pnf(ex(X, iff(A,B)), Variables, all(X, PNF)) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(and(or(not(Ay),B),or(Ay,not(B))),
													 [Y | Variables], PNF).
pnf(ex(X, implies(A,B)), Variables, all(X, PNF)) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(or(not(Ay),B), [Y | Variables], PNF).

pnf(all(X,F),Variables,all(X,PNF)) :- !, pnf(F,[X|Variables], PNF).

pnf(ex(X,F),Variables,ex(X,PNF)) :- !, pnf(F,[X|Variables], PNF).

% Remove double negation
pnf(all(X, not(not(A))), Variables, all(X, PNF)) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(Ay , [Y | Variables], PNF).
pnf(ex(X, not(not(A))), Variables, all(X, PNF)) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(Ay , [Y | Variables], PNF).

% Apply the De Morgan Rules
pnf(all(X, not(and(A,B))),Variables, PNF) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(or(not(Ay),not(B)) , [Y | Variables], PNF).
pnf(all(X, not(or(A,B))),Variables, PNF) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(and(not(Ay),not(B)), [Y | Variables], PNF).

pnf(ex(X, not(and(A,B))),Variables, PNF) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(or(not(Ay),not(B)) , [Y | Variables], PNF).
pnf(ex(X, not(or(A,B))),Variables, PNF) :- !, copy_term((X, A, Variables), (Y, Ay, Variables)), pnf(and(not(Ay),not(B)), [Y | Variables], PNF).

pnf(and(ex(X,A),B),Variables,ex(Y,PNF)) :- !, copy_term((X,A,Variables),(Y,Ay,Variables)), pnf(and(Ay,B),[Y|Variables], PNF).

pnf(or(ex(X,A),B),Variables,ex(Y,PNF)) :- !, copy_term((X,A,Variables),(Y,Ay,Variables)), pnf(or(Ay,B),[Y|Variables], PNF).

pnf(and(all(X,A),B),Variables,all(Y,PNF)) :- !, copy_term((X,A,Variables),(Y,Ay,Variables)), pnf(and(Ay,B),[Y|Variables], PNF).

pnf(or(all(X,A),B),Variables,all(Y,PNF)) :- !, copy_term((X,A,Variables),(Y,Ay,Variables)), pnf(or(Ay,B),[Y|Variables], PNF).

pnf(and(A,ex(X,B)),Variables,ex(Y,PNF)) :- !, copy_term((X,B,Variables),(Y,By,Variables)), pnf(and(A,By),[Y|Variables], PNF).

pnf(or(A,ex(X,B)),Variables,ex(Y,PNF)) :- !, copy_term((X,B,Variables),(Y,By,Variables)), pnf(or(A,By),[Y|Variables], PNF).

pnf(and(A,all(X,B)),Variables,all(Y,PNF)) :- !, copy_term((X,B,Variables),(Y,By,Variables)), pnf(and(A,By),[Y|Variables], PNF).

pnf(or(A,all(X,B)),Variables,all(Y,PNF)) :- !, copy_term((X,B,Variables),(Y,By,Variables)), pnf(or(A,By),[Y|Variables], PNF).

pnf(and(A,B),Variables,PNF) :- pnf(A,Variables,Ap), pnf(B,Variables,Bp), (A\=Ap; B\=Bp), pnf(and(Ap,Bp),Variables,PNF).

pnf(or(A,B),Variables,PNF) :- pnf(A,Variables,Ap), pnf(B,Variables,Bp),(A\=Ap; B\=Bp), pnf(or(Ap,Bp),Variables,PNF).

pnf(PNF,_,PNF).



sprop_conj_normal_form(Term, S, Form) :-
	convertToDnf(Term, Dnf),
	dnf_sp(Dnf, S, Form).

dnf_sp(or(T1,T2), S, or(Form1, Form2)) :-
	!,
	basic_sp(T1, S, Form1),
	dnf_sp(T2, S, Form2).
dnf_sp(T1, S, Form1) :-
	basic_sp(T1, S, Form1).
basic_sp(and(T1,T2), S, and(Form1,Form2)) :-
	!,
	lit_sp(T1, S, Form1),
	basic_sp(T2, S, Form2).
basic_sp(T1, S, Form1) :-
	lit_sp(T1, S, Form1).
lit_sp(not(T1), S, holds(S, T1, false)) :-
	!.
lit_sp(T1, S, holds(S, T1, true)).




formula_kind(Form, _PN, VLIn, _Kind, _Cleanup) :-
	isVarOccS(Form, VLIn, Var, Sort, _Data,Status),
	!,
	(	var(Status)
	->	var_error(Var:Sort)
	;	true
	),
	fail.
formula_kind(true, PN,_VLIn, tf(TF),[]) :-
	!,tfpnft(true, PN, TF).
formula_kind(false, PN, _VLIn, tf(TF),[]) :-
	!,tfpnft(false, PN, TF).
formula_kind(not(Form), PN, VLIn, Kind, ToFree) :-
	!,pnnp(PN, NP),
	formula_kind(Form, NP, VLIn, Kind, ToFree).
formula_kind(external(Header, Specs), PN, _VLIn,
	     external(Header,Specs,PN),[]):-
	!,
	(	Specs == default
	->	true
	;	error('Can only handle default external property type, not ~w',
		      [Specs]),
		fail
	),
	!.

formula_kind(holds(S, A, TF), PN, _VLIn, holds(S, A, TF, PN),[]) :-
	!.
formula_kind('|='(S, F), PN, _VLIn, holds(S, F, PN),[]) :-
	!.
formula_kind(itef(CondFormula, CondVars, InFormula),PN,_VLIn,
	     itef(CondFormula, CondVars, InFormula,PN), []) :-
	!.

formula_kind(Form, PN, _VLIn, cmp(Form,NumCall,LHS,RHS,PN), []) :-
	iscmpoploc(Form, NumCall,LHS,RHS),
	!.

formula_kind(Form, PN, VLIn, Kind, ToFree) :-
	tr_formula_code_prolog_f(Form, PN, Form1, PN1),
	!,
	formula_kind(Form1, PN1, VLIn, Kind, ToFree).

formula_kind(Form, PN, _VLIn, faSat(F,Args,PN), []) :-
	functor(Form, F, A),
	isFAsat(F,A),
	A > 0,
	!,
	Form =.. [F1|Args],
	assert_debug(F==F1).


formula_kind(Form, PN, VLIn, Kind,
	     [free_recursive_constant(Form)|ToFree]) :-
	spec_constant(Form, Form2),
	test_recursive_constante(Form, Form2, Status),
	!,
	var(Status),
	formula_kind(Form2, PN, VLIn, Kind, ToFree).
formula_kind(Form, PN, VLIn, Kind,
	     [free_recursive_denotes(Form)|ToFree]) :-
	trans_denotes_new(Form, VLIn, Form2),
	test_recursive_denotes(Form),
	!,
	formula_kind(Form2, PN, VLIn, Kind, ToFree).

formula_kind(Form, _PN, _VLIn, _Kind, _ToFree) :-
	error('Unrecognised formula ~w(n)', [Form]),!,
	fail.

do_free(Calls) :-
	uchecklist(call, Calls).

vars_in_formula(Form, PN, VLIn, QIn, QOut) :-
	formula_kind(Form, PN, VLIn, Kind, ToFree),
	(vars_in_formula_kind(Kind, VLIn, QIn, QOut)
	->	do_free(ToFree)
	;	do_free(ToFree),
		fail
	).
vars_in_formula_kind(tf(_TF),_VLIn, QIn, QIn).
vars_in_formula_kind(external(H,_S,_PN), VLIn, QIn, QOut) :-
	H =.. [_|Args],
	qvars_in_terms(Args, 1, needs_bound, VLIn, QIn, QOut).
vars_in_formula_kind(holds(S, A, TF, PN), VLIn, QIn, QOut) :-
	holdsbound(PN, Bound),
	qvars_in_term(holds(S, A, TF), 1, Bound, VLIn, QIn, QOut).
vars_in_formula_kind(holds(S, T, PN), VLIn, QIn, QOut) :-
	sprop_conj_normal_form(T, S, Form),
	!,
	vars_in_formula(Form, PN, VLIn, QIn, QOut).

vars_in_formula_kind(itef(CondForm, CondVars, InFormula,PN),
		     VLIn, QIn, QOut) :-
	vars_in_formula(CondForm,pos,VLIn, QIn, QAap),
	correct_negated_q_vars(neg, QAap, QIn, QIn1),
	% trick not understood by myself!
	vars_in_cond_vars(CondVars, VLIn, VL1,QIn1, Q1, [], QHidden),
	vars_in_formula(InFormula, PN, VL1, Q1, Q2),
	correct_negated_q_vars(PN, Q2, Q1, Q3),
	append(QHidden, Q3, QOut).
vars_in_formula_kind(cmp(_Form,NumCall,_LHS,_RHS,_PN),VLIn, QIn, QOut) :-
	NumCall =.. [_|Args],
	qvars_in_terms(Args, 1, needs_bound, VLIn, QIn, QOut).
vars_in_formula_kind(faSat(F, Args, PN), VLIn, QIn, QOut) :-
	vars_in_formulaFA(F, Args, PN, VLIn, QIn, QOut).
vars_in_formula_kind(_Kind,  _VLIn, _QIn, _QOut) :-
%	impl_error('Unrecognised formula kind ~w(phase 1)', [Kind]),
%	!,
	fail.


vars_in_cond_vars([], VLIn, VLIn, QIn, QIn, QHIn, QHIn).
vars_in_cond_vars([tev(Then, Else, Var)|CondVars],VLIn,VLOut,QIn,QOut,
		  QHIn,QHOut) :-
	vars_in_cond_var(Then, Else, Var, VLIn, VL1, QIn, Q1, QHIn, QH1),
	vars_in_cond_vars(CondVars,VL1,VLOut,Q1,QOut, QH1,QHOut).
vars_in_cond_var(Then, Else, VarTerm, VLIn, VLOut, QIn, QOut, QHIn, QHOut) :-
	(is_var_then_ensure_add(VarTerm, VLIn, Sort, VLOut)
	->	qvars_in_term(Sort, 1, needs_bound, VLIn, QIn, Q1),
		qvars_in_term([Then,Else], 1, needs_bound,VLIn,Q1,Q2),
		qvar_in_term(Var, bound, Q2, Q3),
		(vd_fromq(Q3, Var, VarData, QOut)
		->	warning('Variable ~w hides previous quantifier variable',
				[Var:Sort]),
			QHOut = [q(Var, VarData)|QHIn]
		;	QHOut = QHIn,
			QOut = Q3
		)
	;	impl_error('expected variable, got ~w', [VarTerm])
	).

/*
vars_in_formula(Var, _PN, VLIn, _QIn, _) :-
	var(Var),
	!,
	ensureVarDInVarList(VLIn, Var, Sort, _Data),
	var_error(Var:Sort),
	fail.
vars_in_formula(holds(S, A, TF), PN,  VLIn, QIn, QOut) :-
	!,
	vars_in_holds(PN, S, A, TF, VLIn, QIn, QOut).
vars_in_formula(not(Term), PN, VLIn, QIn, QOut) :-
	!,
	pnnp(PN, NP),
	vars_in_formula(Term, NP, VLIn, QIn, QOut).
vars_in_formula(Var:Sort, _PN, VLIn, QIn, QIn) :-
	!,
	ensureVarDInVarList(VLIn, Var, Sort, _Data),
	var_error(Var:Sort),
	fail .
vars_in_formula(Var, _PN, VLIn, _QIn, _QOut) :-
	atom(Var),
	varDInVarList(VLIn, Var, Sort, _Data),
	!,
	var_error(Var:Sort),
	fail.
vars_in_formula(external(Header, Specs), _PN, VLIn, QIn, QOut) :-
	!,
	(	Specs == default
	->	Header =.. [_|Args],
		qvars_in_terms(Args, 1, needs_bound, VLIn, QIn, QOut)
	;	error('Can only handle default external property type, not ~w',
		      [Specs]),
		fail
	).

vars_in_formula(Term, PN, VLIn, QIn, QOut) :-
	spec_constant(Term, Term1),
	test_recursive_constant(Term, Term1),
	!,
	(vars_in_formula(Term1, PN, VLIn, QIn, QOut)
	->	free_recursive_constant(Formula)
	;	free_recursive_constant(Formula),
		fail
	).

vars_in_formula(Formula, PN, VLIn, QIn, QOut) :-
	trans_denotes_new(Formula, VLIn, Formula1),
	!,
	(test_recursive_denotes(Formula)
	->	vars_in_formula(Formula1, PN, VLIn, QIn, QOut),
		free_recursive_denotes(Formula)
	;	free_recursive_denotes(Formula),
		fail
	).

vars_in_formula(Term, _PN, _VLIn, QIn, QIn) :-
	memberchk(Term, [true,false]),
	!.
vars_in_formula(Term, PN, VLIn, QIn, QOut) :-
	is_implies(Term, F1, F2),
	!,
	vars_in_implies(PN, F1, F2, VLIn, QIn, QOut).
vars_in_formula(Formula, _PN, VLIn, QIn, QOut) :-
	iscmpoploc(Formula, NumCall,_LHS,_RHS),
	!,
	NumCall =.. [_|Args],
	qvars_in_terms(Args, 1, needs_bound, VLIn, QIn, QOut).


vars_in_formula(Formula, PN, VLIn, QIn, QOut) :-
	functor(Formula, F, A),
	isFAsat(F,A),
	A > 0,
	!,
	Formula =.. [_|Args],
	vars_in_formulaFA(F, Args, PN, VLIn, QIn, QOut).

vars_in_formula(Formula, _PN, _VLIn, _QIn, _QOut) :-
	error('Unrecognised formula ~w(2)', [Formula]),!,
	fail.
*/
vars_in_formulaFA(AndOr, Args, PN, VLIn, QIn, QOut) :-
	memberchk(AndOr, [and, or]),
	!,
	vars_in_conn(Args, AndOr, PN, VLIn, QIn, QOut).
vars_in_formulaFA(ExAll, Args, PN, VLIn, QIn, QOut) :-
	exallop(ExAll,ExAll1),
	!,
	vars_in_q(Args, ExAll1, PN, VLIn, QIn, QOut).

vars_in_holds(PN, S, A, TF, VLIn, QIn, QOut) :-
	holdsbound(PN, Bound),
	qvars_in_term(holds(S, A, TF), 1, Bound, VLIn, QIn, QOut).

vars_in_formula1(PN, VLIn, QIn, Formula, QOut) :-
	vars_in_formula(Formula, PN, VLIn, QIn, QOut).

vars_in_conn(Args, AndOr, PN, VLIn, QIn, QOut) :-
	maplist(vars_in_formula1(PN, VLIn, QIn), Args, Q1L),
	combine_qvars(QIn, Q1L, AndOr, PN, Args, QOut).
vars_in_implies(PN, F1, F2, VLIn, QIn, QOut) :-
	vars_in_formula(or(not(F1), F2), PN, VLIn, QIn, QOut).
vars_in_q(Args, ExAll, PN, VLIn, QIn, QOut) :-
	!,
	rm_qvars(Args, VLIn, VL1, QIn, Q1, [], QHidden, _Vars, Formula),
	(ExAll == exists
	->	PN1 = PN,
		PN2 = pos
	;	assert_debug(ExAll == forall),
		pnnp(PN, PN1),
		PN2 = neg
	),
	vars_in_formula(Formula, PN2, VL1, Q1, Q2),
	correct_negated_q_vars(PN1, Q2, Q1, Q3),
	append(QHidden, Q3, QOut).


rm_qvars([Arg|Args], VLIn, VLOut, QIn, QOut, QHIn, QHOut, Vars, Formula) :-
	(is_list(Arg)
	->	(Arg == []
		->	warning('quantifier without variables'),
			VLOut = VLIn,
			QOut = QIn
		;	add_q_vars(Arg, VLIn, VLOut, QIn, QOut, QHIn, QHOut, Vars)
		),
		(Args = [Formula]
		->	true
		;	error('Wrong quantifier formula'),
			fail
		)
	;Args == []
	->	warning('quantifier without variables'),
		VLOut = VLIn,
		QOut = QIn,
		Formula = Arg
	;	add_q_var(Arg, VLIn, VL1, QIn, Q1, QHIn, QH1,Var),
		Vars = [Var|Vars1],
		rm_qvars1(Args, VL1, VLOut, Q1, QOut, QH1, QHOut, Vars1, Formula)
	),
	(	nonvar(QHOut),
		is_list(QHOut),
		(	QHOut == [];QHOut = [F|_],nonvar(F))
	->	true
	;	local_trace(aap)
	).



rm_qvars1([Formula], VLIn, VLIn, QIn, QIn, QHIn, QHIn, [], Formula) :-
	!.
rm_qvars1([Arg|Args], VLIn, VLOut, QIn, QOut, QHIn, QHOut, [Var|Vars], Formula) :-
	add_q_var(Arg, VLIn, VL1, QIn, Q1, QHIn, QH1,Var),
	rm_qvars1(Args, VL1, VLOut, Q1, QOut, QH1, QHOut, Vars, Formula).
add_q_vars([], VLIn, VLIn, QIn, QIn, QHIn, QHIn,[]).

add_q_vars([Arg|Args], VLIn, VLOut, QIn, QOut, QHIn, QHOut,[Var|Vars]) :-
	add_q_var(Arg, VLIn, VL1, QIn, Q1, QHIn, QH1,Var),
	add_q_vars(Args, VL1, VLOut, Q1, QOut, QH1, QHOut,Vars).

/*
  If Local variable also present in QIn, we should remove the variable from
  Q for now
  */
add_q_var(X, VLIn, VLOut, QIn, QOut, QHIn, QHOut,Var) :-
	(is_var_then_ensure_add(X, VLIn, Sort, VLOut)
	->	qvars_in_term(Sort, 1, needs_bound, VLIn, QIn, Q2)
	;	iscmpoploc(X, _Call1, LHS, RHS)
	->	(iscmpoploc(RHS, _Call2, LHS2, RHS2)
		->	is_var_then_ensure_add(LHS2, VLIn, Sort, VLOut),
			qvars_in_term(Sort, 1, needs_bound, VLIn, QIn, Q1),
			qvars_in_term([LHS,RHS2], 1, needs_bound,VLIn,Q1,Q2)
		        % ORDER important for hiding: exists(p1:r < p1:r,F)
		;	is_var_then_ensure_add(LHS, VLIn, Sort, VLOut),
			qvars_in_term(Sort, 1, needs_bound, VLIn, QIn, Q1),
			qvars_in_term(RHS, 1, needs_bound, VLIn, Q1, Q2)
			% ORDER see above
		)
	;	error('Unrecognised quantifier range ~w(1)', [X]),
		fail
	),
	(vd_fromq(Q2, Var, VarData, QOut)
	->	warning('Variable ~w hides previous quantifier variable',
			[Var:Sort]),
		QHOut = [q(Var, VarData)|QHIn]
	;	QHOut = QHIn,
		QOut = Q2
	).






qvar_in_term(Var, Bound, QIn, QOut) :-
	assert_debug(atom(Bound)),
	assert_debug(memberchk(Bound, [bound, needs_bound, bound_lost])),
	(vd_fromq(QIn, Var, VarData, Q1)
	->	adjust_vd_what(VarData, Bound, VarData1),
		vd_toq(Var, VarData1, Q1, QOut)
	;	QOut = QIn
	).

adjust_vd_what(initial, PN, PN) :-
	!.
adjust_vd_what(X, _PN, X).


/*
  Case of (or,pos) (and,neg):
  needs_bound occurs in one disj => result is needs_bound
  bound occurs in all disj => result is  bound
  otherwise result is initial?

  Case of (and,pos) (or,neg)
  bound occurs left of any needs_bound => result is bound
  bound occurs right of some needs_bound => result is needs_bound, warn for reorder
  needs_bound occurs => needs_bound
  */
combine_qvars([], _Q1L, _AndOr, _PN, _Args, []).

combine_qvars([q(Var, VarData)|QIn], Q1L, AndOr, PN, Args, QOut) :-
	(VarData == initial
	->	combine_qvar1(Q1L, Var, AndOr, PN, Args,VarData1)
	;	VarData1 = VarData
	),
	combine_qvars(QIn,  Q1L, AndOr, PN, Args, QOut1),
	vd_toq(Var, VarData1, QOut1, QOut).
andorpnandor(AndOr, pos, AndOr) :-
	!.
andorpnandor(and, neg, or) :-
	!.
andorpnandor(or, neg, and) :-
	!.
combine_qvar1(Q1L, Var, AndOr, PN, Args, VarData1) :-
	maplist(select_qvar(Var), Q1L, VDL),
	andorpnandor(AndOr, PN, AndOr1),
	(AndOr1 == and
	->	combine_qvarLand(VDL, Args, VarData1)
	;	combine_qvarLor(VDL, VarData1)
	).
select_qvar(Var, QIn, VarData) :-
	assert_debug(atom(Var)),
	(memberchk(q(Var, VarData), QIn)
	->	true
	;	pr_error('Missing qentry'),
		fail
	).

/* No reshuffling, require bound before needed
   */
combine_qvarLand([], _Args, initial) :-
	!.
combine_qvarLand([initial|BoundList], [_Arg|Args], Res) :-
	!,
	combine_qvarLand(BoundList, Args, Res).
combine_qvarLand([bound|_Rest], _, bound) :-
	!.

combine_qvarLand([BNB|Rest], [Arg|Args], BNB) :-
	!,
	(	BNB \= bound,
		memberchk(bound, Rest)
	->	BNB \= bound,
		nth1(N, Rest, bound),
		nth1(N, Args, NArg),
		warning('Should reorder and/or for more efficient check(~n    ~w ~nafter ~n    ~w)', [Arg, NArg])
	;	true
	).

/* Any is needs_bound -> whole is needs_bound
   all are initial -> whole is initial
   all are bound -> whole is bound
   everything else gives bound_lost
   */
combine_qvarLor(VDL, Res) :-
	combine_qvarLor(VDL, initial, Res).


combine_qvarLor([], Res, Res) :-
	!.
combine_qvarLor([needs_bound|_], _Res1, needs_bound) :-
	!.
combine_qvarLor([E|VDL], Res1, Res) :-
	qvarLor1(E, Res1, Res2),
	combine_qvarLor(VDL, Res2, Res).

qvarLor1(_, needs_bound, _) :-
	!,impl_error(needs_bound1).
qvarLor1(needs_bound, _, _) :-
	!,impl_error(needs_bound2).
qvarLor1(_, bound_lost, bound_lost) :-
	!.
qvarLor1(bound_lost, _, bound_lost) :-
	!.
qvarLor1(initial, bound, bound_lost) :-
	!.
qvarLor1(bound, initial, bound_lost) :-
	!.
qvarLor1(initial, initial, initial) :-
	!.
qvarLor1(bound, bound, bound) :-
	!.
qvarLor1(E, Res1, Res2) :-
	impl_error('Unimplemented ~w', [qvarLor1(E, Res1, Res2)]).















/*
codePrologF(Term, VLInstIn, VLQIn, VLInstOut, Code) :-
	aap.
We need a variable list containing all already bound variables.
Another list should contain those variables occurring in quantifiers
that need to become bound by following sub-terms.

Best be one variable list having properties.

Kinds:
1)earlier encountered and now instantiated variables, we need access
  to the prolog variable that represents the variable.
2)Earlier encountered quantifier
*/


tst(forall([x:between(1, 10), y:between(1, 10), z:between(1, 10)],
	   implies(and(x<y, y<z), x<z))).

test :-
	tst(Form),
	codePrologF(Form, pos, [], _VLOut, Code),
	portray_clause((tst :- Code)).

setup_verbose(Verbose) :-
	retractall(verbose_compiling(_)),
	(Verbose == @off
	->	true
	;Verbose = advanced(Options)
	->	setup_verbose_advanced(Options)
	;	setup_verbose_advanced([log_to_screen,
					log_every_binding,
					log_binding_succeeded_exist,
					log_binding_failed_forall
				       ])
	).

setup_verbose_advanced(Options) :-
	(select(log_to_screen, Options, Options1)
	->	What = [user|What1]
	;	What = What1,
		Options1 = Options
	),
	(select(log_to_file, Options1, Options2)
	->	(select(file=File, Options2, Options3)
		->	true
		;	File = 'checking.log',
			Options3 = Options2
		),
		format('Opening checker log file ~w~n', [File]),
		open(File, write, _, [alias(chklog)]),
		What1 = [chklog]
	;	What1 = [],
		(select(file=File, Options1, Options3)
		->	true
		;	Options3 = Options1
		)
	),
	(What == []
	->	warning('no screen log and no file log so no log')
	;	(	select(mark_formula_by_id_next, Options3, Options4)
		->	(memberchk(log_every_binding,Options4)
			->	warning('mark_formula_by_id_next not supported')
			;	warning('mark_formula_by_id_next superfluous')
			)
		;	Options4 = Options3
		),
		(	member(O, Options4),
			\+ memberchk(O, [log_every_binding,
						    log_binding_succeeded_exist,
						    log_binding_failed_forall
						    ])
		->	pr_error('Unhandled verbose option ~w', [O])
		;	true
		),
		(Options4 == []
		->	warning('Nothing logged, logs will be empty')
		;	true
		),
		assertz(verbose_compiling(advanced(What, Options4)))
	).
cleanup_verbose_advanced :-
	(	verbose_compiling(advanced(What, _Options4)),
		member(Stream, What),
		Stream \= user
	->	close(Stream),
		format('Closed checker log file~n', [])
	;	true
	).




codePrologNew(Formula, Code, Constants) :-
	resetcholds6,
	reset_constant_use,
	(codePrologF(Formula, pos, [], VLOut, Code)
	->	assert_debug(VLOut == []),
		get_reset_constant_use(Constants)
	;	reset_constant_use,
		fail
	).


codePrologF(Formula, PN, VLIn, VLOut, Code) :-
	codePrologF1(Formula, PN, VLIn, VLOut, Code),
	assert_debug(nonvar(VLOut)),
	assert_debug(VLOut == [];VLOut = [_|R],nonvar(R)).

codePrologF1(Form, PN, VLIn, VLOut, Code) :-
	formula_kind(Form, PN, VLIn, Kind, ToFree),
	(codePrologKind(Kind, VLIn, VLOut, Code)
	->	do_free(ToFree)
	;	do_free(ToFree),
		fail
	).

codePrologKind(tf(TF), VLIn, VLIn, Code) :-
	tfcode(TF, Code),!.
codePrologKind(external(Header,Specs,PN), VLIn, VLIn, Code) :-
	!,Header =.. [F|Args],
	transTerms(Args, VLIn, PreCode, LArgs),
	getExtCode(Specs, F, LArgs, Code1),
	pncode(PN, Code1, Code2),
	combine_and([PreCode,Code2], Code),!.
codePrologKind(holds(S, Term, PN), VLIn, VLOut, Code) :-
	!,
	sprop_conj_normal_form(Term, S, Form),
	codePrologF(Form, PN, VLIn, VLOut, Code).

codePrologKind(holds(S, A, TF, PN), VLIn, VLOut, Code) :-
	holdsbound(PN, Binding),
	transTerms([A, TF], VLIn, Binding, VLIn, VL1, PreCode1, PostCode1,
		   [A1, TF1], _Kinds, _PreBound1),
	transHoldsState(S, PN, VLIn, VL1, VLOut, PreCode, PostCode, S1),
	combine_and([PreCode, PreCode1], PreCode2),
	combine_and([PostCode, PostCode1], PostCode2),
	combine_and([PreCode2, PostCode2], ICode),
	(ICode == fail
	->	Code1 = fail
	;	tr_holds_state1(S1, S2),
		addHoldsTerm(holds(S2, A1, TF1), PreCode2, PostCode2,
			     VLIn, VLOut, Code1)
	),
	pncode(PN, Code1, Code),!.
codePrologKind(itef(TTL, CondVars, InFormula,PN),VLIn, VLOut, Code) :-
	!,
	(	codeITEF(TTL, CondVars, InFormula,PN,VLIn, VLOut, Code)
	->	true
	;	pr_error('Failed to compile itef construct'),
		(is_local
		->	local_trace(failed),
			codeITEF(TTL, CondVars, InFormula,PN,VLIn, VLOut,Code)
		;	fail
		)
	).
codePrologKind(cmp(Form,NumCall,LHS,RHS,PN),VLIn, VLIn, Code) :-
	codeCmpSeq(Form, NumCall,LHS,RHS, VLIn, Code1),
	pncmpcode(PN, Code1, Code),!.
codePrologKind(faSat(F, Args, PN), VLIn, VLOut, Code) :-
	codePrologFA(F, Args, PN, VLIn, VLOut, Code),!.
/*codePrologKind(Kind,  _VLIn, _VLOut, _Code) :-
	pr_error('Unrecognised formula kind ~w(phase 2)', [Kind]),
	fail.
  */
codeITEF(TTL, CondVars, InFormula,PN,VLIn, VLOut, Code) :-
	codePrologF(TTL, pos, VLIn, VL1, Code1),
	(VLIn == VL1
	->	true
	;	warning('Variables of TTL condition will only have local scope')
	),
	codeCondVars(CondVars, VLIn, VL2, SCodes, TCodes, ECodes, Vars),
	combine_and(SCodes, SCode),
	combine_and(TCodes, TCode),
	combine_and(ECodes, ECode),
	code_ite(Code1, TCode, ECode, Codea),
	codePrologF(InFormula, PN, VL2, VL3, Code2),
	combine_and([SCode, Codea,Code2], Code),
	rm_local_vars(Vars, VL3, VLOut),
	!.
codeCondVars([], VLIn, VLIn, [], [], [], []).
codeCondVars([tev(ThenExpr, ElseExpr, VarTerm)|CondVars], VLIn, VLOut,
	     [SCode|SCodes],
	     [TCode|TCodes], [ECode|ECodes],[VarName|Vars]) :-
	isVar(VarTerm, VarName, Sort),
	transTerm(Sort, VLIn, SCode, Sort1),
	transTerm(ThenExpr, VLIn, PreCode1, T),
	transExpr(ElseExpr, VLIn, PreCode2, E),
	combine_and([PreCode1, CaseVar=T], TCode),
	combine_and([PreCode2, CaseVar=E], ECode),
	varDToVarList(VarName, Sort, bound(CaseVar, Sort1), VLIn, VL1),
	codeCondVars(CondVars, VL1, VLOut, SCodes, TCodes, ECodes, Vars).



/*
codePrologF1(Term, _PN, VLIn, _VLOut, _Code) :-
	isVarOccS(Term, VLIn, Var, Sort, _Data,Status),
	!,
	var(Status),
	var_error(Var:Sort),
	fail.




codePrologF1(true, PN, VLIn, VLIn, Code) :-
	pncode(PN, true, Code).
codePrologF1(false, PN, VLIn, VLIn, Code) :-
	pncode(PN, fail, Code).
codePrologF1(not(Formula), PN, VLIn, VLOut, Code) :-
	pnnp(PN, NP),
	!,
	codePrologF(Formula, NP, VLIn, VLOut, Code).
	%pncode(neg, Code1, Code).

codePrologF1(external(Header, Specs), PN, VLIn, VLIn, Code) :-
	(Specs == default
	->	true
	;	error('Can only handle default external property type, not ~w',
		      [Specs]),
		fail
	),
	Header =.. [F|Args],
	transTerms(Args, VLIn, PreCode, LArgs),
	getExtCode(Specs, F, LArgs, Code1),
	pncode(PN, Code1, Code2),
	combine_and([PreCode,Code2], Code).


codePrologF1(holds(S, A, TF), PN, VLIn, VLOut, Code) :-
	!,
	holdsbound(PN, Binding),
	transTerms([A, TF], VLIn, Binding, VLIn, VL1, PreCode1, PostCode1,
		   [A1, TF1], _Kinds, _PreBound1),
	transHoldsState(S, PN, VLIn, VL1, VLOut, PreCode, PostCode, S1),
	combine_and([PreCode, PreCode1], PreCode2),
	combine_and([PostCode, PostCode1], PostCode2),
	combine_and([PreCode2, PostCode2], ICode),
	(ICode == fail
	->	Code1 = fail
	;	tr_holds_state1(S1, S2),
		addHoldsTerm(holds(S2, A1, TF1), PreCode2, PostCode2,
			     VLIn, VLOut, Code1)
	),
	pncode(PN, Code1, Code).

codePrologF1(itef(CondFormula, CondVars, InFormula),
	     PN, VLIn, VLOut, Code) :-
	codePrologF(TTL, pos, VLIn, VL1, Code1),
	assert_debug(VLIn == VL1),
	codeCondVars(CondVars, VLIn, VL1, CodeSat, CodeNoSat),
	code_ite(Code1, CodeSat, CodeNoSat, Code1),
	codePrologF(InFormula, PN, VL1, VLOut, Code2)
	combine_and([Code1,Code2], Code).
codePrologF1(Term, PN, VLIn, VLIn, Code) :-
	iscmpoploc(Term, NumCall,LHS,RHS),
	!,
	codeCmpSeq(Term, NumCall,LHS,RHS, VLIn, Code1),
	pncmpcode(PN, Code1, Code).
codePrologF1(Formula, PN, VLIn, VLOut, Code) :-
	tr_formula_code_prolog_f(Formula, PN, Formula1, PN1),
	!,
	codePrologF(Formula1, PN1, VLIn, VLOut, Code).
codePrologF1(Formula, PN, VLIn, VLOut, Code) :-
	functor(Formula, F, A),
	isFAsat(F,A),
	A > 0,
	!,
	Formula =.. [_|Args],
	codePrologFA(F, Args, PN, VLIn, VLOut, Code).

codePrologF1(Formula, PN, VLIn, VLOut, Code) :-
	spec_constant(Formula, Formula1),
	test_recursive_constant(Formula, Formula1),
	!,
	(codePrologF(Formula1, PN, VLIn, VLOut, Code)
	->	free_recursive_constant(Formula)
	;	free_recursive_constant(Formula),
		fail
	).


codePrologF1(Formula, PN, VLIn, VLOut, Code) :-
	trans_denotes_new(Formula, VLIn, Formula1),
	!,
	(	test_recursive_denotes(Formula),
		codePrologF(Formula1, PN, VLIn, VLOut, Code)
	->	free_recursive_denotes(Formula)
	;	free_recursive_denotes(Formula),
		fail
	).


codePrologF1(Formula, _PN, _VLIn, _VLOut, _Code) :-
	error('Unrecognised formula ~w(1)', [Formula]),!,
	fail.
*/

getExtCode(Specs, F, Args, Code) :-
	(Specs == default
	->	true
	;	error('Can only handle default external property type, not ~w',
		      [Specs]),
		fail
	),
	length(Args, L),
	catch_error_fail(ensure_loaded(background),load, [background]),
	Call =.. [F|Args],
	(	predicate_property(background:Call, interpreted)
	->	Code = background:Call
	;	error('External predicate ~w undefined',[F/L]),
		fail
	).


codeCmpSeq(Term, NumCall,LHS,RHS, VLIn, Code) :-
	assert_debug(iscmpoploc(Term, NumCall,LHS,RHS)),
	(memberchk(NumCall, [(_ \= _),(_=_)])
	->	transTerm(LHS, VLIn, PreCode, LArg)
	;	transExpr(LHS, VLIn, PreCode, LArg)
	),
	call_ground(PreCode, PreCode1),
	(PreCode1 == fail
	->	Code = fail
	;	codeCmpSeqRest(RHS, VLIn, PreCode1, NumCall,LArg, Code)
	).
/* Code1 contains LHSCoded op RArgCode
   RHS left part should be coded into RArgCode
   */
codeCmpSeqRest(RHS, VLIn, PreCode1, NumCall,LArg, Code) :-
	functor(NumCall, Op , Two),
	assert_debug(Two == 2),
	(	iscmpoploc(RHS, NumCallRHS, RHS1, RHS2)
	->	Cont = true
	;	Cont = false,
		RHS1 = RHS
	),
	(	memberchk(NumCall, [(_ \= _),(_=_)]),
		(	Cont = false
		->	true
		;	memberchk(NumCallRHS, [(_ \= _),(_=_)])
		)
	->	transTerm(RHS1, VLIn, PreCode2, RArgCode)
	;	transExpr(RHS1, VLIn, PreCode2, RArgCode)
	),
	call_ground(PreCode2, PreCode22),
	(PreCode22 == fail
	->	Code = fail
	;	Code1 =.. [Op,LArg, RArgCode],
		call_ground(Code1, Code11),
		(Code11 == fail
		->	Code = fail
		;	combine_and([PreCode1,PreCode22, Code11], Code111),
			(	Cont == true
			->	codeCmpSeqRest(RHS2, VLIn, Code111, NumCallRHS, RArgCode, Code)
			;	Code = Code111
			)
		)
	).






/*
compile_holds(_Holds1, _, Holds, _, _BindingVars,fail) :-
	\+ call(Holds),
	!,
	warning('Holds term ~w in formula never true',[Holds]).

*/


transHoldsState(State, PN, VLInit, VLIn, VLOut, true, PostCode, S1) :-
	isVarDNewDataS(State, VLIn, Var, Sort, Data, NewData, VLOut,Status),
	!,
	var(Status),
	holdsbound(PN, Binding),
	transVar(Var, Sort, Data, VLInit, Binding,NewData, PostCode, S1, _PreBound).
transHoldsState(Term, PN, VLInit, VLIn, VLOut, PreCode, PostCode, S1) :-
	spec_constant(Term, Term2),
	test_recursive_constant(Term, Term2),
	!,
	(transHoldsState(Term2, PN, VLInit, VLIn, VLOut, PreCode, PostCode, S1)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).
transHoldsState(State, PN, VLInit, VLIn, VLOut, PreCode, PostCode, S1) :-
	(isStateTerm(State, TimeTerm, OtherTerms, TimeTerm1, OtherTerms1, S1)
	->	true
	;	error('holds state argument not a state:~w', [State]),
		fail
	),
	holdsbound(PN, Binding),
	transTerms(OtherTerms, VLInit, Binding, VLIn, VL1, PreCode1, PostCode1,
		   OtherTerms1,_Kinds, _PreBound1),
	transHoldsTime(TimeTerm, PN, VLInit, VL1, VLOut, PreCode2, PostCode2,
		       TimeTerm1),
	combine_and([PreCode1, PreCode2], PreCode),
	combine_and([PostCode1, PostCode2], PostCode).


transTerms(Terms, VLIn, PreCode2, Terms1) :-
	transTerms(Terms, VLIn, needs_bound,VLIn, VLOut, PreCode2, PostCode2,
		   Terms1, _Kinds, PreBound2),
	assert_debug(VLOut == VLIn),
	assert_debug(PreBound2 == prebound),
	assert_debug(PostCode2 == true).


transTerms([], _VLInit, _Binding, VLIn, VLIn, true, true, [], [],prebound).
transTerms([Term|Terms], VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
	   [Term1|Terms1], [Kind|Kinds], PreBound) :-
	transTerm(Term, VLInit, Binding, VLIn, VL1, PreCode1, PostCode1,
		  Term1, Kind, PreBound1),
	transTerms(Terms, VLInit, Binding,VL1, VLOut, PreCode2, PostCode2,
		   Terms1, Kinds, PreBound2),
	combine_and([PreCode1, PreCode2], PreCode),
	combine_and([PostCode1, PostCode2], PostCode),
	combine_pre_bound(PreBound1, PreBound2, PreBound).
combine_pre_bound(prebound, prebound, prebound) :-
	!.

combine_pre_bound(_, _, nextbound).








intervalInteger(Low, Hi, I, Term1) :-
	assert_debug(integer(I)),
	holds:interval(I, BegVal, EndVal),
	Hi >= BegVal,
	Low < EndVal,
	LowT is truncate(BegVal),
	Hi1T is truncate(EndVal),
	(Hi1T < EndVal
	->	HiT = Hi1T
	;	HiT is Hi1T - 1
	),
	L is max(Low, LowT),
	H is min(Hi, HiT),
	between(L, H, Term1).

intervalInteger(I, Int) :-
	assert_debug(integer(I)),
	holds:interval(I, BegVal, EndVal),
	Low is truncate(BegVal),
	Hi1 is truncate(EndVal),
	(Hi1 < EndVal
	->	Hi = Hi1
	;	Hi is Hi1 - 1
	),
	between(Low, Hi, Int).

codeTimeInterval(RealTime, I, Code) :-
	(var(RealTime)
	->	Code = transnew:timeInterval(RealTime, I)
	;number(RealTime)
	->	(	holds:interval(I, BegVal, EndVal),
			RealTime >= BegVal,
			RealTime < EndVal
		->	Code = true
		;	warning('Time ~w falls outside of trace range', [RealTime]),
			Code = fail
		)
	;	error('Expected a number for interval identification, got ~w',
		      [RealTime]),
		fail
	).

int_interval_time(Interval, Time) :-
	holds:interval(Interval, BegVal, EndVal),
	L is ceil(BegVal),
	H is floor(EndVal),
	between(L, H, Time),
	assert_debug(Time >= BegVal),
	Time < EndVal.

timeInterval(RealTime, I) :-
	(number(RealTime)
	->	holds:interval(I, BegVal, EndVal),
		RealTime >= BegVal,
		RealTime < EndVal
	;	error('Time must be interval or number'),
		fail
	).

codeIntervalLimit(BegEnd, I, Value, Code) :-
	(ground(BegEnd)
	->	assert_debug(memberchk(BegEnd, [begin, end])),
		(ground(I)
		->	(holds:interval(I, BegVal, EndVal)
			->	begend(BegEnd, BegVal, EndVal, Value)
			;	error('Missing interval ~w', [I]),
				fail
			),
			Code = true
		;	begend(BegEnd, BegVal, EndVal, Value),
			Code = holds:interval(I, BegVal, EndVal)
		)
	;	Code = transnew:intervalLimit(BegEnd, I, Value)
	).

intervalLimit(BegEnd, I, Value) :-
	assert_debug(ground(BegEnd)),
	assert_debug(memberchk(BegEnd, [begin, end])),
	(integer(I)
	->	true
	;	error('expected integer interval, got ~w', [I]),
		fail
	),
	(holds:interval(I, BegVal, EndVal)
	->	begend(BegEnd, BegVal, EndVal, Value)
	;	error('Missing interval ~w', [I]),
		fail
	).
begend(begin, BegVal, _EndVal, BegVal).
begend(end, _BegVal, EndVal, EndVal).

transTerm(Term, VLIn, PreCode, TermOut) :-
	transTerm(Term, VLIn, needs_bound, VLIn, VLOut, PreCode, PostCode,
		  TermOut,_Kind, PreBound),
	assert_debug(PreBound == prebound),
	assert_debug(VLOut == VLIn),
	assert_debug(PostCode == true).


% Binding: bound or needs_bound:
%          bound: TermOut will be bound runtime (after PreCode is called)
%          needs_bound: TermOut must be bound or instantiated before
%                       being used
transTerm(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, TermOut,
	  Kind, PreBound) :-
	assert_debug(nonvar(Binding)),
	assert_debug(memberchk(Binding, [bound, needs_bound, bound_lost])),
	transTerm1(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
		   TermOut,Kind, PreBound),
	assert_debug((Binding==needs_bound->PreBound == prebound;true)),
	assert_debug(nonvar(PreCode)),
	assert_debug(nonvar(PostCode)).




transTerm1(Term, VLInit, Binding, VLIn, VLOut, true, PostCode, Term1,Kind,
	   PreBound) :-
	isVarDNewDataS(Term, VLIn, Var, Sort, Data, NewData, VLOut,Status),
	!,
	var(Status),
	transVarK(Var, Sort, Data, VLInit, Binding, NewData,PostCode,Term1, Kind,
		  PreBound).
transTerm1(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, Term1,Kind,
	   PreBound) :-
	getExpr(Term, VLIn, What, Status),
	!,
	var(Status),
	transExprWhat(What, VLInit, Binding, VLIn, VLOut, PreCode1, PostCode1,
		  PreBound1,Kind, TermOut),
	codeTermExpr(PreCode1, PostCode1, PreBound1, VLOut, TermOut, PreCode,
		     PostCode, Term1, PreBound).

transTerm1(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, Term1,Kind,
	   PreBound) :-
	spec_constant(Term, Term2),
	test_recursive_constant(Term, Term2),
	!,
	(transTerm1(Term2, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
		    Term1,Kind,PreBound)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).

transTerm1(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, Term1,
	   compound,PreBound) :-
	Term =.. [F|Terms],
	transTerms(Terms, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
		   Terms1, _,PreBound),
	Term1 =.. [F|Terms1].

:- dynamic dyn_warn_pi_e/1.


codeTermExpr(PreCode1, PostCode1, PreBound1,VLOut,TermOut, PreCode, PostCode,
	     Term1, PreBound) :-
	(ground(TermOut)
	->	assert_debug(number(TermOut)),
		assert_debug(PreCode1== true),
		assert_debug(PostCode1== true),
		Term1 = TermOut,
		PreBound = prebound,
		PreCode = PreCode1,
		PostCode = PostCode1
	;	PreBound1 == prebound
	->	assert_debug(PostCode1== true),
		PostCode = true,
		(var(TermOut)
		->	Term1 = TermOut,
			PreCode = PreCode1
		;	combine_and([PreCode1,Term1 is TermOut], PreCode)
		),
		PreBound = prebound
	;PreBound1==initialbound
	->	PreCode = PreCode1,
		assert_debug(var(TermOut)),
		%assert_debug(PostCode \= true),could be new substitution of
		% prebound + initialbound -> intialbound
		Term1 = TermOut,
		PostCode = PostCode1,
		(	plVarDFromVL(VLOut, TermOut,_Var, _Sort, Data),
			Data = bound(PVar, Sort1),
			assert_debug(PVar == TermOut)
		->	check_sort_element_code(Sort1, Term1, SCode),
			combine_and([PostCode1,SCode], PostCode),
			PreCode = PreCode1
		;	true
		),
		PreBound = initialbound % SUSPECT
	;	assert_debug(PreBound1==nextbound),
		assert_debug(var(TermOut)),
		Term1 = TermOut,
		PreCode = PreCode1,
		PostCode = PostCode1,
		PreBound = nextbound % SUSPECT
	).

transExprs(Ts, VLIn, PreCode,TSs) :-
	transExprs(Ts, VLIn, needs_bound, VLIn, VLOut,PreCode,PostCode,
		   PreBounds,_Kinds,TSs),
	assert_debug(uchecklist(==(prebound), PreBounds)),
	assert_debug(PostCode == true),
	assert_debug(VLOut == VLIn).


transExprs([], _VLInit, _Binding, VLIn, VLIn, true, true, [], [], []).
transExprs([T|Ts],VLInit,Binding, VLIn, VLOut, PreCode,PostCode,
	   [PreBound|PreBounds],[Kind|Kinds], [TS|TSs]) :-
	transExpr(T, VLInit,Binding, VLIn, VL1, PreCode1, PostCode1, PreBound,
		  Kind, TS),
	transExprs(Ts, VLInit, Binding, VL1, VLOut,PreCode2,PostCode2,
		   PreBounds,Kinds,TSs),
	combine_and([PreCode1,PreCode2], PreCode),
	combine_and([PostCode1,PostCode2], PostCode).








transExpr(T, VLIn, PreCode, TS) :-
	transExpr(T, VLIn, PreCode, TS, _Kind).

transExpr(T, VLIn, PreCode, TS, Kind) :-
	transExpr(T, VLIn,needs_bound, VLIn, VLOut, PreCode, PostCode,
		  PreBound, Kind, TS),
	assert_debug(PreBound == prebound),
	assert_debug(VLOut == VLIn),
	assert_debug(PostCode == true).



/*
  PreBound = prebound
  In transExpr used to distinguish variables that are just now
  bound compared to terms that are bound before.

  So a variable that occurs and has "initial" entry will be
  initialbound. Such a variable occurring in an expression needs
  special code:
  inboundvar:REAL + 1
  ->     we replace result by Res and add postcode IBVar is Res - 1
  but now this whole term becomes initialbound
  */

transExprS(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, PreBound,
	   Kind, TOut, Status) :-
	getExpr(Term, VLIn, What, Status),!,
	(var(Status)
	->	transExprWhat(What, VLInit,Binding,VLIn,VLOut,PreCode,PostCode,
			      PreBound, Kind, TOut)
	;	true
	).
transExprS(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
	  PreBound,Kind,Interval, X) :-
	error('(sub)term "~w" is not a valid numerical expression',Term),
	(is_local
	->	local_trace(transExprFailed),
		transExprS(Term, VLInit, Binding, VLIn, VLOut,PreCode,PostCode,
			   PreBound,Kind,Interval, X)
	;	X = error
	).


transExpr(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, PreBound,
	  Kind,TOut) :-
	transExprS(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode, PreBound,
		   Kind,TOut, Status),
	(var(Status)
	->	true
	;	error('Some error in expression'),
		fail
	).
transExprWhat(var(Var, Sort, Data,NewData,VLOut), VLInit, Binding, _VLIn,VLOut,
	      true, PostCode, PreBound, Kind, TOut) :-
	transVarK(Var,Sort,Data, VLInit,Binding,NewData,PostCode,TOut,Kind,
		  PreBound),
	(memberchk(Kind, [integer, real])
	->	true
	;	error('Cannot deal with variable ~w of kind "~w"(sort ~w) in expression',
		      [Var, Kind, Sort]),
		fail
	).
transExprWhat(number(Term,Kind), _VLInit, _Binding, VLIn, VLIn,
	      true, true, prebound, Kind, Term).
transExprWhat(arop(Ts, TSs, Term1), VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, Kind, TermOut) :-
	!,
	transExprs(Ts, VLInit, Binding, VLIn, VLOut, PreCode, PostCode1,
		   PreBounds,Kinds, TSs),
	(member(initialbound, PreBounds)
	->	(invert_arop(Term1,TSs, PreBounds, Kinds, Kind, PostCode2,
			     TermOut)
		->	PreBound = initialbound,
			combine_and([PostCode2, PostCode1], PostCode)
		;	error('Cannot invert variable from expression ~w',
			      [Ts]),
			fail
		)
	;	aropkind(Term1, Kinds, Kind),
		(member(nextbound, PreBounds)
		->	assert_debug(\+ ground(Term1)),
			combine_and([PostCode1,TermOut is Term1], PostCode),
			PreBound = nextbound
		;	(ground(Term1)
			->	TermOut is Term1
			;	TermOut = Term1
			),
			PostCode = PostCode1,
			PreBound = prebound
		)
	).
/* What should happen with bindings? We should first require everything to be
   bound
  We really need bound, because otherwise?
   If TTL fails for specific binding and we had kept the Binding,
   we might not need to restrict ourselves to needs_bound.
   OK: TTL?E1:E2
   if Bindings==needs_bound -> no problem.
   otherwise, we may use :
   or(and(TTL, R = E1), and(not(TTL, R = E2)))
*/
transExprWhat(case(TTL, ThenExpr, ElseExpr), VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, RealInteger, CaseVar) :-
	(Binding == needs_bound
	->	true
	;	warning('Please contact lourens:you have an interesting specification which I may be able to optimize')
	),
	codePrologF(TTL, pos, VLIn, VLOut, Code1),
	assert_debug(VLIn == VLOut),
	transExpr(ThenExpr, VLInit,needs_bound, VLOut, VL1, PreCode1,PostCode1,
		  PreBound1, Kind1, T),
	transExpr(ElseExpr, VLInit,needs_bound, VLOut, VL2, PreCode2,PostCode2,
		  PreBound2, Kind2, E),
	min_kind([Kind1, Kind2], RealInteger),
	assert_debug(prebound==PreBound1),
	assert_debug(prebound==PreBound2),
	PreBound = prebound,
	assert_debug(PostCode1 == true),
	assert_debug(PostCode2 == true),
	assert_debug(VL1 == VLOut),
	assert_debug(VL2 == VLOut),
	combine_and([PreCode1, CaseVar=T], Then),
	combine_and([PreCode2, CaseVar=E], Else),
	code_ite(Code1, Then, Else, PreCode),
	PostCode = true.

transExprWhat(closure(Ranges, InitialValue, ClosureVar, Function),
	      VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, Kind, Result) :-
	(isVar(ClosureVar, _VarName, _Sort)
	->	true
	;	error('Expected closure variable, got ~w', [ClosureVar]),
		fail
	),
	(Ranges == []
	->	codeInitialClosure(ClosureVar, InitialValue, VLIn, Result, Kind,
				   PreCode, PostCode)
	;is_list(Ranges)
	->	transExprRangesClosure(Ranges, InitialValue, ClosureVar,
				       Function, VLInit,
				       Binding, VLIn, VLOut, PreCode,
				       PostCode, PreBound, Result, Kind)
	;	transExprRangesClosure([Ranges], InitialValue, ClosureVar,
				       Function, VLInit,
				       Binding, VLIn, VLOut, PreCode,
				       PostCode, PreBound, Result, Kind)
	).


transExprWhat(interval(Term), VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, integer, Interval) :-
	intervalTerm(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
		     Interval,PreBound).
transExprWhat(interval(What, T1), VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, real, Term1) :-
	transTerm(T1, VLInit, Binding, VLIn, VLOut,PreCode1,PostCode1,
		  I, Kind, PreBound),
	(PreBound == prebound
	->	code_assert_integer(I, Kind, PreCode2, T1),
		codeIntervalLimit(What, I, Term1, CodeI),
		combine_and([PreCode1, PostCode1, PreCode2, CodeI], PreCode),
		PostCode = true
	;PreBound == initialbound
	->	assert_debug(var(I)),
		codeIntervalLimit(What, I, Term1, Code),
		combine_and([Code, PostCode1], PostCode)
	;PreBound == nextbound
	->	PreCode = PreCode1,
		% need to adjust postcode
		% result could be A + 3 result
		assert_debug(ground(What)),
		assert_debug(\+ ground(I)),
		assert_debug(memberchk(BegEnd, [begin, end])),
		begend(BegEnd, BegVal, EndVal, Term1),
		Code = holds:interval(I, BegVal, EndVal),
		combine_and([PostCode1, Code], PostCode)
	;	pr_error('PreBound?'),
		fail
	).
transExprWhat(background(Term,Data), VLInit, Binding, VLIn, VLOut,
	      PreCode, PostCode, PreBound, real, Result) :-
	Term =.. [F|Terms],
	transTerms(Terms, VLInit, Binding, VLIn, VLOut,PreCode1,PostCode,
		  Ress1, _Kinds, PreBound),
	(PreBound == prebound
	->	Res1 =.. [F|Ress1],
		combine_and([PreCode1, background_function(Data,Res1,Result)],PreCode)
	;	pr_error('Expected ground background function:~w', [Term])
	).

codeInitialClosure(ClosureVar, InitialValue, VLIn, Result,Kind,PreCode, PostCode) :-
	codeInitialClosure(ClosureVar, InitialValue, VLIn, Result,Kind,PreCode, PostCode,
		_Sort, _Sort1, _VarName).

codeInitialClosure(ClosureVar, InitialValue, VLIn, Result,Kind,PreCode,
		   PostCode,Sort,Sort1, VarName) :-
	isVar(ClosureVar, VarName, Sort),
	transTerm(Sort, VLIn, SCode, Sort1),
	(check_numeric_sort(VarName,Sort,Kind)
	->	true
	;	warning('Closure expects simple numeric sort variables,not ~w',
			[Sort])
	),
	transExpr(InitialValue, VLIn, PreCode1, Result, Kind2),
	combine_and([SCode,PreCode1], PreCode),
	(memberchk(Kind2, [real, integer])
	->	true
	;	error('Initial value not numeric:~w', [InitialValue]),
		fail
	),
	check_sort_element_code(Sort1, Result, CheckCode),
	code_ite(CheckCode,true,(error('Value ~w out of sort range ~w',
				       [Result, Sort1]),fail), PostCode).
transExprRangesClosure(Ranges, InitialValue, ClosureVar, Function, _VLInit,
		       Binding, VLIn, VLOut, PreCode, PostCode, prebound,
		       Result, Kind) :-
	assert_debug(Binding == needs_bound),
	codeInitialClosure(ClosureVar,InitialValue, VLIn, IV,Kind,
			   PreCodeIV,
			   PostCode, Sort, Sort1, VarName),
	assert_debug(PostCode == true),
	padd_q_vars(Ranges, initial, VLIn, VL1, [], QVars, CodeQSet),
	varDToVarList(VarName, Sort, bound(ClosureVarP, Sort1), VL1, VL2),
	vars_in_formula(Function > 0, pos, VL2, QVars, QOut),
	setup_qcode_all_needs_bound(QOut, VL2, VL3, _PVars, CodeQNext),
	transExpr(Function, VL3, PreCodeF, Function1, KindF),
	(KindF == real
	->	true
	;	KindF == Kind
	->	true
	;	warning('Expression type mismatch')
	),
	/*
	combine_and([Code1, bagof(PVars, Code2, PVarsList), PreCode1,PreCodeF,
		     transnew:closure(PVarsList, PVars, ClosureVarP, Function1,
				      Sort1,IV, Result)], PreCode),
	  */
	flag(closure_container, I, I + 1),
	atom_concat(container, I, ContainerFlag),
	code_test_closure(Ranges, InitialValue, ClosureVar, Function, Result, DebugCode),
	combine_and([CodeQNext,PreCodeF,flag(ContainerFlag, ClosureVarP, Function1)],
		    CodeOneApplication1),
	(memberchk(CodeOneApplication1,[fail,true])
	->	CodeLoop = true
	;	CodeLoop = (CodeOneApplication1,fail;true)
	),
	combine_and([PreCodeIV,CodeQSet,flag(ContainerFlag, Old, IV),CodeLoop,
		     flag(ContainerFlag, Result,Old),DebugCode],PreCode),
	assert_debug(PreCode \= fail),
	rm_local_qvars(QVars, VL3, VL4),
	rm_local_vars([VarName], VL4, VLOut).

code_test_closure(Ranges, InitialValue, ClosureVar, Function, Result, Code) :-
	((true;is_local)
	->	Code = format('Result of ~w is ~w~n',
		       [closure(Ranges, InitialValue, ClosureVar, Function), Result])
	;	Code = true
	).

code_test_sort(Sort1, Val) :-
	(check_ground_sort_value(Sort1, _, Val)
	->	true
	;	error('Value ~w outside of Sort ~w',
		      [Val, Sort1]),
		fail
	).
closure([], _PVars, _ClosureVarP,_Function1, Sort1, IV, IV) :-
	(check_ground_sort_value(Sort1, _, IV)
	->	true
	;	error('closure value ~w outside of Closure Sort ~w',
		      [IV, Sort1]),
		fail
	).


closure([PVars1|PVarsList], PVars, ClosureVarP, Function1, Sort1, Result1, Result) :-
	copy_term((PVars,Function1,ClosureVarP),(PVars2,Function2,ClosureVarP2)),
	PVars1 = PVars2,
	ClosureVarP2 = Result1,
	Result2 is Function2,
	(check_ground_sort_value(Sort1, _, Result1)
	->	true
	;	error('closure value ~w outside of Closure Sort ~w',
		      [Result1, Sort1]),
		fail
	),
	closure(PVarsList, PVars, ClosureVarP, Function1, Sort1, Result2, Result).





code_assert_integer(I, Kind, PreCode2,Term) :-
	(Kind == integer
	->	PreCode2 = true
	;Kind == real
	->	warning('Expected interval term, kind integer, got real'),
		PreCode2 = (integer(I)->true;format('ERROR:expected integer got ~w~n',
						    [I]),fail)
	;	error('Term ~w:Expected integer argument kind, got ~w',
		      [Term, Kind]),
		fail
	).




check_numeric_sort(Var,Sort) :-
	check_numeric_sort(Var,Sort,_Kind).

check_numeric_sort(Var,Sort,Kind) :-
	ensure_sort_kind(Sort,Kind),
	(memberchk(Kind, [real, integer])
	->	true
	;	error('Arithmetic expression containing non arithmetic variable ~w',
		      [Var:Sort])
	).

% PreBound:
%     initialbound: variable occurs first time, Binding was bound
%     prebound:     variable was bound already in VLInit
%     nextbound:    variable has been bound somewhere earlier after VLInit
transVarK(Var, Sort, Data, VLInit,Binding,NewData, PostCode, Term1, Kind,PreBound) :-
	transVar(Var, Sort, Data, VLInit, Binding, NewData, PostCode, Term1,
		 PreBound),
	((NewData = bound(_, Sort1)
	 ;	 NewData = bound_lost(_, Sort1)
	 )
	->	ensure_sort_kinde(Sort1, Kind, Status),
		var(Status)
	;	pr_error('missing variable stage2')
	).

transVar(Var, Sort, Data, VLInit, Binding,NewData,PostCode, Term1, PreBound) :-
	(Data = initial(Call, Sort1)
	->	(Binding == needs_bound
		->	pr_error('Needs_bound but not bound ~w', [Var]),
			fail
		;	code_var_bind(Binding,Sort1,NewData,Call,PostCode,
				      Term1),
			PreBound = initialbound
		)
	;	(Data = bound(PVar, _Sort11), B1 = bound
		;Data = bound_lost(PVar, _Sort12), B1 = bound_lost
		)
	->	Term1 = PVar,
		PostCode = true,
		NewData = Data,
		ensureVarDInVarListS(VLInit, Var, Sort,Data1,Status),
		var(Status),
		(Data1 = bound(_,_)
		->	PreBound = prebound,
			assert_debug(Data1 == Data)
		;	assert_debug(Data1 \= bound_lost(_,_)),
			PreBound = nextbound,
			assert_debug(B1 == Binding)
		)
	;	pr_error('transVar data= ~w', [Data]),
		fail
	).

code_var_bind(Binding,Sort1, NewData, Call, PostCode, PVar) :-
	assert_debug(memberchk(Binding, [bound,bound_lost])),
	NewData =..[Binding, PVar, Sort1],
	(Call == true
	->	check_sort_element_code(Sort1, PVar, PostCode)
	;	Call = call(Op, X3)
	->	check_sort_element_code(Sort1, PVar, CodeS),
		ensure_sort_kinde(Sort1, Kind, Status),
		var(Status),
		(Op = =
		->	pr_error('= operator should have been handled at Q'),
			fail
		;	(       (memberchk(Kind, [integer,real])
				;Op = \=
				)
			->	true
			;	impl_error('Operation ~w error', [Op]),
				fail
			),
			Code =.. [Op, PVar, X3],
			combine_and([CodeS,Code], PostCode)
		)
	;	Call = call2(OpL, OpR, LHS, RHS)
	->	ensure_sort_kinde(Sort1, Kind, Status),
		var(Status),
		(memberchk(Kind, [integer,real])
		->	true
		;	impl_error('uncaught:multi restriction range only defined for numbers'),
			fail
		),
		%assert_debug(R1 == Call2),
		check_sort_element_code(Sort1, PVar, CodeS),
		CodeL =.. [OpL, LHS, PVar],
		CodeR =.. [OpR, PVar, RHS],
		combine_and([CodeS, CodeL,CodeR], PostCode)
	;	pr_error('Unrecognised Call ~w', [Call]),
		fail
	).

transHoldsTime(Time, _PN, _VLInit, _VLIn, _VLOut, true, _PostCode, _Time1) :-
	var(Time),
	!,
	pr_error('Do not use prolog variable time'),
	fail.

/*
  transHoldsTime(Term, PN, VLIn, VLOut, PreCode, PostCode, Interval)
  Important: if time contains expressions, the "is" call should be
  in PreCode if the RHS was pre-bound, otherwise the call should be
  in PostCode.
  */
transHoldsTime(time(Term), PN, VLInit,VLIn, VLOut, PreCode, PostCode, Time1) :-
	!,
	holdsbound(PN, Binding),
	transTerm(Term, VLInit, Binding, VLIn, VLOut, PreCode1, PostCode1, Time1,
		  Kind, PreBound),
	(PreBound == prebound
	->	(ground(Time1)
		->	(integer(Time1)
			->	PreCode2 = true
			;	error('Expected integer interval as holds time(X) argument, got ~w', [Time1]),
				fail
			)
		;	code_assert_integer(I, Kind, PreCode2, Term)
		),
		combine_and([PreCode1,PreCode2], PreCode),
		PostCode = PostCode1
	;	code_assert_integer(I, Kind, PostCode2,Term),
		PreCode = PreCode1,
		combine_and([PostCode1,PostCode2], PostCode)
	).






/*
  If Term is bound, no problem => add code for RealTime => Interval
  Otherwise, if it is an integer like sort, we may hack in betweens
  */
transHoldsTime(Term, PN, VLInit, VLIn, VLOut, PreCode, PostCode, Interval) :-
	holdsbound(PN, Binding),
	intervalTerm(Term, VLInit, Binding, VLIn, VLOut, PreCode, PostCode,
		     Interval, _PreBound),
	(	var(Interval),
		plVarDFromVL(VLOut, Interval,_Var, _Sort, Data)
	->	(Data = bound(_PVar, Sort1)
		->	(nonvar(Sort1),
				isIntervalSort(Sort1)
			->	warning('You are doing something really WEIRD,
				       you should probably put time(..) around your time argument')
			;	true
			)
		;	pr_error('Strange binding'),
			fail
		)
	;	true
	).





% Term is a number, needs to be mapped onto Interval number
% PreBound says that variables were Bound before this term
intervalTerm(Term, VLInit, Binding, VLIn, VLOut, PreCode,PostCode,Interval,
	     PreBound) :-
	transTerm(Term,VLInit,Binding,VLIn,VLOut,PreCode1,PostCode1,RealTime,
		  Kind, PreBound),
	(PreBound == prebound
	->	codeTimeInterval(RealTime, Interval, CodeI),
		combine_and([PreCode1,CodeI],PreCode),
		PostCode = PostCode1
	;PreBound == nextbound
	->	codeTimeInterval(RealTime, Interval, CodeI),
		combine_and([PostCode1,CodeI],PostCode),
		PreCode = PreCode1
	;	assert_debug(var(RealTime)),
		assert_debug(PreBound == initialbound),
		(Kind == integer
		->	combine_and([transnew:int_interval_time(Interval,
								RealTime),
				     PostCode1],
				    PostCode),
			PreCode = PreCode1
		;Kind == real
		->	error('Cannot have uninstantiated real for time argument ~w',
			      [Term]),
			local_trace(can),
			fail
		;	error('Expected time argument, got nonnumeric type argument ~w',
			      [Term]),
			fail
		)
	).



codePrologFA(AndOr, Args, PN, VLIn, VLOut, Code) :-
	memberchk(AndOr, [and, or]),
	!,
	codePrologConDisj(Args, AndOr, PN, VLIn, VLOut, Code).
codePrologFA(F, Args, PN, VLIn, VLOut, Code) :-
	exallop(F,ExAll1),
	codePrologQ(ExAll1, Args, PN, VLIn, VLOut, Code).
codePrologConDisj(Args, AndOr, PN, VLIn, VLOut, Code) :-
	andorpnandor(AndOr, PN, AndOr1),
	(AndOr1 == and
	->	codePrologConjs(Args, PN, VLIn, VLOut, Code)
	;	codePrologDisjs(Args, PN, VLIn, VLOut, Code)
	).
codePrologQ(ExAll, Args, PN, VLIn, VLOut, Code) :-
	isolate_qvars(Args, ExAll, VLIn, VL1, QVars, Formula, Code1),
	(ExAll == forall
	->	codePrologAll(Formula, PN, VL1, VL2, QVars, Code2)
	;	codePrologEx(Formula, PN, VL1, VL2, QVars, Code2)
	),
	combine_and([Code1, Code2], Code),
	rm_local_qvars(QVars, VL2, VLOut).



codePrologConjs(Args, PN, VLIn, VLOut, Code) :-
	codePrologConjsL(Args, PN, VLIn, VLOut, CodeL),
	combine_and(CodeL, Code).
codePrologConjsL([], _PN, VLIn, VLIn, []).
codePrologConjsL([Arg|Args], PN, VLIn, VLOut, [Code|CodeL]) :-
	codePrologF(Arg, PN, VLIn, VL1, Code),
	(Code == fail
	->	CodeL = [],
		VLOut = VL1
	;	codePrologConjsL(Args, PN, VL1, VLOut, CodeL)
	).


/*
  If variables should propagate past this disjunction, or even may
  propagate, then all disjunctions should contain the variable.
  In such a case we need to backtrack.
  */
codePrologDisjs([], _PN, VLIn, VLIn, fail).
codePrologDisjs([Arg|Args], PN, VLIn, VLOut, Code) :-
	codePrologF(Arg, PN, VLIn, VLOut1, CodeL),
	codePrologDisjs(Args, PN, VLIn, VLOut2, CodeR),
	(CodeL == true
	->	VLOut = VLOut1,
		Code = CodeL
	;CodeR == true
	->	VLOut = VLOut2,
		Code = CodeR
	;CodeL == fail
	->	VLOut = VLOut2,
		Code = CodeR
	;CodeR == fail
	->	VLOut = VLOut1,
		Code = CodeL
	;	disj_vl_and_backtrack(VLIn, VLOut1, VLOut2, BacktrackTF, VLOut,
				      CodeL1,CodeR1),
		combine_and([CodeL, CodeL1], CodeL2),
		combine_and([CodeR, CodeR1], CodeR2),
		combine_or_bt(BacktrackTF, [CodeL2,CodeR2], Code)
	).

/*
  This is in codeProlog phase, so all needed variables should be
  correct wrt boundedness. First we need to combine the instness by
  taking the least bounded instness of all  disjs.
  Backtracking will be needed if some variable gets its first bound in
  multipl disjunctions.

  Instantiation could be mixed i.e. bound(PVar1, Sort)
                               and  initial(Call, Sort1)
  Variable could occur only in one side of disjunction, then
    variable should not occur in VLIn and may be left out (is local qvar)

  Mixed case: if lateron the variable occurs, it should not have needs_bound

  We add a case v(mixed, mixed(PVar, Call, Sort
  */
disj_vl_and_backtrack(VLIn, VLOut1, VLOut2, BackTrackTF, VLOut, CodeL,CodeR) :-
	disj_vl_and_backtrack1(VLIn, VLOut1, VLOut2, VLOut, CodeL,CodeR),
	(	(
			VLIn == []
		;
			VLOut1 == VLIn
		)
	->	BackTrackTF = false
	;	BackTrackTF = true
	).
disj_vl_and_backtrack1([], _VLOutL, _VLOutR, [], true,true).

disj_vl_and_backtrack1([ds_vd(Var1, Sort1, DataIn)|VLIn], VLOutL, VLOutR,
		       VLOut, CodeL,CodeR) :-
	(	varDFromVarList(VLOutL, Var1, Sort1, DataL, VLOutL1),
		varDFromVarList(VLOutR, Var1, Sort1, DataR, VLOutR1)
	->	combineLeave(DataIn, DataL, DataR, Var1, DataOut, CodeL1,
			     CodeR1),
		disj_vl_and_backtrack1(VLIn, VLOutL1, VLOutR1, VLOut1, CodeL2,
				       CodeR2),
		varDToVarList(Var1, Sort1, DataOut, VLOut1, VLOut),
		combine_and([CodeL1, CodeL2], CodeL),
		combine_and([CodeR1, CodeR2], CodeR)
	;	pr_error('Missing variable from disjunction'),
		fail
	).
combineLeave(DataIn, initial(Call1, Sort1), initial(Call2,Sort2),
	     _Var, initial(Call1, Sort1), true,true) :-
	!,
	(	Call1 == Call2,
		Sort1 == Sort2,
		DataIn == initial(Call1, Sort1)
	->	true
	;	pr_error('Unhandled disjunction'),
		fail
	).
combineLeave(DataIn, Data1, Data2, _Var, DataIn,
	     true, true) :-
	DataIn \= initial(_Call1, _Sort1),
	!,
	(	Data1 == DataIn,
		Data2 == Data2
	->	true
	;	pr_error('Unhandled disjunction'),
		fail
	).

combineLeave(initial(_, S1), Data1, Data2, _Var, bound_lost(PVar1, Sort1),
	     true, true) :-
	memberchk(bound_lost(PVar1, Sort1), [Data1, Data2]),
	assert_debug(S1 == Sort1),
	!.


combineLeave(initial(_, S1), bound(VarL, SL), bound(VarR, SR), Var,
	     bound(VarN, S1), CodeL, CodeR) :-
	!,
	(	var(VarL),
		var(VarR)
	->	VarN = VarL,
		VarR = VarL,
		CodeL = true,
		CodeR = true,
		(is_local
		->	format('DISJ VARS combined:~w ~w~n', [Var, VarL])
		;	true
		)
	;	CodeL = (VarN = VarL),
		CodeR = (VarN = VarR),
		(is_local
		->	format('DISJ VARS ASSIGNED:~w ~w ~w ~w~n', [Var, VarL,
								    VarR, VarN])
		;	true
		)
	),
	assert_debug(S1 == SL,S1 == SR).
combineLeave(initial(_, S1), Data1, Data2, _Var, bound_lost(PVar1, Sort1),
	     true, true) :-
	select(bound(PVar1, Sort1), [Data1, Data2], [DataLeft]),
	!,
	assert_debug(DataLeft = initial(_, S1)).

combineLeave(DataIn, DataL, DataR, Var,_DataOut, true, true) :-
	pr_error('Unimplemented mixed disjunction for variable ~w:~w ~w ~w:contact lourens@cs.vu.nl please',
		 [Var, DataIn, DataL, DataR]),
	fail.


codePrologDisjsL([], _PN, VLIn, VLIn, []).
codePrologDisjsL([Arg|Args], PN, VLIn, [VLOut|VLOutL], [Code|CodeL]) :-
	codePrologF(Arg, PN, VLIn, VLOut, Code),
	codePrologDisjsL(Args, PN, VLIn, VLOutL, CodeL).
/*
  forall(Q, implies(F1, F2))
a :-
	\+	(preinstantiate,
		code(f1),
		code(not(f2))
		).

forall(Q, F) :-
	\+ (	instantiate,
		code(not(F))
	   ).
*/
/*
  whole formula to be checked:
  once if mark_formula_by_id_next

  print formula if not mark_formula_by_id_next or
  if not log_every_binding

  print id if mark_formula_by_id_next and not first occurrence and
  log_every_binding
  */
:- dynamic verbose_compiling/1.
kind_text(log_binding_succeeded_exist, 'Formula ~w SUCCEEDED for instantiation ').
kind_text(log_binding_failed_forall, 'Formula ~w FAILED for instantiation ').
kind_text(log_every_binding, 'Formula ~w NEXT instantiation ').

kind_inform(Kind, Streams, Text) :-
	(kind_text(Kind, Text)
	->	true
	;	pr_error('Unhandled verbose-kind'),
		fail
	),
	verbose_compiling(V),
	(V = advanced(Streams, Options)
	->	memberchk(Kind, Options)
	;	pr_error('Unhandled verbose mode'),
		fail
	).
kind_inform(Kind, Formula, QVars, VLOut, Code) :-
	(kind_inform(Kind, Streams, Text)
	->	kind_inform_streams(Streams, Text, Formula, QVars, VLOut, Code)
	;	Code = true
	).
kind_inform_streams([], _Text, _Formula, _QVars, _VLOut, true).

kind_inform_streams([Stream|Streams], Text, Formula, QVars, VLOut, Code) :-
	kind_inform_stream(Stream, Text, Formula, QVars, VLOut, Code1),
	kind_inform_streams(Streams, Text, Formula, QVars, VLOut, Code2),
	combine_and([Code1, Code2], Code).
kind_inform_stream(Stream, Text, Formula, QVars, VLOut, Code) :-
        (is_local
        ->     flag(dkis, I, I + 1),
               format('DKIS:~w~n', [I]),
               (I = 10
               ->     local_trace(q10)
               ;      true
               )
        ;      true
        ),
	qvarInstInform(QVars, Stream, VLOut, InstCode),
	combine_and([format(Stream, Text, [Formula]),InstCode], Code).

succeededInform(Formula, QVars, VLOut, Code) :-
	kind_inform(log_binding_succeeded_exist,
		   Formula, QVars, VLOut, Code).
failedInform(Formula, QVars, VLOut, Code) :-
	kind_inform(log_binding_failed_forall,
		   Formula, QVars, VLOut, Code).
bindInform(Formula, QVars, VLOut, Code) :-
	kind_inform(log_every_binding,
		   Formula, QVars, VLOut, Code).
qvarInstInform([], Stream, _VLOut, format(Stream,'~n', [])).
qvarInstInform([q(Var, _VarData)|QVars], Stream, VLOut,
	       format(Stream,Format,[PlVar|Args])):-
	ensureVarDInVarList(VLOut, Var, Sort, Data),
	qvarInstInform(QVars, Stream, VLOut, format(Stream, Format1, Args)),
	term_to_atom(Sort, S),
	concat_atom([' ',Var,:,S, '=~w  ',Format1], Format),
	varDataPlVar(Data, PlVar).
/*
qvarInstInform([], _VLOut, format('~n', [])).
qvarInstInform([q(Var, _VarData)|QVars], VLOut, format(Format,[PlVar|Args])):-
	ensureVarDInVarList(VLOut, Var, Sort, Data),
	qvarInstInform(QVars, VLOut, format(Format1, Args)),
	term_to_atom(Sort, S),
	concat_atom([' ',Var,:,S, '=~w  ',Format1], Format),
	varDataPlVar(Data, PlVar).
*/
varDataPlVar(initial(_Call,_Sort1), 'NOT_BOUND') :-
	!.
varDataPlVar(bound(PVar1, _Sort1), PVar1) :-
	!.
varDataPlVar(bound_lost(_PVar1, _Sort), 'VALUE_LOST') :-
        !.
varDataPlVar(Data, PlVar) :-
	pr_error('Unhandled VarData ~w,~w',[Data, PlVar]),
	fail.



codePrologAll(Formula, PN, VLIn, VLOut, QVars, Code) :-
	!,
	pnnp(PN, NP),
	codePrologEx(not(Formula), NP, VLIn, VLOut, QVars, Code).


/*
  For existential quantifiers and implies forall quantifiers
  q(Var, VarData) => VarData = needs_bound : first occurrence of Var
                                             must be bound
                     VarData = bound       : first occurrence will bind
                     VarData = initial     : no occurrence
  */
setup_qcode([], VLIn, VLIn, true).
setup_qcode([q(Var, VarData)|QIn], VLIn, VLOut, Code) :-
	setup_qcode1(VarData, Var, VLIn, VL1, Code1),
	setup_qcode(QIn, VL1, VLOut, Code2),
	combine_and([Code1, Code2], Code).
setup_qcode1(initial, Var, VLIn, VLIn, Code) :-
	ensureVarDInVarList(VLIn, Var, Sort, Data),
	(Data = initial(Call, Sort1)
	->	true
	;	pr_error('Unexpected variable kind'),
		fail
	),
	warning('Quantifier range variable ~w not used in quantifier body',
		[Var:Sort]),
	code_some_element_exists(Sort1, Call, Code).
setup_qcode1(Bound, Var, VLIn, VLOut, Code) :-
	memberchk(Bound, [bound, bound_lost]),
	!,
	varDFromVarList(VLIn, Var, Sort, Data, VL1),
	(Data = initial(Call,Sort1)
	->	(code_out_eq(Call, Sort1, Code,PVar)
		->	varDToVarList(Var, Sort, bound(PVar, Sort1),
				      VL1, VLOut)
		;	Code = true,
			varDToVarList(Var, Sort, initial(Call,Sort1),
				      VL1, VLOut)
		)
	;	pr_error('Unexpected variable kind'),
		fail
	).
setup_qcode1(needs_bound, Var, VLIn, VLOut, Code) :-
	varDFromVarList(VLIn, Var, Sort, Data, VL1),
	varDToVarList(Var, Sort, bound(PVar, Sort1), VL1, VLOut),
	(Data = initial(Call, Sort1)
	->	(code_out_eq(Call, Sort1, Code,PVar)
		->	true
		;	code_instantiate(Call, Var, Sort1, Code,PVar)
		)
	;	pr_error('Unexpected variable kind'),
		fail
	).
setup_qcode_all_needs_bound([], VLIn, VLIn, [], true).
setup_qcode_all_needs_bound([q(Var, VarData)|QIn], VLIn, VLOut, [PVar|PVars], Code) :-
	assert_debug(VarData == needs_bound),
	varDFromVarList(VLIn, Var, Sort, Data, VL1),
	varDToVarList(Var, Sort, bound(PVar, Sort1), VL1, VL2),
	(Data = initial(Call, Sort1)
	->	(code_out_eq(Call, Sort1, Code1,PVar)
		->	true
		;	code_instantiate(Call, Var, Sort1, Code1,PVar)
		)
	;	pr_error('Unexpected variable kind'),
		fail
	),
	setup_qcode_all_needs_bound(QIn, VL2, VLOut, PVars, Code2),
	combine_and([Code1, Code2], Code).


code_out_eq(call(_X2 = X3), Sort, Code, T3) :-
	!,
	T3 = X3,
	check_sort_element_code(Sort, T3, Code).



/* Sort is already transformed
   Call is Call from iscmpoploc(RangeArg, Call1)
   already dealt with Var = Expr Call
   */
code_instantiate(Call, Var, Sort, Code, PVar) :-
	assert_debug(nonvar(Sort)),
	(	bi_sort(Sort, Kind)
	;	isIntervalSort(Sort),
		Kind = integer
	),
	!,
	(Kind == integer
	->	(Call == true
		->	between_sort_test(Var,Sort, Low, Hi),
			code_between(Low, Hi, PVar, Code)
		;	Call = call(Op, CmpTerm)
		->	between_sort_test(Var,Sort, Low, Hi),
			code_between_call(Op,Low,Hi,CmpTerm,PVar,Code)
		;	Call = call2(Op1, Op2, CmpTerm1, CmpTerm2)
		->	(is_between_sort(Sort, Low, Hi)
			->	code_between_call2(Op1,Op2,Low,Hi,CmpTerm1,
						   CmpTerm2, PVar,Code)
			;	is_same_sort(Sort,integer)
			->	code_between_call2I(Op1,Op2,CmpTerm1,CmpTerm2,
						    PVar,Code)
			;	error('Cannot instantiate varianble ~w of builtin sort ~w',
				      [Var,Sort]),
				fail
			)
		)
	;	error('Cannot instantiate varianble ~w of builtin sort ~w',
				      [Var,Sort]),
		fail
	).




code_instantiate(Call, Var, Sort, Code, PVar) :-
	(Call == true
	->	code_sort_element(Sort, PVar, Code, true)
	;Call = call(\=, Term)
	->	code_sort_element(Sort, PVar, Code1, true),
		combine_and([Code1, PVar \= Term], Code)
	;	error('Comparison ~w not available for sort ~w (variable ~w)',[Call, Sort,Var]),
		fail
	).


is_between_sort(Sort, Low, Hi) :-
	assert_debug(nonvar(Sort)),
	(Sort = between(Low, Hi)
	->	true
	;	isIntervalSort(Sort),
		(	holds:last_time(LT)
		->	Low = 0,
			Hi = LT
		;	pr_error('Missing last_time'),
			fail
		)
	).
between_sort_test(Var,Sort, Low, Hi) :-
	(is_between_sort(Sort, Low, Hi)
	->	true
	;	error('Cannot instantiate variable ~w of builtin sort ~w',
		      [Var,Sort]),
		fail
	).




code_some_element_exists(Sort1, true, Code) :-
	!,
	(ground(Sort1)
	->	(bi_sort(Sort1, _Kind)
		->	(is_inf_sort(Sort1)
			->	Code = true
			;	sort_element(Sort1, _)
			->	Code = true
			;	Code = fail
			)
		;	sort_element(Sort1, _)
		->	Code = true
		;	Code = fail
		)
	;	Code = once(sort_element(Sort1, _))
	).

code_some_element_exists(Sort, Call, Code) :-
	assert_debug(nonvar(Sort)),
	(	bi_sort(Sort, Kind)
	;	isIntervalSort(Sort),
		Kind = integer
	),
	!,
	(Kind == integer
	->	(Call == true
		->	(is_between_sort(Sort, Low, Hi)
			->	(	ground(Low),
					ground(Hi)
				->	(Hi >= Low
					->	Code = true
					;	Code = fail
					)
				;	Code = (Hi >= Low)
				)
			;	is_same_sort(Sort, integer)
			->	Code = true
			;	pr_error('Unexpected bi sort ~w', [Sort]),
				fail
			)
		;	Call = call(Op, CmpTerm)
		->	(is_between_sort(Sort, Low, Hi)
			->	code_between_call(Op,Low,Hi,CmpTerm,_PVar1,Code1),
				mk_exists_code([Low,Hi,CmpTerm], Code1, Code)
			;	is_same_sort(Sort, integer)
			->	Code = true
			;	pr_error('Unexpected bi sort ~w', [Sort]),
				fail
			)
		;	Call = call2(Op1, Op2, CmpTerm1, CmpTerm2)
		->	(is_between_sort(Sort, Low, Hi)
			->	code_between_call2(Op1,Op2,Low,Hi,CmpTerm1,
						   CmpTerm2, _PVar2,Code1),
				mk_exists_code([Low,Hi,CmpTerm1,CmpTerm2], Code1, Code)
			;	is_same_sort(Sort,integer)
			->	code_between_call2I(Op1,Op2,CmpTerm1,CmpTerm2,
						    _PVar3,Code1),
				mk_exists_code([CmpTerm1,CmpTerm2], Code1, Code)
			;	pr_error('Unexpected sort kind for sort ~w', [Sort]),
				fail
			)
		;	pr_error('Call?'),
			fail
		)
	;Kind == real
	->	(is_same_sort(Sort, real)
		->	( (Call == true
			  ;Call = call(Op, CmpTerm)
			  )
			->	Code = true
			;	Call = call2(Op1, Op2, CmpTerm1, CmpTerm2)
			->	combine_ops(Op1, Op2, Op),
				CallN =.. [Op, CmpTerm1,CmpTerm2],
				mk_exists_code([CmpTerm1,CmpTerm2], CallN, Code)
			;	pr_error('Call?'),
				fail
			)
		;	pr_error('Unexpected bi sort ~w', [Sort]),
				fail
		)
	;	pr_error('Unhandled range kind ~w', [Kind]),
		fail
	).

code_some_element_exists(Sort, _Call, _Code) :-
	pr_error('restrictions on non builtin sort ~w?',[Sort]),
	fail.

mk_exists_code(Depends, Code1, Code) :-
	(ground(Depends)
	->	(call(Code1)
		->	Code = true
		;	Code = fail
		)
	;	Code = once(Code1)
	).






codePrologEx(Formula, PN, VLIn, VLOut, QVars, Code) :-
	vars_in_formula(Formula, pos, VLIn, QVars, QOut),
	setup_qcode(QOut, VLIn, VL1, Code1),
	bindInform(Formula, QVars, VL1, BindInform),
	codePrologF(Formula, pos, VL1, VLOut, Code2),
	succeededInform(Formula, QVars, VLOut, SucceededInform),
	combine_and([Code1, BindInform, Code2, SucceededInform], Code3),
	pncode(PN, Code3, Code).







