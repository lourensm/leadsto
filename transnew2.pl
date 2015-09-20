:- module(transnew2,
	  [
	   codePrologNew2/3
	  ]).

:- use_module(util).
:- use_module(satgenut).
:- use_module(varutil).
:- use_module(transutil).

/*
12345678901234567890123456789012345678901234567890123456789012345678901234567890
TODO: 
Dan, toch alternatieven toestaan: stel gekozen volgorde toch
niet ok later?? Hmm, laat toch maar zitten.

Eerst debuggen, heel eventueel transformed formule delegeren, NEEN.
Hoewel, misschien is tweede fase wel identiek.
In ieder gevaal ga door, test dan eerst met simpele varianten,
dan exists(exists())
dan ook forall omzetten.

TODO:
comparisons zover mogelijk naar voren halen?
Dat zou zeker moeten igv i < 4 and i > 0

EERST:
Voeg annotatie toe aan not, aan exists, aan rest?
Dat aangeeft dat variabelen op die plek geinitialiseerd moeten worden.
Propageer cmpops zover mogelijk naar voren.

Zodra we een comparison tegenkomen die een prefer_bind variable bevat,
neem die op in de variable list, of in de delayed cmpops?

Meteen moeten we ook de conditions en equalities meesleuren in alle
conjunctions. Dat zou meteen bij de variable binding moeten worden opgeslagen?

Niet altijd. Zolang we binnen 1 conjunctie spoor zitten wel.
Maar zodra we een disjunction hebben waar een condition alleen
in 1 spoor zit, moet er iets speciaals gebeuren.

We moeten weten wanneer een disjunction als ";" gecodeerd gaat worden.

De tussenweg is nu weer: Sleep constraints mee, tot dat niet meer kan
en pleur ze dan in de formule.

Houd een lijst constraints bij, samen met VLIn, VLOut.
Probeer die constraints op een of andere manier genormaliseerd te
houden. We zouden iets geavanceerds kunnen doen a la
x^2 + y^2 < z^2 vervangen door and(-|z| < x < |z|, -|z| < y < |z|,
				   x^2 + y^2 < z^2)

Zodra we in een disjunctie zitten en 1 spoor bevat een conditie, de
andere niet, dan simpel de conditie in de formule zetten, met eisen
aan instness.

Trouwens, cmpop = moet ook anders.
Waarschijnlijk na elke nieuwe subformule moet = gecheckt worden.
We moeten ook echt rekening houden met echte instantiated variables.

De comparisons moeten dus uit de kwantoren gepeuterd worden.
Stel we moeten gaan instantieren ergens, een integer? Dan moeten de bounds
beschikbaar zijn.
Dus, zodra we een needs_bound nieuw introduceren, moeten we checken of het een
finite domain is, of dat er bound integer grenzen zijn.

Er is wat voor te zeggen om dan backtracken toe te staan omdat er multiple grenzen kunnen heersen?

Eerste ronde in ieder geval: elke subformule krijgt eerst
binnen: bindingstoestand van variables.

Analyse moet opleveren: interne ordening en verlangde bindingstoestand
van variables.
Als dat needs_bound is, zou subformule zelf de verantwoordelijkheid
kunnen nemen tot binding van variabelen, internals weet waar dat
zou kunnen gebeuren.
Maar voor dat geval moet ook eventueel van conditions de binding ok
zijn:
Als het eindig domein is dan niet persee, anders beide ranges
anders retourneer:cannot bind locally.

Zou een subvariant haalbaar zijn: geef door needs_bound en laat
binding op een makkelijke plaats gebeuren.
Maar dan moet een conjunctie ook geanalyseerd worden als volgt:

beide apart met sommige variables initial
dan combineren:als 1 tak initial blijft kan initialisatie code
lokaal in andere tak gestopt worden.

Hum nee, eerst vereenvoudigde systeem:
Orden alle needs_bounds en delegeer naar boven.

*/

codePrologNew2(Formula, Code, Constants) :-
	%resetcholds6,
	reset_constant_use,
	tr_form_den_const(Formula, F1),
	normalise_formula_minimal(F1, F2),
	resetcholds6,
	VLCHIn = ds_vlch([], [], []),
	(codePrologF4(F2, pos, VLCHIn, VLCHOut, Code)
	->	ensure(VLCHOut = ds_vlch(VLOut, CondsOut, HOut)),
		assert_debug(VLOut = []),
		get_reset_constant_use(Constants),
		numbervars(VLCHOut, 0, _),
		format('Code: ~w~nConds:~w~nHolds:~w~nConstants:~w~n',
		       [Code, CondsOut, HOut, Constants])
	;	reset_constant_use,
		fail
	).





/*
  We need to determine whether in some subformula quantifier variables
  occur, and if so in what way.
  Then, we try to rewrite the formula in a way such that binding
  subformulae occur before subformulae where the variables are needed bound.

  But there is a problem if subformulae would like to rearrange quantifiers
  themselves.

  First try: Give QVars annotations:
  prefer_bind, must_bind, is_bound
  result would be:
  -cannot fulfill wishes
  -can fulfill wishes, multi-solutions
  -per qvar: will_bind, needs_pre_bound, not_occurring
  -also per qvar: binding_complexity, how

  Add extra: Inherited conjunction conditions which should hold. It could be
  that the variables are not bound; Inherited conditions may be instantiated
  and coded into the transformation, but may also be passed on.

  Should we combine Conditions and Qvars? Probably.

  QIn, Qi, QOut will become QCIn, QC1, QCOut
  where all are ds_qc(QVars, Conditions)

  We do need two passes. Possibly even backtracking:
  somewhere in a subconjunction, we may know: need_bound:
         for instance if we have two negated ocuurences of holds containing
         VAR.
         In a disjunction a branch could contain unique constraints which
         might need instantiation.

  Until now idea: have code generation done same time as analysis.
  So suppose: we encounter a needs_bound somewhere. We simply assume
  the environment of current subformula has done that and proceed with coding.
  But who takes care of conditions then?

  We could start out, assuming we arrive at a solution somehow.

  If some subformula has needs_bound for some variable, we simply pass
  out the requirement.
  So, variables should be stored as
  VarName*OriginalSort*CodedSort*PrologRep*Data
  where only PrologRep is a real key. VarName could become hidden.

  The VarList as input says how the variables are bound before entering the
  code for this subformula. It should be considered as the result of the
  previous conjunction element.

  There are also Constraints: X < Y etc. There are two kinds:
  Conditions that have been imposed and conditions that need checking.
  This is also a "conjunctive" requirement.
  Data:
  bound: In/Out: the Variable is bound
  bound_lost: the variable has been bound in some sub formula, but negation
              has lost the binding.
  initial/(was:prefer_bind) not encountered variable in formula yet.
  conflict:   two parts of conjunction did something with variable, there was a
              conflict: first subformula did bound_lost, second did bound or
                        needs_bound
  needs_bound: LHS, initial status was intial, but we need variable to be
              bound. We require this for variables in negative holds formulae

  Now we may (simply) code subformulae, possibly requiring the environment
  to pre-instantiate variables by labeling them "needs_bound".
  We may probably always prefer bound to needs_bound? Is this ok wrt
  code generation? We may not come back to our decision because
  "initial"->"bound" coding
  of holds will be different from "bound"->"bound".
  THIS IS A PROBLEM: If we encounter initial->bound and then negate the
  formula, we get bound_lost which may later require needs_bound.

  So, we probably still need a second pass? Holds subformulae need to be
  optimized, but only those that really occur in the formula with the right
  binding.
  Probably we should also pre-check whether some holds expression might fail
  always, possibly also succeed?
  The HoldsCode will probably be a variable that will be instantiated at the
  end:
  We have a list of holds occurrences in the PreCompiled formula containing all
  occurring HoldsFormulae.

  But? Occurring interval expressions? They may be bound or not.
  An extra peephole optimizing round?

  So, we have:
  VL/Constraints in/out VLCIn/VLCOut
  Code out
  Holds occurrences in/out : share code for structural equivalent occurrences
  PosNeg:in  We need the result PosNeg


  Who is responsible for binding in case of needs_bound? Not the Formula-lee
  because a conjunction could take care of binding.

  Subalgorithm 1: Try:
  subformulae may return needs_bound. The instantiation - if needed - will be
  dealt with at beginning quantifier. All conditions involving local
  needs_bound quantifiers should be left untouched?
  Conditions involving just bound variables that are present in conditions
  should have their code put right after the holds code.
*/

codePrologF4(true, PN, VLCHIn, VLCHIn, Code) :-
	!,pncode(PN, true, Code).
codePrologF4(false, PN, VLCHIn, VLCHIn, Code) :-
	!,pncode(PN, fail, Code).
codePrologF4(not(F), PN, VLCHIn, VLCHOut, Code) :-
	!,
	pnnp(PN, NP),
	codePrologF4(F, NP, VLCHIn, VLCHOut, Code).
codePrologF4(exists(QVarsIn, Formula), PN, VLCHIn, VLCHOut, Code) :-
	!,assert_debug(is_list(QVarsIn)),
	local_trace(ap),
	setupqvars2(QVarsIn, VLCHIn, VLCH1, LocVars),
	codePrologF4(Formula, pos, VLCH1, VLCH2, Code1),
	rm_local_qvars(LocVars, VLCH2, VLCH3, Code1, Code2),
	pncode(PN, Code2, Code),
	adjustNegatedBindings(VLCH3, PN, VLCHOut).


codePrologF4(and(F1, F2), PN, VLCHIn, VLCHOut, Code) :-
	!,andorpnandor(and, PN, AndOr1),
	codePrologF4ConDisj(AndOr1, F1, F2, PN, VLCHIn, VLCHOut, Code).
codePrologF4(or(F1, F2), PN, VLCHIn, VLCHOut, Code) :-
	!,andorpnandor(or, PN, AndOr1),
	codePrologF4ConDisj(AndOr1, F1, F2, PN, VLCHIn, VLCHOut, Code).
codePrologF4(holds(S, A, TF), PN, VLCHIn, VLCHOut, Code) :-
	!,codeHoldsNew2(S, A, TF, PN, VLCHIn, VLCHOut, Code).
codePrologF4(FIn, PN, VLCHIn, VLCHOut, Code) :-
	iscmpopflist(FIn, CmpSeq),
	code_cmp_formula(CmpSeq, PN, VLCHIn, VLCHOut, Code).

/* neg is problematical: conjunction becomes disjunction
   a < b < c   lets only allow sequence
   */
code_cmp_formula(CmpSeq, PN, VLCHIn, VLCHOut, Code) :-
	rm_compilable_cmp(CmpSeq, VLCHIn, VLCH1, Code, CmpSeq1),
	ensure(VLCH1 = ds_vlch(VLIn, CondsIn, HIn)),
	ensure(VLCHOut = ds_vlch(VLIn, CondsOut, HIn)),
	(CmpSeq == []
	->	CondsOut = CondsIn
	;	ensure(CondsIn = ds_cht(CondsMetIn, CondsToDoIn)),
		ensure(CondsOut = ds_cht(CondsMetIn,
					 [ds_c(PN, CmpSeq1)|CondsToDoIn]))
	).
/* Need to handle special case A < B = C = D < E */
rm_compilable_cmp([T1|R], VLCHIn, VLCH1, Code, CmpSeq1) :-
	!,
	rm_compilable_cmp1(T1, R, VLCHIn, VLCH1, Code, CmpSeq1).
rm_compilable_cmp(CmpSeq, VLCHIn, VLCH1, Code, CmpSeq1) :-
	impl_error('rm_compilable_cmp ~w',
		   [rm_compilable_cmp(CmpSeq, VLCHIn, VLCH1, Code, CmpSeq1)]).
rm_compilable_cmp1(T1, R, VLCHIn, VLCHOut, Code, CmpSeq) :-
	tr_is_bound_expr_vlch(T1, VLCHIn, T11, IsBound),
	(is_bound(IsBound)
	->	rm_compilable_cmp_bound_prefix(R,T11,VLCHIn,VLCHOut,Code,
					       CmpSeq)
	;	rm_compilable_cmp_unbound_prefix(R,T11, VLCHIn,VLCHOut,Code,
						 CmpSeq)
	).
/* We know T21 is the first term, R may be [Op,T|_]
we know T21 is a bound term.*/
rm_compilable_cmp_bound_prefix([], _T21, VLCHIn, VLCHIn, true, []) :-
	!.
rm_compilable_cmp_bound_prefix([Op, T3|R], T21, VLCHIn, VLCHOut,Code,CmpSeq) :-
	!,
	tr_is_bound_expr_vlch(T3, VLCHIn, T31, IsBound),
	(is_bound(IsBound)
	->	code_cmp(T21, Op, T31, Code1),
		add_met_cond_vlch(VLCHIn, T21, Op1, T31, VLCH1),
		rm_compilable_cmp_bound_prefix(R,T31,VLCH1, VLCHOut, Code2,
					       CmpSeq),
		combine_and([Code1, Code2], Code)
	;	rm_compilable_cmp_unbound_prefix(R,T31,VLCHIn,VLCHOut,Code2,
						 CmpSeq1),
		CmpSeq = [T21, Op1|CmpSeq1]
	).
rm_compilable_cmp_bound_prefix(Seq, _T21, _VLCHIn, _VLCHOut, _Code, _CmpSeq) :-
	impl_error('unexpected cmp sequence ~w', [Seq]),
	fail.


/* We know there is a non bound prefix _T Op .. to T2, will be added to seq
   elsewhere */
rm_compilable_cmp_unbound_prefix([],T11, VLCHIn,VLCHIn,true, [T11]) :-
	!.
rm_compilable_cmp_unbound_prefix([Op,T2|R],T11, VLCHIn,VLCHOut,Code, CmpSeq) :-
	!,
	tr_is_bound_expr_vlch(T2, VLCHIn, T21, IsBound),
	(is_bound(IsBound)
	->	rm_compilable_cmp_bound_prefix(R, T21, VLCHIn, VLCHOut,Code,
					       CmpSeq1)
	;	rm_compilable_cmp_unbound_prefix(R,T21, VLCHIn,VLCHOut,Code,
						 CmpSeq1)
	),
	CmpSeq = [T11, Op|CmpSeq1].
rm_compilable_cmp_unbound_prefix(Seq, _T21, _VLCHIn, _VLCHOut,_Code,_CmpSeq) :-
	impl_error('unexpected cmp sequence ~w', [Seq]),
	fail.





codePrologF4ConDisj(and, F1, F2, PN, VLCHIn, VLCHOut, Code) :-
	codePrologF4(F1, PN, VLCHIn, VLCH1a, Code1a),
	codePrologF4(F2, PN, VLCH1a, VLCHOuta, Code2a),
	combine_and([Code1a, Code2a], Codea),
	codePrologF4(F2, PN, VLCHIn, VLCH1b, Code1b),
	codePrologF4(F1, PN, VLCH1b, VLCHOutb, Code2b),
	combine_and([Code1b, Code2b], Codeb),
	combine_and_vlch(VLCHIn, VLCHOuta, VLCHOutb, Codea,Codeb,VLCHOut,Code).
codePrologF4ConDisj(or, F1, F2, PN, VLCHIn, VLCHOut, Code) :-
	codePrologF4(F1, PN, VLCHIn, VLCHOuta, Code1a),
	codePrologF4(F2, PN, VLCHIn, VLCHOutb, Code2a),
	combine_or([Code1a,Code2a], Code),
	(memberchk(Code, [true, fail])
	->	assert_debug((VLCHOuta == VLCHIn;VLCHOutb == VLCHIn)),
		VLCHOut = VLCHIn
	;	combine_or_vlch(VLCHOuta, VLCHOutb, VLCHOut)
	).
combine_and_vlch(VLCHIn, VLCHOuta, VLCHOutb, Codea, Codeb, VLCHOut, Code) :-
	ensure(VLCHIn = ds_vlch(VLIn, _CondsIn, _HIn)),
	ensure(VLCHOuta = ds_vlch(VLOuta, _CondsOuta, _HOuta)),
	ensure(VLCHOutb = ds_vlch(VLOutb, _CondsOutb, _HOutb)),
	and_vlch_order(VLIn, VLOuta, VLOutb, any, Order),
	(Order == left
	->	Code  = Codea,
		VLCHOut = VLCHOuta
	;Order == right
	->	Code  = Codeb,
		VLCHOut = VLCHOutb
	;Order == any
	->	Code  = Codea,
		VLCHOut = VLCHOut
	;	impl_error('Unrecognised order ~w', [Order])
	).
and_vlch_order([], VLOuta, VLOutb, OrderIn, OrderIn) :-
	assert_debug(VLOuta == []),
	assert_debug(VLOutb == []).


and_vlch_order([VE|VLIn], VLOuta, VLOutb, OrderIn, OrderOut, [VE1|VLOut]) :-
	ensure(VE = ds_vd(Var, Sort, Data)),
	VE1 = ds_vd(Var, Sort, Data1),
	ensure(Data = ds_v4(_Binding, PLVar, SortPTerm, Condition)),
	Data1  = ds_v4(BindingC, PLVar, SortPTerm, Condition),
	ensureVarDFromVarList(VLOuta, VLOuta1, Var, Sort, Data1a),
	ensure(Data1a = ds_v4(Bindinga, PLVara, SortPTerma, Conditiona)),
	ensureVarDFromVarList(VLOutb, VLOutb1, Var, Sort, Data1b),
	ensure(Data1b = ds_v4(Bindingb, PLVarb, SortPTermb, Conditionb)),
	assert_debug((PLVara == PLVar,PLVarb == PLVar)),
	assert_debug(SortPTerma == SortPTermb),
	assert_debug(Conditiona == Conditionb),
	local_binding_order(Bindinga, Bindingb, LocalOrder, ConflictBinding),
	combine_order(LocalOrder, OrderIn, OrderOut1),
	(OrderOut1 == conflict
	->	Order1 = OrderIn,
		BindingC = ConflictBinding
	;	Order1 = OrderOut1,
		(OrderOut1 == left
		->	BindingC =  Bindinga
		;	assert_debug(OrderOut1 == right),
			BindingC = Bindingb
		)
	),
	and_vlch_order(VLIn, VLOuta1, VLOutb1, Order1, OrderOut, VLOut).

	

local_binding_order(initial, initial, any, _).
local_binding_order(bound, bound, any, _).
local_binding_order(bound, needs_bound, left, needs_bound).
local_binding_order(needs_bound, bound, right, needs_bound).
local_binding_order(needs_bound, needs_bound, any, _).
local_binding_order(_, _, _Error, _) :-
	impl_error('Unexpected different analysis of variables').

/* TODO:
trans_pl_vars - trans_pl_vars_update_bindingsl
express second in first => save this work
*/
match_cwa(Atom, VLIn, AlwaysSome) :-
	trans_pl_vars(Atom, VLIn, ds_pt(PAtom, _PVars)),
	(spec:sortdef('TRACE', Elements)
	->	true
	;	impl_error('Missing TRACE sort')
	),
	(setof(Trace1-Atom1,matching_cwa1(PAtom, Trace1, Atom1), TAs)
	->	TAs = [Trace2-Atom2|TAsR],
		(select(Trace2, Elements, Elements1)
		->	true
		;	impl_error('Missing trace'),
			fail
		),
		chkallsamecwa(Elements1, TAsR, Trace2, Atom2),
		(subsumes_chk(Atom2, Atom)
		->	AlwaysSome = always
		;	AlwaysSome = some
		)
	;	fail
	).
chkallsamecwa([], TAsR, _Trace2, _Atom2) :-
	(TAsR == []
	->	true
	;	impl_error('Unrecognised traces')
	).

chkallsamecwa([Trace1|Elements], TAsR, Trace2, Atom2) :-
	(select(Trace1-Atom1, TAsR, TAsR1)
	->	(Atom1 =@= Atom2
		->	true
		;	error('cwa specification mismatch: ~w <-> ~w',
			      [Trace2:Atom2, Trace1:Atom1])
		),
		chkallsamecwa(Elements, TAsR1, Trace2, Atom2)
	;	error('cwa specification ~w does not occur in trace ~w',
		      [Trace2:Atom2, Trace1]),
		fail
	).

matching_cwa1(Atom, Trace, Atom1) :-
	assert_debug(nonvar(Atom)),
	functor(Atom, F, A),
	functor(Atom1, F, A),
	holds:cwa(Trace, Atom1),
	\+ \+ Atom = Atom1.


holdsbound2(pos, bound).
holdsbound2(neg, bound_lost).
/* We need to handle cwa in a sense, and if TFU is not bound, more
   complications */
holdsbound(PN, A, TF, VLIn, Binding) :-
	(TF == true
	->	holdsbound(PN, Binding)
	;	nonvar(A),
		(match_cwa(A, VLIn, AlwaysSome)
		->	(AlwaysSome == some
			->	impl_error(['Cannot deal with atom cwa ',
					    'spec that sometimes matches:~w'],
					   [A]),
				fail
			;	true
			),
			(TF == unknown
			->	warning('cwa atom ~w is never unknown', [A])
			;TF == false
			->	pnnp(PN,NP),
				holdsbound(NP, Binding)
			;	impl_error(['Cannot deal with variable tfu ',
					    'argument yet']),
				fail
			)
		;	(TF == false
			->	holdsbound(PN, Binding)
			;TF == unknown
			->	pnnp(PN, NP),
				holdsbound(NP, Binding)
			;	impl_error(['Cannot deal with variable ',
					    'tfu argument yet']),
				fail
			)
		)
	).

/* First try minimum approach: let environment deal with unbound
   conditions
   But it would be nice to know what variables will be prebound here.
   Possibly not a good idea yet: we are not sure yet whether the current
   VLIn situation will really be chosen.
*/
codeHoldsNew2(S, A, TF, PN, VLCHIn, VLCHOut, Code) :-
	ensure(VLCHIn = ds_vlch(VLIn, CondsIn, HIn)),
	ensure(VLCHOut = ds_vlch(VLOut, CondsIn, HOut)),
	!,
	holdsbound(PN, A, TF, VLIn, Binding),
	trans_pl_vars_update_bindingsl([S, A, TF], Binding, VLIn, VLOut,[],
				      _PVOut,[S1, A1, TF1]),
	add_holds_entry(S1, A1, TF1, HIn, HOut, Code).

add_holds_entry(S1, A1, TF1, HIn, HOut, Code) :-
	copy_term(h(S1, A1, TF1), H),
	numbervars(H, 0, _),
	(memberchk(H, HIn)
	->	HOut = HIn
	;	HOut = [h(H, Code)|HIn]
	).

/*
  A problem here? We may wish to instantiate some range variable
  at a more convenient postion than at the start, for instance because some
  range variable gets instantiated later on.

  What to do? Suppose we have a conjunction where the RHS has needs_bound V1
  The LHS does not. Then we should put the instantiating code in the middle.

  Initially coding holds could be simple: just put it in the holds list.
  But when we optimize:
  * It could be some range variable got bound because of earlier other single
    value holds.
  * It could be there is a simple ground condition that must hold which could
    limit the range of holds.
  * Coding this holds could have effect on other places because the holds
    nowhere holds, or has a single instantiation.

  POSSIBLY: just start and then put everything through second pass.
  
  */

/*
  rm_local_qvars(VLCHIn, VLCH1, Code1, Code):
  LHS  SubFormulaNeedsBound RHS
  Any needs_bound variables that are initial at LHS need to be handled.
  Could be by adding Code LHS, could also add code lhs and then redo coding of
  formula. Why? Because otherwise VLCHRHS needs updating.
  If we have multiple needs_bound, one binding will not make other one
  unnecessary? OR? Constraint X = Y + 1 => could have both need_bound?
  Probably not, adding needs_bound should take constraints into consideration.
  Lots of things to arrange:
  - needs_bound should be marked.
  - remaining conditions should be handled
  - local variables and conditions referring to local variables should be
    removed.
  */
rm_local_qvars(LocVars, VLCH2,VLCHOut,Code1, exists(InstCode,Code1)) :-
	handleLocVars(LocVars, VLCH2, VLCHOut, InstCode).
/*
  Horribly complicated: Only need to really deal with needs_bound variables and
  unhandled conditions.
  Needs_bound variables: is type infinite -> error
  type finite constrained? Are there conditions having variable in them?
  Only take those conditions into account that have all other pvars bound.
  Update VL1 for variable to become bound.

  Could be preferable: needs_bound at exit: require bound of conditions
  restricting them. But only at the end, because earlier another conjunction
  part could bind.
  So at the end: propagate needs_bound: From inside out: analyse vqvars
  and look up conditions and possibly add needs_bound to earlier qvars.
  */
handleLocVars([], VLCHIn, VLCHIn,true).

handleLocVars([ds_plv(VarName, Sort,PLVar)|LocVars], VLCH2, VLCHOut,
	      InstCode) :-
	ensure(VLCH2 = ds_vlch(VL2, Conds2, H2)),
	ensureVarDFromVarList(VL2, VL3, VarName, Sort, Data),
	ensure(Data = ds_v4(Binding, PLVar, SortPTerm, Condition)),
	(Binding == bound
	->	assert_debug(Condition == []),
		VL4 = VL3,
		Conds4 = Conds2,
		InstCode1 = true
	;Binding == needs_bound
	->	check_finite_sort_require_bound_range_conditions(PLVar,
								 SortPTerm,
								 Condition,
								 Conds2,
								 Conds3,
								 RangeConditions,
								 VL3, VL4),
		precode_instantiate(VarName, Sort,PLVar, SortPTerm, Condition,
				    Conds3,
				    Conds4, InstCode1)
	;	impl_error('unhandled binding kind'),
		fail
	),
	VLCH4 = ds_vlch(VL4, Conds4, H2),
	handleLocVars(LocVars, VLCH4, VLCHOut, InstCode2),
	combine_and([InstCode1, InstCode2], InstCode).
precode_instantiate(VarName, Sort,PLVar, SortPTerm, Condition, CondsIn,
				    CondsOut, InstCode) :-
	warning('Code not done:should check and remove conds'),
	assert_debug(Condition == []),
	(CondsIn == []
	->	true
	;	impl_error('Unimplemented conditions')
	),
	CondsOut = CondsIn,
	InstCode = instcode(VarName, Sort,PLVar, SortPTerm, Condition, InstCode).



/*
  If we are dealing with a needs_bound:
  SortPTerm must be ensured to become ground.
  The resulting sort must be countable.
  There must be sufficient number of constraints.
  Gather all constraints and make them into min, max value + extra tests.
  Constraints must all constraints containing current variable. Any other
  variable must be LHS qvars.
  */
check_finite_sort_require_bound_range_conditions(PLVar,SortPTerm,Condition,
						 CondsIn,CondsOut,
						 RangeConditions,VL3,VL4) :-
	ensure(CondsIn = ds_cnds(CondsMetIn, CondsTodoIn)),
	single_condition(Condition, PLVar, CondsMetIn, CondsTodoIn, [], [],
			 LowConds1, HiConds1, CondsTodo1, CondsMet1),
	rm_and_bind_plvar_conditions(CondsTodo1, CondsTodoOut, CondsMet1,
				     CondsMetOut,PLVar,LowConds1, LowCondsOut,
				     HiConds1, HiCondsOut).

/* FIRST:
   Define a condition structure. Allow A < B <= C < D
   Think about equality. Possibly we should inline them as soon as possible.
   */
/* Negation is ugly: we really need to encode all conditions referring to
   bound_lost entries.
   */
adjustNegatedBindings(VLCHIn, pos, VLCHIn) :-
	!.

adjustNegatedBindings(VLCH3, PN, VLCHOut) :-
	impl_error('Unimplemented ~w', [adjustNegatedBindings(VLCH3, PN,
							      VLCHOut)]),
	fail.








/* Complicated: Hidden variables: also implies hiding
   conditions referring to them, unless they are bound already, then
   we need to refer to them.
   exists(q < 3, exists(p < q, q < p, holds(aap(p,q)))).
   ok, because toplevel q not bound, so condition not evaluable.
   exists(q < 3, and(holds(noot(q)), exists(p < q, q < p, holds(aap(p,q))))
   also no problem because condition would have been removed.
   The sort specification should possibly also be postponed until
   its occurence.
   What to do with Sort?? It depends on elements in the surroundings.
   Possibly require sorts to be preinstantiated? Aha, if it is hidden,
   then we cannot correct it, but it could still be ok even if not bound
   because we know the whole fails before getting to this part.
   So, we should in a way mark the sort situation: Test whether
   the Sort contains variables that are hidden but not bound, and if
   so, somehow export an needs_bound as soon as ...
   STOP, simply require variables to be bound for sorts!

   We could preinstantiate any terms: substitute prolog variables for
   ttl variables. When we encounter a quantor variable which hides another
   one, we temporarily remove the old variable from QC. It will not change
   anywhere during visibility of local variable. But the variable could change
   status even if invisible:
   1.Variable x quantor. variable y as well and z, z is constrained by x.
   2.x = y occurs
   3.in conjunction stream:Variable x reoccurs in sub quantor
   4.y gets instantiated internally
   5. z gets bound

   Solution? Variables occur in order in VLIn.
   Lookup by ttl name provides the first occurrence.
   Lookup by prolog variable name provides the real name.
   Any Term occurring somewhere should now be encoded wrt the current VLIn
   Any condition should also be coded.

   What will happen with needs_bound now? vars_in_formula2?
   On the one hand we are busy transforming the formula; on the
   other hand we are coding: adding comparisons where possible.
   Should we handle instantiation as well?

   In that case, needs_bound should somehow either itself add code
   or otherwise propagate the requirement out. This propagation
   could be through the variable list: needs_bound and then have the prolog
   variable available.
   Then on combining a conjunction, ... etc.
   Then we need to know what has changed within a sub formula.
   But, as soon as we require: needs_bound, the environment will ensure that
   and then the environment could handle conditions.

   We need a different strategy: We do not do transTerm of conditions and
   Sort names immediately but in two passes: First we simply substitute
   variables with their PLVars. When we need the term we really code.
   Possibly in a later phase have intermediate version of variables in terms.
   If variables get instantiated compile time by optimzation, we lose them.
   (Could be ok though)
   */
setupqvars2([], VLCHIn, VLCHIn, []).
setupqvars2([Var|Vars], VLCHIn, VLCHOut, [VLPLVar|VLPLVars]) :-
	setupqvar2(Var, VLCHIn, VLCH1, VLPLVar),
	setupqvars2(Vars, VLCH1, VLCHOut, VLPLVars).

/* Binding is what the current activity would do:
holds pos, finite range:  bound
holds neg: infinite range needs_bound
*/
trans_pl_vars_update_bindings(Term, Binding, VLIn, VLOut, PVIn, PVOut,PLVar) :-
	isVarDNewDataS(Term, VLIn, _Var, _Sort, Data, NewData, VLOut,Status),
	!,
	var(Status),
	ensure(Data = ds_v4(Binding1, PLVar, SortPTerm, Constraints)),
	update_binding(Binding, Binding1, Binding2),
	NewData = ds_v4(Binding2, PLVar, SortPTerm, Constraints),
	pl_var_in_vars(PVIn, PVOut, PLVar).
trans_pl_vars_update_bindings(Term, Binding, VLIn, VLOut, PVIn, PVOut,PTerm) :-
	Term =.. [F|Terms],
	trans_pl_vars_update_bindingsl(Terms, Binding,VLIn, VLOut, PVIn, PVOut,
				       PTerms),
	PTerm =.. [F|PTerms].
trans_pl_vars_update_bindingsl([], _Binding,VLIn, VLIn, PVIn, PVIn,[]).
trans_pl_vars_update_bindingsl([Term|Terms], Binding,VLIn, VLOut, PVIn, PVOut,
				       [PlTerm|PlTerms]) :-
	trans_pl_vars_update_bindings(Term,Binding,VLIn, VL1, PVIn,PV1,PlTerm),
	trans_pl_vars_update_bindingsl(Terms, Binding,VL1, VLOut, PV1, PVOut,
				       PlTerms).

update_binding(needs_bound, initial, needs_bound).
update_binding(needs_bound, needs_bound, needs_bound).
update_binding(needs_bound, bound, bound).
update_binding(bound, initial, bound).
update_binding(bound, needs_bound, bound).
update_binding(needs_bound, bound, needs_bound).
% But a shame.

trans_pl_vars(Term, VLIn, ds_pt(PTerm, PVars)) :-
	trans_pl_vars(Term, VLIn, [], PVars, PTerm).

trans_pl_vars(Term, VLIn, PVIn, PVOut, PLVar) :-
	isVarOccS(Term, VLIn, _Var, _Sort,Data,Status),
	!,
	var(Status),
	ensure(Data = ds_v4(_Binding, PLVar, _SortPTerm, _Constraints)),
	pl_var_in_vars(PVIn, PVOut, PLVar).
trans_pl_vars(Term, VLIn, PVIn, PVOut, PlTerm) :-
	Term =.. [F|Terms],
	trans_pl_varsl(Terms, VLIn, PVIn, PVOut, PlTerms),
	PlTerm =.. [F|PlTerms].
trans_pl_varsl([], _VLIn, PVIn, PVIn, []).
trans_pl_varsl([Term|Terms], VLIn, PVIn, PVOut, [PlTerm|PlTerms]) :-
	trans_pl_vars(Term, VLIn, PVIn, PV1, PlTerm),
	trans_pl_varsl(Terms, VLIn, PV1, PVOut, PlTerms).

pl_var_in_vars([], [PLVar], PLVar).
pl_var_in_vars([PLVar1|PLIn], PLOut, PLVar) :-
	pl_var_in_vars(PLVar1, PLIn, PLOut, PLVar).
pl_var_in_vars(PLVar1, PLIn, PLIn, PLVar) :-
	PLVar1 == PLVar,
	!.
pl_var_in_vars(PLVar1, PLIn, [PLVar1|PLOut], PLVar) :-
	pl_var_in_vars(PLIn, PLOut, PLVar).


addNewVarWarnDuplicate(VarName, Sort, VLIn, VLOut, PLVar) :-
	trans_pl_vars(Sort, VLIn, SortPTerm),
	(	varDFromVarListS(VLIn, VarName, Sort2, _,
				 _VL2, Status)
	->	(nonvar(Status)
		->	fail
		;	warning(['Variable ~w hides previous quantifier ',
				 'variable ~w'],[VarName:Sort,VarName:Sort2])
		)
	;	true
	),
	varDToVarList(VarName, Sort, ds_v4(initial, PLVar, SortPTerm,[]),
		      VLIn,VLOut).

setupqvar2(Var, VLCHIn, VLCHOut, ds_plv(VarName, Sort,PLVar)) :-
	ensure(VLCHIn = ds_vlch(VLIn, CondsIn, HIn)),
	ensure(VLCHOut = ds_vlch(VLOut, CondsOut, HIn)),
	(isVar(Var, VarName, Sort)
	->	addNewVarWarnDuplicate(VarName, Sort, VLIn, VLOut, PLVar),
		CondsOut = CondsIn
	;	iscmpoploc2(X, Op1, LHS, RHS)
	->	(iscmpoploc2(RHS, Op2, LHS2, RHS2)
		->	trans_pl_vars([LHS,RHS2], VLIn,[], PVOut,[LHSC, RHSC]),
			(isVar(LHS2, VarName, Sort)
			->	addNewVarWarnDuplicate(VarName, Sort, VLIn,
						       VLOut,PLVar)
			;	error('Quantifier argument not recognised:~w',
				      [Var]),
				fail
			),
			add_cond_contains_initial(CondsIn,
						 [LHSC, Op1, PLVar, Op2, RHSC],
						 CondsOut)
		;	trans_pl_var(RHS, VLIn,[], PVOut, RHSC),
			(isVar(Var, VarName, Sort)
			->	addNewVarWarnDuplicate(VarName, Sort, VLIn,
						       VLOut, PLVar)
			;	error('Quantifier argument not recognised:~w',
				      [Var]),
				fail
			),
			add_cond_contains_initial(CondsIn, [PLVar, Op1, RHSC],
						  CondsOut)
		)
	;	error('Unrecognised quantifier range ~w(1)', [X]),
		fail
	).
add_cond_contains_initial(ds_cnds(CondsMetIn, CondsTodoIn), Cond,
			  ds_cnds(CondsMetIn, [Cond|CondsTodoIn])).



/* First only expect predicate to deal with binding at end of exists?
   Where should needs_bound -> bound be transformed?
   Not within conjunction parts because some subformula could bind.
   Not in disjunction, because disjunction could be surrounded by
   conjunction binding in other branch.
   Suppose we are at the toplevel wrt variable and we meet a conjunction.
   One branch has don't care, other has needs_bound then prefer postponing
   binding. needs_bound in disjunction.

   We should be able to say to some partial code to instantiate some variable:
   and(F1,F2) -> Code
   So there should be instantiation hooks when taken, inserting the
   instantiation at the right position. But then we need to know what the
   variable situation of all variables is at that point. But that could depend
   on other instantiation activations.

   So really precompute all effects and add the instantiation activations in
   the correct order.
   1)We determine that variable x should be instantiated in subcode Code1
   2)We first ask what conditions involve variable x
   3)Are they bound already?? Oh already meaning before entering Code1?
   4)If so, simple activate instantiation of x, perform any outstanding
   conditions
   5)If not, what is the status of other conditions? Are they essentially
   needs_bound?
     If not, we may postpone them, unless they constrain variable x.

   Zero order solution could be: reorder maximally such that bound used as
   much as possible. Handle all left needs_bound variables simple:
   If all conditions are bound, ok, otherwise require them being bound before
   the first occurrence of needs_bound x as a conjunction.
*/











combine_and_formulae21([], OrderIn, QOuta, QOutb, OrderIn) :-
	assert_debug(QOuta == []),
	assert_debug(QOutb == []).

combine_and_formulae21([q(VarName, How)|QIn], OrderIn, QOuta,QOutb,OrderOut) :-
	(	vd_fromq(QOuta, VarName, Va, Q1a),
		vd_fromq(QOutb, VarName, Vb, Q1b)
	->	combine_one_q(OrderIn, How, Va, Vb, Order1),
		combine_and_formulae21(QIn, Order1, Q1a,Q1b,OrderOut)
	;	impl_error('Missing qvar in branch')
	).
combine_one_q(OrderIn, How, Va, Vb, OrderOut) :-
	combine_one_q(How, Va, Vb, How1),
	combine_order(OrderIn, How1, OrderOut).


combine_one_q(prefer_bind, conflict, bound, right) :- !.
combine_one_q(prefer_bind, bound, conflict, left) :- !.
combine_one_q(prefer_bind, bound, bound, any) :- !.
combine_one_q(prefer_bind, needs_bound, needs_bound, any) :- !.
combine_one_q(prefer_bind, prefer_bind, prefer_bind, any) :- !.
combine_one_q(prefer_bind, bound, needs_bound, left) :- !.
combine_one_q(prefer_bind, needs_bound, bound, right) :- !.
combine_one_q(bound, Va, Vb, any) :-
	!,
	assert_debug(Va == bound),
	assert_debug(Vb == bound).
combine_one_q(prefer_bind, bound_lost, bound_lost, any) :-
	!.
combine_one_q(prefer_bind, conflict, conflict, conflict) :-
	!.
combine_one_q(How, Va, Vb, How1) :-
	impl_error('Unimplemented ~w', [combine_one_q(How, Va, Vb, How1)]).


combine_order(any, LRCA, LRCA) :-
	!.
combine_order(LRCA, any, LRCA) :-
	!.
combine_order(conflict, _, conflict) :-
	!.
combine_order(_, conflict, conflict) :-
	!.
combine_order(left, right, conflict) :-
	!.
combine_order(LR, LR, LR) :-
	memberchk(LR, [left, right]),
	!.
combine_order(A, B, _) :-
	impl_error('~w', [combine_order(A, B, _)]).


combine_and_formulae2(QIn, QOuta, QOutb, Fa, Fb, QOut, F) :-
	combine_and_formulae21(QIn, any, QOuta, QOutb, Order),
	(Order == left
	->	F = Fa,
		QOut = QOuta
	;Order == right
	->	F = Fb,
		QOut = QOutb
	;Order == any
	->	F = Fa,
		QOut = QOuta
	;	impl_error('Unrecognised order ~w', [Order])
	).




qvar_in_term(Var, Sort, Bound, QIn, QOut) :-
	assert_debug(atom(Bound)),
	assert_debug(memberchk(Bound, [bound, needs_bound])),
	(vd_fromq(QIn, Var, VarData, Q1)
	->	(adjust_data_to_sort(VarData, Sort, VarData2)
		->	true
		;	VarData2 = VarData
		),
		adjust_vd_what(VarData2, Bound, VarData1),
		vd_toq(Var, VarData1, Q1, QOut)
	;	QOut = QIn
	).
adjust_data_to_sort(prefer_bind, Sort, must_bind) :-
	is_infinite_sort(Sort),
	!.


adjust_vd_what(must_bind, needs_bound, needs_bound) :-
	!.
adjust_vd_what(must_bind, bound, bound) :-
	!.
adjust_vd_what(conflict, _, conflict) :-
	!.
adjust_vd_what(prefer_bind, needs_bound, needs_bound) :-
	!.
adjust_vd_what(needs_bound, _, needs_bound) :-
	!.
adjust_vd_what(prefer_bind, bound, bound) :-
	!.
adjust_vd_what(bound, _, bound) :-
	!.
adjust_vd_what(bound_lost, needs_bound, needs_bound).
adjust_vd_what(needs_bound, bound_lost, needs_bound).
adjust_vd_what(VarData, Bound, VarData1) :-
	impl_error('~w', [adjust_vd_what(VarData, Bound, VarData1)]),
	fail.

normalise_formula2(Formula, Formula1) :-
	tr_form_den_const(Formula, F1),
	normalise_formula_all(F1, Formula1).


% normalise FormulaIn:
%
% constants and denotes/subproperties should have been substituted
% Perform trivial simplifications:
% Propagate, remove true/false constant subformulae
% Normalise connectors: and(and(F1,F2), F3) => and(F1, and(F2, F3))
%             same for or
normalise_formula_all(F1, F2) :-
	normalise_formula_kind(F1, all, F2).

normalise_formula_kind(F1, Kind, F2) :-
	(normalise_formula_step(F1, Kind, F3)
	->	normalise_formula_kind(F3, Kind, F2)
	;	F2 = F1
	).
normalise_formula_minimal(F1, F2) :-
	normalise_formula_kind(F1, minimal, F2).

morgan_kind(all).
export_or(all).

normalise_formula_step(not(true), _Kind, false).
normalise_formula_step(not(false), _Kind, true).

normalise_formula_step(not(not(F1)), _Kind, F1).
normalise_formula_step(not(and(F1,F2)), Kind, or(not(F1), not(F2))) :-
	morgan_kind(Kind).
normalise_formula_step(not(or(F1,F2)), Kind, and(not(F1), not(F2))) :-
	morgan_kind(Kind).
normalise_formula_step(not(F), Kind, not(F1)) :-
	normalise_formula_step(F, Kind, F1).
normalise_formula_step(and(true, F), _Kind, F).
normalise_formula_step(and(F, true), _Kind, F).
normalise_formula_step(and(false, _), _Kind, false).
normalise_formula_step(and(_, false), _Kind, false).
normalise_formula_step(and(and(F1, F2),F3), _KInd, and(F1, and(F2, F3))).
normalise_formula_step(and(or(F1, F2),F3), Kind,
		       or(and(F1, F3), and(F2, F3))) :-
	export_or(Kind).

normalise_formula_step(and(F1,or(F2,F3)), Kind,
		       or(and(F1, F2), and(F1, F3))) :-
	export_or(Kind).
normalise_formula_step(and(F1,F2), Kind, and(F3,F2)) :-
	normalise_formula_step(F1, Kind, F3).
normalise_formula_step(and(F1,F2), Kind, and(F1,F3)) :-
	normalise_formula_step(F2, Kind, F3).
normalise_formula_step(or(true, _), _Kind, true).
normalise_formula_step(or(_,true), _Kind, true).
normalise_formula_step(or(false,F), _Kind, F).
normalise_formula_step(or(F, false), _Kind, F).

normalise_formula_step(or(or(F1, F2),F3), _Kind, or(F1, or(F2, F3))).
normalise_formula_step(or(F1, F2), Kind, or(F3, F2)) :-
	normalise_formula_step(F1, Kind, F3).
normalise_formula_step(or(F1, F2), Kind, or(F1, F3)) :-
	normalise_formula_step(F2, Kind, F3).
normalise_formula_step(exists(QVars, Formula), Kind, exists(QVars, Formula1)) :-
	normalise_formula_step(Formula, Kind, Formula1).
normalise_formula_step(exists(QVars, or(F1, F2)),Kind,
		       or(exists(QVars, F1),exists(QVars, F2))) :-
	export_or(Kind).

normalise_formula_step(forall(QVars, Formula), _Kind,
		       not(exists(QVars, not(Formula)))).

/*
setup_vars(or(F1, F2), or(F11, F12)) :-
	setup_vars1(F1, [], F11),
	setup_vars1(F2, [], F12).
setup_vars1(not(Formula1), VLIn, Formula2) :-
	!,aap.
setup_vars1(exists(QVars, Formula1), VLIn, Formula2) :-
	!,noot.
setup_vars1(holds(S, A, TF), VLIn, Formula2) :-
	!,mies.
setup_vars1(and(F1, F2), VLIn, and_annotated(Annot, F11, F12)) :-
	!,zus.
setup_vars1(Formula, VLIn, Formula) :-
	iscmpoploc(Formula, _NumCall,_LHS,RHS),
	!,
	jet.
*/

