:- module(util,
	  [
	   mk_oldnewregex/3,
	   convertToDnf/2,
	   push_sprop_ops/0,
%	   pop_operators/0,
	   valid_sprop_term/3,
	   atom_to_sprop/2,
	   uchecklist/2,
	   substitute/4,
	   substitute_list/3,
	   valid_term_no_bind0/4,
	   var_containing_constant/3,
	   any_term/3,
	   transold/0,
	   isolate_ioi/3,
	   isolate_ioi/4,
	   instantiate_elemente/4,
	   sort_info/2,
	   config_mod/1,
	   has_printer_class/0,
	   bind_bindings/1,
	   rmmins/2,
	   pnnp/2,
	   pntf/2,
	   setof1/3,
	   bagof1/3,
	   kind_prefix/2,
	   min_kind/2,
	   local_warning/1,
	   local_warning/2,
	   trfloat/2,
	   local_format/2,
	   local_format/1,
	   background_function/3,
	   background_ar_fn/2,
	   randomf/1,
	   spec_pred/1,
	   trace_pred/1,
	   set_dynamic_load_results/0,
	   pre_eval_num/2,
	   pre_eval_num1/2,
	   fatal_status_check/1,
	   rm_real_args/4,
	   multi_rec_error/3,
	   setupmainnewn/6,
	   combine_reporteds/3,
	   my_autoload_all/0,
	   ensure/1,
	   combine_or/2,
	   last_interval/1,
	   update_wd/0,
	   combine_ops/3,
	   iscmpopf/4,
	   iscmpopflist/2,
	   call_ground/2,
	   multi_term_condition/2,
	   multi_term_condition/3,
	   print_var_names/2,
	   set_dynamic_spec/0,
	   pncode/3,
	   my_term_to_atom/2,
	   tell_error/1,
	   trans_denotes_new/3,
	   tr_formula_code_prolog_f/4,
	   tfft/2,
	   showprof/0,
	   cmd_constant/2,
	   load_cmd_constants/0,
	   check_constant/2,
	   spec_constant/2,
	   contains_var/3,
	   var_occurs_in/2,
	   invert_arop/7,
	   aropkind/3,
	   test_num/2,
	   informu/2,
	   cmpopusr/1,
	   cmpopusr/2,
	   push_form_ops/0,
	   is_same_sort/2,
	   code_between_call2I/6,
	   code_between_call2/8,
	   iscmpoploc/4,
	   iscmpoploc/7,
	   pr_error/1,
	   pr_error/2,
	   update_option/2,
	   check_ground_sort_value/3,
	   isIntervalSort/1,
	   code_between_call/6,
	   code_ite/4,
	   code_sort_element/4,
	   sort_size/2,
	   pncmpcode/3,
	   is_inf_sort/1,
	   check_sort_element_code/3,
	   arop/4,
	   combine_and/2,
	   combine_or_bt/3,
	   iscmpoploc/2,
	   set_bool_option/2,
	   reset_option/1,
	   doprof/0,
	   endprof/0,
	   getprof/1,
	   setprof/1,
	   sync_display/0,
	   is_infinite_sort/1,
	   cmpop/1,
	   local_inform/1,
	   local_inform/2,
	   atom_key/2,
	   range_var_condition/6,
	   valid_atom/3,
	   check_sort_value/3,
	   denotes/2,
	   sat_format/2,
	   sat_format/1,
	   sort_kind/2,
	   bi_sort/2,
	   ensure_sort_kinde/3,
	   ensure_sort_kind/2,
	   sort_element/2,
	   bi_sort_elemente/3,
	   setupmainnew/4,
	   set_term_option_bind/4,
	   valid_term_bg/3,
	   tfonoff/2,
	   get_option/1,
	   set_option/1,
	   setupmain1/4,
	   getLocalOptions/3,
	   reserved0/1,
	   ground_vars_dollar/2,
	   bind_binding/1,
	   no_vars/1,
	   valid_term_no_bind/4,
	   ground_anonymous_vars/1,
	   pl_pce/2,
	   pl_pce/3,
	   range_var_condition/5,
	   cmpop/7,
	   quantor/4,
	   reserved/1,
	   valid_term/3,
	   valid_terml/3,
	   local_trace/1,
	   clear_module/1,
	   list_pre_postfix/4,
	   chain_iterate/3,
	   setlocal/0,
	   log/1,
	   log/2,
	   load_module_fatal/3,
	   load_module_fatal/5,
	   assert_debug/2,
	   assert_debug/1,
	   ensure_set_once/1,
	   fatal_fail/1,
	   fatal_error/2,
	   impl_error/1,
	   impl_error/2,
	   fatal_error/1,
	   ferror/1,
	   ferror/2,
	   fwarning/1,
	   fwarning/2,
	   errorv/3,
	   ferrorv/3,
	   error/2,
	   warning/1,
	   warning/2,
	   warning_once/1,
	   user_forced_halt/0,
	   finalhalt/1,
	   error/1,
	   bagof1/3,
	   select2/4,
	   skipPrologArgs/2,
	   is_win32/0,
	   is_local/0,
	   win_get_file/2,
	   cmp_le/2,
	   cmp_lt/2,
	   cmp_eq/2,
	   cmp_gt/2,
	   cmp_ge/2,
	   max_new/3,
	   min_new/3,
	   source_details/2,
	   version_details/2,
	   version_header/2,
	   may_be_option/1,
	   list_and/2,
	   catch_fatal_halt/2,
	   code_between/4,
	   test_recursive_constante/3,
	   test_recursive_constant/2,
	   free_recursive_constant/1,
	   reset_constant_use/0,
	   get_reset_constant_use/1,
	   catch_error_fail/3,
	   reset_sorts/0,
	   update_sorts/0,
	   update_sort/1,
	   find_set/2
	  ]).
%:- use_module(library(pce)).
:- use_module(pce_stuff).
:- use_module(formats).
:- use_module(logutil).
:- use_module(varutil).
:- use_module(satgenut).

:- use_module(library(lists)).
%:- use_module(pce_boot(pce_operator)).
:- use_module(transnew).

%:- use_module(pareto).

%%	find_set(+S,-D) is det.
%
%	D is unique, sorted version of S
find_set(S, D) :-
	setof(D1, member(D1, S), D).

mk_oldnewregex(Old, New, X) :-
	current_prolog_flag(version, V),
	(V >= 50513
	->	new(X, regex(New))
	;	new(X, regex(Old))
	).


:- module_transparent uchecklist/2.
uchecklist(Goal, List) :-
	uchecklist2(List, Goal).

:- module_transparent uchecklist2/2.

uchecklist2([], _).
uchecklist2([Elem|Tail], Goal) :-
	call(Goal, Elem),
	uchecklist2(Tail, Goal).

:- module_transparent ensure/1.

:- module_transparent assert_debug/1, assert_debug/2, ensure_set_once/1.
:- module_transparent catch_fatal_halt/2, catch_error_fail/3.
:- use_module(library(find_file)).
/*
:- multifile portray/1.
:- dynamic portray/1.
*/
convertToDnf(X,Y) :- dnf(X,Z), convertToDnf(Z,Y).
convertToDnf(X,X).
% Remove if's and iff's
dnf(iff(A,B), or(and(A,B),and(not(B),not(A)))).
dnf(implies(A,B), or(not(A),B)).

% Remove double negation
dnf(not(not(B)),B).

% Apply the De Morgan Rules
dnf(not(and(A,B)),or(not(A),not(B))).
dnf(not(or(A,B)),and(not(A),not(B))).

% Apply the Distribution rules
dnf(and(A,or(B,C)),or(and(A,B),and(A,C))).
dnf(and(or(A,B),C),or(and(A,C),and(B,C))).


% Recursively break down and subformulas which are not in dnf
dnf(and(A1,B), and(A2,B)) :- dnf(A1,A2).
dnf(and(A,B1), and(A,B2)) :- dnf(B1,B2).
dnf(or(A1,B), or(A2,B)) :- dnf(A1,A2).
dnf(or(A, B1), or(A, B2)) :- dnf(B1,B2).

dnf(and(and(A,B), C), and(A,and(B,C))).
dnf(or(or(A,B), C), or(A,or(B,C))).



dnf(not(A1),not(A2)) :- dnf(A1,A2).

/* part of ttlchecker, but needed in form_config */
var_containing_constant(cwa_node, X, cwa(X)).
var_containing_constant(external_node, X, external(X)).
var_containing_constant(other_node, X, X).

substitute_list([], InTerm, InTerm).
substitute_list([A = B|List], InTerm, OutTerm) :-
	substitute(InTerm, A, B, Term1),
	substitute_list(List, Term1, OutTerm).

substitute(InTerm, InTerm, Into, Into).
substitute(InTerm, What, Into, OutTerm) :-
	nonvar(InTerm),
	InTerm \== What,
	InTerm =.. [F|Args],
	substitutel(Args, What, Into, OutArgs),
	OutTerm =.. [F|OutArgs].
substitutel([], _What, _Into, []).

substitutel([Arg|Args], What, Into, [OutArg|OutArgs]) :-
	substitute(Arg, What, Into, OutArg),
	substitutel(Args, What, Into, OutArgs).






transold :-
	fail.

isolate_ioi('|'(Prefix,Atom1), Direction, Role, Atom1) :-
	(memberchk(Prefix, [input(Role),output(Role),internal(Role)])
	->	functor(Prefix, Direction, _)
	;	warning('Unrecognised atom prefix ~w', [Prefix]),
		fail
	).
isolate_ioi('|'(Prefix,Atom1), Prefix, Atom1) :-
	(memberchk(Prefix, [input(_),output(_),internal(_)])
	->	true
	;	warning('Unrecognised atom prefix ~w', [Prefix]),
		fail
	).
% reset_sorts: remove all old sort info
reset_sorts :-
	retractall(dyn_sort_info(_,_)).

% update_sorts: update all info wrt all present spec:sortdef/2 entries
update_sorts :-
	(	spec:sortdef(Sort, Elements),
		pre_code_sortdef(Sort, Elements),
		fail
	;	dyn_sortdef(Sort1, Sort, Elements, VLUsed, CElements),
		functor(Sort1, F, A),
		functor(Sort2, F, A),
		(	dyn_sortdef(Sort2, SortA, ElementsA, VLUsedA, CElementsA),
			dyn_sortdef(Sort1, Sort, Elements, VLUsed, CElements)
		\=@=
		dyn_sortdef(Sort2, SortA, ElementsA, VLUsedA, CElementsA)

		->	error('Multiple Sort definition conflict ~w - ~w',
				      [Sort, SortA])
		;	true
		),
		local_trace(csd),
		code_sortdef(Sort1, Sort, Elements, VLUsed, CElements),
		fail
	;	retractall(dyn_sortdef(_,_,_,_,_))
	).

new_sortdefs :-
	is_local,
	impl_error('new_sortdefs unimplemented').


code_sortdef(Sort1, Sort, Elements, VLUsed, CElements) :-
	new_sortdefs,
	!,
	assertz(spec:sortdef(Sort1, VLUsed, CElements, sortdef(Sort,Elements))),
	retract(spec:sortdef(Sort, Elements)).

code_sortdef(Sort1, Sort, Elements, VLUsed, CElements) :-
	SortsCutSize = 100,
	vl_size(VLUsed, SortsCutSize, VLSize),
	cmp_le(VLSize, SortsCutSize),% generate all sortdefs
	!,
	(	instantiate_vl(VLUsed),
		flag(sortsize, Old, 0),
		CutI = 100,
		(	bagof(SortElem,
			      CElements^numbered_sort_element(CElements,SortElem,
							      CutI),
			      Terms)
		->	true
		;	warning('Empty sort ~w', [Sort]),
			Terms = []
		),
		flag(sortsize, SortL, Old),
		(SortL >= CutI
		->	impl_error('Sort too big, should recode ~w', [Sort])
		;	assertz(spec:sortdef(Sort1, Terms)),
			local_inform('New sortdef:~w~n',[sortdef(Sort1, Terms)])
		),
		fail
	;	retract(spec:sortdef(Sort, Elements))
	).
vl_size(VLUsed, SortsCutSize, VLSize) :-
	flag(sortsize, Old, 0),
	(	instantiate_vl(VLUsed),
		flag(sortsize, I, I + 1),
		(I + 1 > SortsCutSize
		->	VLSize is I + 1
		;	fail
		)
	->	flag(sortsize, _, Old)
	;	flag(sortsize, VLSize, Old)
	).


numbered_sort_element(CElements,SortElement, CutI) :-
	numbered_sort_element1(CElements,SortElement),
	flag(sortsize, I, I + 1),
	(I + 1 >= CutI
	->	!
	;	true
	).
:- dynamic dyn_sortdef/5.


numbered_sort_element1(CElements,SortElement) :-
	member(CElement, CElements),
	instantiate_celement(CElement,SortElement).
instantiate_vl([]).
instantiate_vl([v(_Var, Sort, Term, _UsedInitial)|VLUsed]) :-
	get_sort_element_inter(Sort, Term),
	instantiate_vl(VLUsed).
instantiate_celement(subsort(_Term,TrTerm,LocalVars),SortElement) :-
	instantiate_vl(LocalVars),
	get_sort_element_inter(TrTerm, SortElement).

instantiate_celement(term(_Term,TrTerm, LocalVars), TrTerm) :-
	instantiate_vl(LocalVars).

get_sort_element_inter(between(I,J), SortElement) :-
	!,between(I,J,SortElement).
get_sort_element_inter(Sort, Element) :-
	sort_element(Sort, Element).




pre_code_sortdef(Sort, Elements) :-
	ground(Sort),
	no_vars(sortdef(Sort, Elements)),
	forall(member(E, Elements),
	       E \= 'SUBSORT'(_)
	      ),
	!.
pre_code_sortdef(Sort, Elements) :-
	code_sort_header(Sort, Sort1, VarList),
	code_sort_body(Elements, VarList, VLUsed, CElements),
	assertz(dyn_sortdef(Sort1, Sort, Elements, VLUsed, CElements)),
	!.
pre_code_sortdef(Sort, Elements) :-
	impl_error('~w failed', [code_sortdef(Sort, Elements)]).

code_sort_body([], VLIn, VLIn, []).
code_sort_body([Element|Elements], VLIn, VLOut,[E|EOut]) :-
	code_sort_elements(Element, VLIn, VL1, E),
	code_sort_body(Elements, VL1, VLOut, EOut).
code_sort_elements('SUBSORT'(Term), VLIn, VLOut,
		  subsort(Term,TrTerm,LocalVars)) :-
	!,tr_sort_term(Term, VLIn, VLOut, TrTerm, [], LocalVars).

code_sort_elements(Term, VLIn, VLOut, term(Term,TrTerm, LocalVars)) :-
	tr_sort_term(Term, VLIn, VLOut, TrTerm, [], LocalVars).



tr_sort_term(V:S, VLIn, VLOut, TrTerm, LVIn, LVOut) :-
	!,
	(	vinvlused(VLIn, V, S, VLOut, TrTerm, Status)
	->	var(Status),
		LVOut = LVIn
	;	vinvlused(V, S, LVIn, LVOut, TrTerm, Status),
		VLOut = VLIn
	->	var(Status)
	;	vusedtovl(LVIn, LVOut, V, S, TrTerm),
		VLOut = VLIn
	).
tr_sort_term(Term, VLIn, VLOut, TrTerm, LVIn, LVOut) :-
	spec_constant(Term, Term2),
	test_recursive_constant(Term, Term2),
	!,
	(tr_sort_term(Term2, VLIn, VLOut, TrTerm, LVIn, LVOut)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).
tr_sort_term(Term, VLIn, VLIn, Term, LVIn, LVIn) :-
	number(Term),
	!.
tr_sort_term(Term, _VLIn, _VLOut, _Term, _LVIn, _LVOut) :-
	arop(Term, _Ts, _TSs, _Term1),
	!,
	error('No expressions allowed in sort elements:~w',[Term]),
	fail.
tr_sort_term(Term, VLIn, VLOut, TrTerm, LVIn, LVOut) :-
	Term =.. [F|Args],
	tr_sort_terms(Args, VLIn, VLOut, TrArgs, LVIn, LVOut),
	TrTerm =.. [F|TrArgs].
tr_sort_terms([], VLIn, VLIn, [], LVIn, LVIn).
tr_sort_terms([Arg|Args], VLIn, VLOut, [TrArg|TrArgs], LVIn, LVOut) :-
	tr_sort_term(Arg, VLIn, VL1, TrArg, LVIn, LV1),
	tr_sort_terms(Args, VL1, VLOut, TrArgs, LV1, LVOut).

code_sort_header(Sort, _Sort1, _VarList) :-
	Sort = _:_,
	!,
	error('Sort name cannot be variable:~w', [Sort]),
	fail.
code_sort_header(Sort,_Sort1, _VarList) :-
	number(Sort),
	!,
	error('Sort name cannot be number:~w', [Sort]),
	fail.
code_sort_header(Sort, Sort1, VarList) :-
	Sort =.. [F|Args],
	code_sort_args(Args, Args1, [], VarList),
	Sort1 =.. [F|Args1].
code_sort_args([], [], VL, VL).
code_sort_args([Arg|Args], [Arg1|Args1], VLIn, VLOut) :-
	code_sort_arg(Arg, Arg1, VLIn, VL1),
	code_sort_args(Args, Args1, VL1, VLOut).
code_sort_arg(V:S, Arg1, VLIn, VLOut) :-
	!,vtovl(V,S,VLIn, VLOut, Arg1).
code_sort_arg(Arg, Arg1, VLIn, VLOut) :-
	Arg =.. [F|Args],
	code_sort_args(Args, Args1, VLIn, VLOut),
	Arg1 =.. [F|Args1].

vtovl(V,S,VLIn, [v(V,S,Arg1,initial)|VLIn], Arg1) :-
	(memberchk(v(V,_S1,_),VLIn)
	->	error('Variables can be used only once in sort name:~w',
		      [V:S]),
		fail
	;	true
	).
vinvlused([v(V1,S1,TrTerm1,Data)|VLIn], V, S, VLOut, TrTerm, Status) :-
	(V1 == V
	->	(S1 == S
		->	set_used_data(V, S, TrTerm1, Data, VSData),
			VLOut = [VSData|VLIn],
			TrTerm = TrTerm1
		;	error('Variable with different sorts:~w <-> ~w',
			      [V1:S1, V:S]),
			Status = error
		)
	;	vinvlused(VLIn, V, S, VLOut1, TrTerm, Status),
		VLOut = [v(V1,S1,TrTerm1,Data)|VLOut1]
	).
% caller should check for not present V
vusedtovl(LVIn, [VSData|LVIn], V, S, TrTerm) :-
	set_used_data(V, S, TrTerm, initial, VSData).

set_used_data(V, S, TrTerm1, _Data, v(V,S,TrTerm1,used)).

% update_sort(Sort) reevaluate sort_info referring to Sort
% also if there is no sortdef -> retract it
% this Sort refers to some spec:sortdef(Sort, _) entry added removed or changed
update_sort(_Sort) :-
	retractall(dyn_sort_info(_, _)).




has_printer_class :-
	get(@pce, convert, win_printer, class, _).
pnnp(pos,neg).
pnnp(neg,pos).
pntf(pos, true).
pntf(neg, false).

config_mod(Mod) :-
	member(Mod, [form_config, lt_config]),
	current_module(Mod),!.

kind_prefix(error, 'ERROR:').
kind_prefix(warning, 'WARNING:').

background_ar_fn(Term,module(Module)) :-
	functor(Term, F, A),
	memberchk(F/A-Module, [pareto_distance/2-pareto,
			       pareto_distance/3-pareto,
			       nash_distance/2-pareto,
			       nash_distance/3-pareto,
			       epp_distance/2-pareto,
			       epp_distance/3-pareto]).

background_function(module(Mod), Term, Result) :-
	%ensure_loaded(Mod),
	Mod:call(background_function(Term, Result)).

randomf(F) :-
        flag(rndwarning, I, I + 1),
        (I = 0
	->     warning('Bad random number quality, should be continuous interval (0,1) but is finegrained 0.00001...')
	;      true
        ),
	F is random(100000)/100000.0.


resource(tree_icon,	pixmap,	'tree.xpm').
my_autoload_all :-
	pce_autoload_all,
	pce_autoload_all,
	pce_autoload_all,
	(pce_autoload:autoload(Class, _File),
	\+ get(@classes, member, Class, _),
	\+ pce_prolog_class(Class)
	->	%fatal_error('Unexpectedly deep autoload nesting depth')
	        warning('Unexpectedly deep autoload nesting depth')
	;	true
	).


call_ground(In, Out) :-
	(ground(In)
	->	(call(In)
		->	Out = true
		;	Out = fail
		)
	;	Out = In
	).


my_term_to_atom(Term, Atom) :-
	(ground(Term)
	->	term_to_atom(Term, Atom1),
		Atom = Atom1
	;var(Term)
	->	term_to_atom(Term, Atom)
	;var(Atom)
	->	term_to_atom(Term, Atom)
	;	impl_error('wrong usage of term_to_atom')
	).





tell_error(File) :-
	catch_error_fail(tell(File), tell, [File]).


tr_formula_code_prolog_f(Formula, PN, or(not(F1), F2), PN) :-
	is_implies(Formula, F1, F2),
	!.

tfft(true, false).
tfft(false, true).
aooa(and, or).
aooa(or, and).

:- dynamic dyn_constant_used/2.
test_recursive_constante(Name,Value,Status) :-
	(dynInConstant(Name)
	->	error('Recursive constants not allowed:~w',
		      [Name]),
		local_trace(jet),
		Status = error
	;	asserta(dynInConstant(Name)),
		(dyn_constant_used(Name,Value)
		->	true
		;	assertz(dyn_constant_used(Name,Value))
		)
	).
test_recursive_constant(Name,Value) :-
	test_recursive_constante(Name,Value,Status),
	var(Status).

free_recursive_constant(Term) :-
	assert_debug(nonvar(Term)),
	(retract(dynInConstant(Term))
	->	true
	;	pr_error('Missing constant entry ~w', [Term])
	).

:- dynamic dynInConstant/1.
reset_constant_use :-
	retractall(dynInConstant(_)),
	retractall(dyn_constant_used(_,_)).
spec_constant_used(constant(Name, Val)) :-
	spec_constant(Name, Val),
	\+ \+ dyn_constant_used(Name, Val).

get_reset_constant_use(Constants) :-
	(setof(Constant, spec_constant_used(Constant), Constants)
	->	true
	;	Constants = []
	),
	reset_constant_use.

spec_constant(Name, Val) :-
	spec:constant(Name, Val).



check_constant(Name, Value) :-
	(	ground(Name),
		ground(Value)
	->	true
	;	error('constant contains prolog variable:~w',
		      [constant(Name, Value)]),
		fail
	),
	(contains_var(Name, Var, Sort)
	->	error('constant ~w contains variable:~w',
		      [constant(Name, Value), Var:Sort]),
		fail
	;	true
	),
	(reserved(Name)
	->	error('constant(~w, ~w): ~w is a reserved name',
		      [Name, Value, Name]),
		fail
	;	true
	).
% END CONSTANTS

var_occurs_in(V, Term) :-
	var(Term),
	!,
	V == Term.
var_occurs_in(V, Term) :-
	Term =.. [_|Args],
	var_occurs_in_l(V, Args).

var_occurs_in_l(V, [F|Args]) :-
	(var_occurs_in(V, F)
	->	true
	;	var_occurs_in_l(V, Args)
	).

local_option(wd, single('-filewd', set_option(filewd)),
	     'Use file argument directory as working directory',[]).
local_option(wd,
             arg('-wd', Arg, set_option(wd(Arg)),'WorkingDirectory'),
	     'Set working directory',[]).
local_option(constant,
             arg('-constant', Arg, set_option_constant(Arg),'NAME=VALUE'),
	     'Add constant to specification',[]).
local_option(constant,
             arg('-addterm', Arg, set_option(addterm(Arg)),'SPECTERM'),
	     'Add term to specification',[]).

local_option(log, single('-nologging', set_logging(nothing)),
	     'No logging',[]).
local_option(log, arg('-nolog', Arg, reset_logging(Arg), 'LogTag'),
	     'Logging control: restart with -log help for more info', []).
local_option(log, arg('-log', Arg, set_logging(Arg), 'LogTag'),
	     'Logging control: restart with -log help for more info', []).
local_option(log, arg('-logging', Arg, set_logging(Arg), 'LogTag'),
	     '(same as -log)', []).
local_option(debugging, single('-h',set_option(dohelp)),
	     'Print help and exit', []).
local_option(debugging, single('--help',set_option(dohelp)),
	     'Print help and exit', []).
local_option(debugging, single('-local',setlocal),
	     'Debug mode local', [help_sort(back(10))]).

atom_to_term_non_var(CommandAtom, Term) :-
	atom_to_term(CommandAtom, Term, Bindings),
	bind_bindings(Bindings).
bind_bindings([]).
bind_bindings([A=B|Rest]) :-
	A = B,
	!,
	bind_bindings(Rest).
set_option_constant(Arg) :-
	atom_to_term_non_var(Arg, Term),
	(Term = (Name = Value)
	->	(check_constant(Name, Value)
		->	add_cmd_constant(Name, Value)
		;	true
		)
	;	error('expected -constant NAME=VALUE, got -constant ~w',
		      [Term])
	).
:- dynamic dyn_add_cmd_constant/2.
cmd_constant(Name, Value) :-
	dyn_add_cmd_constant(Name, Value).

load_cmd_constants :-
	(	dyn_add_cmd_constant(Name, Value),
		spec:asserta(constant(Name, Value)),
		fail
	;	true
	).

add_cmd_constant(Name, Value) :-
	(dyn_add_cmd_constant(Name, _)
	->	warning('Multiple command line constants with name ~w(previous ones probably ignored)',
			[Name])
	;	true
	),
	% order assertz, assertz constant(..)
	assertz(dyn_add_cmd_constant(Name, Value)).

/*
application consists of various functionalities
  - lt simulation
  - lt load_trace
  - lt show_trace_result
  - lt editor
      - tree editor
  - ttl editor
      - tree editor
  - ttl checker
  - general options : -local -debugging

  ltsim,ltshow,
  ftree,lttree,
  Application has name + help header text.
  Application has list of part functionality:
     Need module in which to call parts but also label for part options

  setupmainnew(Header, Pgm, OptionSpecs, SingleNonOptionArgOrEmptyList)
  OptionSpecs : os(Module, Tag)

  Module:local_option(Tag, Code, HelpCode, Options)
  Tag: distinguishes groups of tags
  Code: single('-opt1', Call)
  Code: arg('-opt2',Arg, Call, HelpArgText)
  HelpCode : text for help entry
  Options: help_sort(front(SortKey))
           help_sort(back(InverseSortKey))
  Other entries are not sorted.
  Normal order is order of OptionSpecs, Module:local_option
  */
setupmainnew(Header, Pgm,OptionSpecs,  File) :-
	setupmainnewn(Header, Pgm,OptionSpecs,  1, Files, false),
	(Files == []
	->	File = []
	;Files = [File]
	->	true
	;	impl_error('Missing args')
	),
	send(class(frame), class_variable_value,
	     icon_image, resource(tree_icon)).



setupmainnewn(Header, Pgm,OptionSpecs, N, Files, ExactArgs) :-
	(   memberchk(file(File), OptionSpecs)
	->  Files = [File]
	;   unix(argv(Argv)),
	    Argv = [_|Args1],
	    skipPrologArgs(Args1, Args0),
	    getLocalOptionsNew(Args0, OptionSpecs, Args2),
	    (Args2 == []
	    ->	Files = [],
		check_files_args0(Files, N, ExactArgs, [Pgm|Args1], OptionSpecs,Header)
	    ;Args2 = [--|Files]
	    ->	check_files_args0(Files, N, ExactArgs, [Pgm|Args1], OptionSpecs,Header)
	    ;	member(E, Args2),
		may_be_option(E)
	    ->	format('You seemed to start up with "~w"~n',[[Pgm|Args1]]),
		version_header(Header, []),
	     do_help1(OptionSpecs),
	     fatal_error('Unrecognised option ~w', [E])
	    ;	Files = Args2,
		check_files_args0(Files, N, ExactArgs, [Pgm|Args1], OptionSpecs,Header)
	    ->	true
	    ;	format('You seemed to start up with "~w"~n',[[Pgm|Args1]]),
		do_help1(OptionSpecs),
	     fatal_error('Run with zero or one file argument(s)')
	    ),
	    version_header(Header, Files),
	    adjust_wd(Files, OptionSpecs),
	    (get_option(dohelp)
	    ->	do_help1(OptionSpecs),
		finalhalt(0)
	    ;	true
	    )
	).
check_files_args0(Files, N, ExactArgs, PgmArgs1, OptionSpecs, Header) :-
	(get_option(dohelp)
	->	do_help1(OptionSpecs),
		finalhalt(0)
	;	true
	),
	length(Files, L),
	(L =:= N
	->	true
	;	L < N,
		ExactArgs \= true
	->	true
	;	format('You seemed to start up with "~w"~n',[PgmArgs1]),
		version_header(Header, []),
		do_help1(OptionSpecs),
		fatal_error('Wrong number of arguments')
	).

:- dynamic specified_wd/1.
update_wd :-
	(retract(specified_wd(WD))
	->	working_directory(_, WD)
	;	true
	).

adjust_wd(Files, OptionSpecs) :-
	(get_option(wd(WD))
	->	(exists_directory(WD)
		->	assertz(specified_wd(WD))
		;	error('Could not adjust working directory'),
			finalhalt(1)
		)
	;	get_option(filewd)
	->	(Files == []
		->	error('Option -filewd requires FILE'),
			do_help1(OptionSpecs),
			finalhalt(1)
		;	(Files = [File|_]
			->	true
			;	impl_error('Missing file argument')
			),
			(	prolog_to_os_filename(IFile, File),
				access_file(IFile, read),
				file_directory_name(IFile, Dir)
			->	assertz(specified_wd(Dir))
			;	error('Could not adjust working directory'),
				finalhalt(1)
			)
		)
	;	true
	).


%  setupmain1(Header, Pgm, File)
%    Module: Prefix for actions and option info
%    Header: The text accompanying version info
%    Pgm   : The name used in the error message for unrecognised option.
%    File  : The single optional non option argument ([] if not present)
%
% Module must define Module:local_option1(Opt, Action) and then Module:Action
% Module must define Module:local_option2(Opt, Arg, Action) and then Module:Action
% Module must define Module:do_help1
setupmain1(Module, Header, Pgm, File) :-
	unix(argv(Argv)),
	Argv = [_|Args1],
	skipPrologArgs(Args1, Args0),
	getLocalOptions(Module, Args0, Args2),
	(Args2 == []
	->	File = []
	;Args2 = [--,File]
	->	true
	;	Args2 = [E|_Rest],
		may_be_option(E)
	->	format('You seemed to start up with "~w"~n',[[Pgm|Args1]]),
		Module:do_help1,
		fatal_error('Unrecognised option ~w', [E])
	;	Args2 = [File]
	->	true
	;	format('You seemed to start up with "~w"~n',[[Pgm|Args1]]),
		Module:do_help1,
		fatal_error('Run with zero or one file argument(s)')
	),
	version_header(Header, File).
getLocalOptionsNew(Args, _OptionSpecs, _Args2) :-
	var(Args),
	!,
	impl_error('Var args').

getLocalOptionsNew([], _OptionSpecs, []).
getLocalOptionsNew([Arg0|Args0], OptionSpecs, Args2) :-
	(getLocalOption(Arg0, Args0, OptionSpecs, Args1)
	->	Args2 = Args3
	;	Args2 = [Arg0|Args3],
		Args1 = Args0
	),
	getLocalOptionsNew(Args1, OptionSpecs, Args3).
getLocalOption(Arg0, Args0, OptionSpecs, Args1) :-
	member(os(Module, TagL), OptionSpecs),
	list_or_single_element(TagL, Tag),
	Module:local_option(Tag, Code, _HelpCode, _Options),
	handle_option(Code, Arg0, Args0, Module, Args1),!.
list_or_single_element(TagL, Tag) :-
	is_list(TagL),
	!,
	member(Tag, TagL).
list_or_single_element(Tag, Tag).

help_lhs(OptionSpecs, N) :-
	flag(help_lhs, Old, 0),
	(	member(os(Module, TagL), OptionSpecs),
		list_or_single_element(TagL, Tag),
		Module:local_option(Tag, Code, _HelpCode, _Options),
		lhs_size(Code, N1),
		flag(help_lhs, Now, max(Now, N1)),
		fail
	;	flag(help_lhs, N, Old)
	).
lhs_size(single(Opt, _Call), N1) :-
	!,
	atom_codes(Opt, C),
	length(C, N1).
lhs_size(arg(Opt,_Arg, _Call, HelpArgText), N) :-
	!,
	atom_codes(Opt, C),
	length(C, N1),
	atom_codes(HelpArgText, C2),
	length(C2, N2),
	N is N1 + N2 + 1.


do_help1(OptionSpecs) :-
	help_lhs(OptionSpecs, LM1),
	LM is LM1 + 4,
	(setof(Key-e(Code, HelpCode, Options),
	      option(front,OptionSpecs, Code, HelpCode, Options,Key),
	      FrontEntries)
	->	(	member(Key-e(Code, HelpCode, Options),FrontEntries),
			write_help(Code, HelpCode, Options, LM),
			fail
		;	true
		)
	;	true
	),
	(	option(middle,OptionSpecs, Code, HelpCode, Options, _Key),
		write_help(Code, HelpCode, Options, LM),
		fail
	;	true
	),
	(	setof(Key-e(Code, HelpCode, Options),
		      option(back,OptionSpecs, Code, HelpCode, Options,Key),
		      BackEntries)
	->	reverse(BackEntries, BackEntries1),
		(	member(Key-e(Code, HelpCode, Options),BackEntries1),
			write_help(Code, HelpCode, Options, LM),
			fail
		;	true
		)
	;	true
	).
write_help(single(Opt, _Call), HelpCode, _Options, LM) :-
	format('   ~w~t~*|: ~w~n', [Opt, LM, HelpCode]).
write_help(arg(Opt, _Arg, _Call, HT), HelpCode, _Options, LM) :-
	format('   ~w ~w~t~*|: ~w~n', [Opt, HT, LM, HelpCode]).
option(FMB,OptionSpecs, Code, HelpCode, Options,Key) :-
	member(os(Module, TagL), OptionSpecs),
	list_or_single_element(TagL, Tag),
	Module:local_option(Tag, Code, HelpCode, Options),
	fmb_option(FMB, Options,Key).
fmb_option(Kind, Options, Key) :-
	memberchk(help_sort(front(Key)),Options),
	!,
	Kind = front.
fmb_option(Kind, Options, Key) :-
	memberchk(help_sort(back(Key)),Options),
	!,
	Kind = back.
fmb_option(middle, _Options, _Key).


handle_option(single(Arg0, Call), Arg0, Args0, Module, Args0) :-
	Module:call(Call),
	!.

handle_option(arg(Arg0,Arg, Call, _H), Arg0, Args, Module, Args1) :-
	(Args = [--,Arg|Args1]
	->	true
	;	Args = [Arg|Args1]
	->	(may_be_option(Arg)
		->	fatal_error('Option "~w" requires argument.', [Arg0])
		;	true
		)
	;	fatal_error('Option "~w" requires argument.', [Arg0])
	),
	Module:call(Call),
	!.


getLocalOptions(Module, Args0, Args2) :-
	(	Module:local_option1(Opt, Action),
		select(Opt, Args0, Args1),
		Module:call(Action)
	->	getLocalOptions(Module, Args1, Args2)
	;	Module:local_option2(Opt, Arg, Action),
		select2(Opt, Arg, Args0, Args3),
		Module:call(Action)
	->	getLocalOptions(Module, Args3, Args2)
	;	Args2 = Args0
	).



:- dynamic dyn_option/1.

% Usage:
% local_option2('-gui', Arg,
%               set_term_option_bind(Arg, TermArg, gui(TermArg),
%						gui_option_syntax)).
set_term_option_bind(Atom, Arg, Option, ErrorTag) :-
	valid_term_bg(Atom, Arg, ErrorTag),
	set_option(Option).
reset_option(X) :-
	assert_debug(ground(X)),
	retractall(dyn_option(X)).

:- dynamic dynProf/1, dynTiming/2, dynTiming1/2.

dotime :-
	(do_log(timing)
	->	retractall(dynTiming(_,_)),
		get_time(T0),
		statistics(cputime, CPU0),
		asserta(dynTiming(T0,CPU0))
	;	true
	).
endtime :-
	(do_log(timing)
	->	get_time(T1),
		statistics(cputime, CPU1),
		(retract(dynTiming(T0,CPU0))
		->	true
		;	pr_error('Missing time entry')
		),
		T is T1 - T0,
		CPU is CPU1 - CPU0,
		assertz(dynTiming1(T, CPU))
	;	true
	).
showtime :-
	(do_log(timing)
	->	(retract(dynTiming1(T, CPU))
		->	alog(timing, 'TIME:~w CPUTIME:~w~n',[T, CPU])
		;	pr_error('Missing time entry')
		)
	;	true
	).

doprof :-
	dotime,
	(	dynProf(Kind),
		Kind \= off
	->	ignore(catch(profiler(_, Kind), Error, restoreProfileProblem(Error)))
	;	true
	).
endprof :-
	endtime,
	(	dynProf(Old),
		Old \= off
	->	profiler(_,off)
	;	true
	).
showprof :-
	showtime,
	(	dynProf(Old),
		Old \= off
	->	show_profile(100)
	;	true
	).
restoreProfileProblem(Error) :-
	format('WARNING: profiling probably disabled(~w)~n', [Error]),
	retractall(dynProf(_)).
getprof(How) :-
	dynProf(How).

setprof(How) :-
	retractall(dynProf(_)),
	assertz(dynProf(How)).
set_bool_option(X, TF) :-
	assert_debug(memberchk(TF, [true, false])),
	(TF == true
	->	set_option(X)
	;	reset_option(X)
	).

set_option(X) :-
	assert_debug(ground(X)),
	(dyn_option(X)
	->	true
	;	assertz(dyn_option(X))
	).
get_option(X) :-
	dyn_option(X).

update_option(AnyOld, New) :-
	(	get_option(AnyOld),
		reset_option(AnyOld),
		fail
	;	set_option(New)
	).




bind_binding(Name = Var) :-
	(	var(Var),
		atom(Name),
		atom_codes(Name, [F|_]),
		(	F = 95
		;	F >= 65,
			F =< 90
		)
	->	Var = '$VAR'(Name)
	;	impl_error('Constant not a prolog variable:~w', [Name]),
		fail
	).

local_trace(Where) :-
	(is_local
	->	format('LOCAL TRACE:~w~n', [Where]),
		trace
	;	true
	).


clear_module(Module) :-
	(	predicate_property(Module:Pred, interpreted),
		\+ predicate_property(Module:Pred, imported_from(_)),
		functor(Pred, F, A),
		(predicate_property(Module:Pred, dynamic)
		->	retractall(Module:Pred),
			abolish(Module:F/A)
		;	impl_error('Cannot clear ~w', [Module:F/A])
		),
		fail
	;	true
	).


recover_load_module(E) :-
	print_message(error, E),
	fail.

recover_load_module_fatal(E,FErrorTag,File) :-
	print_message(error, E),
	(ferror(FErrorTag, Format, 1)
	->	fatal_error(Format, [File])
	;	fatal_error('Could not load file "~w" (details:~w)',
			    [File, FErrorTag])
	).


user:recover_fatal(E, FErrorTag, Call) :-
	print_message(error, E),
	(ferror(FErrorTag, Format, 1)
	->	fatal_error(Format, [Call])
	;	impl_error('Call "~w" failed(tag:~w)',
			    [Call, FErrorTag])
	).

user:recover_error_fail(E, FErrorTag, Args) :-
	print_message(error, E),
	ferror(FErrorTag, Args),
	fail.
catch_error_fail(Call, FErrorTag, Args) :-
	catch(Call, E, user:recover_error_fail(E, FErrorTag, Args)).

catch_fatal_halt(Call, FErrorTag) :-
	((\+ is_local;is_win32)
	->	catch(Call, E, user:recover_fatal(E, FErrorTag, Call))
	;	Call
	).

load_module_fatal(File, ModulePrefix, FStart, FEnd, FErrorTag) :-
	(	FStart \= [],
		fmessage(FStart, Format, 1)
	->	inform(Format, [File])
	;	inform('Loading file "~w"...', [File])
	),
	load_module_fatal(File, ModulePrefix, FErrorTag),
	(	FEnd \= [],
		fmessage(FEnd, Format, 1)
	->	inform(Format, [File])
	;	inform('Loading file "~w" done', [File])
	).

load_module_fatal(File, ModulePrefix, FErrorTag) :-
	catch(ModulePrefix:ensure_loaded(File),E,
	      recover_load_module_fatal(E,FErrorTag,File)).
load_module(FileName, ModulePrefix) :-
	catch(ModulePrefix:ensure_loaded(FileName),E, recover_load_module(E)).

app_info(App,VersionText,I) :-
	(	predicate_property(user:app_version(_,_,_),interpreted),
		setof(a(App1,VersionText1,I1),
		      user:app_version(App1,VersionText1,I1),
		      As),
		As = [a(App,VersionText,I)]
	->	true
	;	warning('SLOPPY INSTALLATION OF APPLICATION'),
		App = 'NO APPLICATION',
		VersionText = 'UNDETERMINED VERSION',
		I = -1
	).


version_details(VersionText, app(App,I,TNs)) :-
	(	app_info(App,VersionText,I),
		integer(I)
	->	(	app_info(App1,VersionText1,I1),
			(	App1 \= App
			;	VersionText1 \= VersionText
			;	I \= I1
			)
		->	impl_error('Multiple version info')
		;	true
		),
		findall(Tag:Num, user:version(Tag, Num), TNs)
	;	impl_error('Missing version info')
	).

source_details(FName, Details) :-
	(FName \= @nil
	->	new(SaveFile, file(FName)),
		(send(SaveFile, exists)
		->	get(SaveFile, size, SZ),
			get(SaveFile, absolute_path, AFName),
			get(SaveFile, time, modified, MDate),
			get(MDate?string, value, MTime),
			Details = file(FName, [size(SZ),path(AFName),
					       mdate(MTime)])
		;	Details = file(FName, [not_exists])
		)
	;	Details = []
	).

detail_header :-
	do_log(versiond),
	!,
	format(user_error,'~n          Version details:~n', []),
	app_info(App,_VersionText,I),
	format(user_error,'                app/~w:~w~n', [App,I]),
	(	user:version(Tag, Num),
		format(user_error,'                ~w:~w~n', [Tag,Num]),
		fail
	;	true
	).
detail_header.

version_header(PgmName, Files) :-
	do_log(versionh),
	!,
	version_details(VersionText, _Details),
	format(user_error,
	       '~n    ~w version ~w', [PgmName, VersionText]),
	(	Files = []
	->	true
	;Files = [File]
	->	format(user_error,'  FILE:~w~n', [File])
	;	format(user_error,'  FILES:~w~n', [Files])
	),
	nl(user_error),
	detail_header,
	nl(user_error),
	nl(user_error).
version_header(_PgmName, _File).

log_entry(version_header, 'Print program name and version info', versionh, on).
log_entry(version_details, 'Print version details', versiond, off).
log_entry(termination, 'Termination of program', termination, on).
log_entry(timing, 'Printout time main operation takes', timing, on).


win_get_file(File, Ext) :-
	(get(@finder, file, @on, Ext, '.', File)
	->	true
	;	fatal_error('No specification for trace')
	).

is_win32 :-
	prolog_flag(arch, 'i386-win32').

%  select2(A1, A2, LIn, LOut)    TESTED: select2(+A1, ?A2, +LIn, -LOut)
%     select two consecutive elements from list
select2(A1, A2, LIn, LOut) :-
	select21(LIn, A1, A2, LOut).
select21([A1, A2|LIn], A1, A2, LIn).
select21([X|LIn], A1, A2, [X|LOut]) :-
	select21(LIn, A1, A2, LOut).

% list_pre_postfix(List, Element, Prefix, Postfix)
%    L contains Element and Prefix . [Element] . Postfix = List
list_pre_postfix(List, Element, Prefix, Postfix) :-
	append(Prefix, [Element|Postfix], List).

:- meta_predicate setof1(?, :, ?).
setof1(S1, S2, S3) :-
	(setof(S1, S2, S3)
	->	true
	;	S3 = []
	).

user_forced_halt :-
	halt_message(0),
	halt.

halt_message(E) :-
	(E = 0
	->	alog(termination, '~nNormal termination of program~n~n')
	;	alog(termination,'~nProgram terminates with code ~w~n~n', [E])
	).

finalhalt(E) :-
	halt_message(E),
	(prolog_flag(arch, 'i386-win32')
	->	send(@pce, confirm, 'PROGRAM ended, close window?'),
		halt
	;	halt(E)
	).

bagof1(S1, S2, S3) :-
	(bagof(S1, S2, S3)
	->	true
	;	S3 = []
	).
local_format(Format) :-
	local_format(Format, []).

local_format(Format, Args) :-
	(is_local
	->	format(Format, Args)
	;	true
	).

lformat(Format, Args) :-
	(Format = [_|_]
	->	concat_atom(Format, F1)
	;	F1 = Format
	),
	format(F1, Args).

prallbutlast([X1,X2|R], F1) :-
	!,
	format(X1, []),
	prallbutlast([X2|R], F1).
prallbutlast([F1],F1).


pr_error(Format) :-
	pr_error(Format, []).
pr_error(Format, Args) :-
	telling(Old),
	tell(user),
	write('IMPLEMENTATION ERROR:'),
	lformat(Format, Args),
	nl,
	(is_local
	->	trace
	;	true
	),
	tell(Old).



impl_error(Format) :-
	impl_error(Format, []).
impl_error(Format, Args) :-
	telling(Old),
	tell(user),
	write('IMPLEMENTATION ERROR:'),
	lformat(Format, Args),
	nl,
	(is_local
	->	trace
	;	true
	),
	tell(Old),
	finalhalt(1).

log(Format) :-
	lformat(Format, []),
	nl.
log(Format, Args) :-
	lformat(Format, Args),
	nl.


:- module_transparent fatal_fail/1.

fatal_fail(Call) :-
	is_local,
	!,
	format(user_error,
	       '~nIMPLEMENTATION ERROR call ~w failed, DEBUGGING~n', [Call]),
	local_trace(fatal_fail),
	call(Call).
fatal_fail(Call) :-
	impl_error('Call ~w failed', [Call]).

fatal_error(Format) :-
	fatal_error(Format, []).
fatal_error(Format, Args) :-
	format(user_error, '~nFATAL ERROR:',[]),
	lformat(Format, Args),
	nl,
	(is_local
	->	trace
	;	true
	),
	finalhalt(1).


setlocal :-
	assertz(is_local).

:- dynamic is_local/0.

/* ensure/1: even without debugging, call should be done,
   variables could become bound.
   */
ensure(Call) :-
	(Call
	->	true
	;	impl_error('Call ~w failed', [Call]),
		fail
	).

ensure_set_once(Call) :-
	(Call
	->	true
	;	impl_error('~w failed', [ensure_set_once(Call)]),
		fail
	).

assert_debug(Call,Where) :-
	(\+ \+ Call
	->	true
	;	impl_error('ASSERTION FAILED:~w (at ~w)~n', [Call, Where])
	).
assert_debug(Call) :-
	(\+ \+ Call
	->	true
	;	impl_error('ASSERTION FAILED:~w~n', [Call])
	).

ferrorv(Tag, Args, Status) :-
	(var(Status)
	->	Status = error,
		ferror(Tag, Args)
	;	true
	).
errorv(Format, Args, Status) :-
	(var(Status)
	->	Status = error,
		error(Format, Args)
	;	true
	).

error(Format) :-
	error(Format, []).

error(Format, Args) :-
	telling(Old),
	tell(user),
	write('***ERROR:'),
	lformat(Format, Args),
	nl,nl,
	tell(Old).


construct_format1(0, '') :-
	!.
construct_format1(1, '  %w') :-
	!.
construct_format1(I, Format) :-
	I > 1,
	I1 is I - 1,
	construct_format1(I1, Format1),
	atom_concat('   %w   ', Format1, Format).


construct_format(N, Format) :-
	construct_format1(N, Format),
	concat_atom(['%w  : ', Format, ' (undefined)'], Format).

fformat(Args, Tag, Format) :-
	(is_list(Args)
	->	true
	;	impl_error('Wrong usage of fformat: first arg must be list, is ~w', [Args])
	),
	length(Args, N),
	(ferror(Tag, Format, N)
	->	true
	;	construct_format(N, Format)
	).

ferror(Tag) :-
	ferror(Tag, []).

ferror(Tag, Args) :-
	fformat(Args, Tag, Format),
	error(Format, Args).
fwarning(Tag) :-
	fwarning(Tag, []).

fwarning(Tag, Args) :-
	fformat(Args, Tag, Format),
	warning(Format, Args).

:- dynamic dyn_warned/1.

warning_once(Txt) :-
	(dyn_warned(Txt)
	->	true
	;	warning(Txt),
		assertz(dyn_warned(Txt))
	).

warning(Format) :-
	warning(Format, []).

warning(Format, Args) :-
	telling(Old),
	tell(user),
	write('WARNING:'),
	lformat(Format, Args),
	nl,
	tell(Old).

local_warning(F) :-
	local_warning(F, []).

local_warning(F, A) :-
	(is_local
	->	warning(F, A)
	;	true
	).

sat_format(Format) :-
	sat_format(Format, []).

sat_format(Format, Args) :-
	telling(Old),
	tell(user),
	lformat(Format, Args),
	nl,
	tell(Old).

local_inform(F) :-
	local_inform(F, []).

local_inform(F, A) :-
	(is_local
	->	inform(F, A)
	;	true
	).

inform(Format) :-
	inform(Format, []).
informu(Format, Args) :-
	inform(Format, Args).

inform(Format, Args) :-
	telling(Old),
	tell(user),
	lformat(Format, Args),
	nl,
	tell(Old).

cmp_ge(T1, T2) :-
	(T1 == maxinf
	->	true
	;T2 == maxinf
	->	fail
	;T2 == mininf
	->	true
	;T1 == mininf
	->	fail
	;	T1 >= T2
	).

cmp_eq(maxinf, maxinf) :-
	!.
cmp_eq(maxinf, _) :-
	!,
	fail.
cmp_eq(_, maxinf) :-
	!,
	fail.
cmp_eq(mininf, mininf) :-
	!.
cmp_eq(mininf, _) :-
	!,
	fail.
cmp_eq(_, mininf) :-
	!,fail.
cmp_eq(T1, T2) :-
	T1 =:= T2.

cmp_gt(T1, T2) :-
	\+ cmp_le(T1, T2).
cmp_lt(T1, T2) :-
	\+ cmp_ge(T1, T2).
cmp_le(T1, T2) :-
	cmp_ge(T2, T1).

add_new(mininf, mininf, mininf) :-
	!.
add_new(maxinf, maxinf, maxinf) :-
	!.
add_new(mininf, maxinf, _) :-
	!,
	fatal_error('Cannot add mininf + maxinf').
add_new(maxinf, mininf, _) :-
	!,
	fatal_error('Cannot add mininf + maxinf').
add_new(X, _, X) :-
	member(X, [mininf, maxinf]),
	!.
add_new(_, X, X) :-
	member(X, [mininf, maxinf]),
	!.
add_new(X, Y, XY) :-
	XY is X + Y.


min_new(X, Y, Res) :-
	(cmp_le(X, Y)
	->	Res = X
	;	Res = Y
	).

max_new(X, Y, Res) :-
	(cmp_ge(X, Y)
	->	Res = X
	;	Res = Y
	).

pncode(PN, CodeIn, CodeOut) :-
	presimplify_code(CodeIn, Code1),
	!,
	pncode(PN, Code1, CodeOut).

pncode(pos, Code, Code) :-
	!.
pncode(neg, true, fail) :-
	!.
pncode(neg, fail, true) :-
	!.
pncode(neg, \+ Code, Code) :-
	!.
pncode(neg, Code, (\+ Code)).


combine_or([], fail).
combine_or([F1a|R], Code) :-
	simplified_code(F1a, F),
	(F == fail
	->	combine_or(R, Code)
	;F == true
	->	Code = true
	;F = (F1;F2)
	->	combine_or([F1,F2|R], Code)
	;	combine_or(R, Code1),
		(Code1 == fail
		->	Code = F
		;	Code1 == true
		->	Code = true
		;	Code = (F;Code1)
		)
	).

combine_or_no_bt([], fail).
combine_or_no_bt([F1a|R], Code) :-
	simplified_code(F1a, F),
	(F == fail
	->	combine_or_no_bt(R, Code)
	;F == true
	->	Code = true
	;F = (F1;F2)
	->	combine_or_no_bt([F1,F2|R], Code)
	;	combine_or_no_bt(R, Code1),
		(Code1 == fail
		->	Code = F
		;	Code1 == true
		->	Code = true
		;	Code = (F->true;Code1)
		)
	).
combine_or_bt(true, CodeL, Code) :-
	combine_or(CodeL, Code).
combine_or_bt(false, CodeL, Code) :-
	combine_or_no_bt(CodeL, Code).

simplified_code(CIn, COut) :-
	(	nonvar(CIn),
		presimplify_code(CIn, COut)
	->	true
	;	COut = CIn
	).
presimplify_code(Code, _) :-
	var(Code),
	!,
	fail.

presimplify_code(between(X,Y,Z), Res) :-
	(	integer(X),
		integer(Y)
	->	(X > Y
		->	warning('~w: range contains no values -> will fail',
				[between(X,Y,Z)]),
			!,
			Res = fail
		;	X = Y
		->	(ground(Z)
			->	(integer(Z)
				->	(Z = X
					->	!,Res = true
					;	warning('~w:Type test will fail',
							[between(X,Y,Z)]),
						!,Res = fail
					)
				;	warning('~w:Type mismatch-> fail',
						[between(X,Y,Z)]),
					!,Res = fail
				)
			;	var(Z)
			->	!,Res = (Z = X)
			;	(integer(Z)
				;number(Z),Z =:= X
			->	(Z =:= X
				->	!,Res = true
				;	warning('~w:Type test will fail',
							[between(X,Y,Z)]),
						!,Res = fail
				)
			;	warning('~w:Type mismatch-> fail',
						[between(X,Y,Z)]),
					!,Res = fail
				)
			)
		;	ground(Z)
		->	(integer(Z)
			->	(between(X,Y,Z)
				->	!,Res = true
				;	warning('~w:Type test will fail',
							[between(X,Y,Z)]),
					!,Res = fail
				)
			;	number(Z)
			->	warning('~w:Type test will fail mbetween requires integer',
							[between(X,Y,Z)]),
				!,Res = fail
			;	warning('~w:Type mismatch-> fail',
						[between(X,Y,Z)]),
					!,Res = fail
			)
		;	fail
		)
	;	(nonvar(X),\+integer(X);nonvar(Y),\+integer(Y))
	->	warning('~w:Type mismatch(first args must be integer)-> fail',
			[between(X,Y,Z)]),
		!,Res = fail
	;	ground(Z),
		\+ integer(Z)
	->	warning('~w:Type mismatch(last arg must be integer)-> fail',
			[between(X,Y,Z)]),
		!,Res = fail
	;	fail
	).


combine_and(L, _) :-
	assert_debug(nonvar(L)),
	fail.

combine_and([], true) :-
	!.
combine_and([F1a|R], Code) :-
	assert_debug(nonvar(F1a)),
	simplified_code(F1a,F),
	(F == fail
	->	Code = fail
	;F == true
	->	combine_and(R, Code)
	;F = (F1,F2)
	->	combine_and([F1,F2|R], Code)
	;	combine_and(R, Code1),
		(Code1 == true
		->	Code = F
		;Code1 == fail
		->	Code = fail
		;	Code = (F,Code1)
		)
	).


list_andn([], true) :-
	!.
list_andn([P], P) :-
	!.
list_andn(L, A) :-
	A =.. [and|L].

list_and([], true).
list_and([P], P) :-
	!.
list_and([P|Q], and(P,Q1)) :-
	list_and(Q, Q1).


may_be_option(E) :-
	atom_concat(-,_,E),
	!.
may_be_option(E) :-
	atom_concat('--',_,E).

double_prolog_arg('-x').
double_prolog_arg('-f').
double_prolog_arg('-p').
double_prolog_arg('-g').
double_prolog_arg('-L').
isStackArg(Arg) :-
	atom_chars(Arg, [-,GL|_]),
	memberchk(GL, ['G','L']),
	!.

skipPrologArgs([],[]) :- !.
skipPrologArgs([Arg|Args], Args1) :-
	isStackArg(Arg),
	!,
	skipPrologArgs(Args, Args1).

skipPrologArgs([--|Args], Args1) :-
	skipPrologArgs(Args, Args1),
	!.
skipPrologArgs([Option,_|ArgsIn], ArgsOut) :-
	double_prolog_arg(Option),
	!,skipPrologArgs(ArgsIn, ArgsOut).
skipPrologArgs([Arg|ArgsIn],[Arg|ArgsOut]) :-
	skipPrologArgs(ArgsIn, ArgsOut).

/*
chain_iterate(Chain, ElemCode, BetweenCode) :-
	send(block(assign(new(First,var),@on),
		   message(Chain, for_all,
			   and(
			       if(First == @on,
				  assign(First,@off,global),
				  BetweenCode,
				  ElemCode)))),
	     execute).
*/
ensure_object(In, Out) :-
	(In = (@ _)
	->	Out = In
	;	new(Out, In)
	).

chain_iterate(Chain, ElemCode, BetweenCode) :-
	chain_list(Chain, List),
	ensure_object(ElemCode, ElemCode1),
	ensure_object(BetweenCode, BetweenCode1),
	list_iterate1(List, ElemCode1, BetweenCode1).
list_iterate1([], _ElemCode, _BetweenCode).
list_iterate1([F], ElemCode, _BetweenCode) :-
	!,
	send(ElemCode, forward, F).
list_iterate1([F|R], ElemCode, BetweenCode) :-
	send(ElemCode, forward, F),
	send(BetweenCode, execute),
	list_iterate1(R, ElemCode, BetweenCode).

contains_var(X, _, _) :-
	var(X),
	!,
	pr_error('contains_var called on term containing prolog var').

contains_var(X, _, _) :-
	atom(X),
	!,
	fail.
contains_var(Var:Sort,Var, Sort) :-
	!.
contains_var(Term, Var, Sort) :-
	functor(Term, _, A),
	between(1, A, I),
	arg(I, Term, Arg),
	contains_var(Arg, Var, Sort).

no_vars(X) :-
	atom(X).
no_vars(_:_) :-
	!,
	fail.
no_vars(X) :-
	X =.. [_|Args],
	uchecklist(no_vars, Args).




ground_vars_dollar(Term, Vars) :-
	uchecklist(bind_binding, Vars),
	ground_anonymous_vars_dollar(Term).

ground_anonymous_vars_dollar(Term) :-
	free_variables(Term, VL),
	(VL = [Var]
	->	Var = '$VAR'('_')
	;	ground_anonymous_vars_dollar(VL, 1)
	).
ground_anonymous_vars_dollar([], _).
ground_anonymous_vars_dollar(['$VAR'(F)|R], N) :-
	atom_concat('_', N, F),
	N1 is N + 1,
	ground_anonymous_vars_dollar(R, N1).





ground_vars(Term, Vars) :-
	uchecklist(call, Vars),
	ground_anonymous_vars(Term).

ground_anonymous_vars(Term) :-
	free_variables(Term, VL),
	(VL = [Var]
	->	Var = '_'
	;	ground_anonymous_vars(VL, 1)
	).
ground_anonymous_vars([], _).
ground_anonymous_vars([F|R], N) :-
	atom_concat('_', N, F),
	N1 is N + 1,
	ground_anonymous_vars(R, N1).


valid_atom(This, Text, Term) :-
	valid_term(This, Text, Term),
	valid_atom0(This, Text, Term).
valid_atom0(This, Term) :-
	valid_atom0(This, _Text, Term).

valid_atom0(This, Text, Term) :-
	(var(Text)
	->	term_to_atom(Term, Text)
	;	true
	),
	(atom(Term)
	->	true
	;	multi_rec_error(This, 'Expected name, got %s', [Text]),
		fail
	).
valid_terml(This, Text, Term) :-
	valid_term(This, Text, Term),
	(is_list(Term)
	->	multi_rec_error(This, 'Name %s is list:not allowed', [Text]),
		fail
	;	true
	).

push_form_ops :-
	push_operators([
			op(150, xfx, :),
			op(700, xfy, <),
			op(700, xfy, <=),
			op(700, xfy, =<),
			op(700, xfy, >),
			op(700, xfy, >=)
			]).
atom_to_sprop(T0, Term) :-
	get(T0, value, T1),
	push_sprop_ops,
	atom_to_term_non_var(T1,T2),
	term_to_atom(T2, Term),
	(atom_to_term(T1, Term, Bindings),
		Bindings \= []
	->	warning('Output of ~w incorrect', [T1])
	;	true
	),
	pop_operators.



push_sprop_ops :-
	push_operators([
			op(1000, xfy, and),
			op(1100, xfy, or),
			op(900, fy, not)
			]).

atom_to_term_op(T1, Term, Bindings) :-
	push_form_ops,
	atom_to_term(T1, Term, Bindings),
	pop_operators.





atom_to_term_int(T1, Term, Bindings) :-
	(current_prolog_flag(allow_variable_name_as_functor, Old)
	->	(set_prolog_flag(allow_variable_name_as_functor, true)
		->	atom_to_term_op(T1, Term, Bindings),
			set_prolog_flag(allow_variable_name_as_functor, Old)
		;	atom_to_term_op(T1, Term, Bindings)
		)
	;	atom_to_term(T1, Term, Bindings)
	).

valid_term_no_bind(This, Text, Term, Bindings) :-
	get(Text, value, T1),
	valid_term_no_bind0(This, T1, Term, Bindings).

valid_term_no_bind0(This, T1, Term, Bindings) :-
	catch(atom_to_term_int(T1, Term, Bindings), E,
	      valid_term_error(This,T1,E)).

any_term(_This, Text, Term) :-
	get(Text, value, T1),
	(catch(atom_to_term_int(T1, Term, Bindings), _E,
	      fail)
	->	uchecklist(call, Bindings)
	;	warning('Name ~w not prolog syntax, interpreted as atom',
			[T1]),
		Term = T1
	).

valid_sprop_term(Msg, Text, Term) :-
	push_sprop_ops,
	(valid_term(Msg, Text, Term)
	->	pop_operators
	;	pop_operators, fail
	).

valid_term(This, Text, Term) :-
	get(Text, value, T1),
	catch(atom_to_term_int(T1, Term, Bindings), E,
	      valid_term_error(This,T1,E)),
	uchecklist(call, Bindings).
valid_term_error(This,Text, _E) :-
	multi_rec_error(This, 'Syntax error in name %s', [Text]),
	fail.

valid_term_bg(Atom, Term, Tag) :-
	catch(atom_to_term_int(Atom, Term, Bindings), E,
	      valid_term_error_bg(Atom,E,Tag)),
	uchecklist(call, Bindings).
valid_term_error_bg(Atom, _E, Tag) :-
	ferror(Tag, [Atom]),
	fail.

quantor(exists(Ranges,T), Ranges, exists, T).
quantor(forall(Ranges,T), Ranges, forall, T).

formnargformulakind(and).
formnargformulakind(or).

reserved(true).
reserved(false).
reserved(T) :-
	reserved0(T).

reserved0(not(_)).
reserved0(X) :-
	functor(X, AndOr, _),
	memberchk(AndOr, [and, or, exists, forall, implies, pxor]),
	local_trace(reserved0).

multi_rec_error(VisualInfo, Format, Args) :-
	Method =.. [send, Msg, report, error, Format|Args],
	(send(VisualInfo, instance_of, chain)
	->	chain_list(VisualInfo, Vs),
		(	member(Msg, Vs),
			call(Method),
			fail
		;	true
		)
	;	Msg = VisualInfo,
		call(Method)
	).
combine_reporteds(@default, Other, Other) :-
	!.
combine_reporteds(This, @default, This) :-
	!.

combine_reporteds(This, Other, C) :-
	chain_list(C, [This, Other]).

/*
  Op : none;op(NumPos, OPos, NumNeg, ONeg)
  RHS: Term of comparison or true
  */
range_var_condition(Term , VarName, Sort, Op, RHS) :-
	range_var_condition(Term , VarName, Sort, Op, RHS, []).

range_var_condition(VarName : Sort , VarName, Sort, none, true, _) :-
	!.

range_var_condition(Term , VarName, Sort, Op, RHS, Msg) :-
	(iscmpoploc(Term, NumPos, OPos, NumNeg, ONeg, LHS, RHS1)
	->	(	iscmpoploc(RHS1, NumPos1, OPos1, NumNeg1, ONeg1, LHS1, RHS2)
		->	LHS1 = VarName : Sort,
			!,
			valid_atom0(Msg, VarName),
			(	cmpopkind(NumPos, NumMainOp, _),
				cmpopkind(NumPos1, NumMainOp, _)
			->	true
			;	warning('Only matching comparison allowed in range'),
				fail
			),
			RHS = [LHS, RHS2],
			Op = op2(NumPos, NumPos1, OPos, OPos1, NumNeg, NumNeg1,
				 ONeg, ONeg1)
		;	RHS = RHS1,
			LHS = VarName : Sort,
			!,
			valid_atom0(Msg, VarName),
			Op = op(NumPos, OPos, NumNeg, ONeg)
		)
	;	fail
	).
range_var_condition(Term , _VarName, _Sort, _Op, _RHS, Msg) :-
	(Msg == []
	->	fatal_error('Unrecognised range element ~w', [Term])
	;	pl_pce(Term, PT),

		multi_rec_error(Msg,
				'Expected <VarName>:<SortName> [<OP> <Term>] got %s', [PT]),
		fail
	).
multi_term_condition(Term, Msg) :-
	multi_term_condition(Term, _IsBinary, Msg).

multi_term_condition(Term, IsBinary, Msg) :-
	(	iscmpoploc(Term, _NumPos, _OPos, _NumNeg, _ONeg, LHS, RHS1)
	->	(iscmpoploc(LHS, _, _, _, _, _, _)
		->	(Msg == []
			->	fatal_error('Comparison formula ~w not recognised', [Term])
			;	pl_pce(Term, PT),
				multi_rec_error(Msg,
				     'Expected <Term> <CMPOP> <Term> {<CMPOP> <Term>}* got %s', [PT]),
				fail
			)
		;	true
		),
		(	iscmpoploc(RHS1, _NumPos1, _OPos1, _NumNeg1, _ONeg1,
				   _LHS1, _RHS2)
		->	IsBinary1 = false
		;	IsBinary1 = true
		),
		(IsBinary = IsBinary1
		->	true
		;	fatal_error('Comparison formula ~w not recognised',
				    [Term])
		)
	;	Msg == []
	->	fatal_error('Comparison formula ~w not recognised', [Term])
	;	pl_pce(Term, PT),
		multi_rec_error(Msg,
		     'Expected <Term> <CMPOP> <Term> {<CMPOP> <Term>}* got %s', [PT]),
		fail
	).
pncmpcode(pos, Code1, Code1) :-
	!.

pncmpcode(neg, Code1, Code2) :-
	(Code1 = (CodeA,CodeB)
	->	pncmpcode(neg, CodeB, Code2B),
		Code2 = (CodeA, Code2B)
	;cmpop(Code1, _, Code2, _OtherCall, _NegOtherCall, _X1, _X2)
	->	true
	;memberchk(Code1, [true, false])
	->	pncode(neg, Code1, Code2)
	;	impl_error('No inverse for comparator ~w', [Code1])
	).

iscmpoploc(Term, NumPos, OPos, NumNeg, ONeg, LHS, RHS) :-
	(	cmpop(Term, _, NumNeg, OPos, ONeg, LHS, RHS)
	->	NumPos = Term
	;	cmpop(NumPos, Term, NumNeg, OPos, ONeg, LHS, RHS)
	).

/* remove equalities, require one order, only single \= allowed
 Slope: inc, dec, eq, neq
   If Slope == neq, only one comparison
   inc, dec may also contain eq sub comparisons
   */
iscmpopfresult(Term, Result, Slope, Status) :-
	iscmpopf(Term, Op1, LHS, RHS),
	iscmpopfresult1(Op1, LHS, RHS, Term, initial, Slope, Result, Status).


iscmpopfresult1(Op1, LHS, RHS, InputTerm, SlopeIn, SlopeOut, Result, Status) :-
	opkind(Op1, Slope, _Eq),
	!,
	(	compatible_slope(SlopeIn, Slope, Slope1)
	->	iscmpopfresult1(LHS, InputTerm, Slope1, Slope2, LResult, Status),
		(var(Status)
		->	iscmpopfresult1(RHS,InputTerm,Slope2,SlopeOut, RResult, Status),
			(var(Status)
			->	combine_results(LResult, Op1, RResult, Result)
			;	true
			)
		;	true
		)
	;	error('increasing and decreasing order operators may not be combined:~w',
			      [InputTerm]),
		Status = error('increasing and decreasing order operators may not be combined:~w',
			      [InputTerm])
	).
iscmpopfresult1(Term, _InputTerm, SlopeIn, SlopeIn, [Term], _Status).
combine_results(LResult, Op1, RResult, Result) :-
	append(LResult, [Op1|RResult], Result).

compatible_slope(initial, Slope, Slope) :-
	!.
compatible_slope(neq, _Slope, _) :-
	!,
	fail.
compatible_slope(_, neq, _) :-
	!,
	fail.
compatible_slope(eq, Slope, Slope) :-
	!.
compatible_slope(Slope,eq,  Slope) :-
	!.
compatible_slope(Slope, Slope, Slope) :-
	!.


iscmpopf(Term, Op, LHS, RHS) :-
	iscmpoploc(Term, NumCall, LHS, RHS),
	functor(NumCall, Op, 2).
iscmpopflist(Term, Seq) :-
	iscmpopf(Term, Op1, LHS, RHS),
	iscmpopflist1(LHS, SeqL),
	iscmpopflist1(RHS, SeqR),
	append(SeqL, [Op1|SeqR], Seq).
iscmpopflist1(Term, Seq) :-
	iscmpopflist(Term, Seq),!.
iscmpopflist1(Term, [Term]).


iscmpoploc(Term, NumCall, LHS, RHS) :-
	iscmpoploc(Term, NumCall, _OPos, _NumNeg, _ONeg, LHS, RHS).
iscmpoploc(Term, NumCall) :-
	iscmpoploc(Term, NumCall, _OPos, _NumNeg, _ONeg, _LHS, _RHS).

cmpopusr(Op) :-
	(	cmpop(Op1, _, _, _, _, _, _),
		functor(Op1, Op, _),
		Op \= (=<)
		;
		Op = '<='
	).
% allow =<
cmpopusr(Op, Op1) :-
	assert_debug(ground(Op)),
	(	functor(OpC, Op, 2),
		cmpop(OpC, _, _, _, _, _, _),
		(Op = =<
		->	Op1 = '<='
		;	Op1 = Op
		)
	;	Op = '<=',
		Op1 = Op
	).

combine_ops(Op1, Op2, Op) :-
	opkind(Op1, Slope1, Eq1),
	opkind(Op2, Slope2, Eq2),
	(memberchk(eq, [Slope1, Slope2])
	->	(Slope1 = eq
		->	Op = Op2
		;	Op = Op1
		)
	;memberchk(neq, [Slope1, Slope2])
	->	(memberchk(eq, [Slope1, Slope2])
		->	opkind(Op, neq, neq)
		;	pr_error('Conflicting range in multi comparison'),
			fail
		)
	;	Slope1 = Slope2
	->	assert_debug(memberchk(Slope1, [inc,dec])),
		(	Eq1 = eq,
			Eq2 = eq
		->	Eq = eq
		;	Eq = neq
		),
		opkind(Op, Slope1, Eq)
	;	pr_error('Conflicting range in multi comparison'),
		fail
	).

opkind1((<), (>=), inc,neq).
opkind1((>), (=<), dec, neq).
opkind1((=), (\=), eq, eq).
opkind(Op, Slope, Eq) :-
	opkind1(Op, _, Slope, Eq),
	!.
opkind(Op, Slope, Eq) :-
	opkind1(_, Op, Slope1, Eq1),
	inv_slope(Slope1, Slope),
	inv_eq(Eq1, Eq).
inv_slope(inc, dec).
inv_slope(dec, inc).
inv_slope(eq, neq).
inv_slope(neq, eq).
inv_eq(eq, neq).
inv_eq(neq, eq).

cmpop(Internal, Alternative, Inverse, StringOp, InvStringOp, LHS, RHS) :-
	cmpop(Internal, Alternative, Inverse, StringOp, InvStringOp, LHS, RHS,
	      _IncDecEqNeq, _EqNeq).

cmpop(A < B, lt(A,B),A>=B,A@<B,A@>=B,A,B).
cmpop(A>B,gt(A,B),A=<B,A@>B,A@=<B,A,B, dec, neq).
cmpop(A>=B,ge(A,B),A<B,A@>=B,A@<B,A,B, dec, eq).
cmpop(A=<B,le(A,B),A>B,A@=<B,A@>B,A,B, inc,eq).
cmpop(A =< B,'<='(A,B),A>B,A@=<B,A@>B,A,B, inc, eq).
cmpop(A=B,eq(A,B),A\=B,A=B,A\=B,A,B, eq, eq).
cmpop(A\=B,neq(A,B),A=B,A\=B,A=B,A,B, neq, neq).
cmpop(Op) :-
	cmpop(OpF,_,_,_,_,_,_),
	functor(OpF, Op, 2).

cmpopfkind(F, Main, Eq) :-
	functor(C, F, 2),
	cmpopkind(C, Main, Eq).

cmpopkind(_A < _B, <, \=).
cmpopkind(_A =< _B, <, =).
cmpopkind(_A > _B, >, \=).
cmpopkind(_A >= _B, >, =).

min_kind([], integer).
min_kind([integer|Rest], Kind) :-
	!,min_kind(Rest, Kind).
min_kind([real|_Rest], real) :-
	!.
min_kind(L, _) :-
	impl_error('Expected only integer real kinds, got ~w', [L]),
	fail.

% We expect the result to be ground at call time
invert_arop(T1S + T2S, [T1S,T2S], [initialbound, prebound], Kinds, Kind,
	    PostCode, Res) :-
	PostCode = (T1S is Res - T2S),
	min_kind(Kinds, Kind).

invert_arop(T1S + T2S, [T1S,T2S], [prebound, initialbound], Kinds, Kind,
	    PostCode, Res) :-
	PostCode = (T2S is Res - T1S),
	min_kind(Kinds, Kind).
invert_arop(T1S - T2S, [T1S,T2S], [initialbound, prebound], Kinds, Kind,
	    PostCode, Res) :-
	PostCode = (T1S is Res + T2S),
	min_kind(Kinds, Kind).
invert_arop(T1S - T2S, [T1S,T2S], [prebound, initialbound], Kinds, Kind,
	    PostCode, Res) :-
	PostCode = (T2S is T1S - Res),
	min_kind(Kinds, Kind).
invert_arop(T1S * T2S, [T1S,T2S], [initialbound, prebound], _Kinds, real,
	    PostCode, Res) :-
	PostCode =((T2S =:= 0->pr_error('division zero');true),T1S is Res/T2S).
invert_arop(T1S * T2S, [T1S,T2S], [prebound, initialbound], _Kinds, real,
	    PostCode, Res) :-
	PostCode = ((Res =:= 0->pr_error('division zero');true),
			   T2S is T1S / Res).
invert_arop(T1S / T2S, [T1S,T2S], [initialbound, prebound], _Kinds, real,
	    PostCode, Res) :-
	PostCode = (T1S is T2S*Res).
invert_arop(T1S / T2S, [T1S,T2S], [prebound, initialbound], _Kinds, real,
	    PostCode, Res) :-
	PostCode =((Res =:= 0->pr_error('division zero');true),T2S is T1S/Res).
invert_arop(-T1S, [T1S], [initialbound], [Kind], Kind, PostCode, Res) :-
	PostCode = (T1S is - Res).

aropkind(OpCall, Kinds, Kind) :-
	arop(OpCall, _, Args, _Res),
	assert_debug(\+ \+ Args = Kinds),
	functor(OpCall, F, A),
	(simple_int_arop_kind(F,A)
	->	(member(Kind1,Kinds),
			(Kind1 == real
			->	Kind = real
			;Kind1 \== integer
			->	pr_error('Kind error')
			;	fail
			)
		->	Kind == real
		;	Kind = integer
		)
	;int_always_kind(F,A)
	->	Kind = integer
	;	Kind = real
	).
simple_int_arop_kind(F, 2) :-
	memberchk(F, [+,-,*,max,min,^]),
	!.
simple_int_arop_kind(F,1) :-
	memberchk(F, [-,abs]),
	!.
int_always_kind(F,1) :-
	memberchk(F,[sign,round,integer,truncate,floor,ceiling]).


arop(T1 + T2, [T1, T2], [T1S,T2S], T1S + T2S).
arop(T1 - T2, [T1, T2], [T1S,T2S], T1S - T2S).
arop(T1 / T2, [T1, T2], [T1S,T2S], T1S / T2S).
arop(T1 * T2, [T1, T2], [T1S,T2S], T1S * T2S).
arop(T1 ** T2, [T1, T2], [T1S,T2S], T1S ** T2S).
arop(max(T1,T2), [T1, T2], [T1S,T2S], max(T1S,T2S)).
arop(min(T1,T2), [T1, T2], [T1S,T2S], min(T1S,T2S)).
arop((T1^T2), [T1, T2], [T1S,T2S], (T1S^T2S)).
arop(-T1, [T1], [T1S], -T1S).
arop(T, [Arg], [Arg1], T1) :-
	functor(T, F, 1),
	single_ar(F),
	arg(1, T, Arg),
	functor(T1, F, 1),
	arg(1, T1, Arg1),!.
single_ar(abs).
single_ar(sign).
single_ar(cos).
single_ar(tan).
single_ar(asin).
single_ar(acos).
single_ar(atan).
single_ar(log).
single_ar(log10).
single_ar(exp).
single_ar(round).
single_ar(integer).
single_ar(truncate).
single_ar(floor).
single_ar(ceiling).
single_ar(sqrt).


tfonoff(true, @on).
tfonoff(false, @off).
/*
pl_pce(In, _Vars, Out) :-
	ground(In),
	!,
	pl_pce(In, Out).
*/
print_var_names(Term, Vars) :-
	\+ \+ (free_variables(Term, V),
		      uchecklist(set_name_from_vars(Vars),V),
		      flag(portray_var_names, Old, 1),
		      print(Term),
		      flag(portray_var_names, _, Old)
	      ).
pl_pce(In, Vars, Out) :-
	free_variables(In, V),
	uchecklist(set_name_from_vars(Vars),V),
	flag(portray_var_names, Old, 1),
	sformat(S, '~p', [In]),
	string_to_atom(S, Out),
	flag(portray_var_names, _, Old).

pl_pce(Term, Term1) :-
	numbervars(Term, 0, _),
	pl_pce(Term, [], Term1).


set_name_from_vars([], V) :-
	V = '$VAR'('_').
set_name_from_vars([Name=V1|Vars], V) :-
	(V1 == V
	->	V = '$VAR'(Name)
	;	set_name_from_vars(Vars, V)
	).

test_pl :-
	(	repeat,
		read_term(T, [variable_names(V)]),
		pl_pce(T, V, W),
		send(@pce, format, 'PCE:%s\n', W),
		format('~w~n', [W]),
		fail
	).

/*
pl_pce(Term, Term) :-
	integer(Term),!.
pl_pce(Term, PTerm) :-
	number(Term),!,
	term_to_atom(Term, PTerm).
pl_pce(Term, PTerm) :-
	pl_pce0(Term, [], _, PTerm0),
	sformat(PTerm1, '~w', [PTerm0]),
	string_to_atom(PTerm1, PTerm).
*/
/*
pl_pce(In, Vars, Out) :-
	pl_pce0(In, Vars, _, Out1),
	sformat(PTerm1, '~w', [Out1]),
	string_to_atom(PTerm1, Out).
*/
% HORRIBLE:
% SWI-prolog write -> no 0.0
% writeq -> 0.0
pl_pce0(Term, In, In, Term) :-
	integer(Term),!.
pl_pce0(Term, In, In, PTerm) :-
	number(Term),!,
	term_to_atom(Term, PTerm).
pl_pce0(Term, In, In, Term) :-
	atomic(Term),!.
pl_pce0(Term, In, Out, PTerm) :-
	nonvar(Term),
	!,
	Term =.. [F|Args],
	pl_pce0l(Args, In, Out, PArgs),
	PTerm =.. [F|PArgs].
pl_pce0(Term, In, Out, Term2) :-
	var(Term),
	!,
	var_name_from_vl(In, Out, Term, Term1),
	Term2 = Term1.
var_name_from_vl([], ['_'=Term], Term, '_').
var_name_from_vl([Term1=Var|R], [Term1=Var|R], Term, Term1) :-
	Var == Term,
	!,
	assert_debug(Term1 \= '_').
var_name_from_vl([T|R], [T|R1], Term, Term1) :-
	var_name_from_vl(R, R1, Term, Term1).

pl_pce0l([], In, In, []).
pl_pce0l([T|Ts], In, Out, [T1|T1s]) :-
	pl_pce0(T, In, IO, T1),
	pl_pce0l(Ts, IO, Out, T1s).

/*
user:portray('$VAR'(Atm)) :-
	\+ number(Atm),
	%local_trace(portray),
	!,
	write(Atm).

user:portray(X) :-
	atomic(X),
	\+ number(X),
	!,
	write(X).

user:portray('$VAR'(Atm)) :-
	atom(Atm),
	flag(portray_var_names, X, X),
	X \= 0,
	!,
	write(Atm).
*/

fatal_status_check(Status) :-
	(var(Status)
	->	true
	;	fatal_error('Cannot continue')
	).



ensure_sort_kind(Sort, Kind) :-
	ensure_sort_kinde(Sort, Kind, Status),
	(var(Status)
	->	true
	;	fatal_error('Undefined sort "~w"', [Sort])
	).

ensure_sort_kinde(Sort, Kind, Status) :-
	(sort_kind(Sort, Kind)
	->	true
	;	ferrorv(undefined_sort, [Sort], Status)
	).

sort_kind(Sort, Kind) :-
	(bi_sort(Sort, Kind)
	->	true
	;spec:sortdef(Sort, _Elements)
	->	Kind = sortdef
	;memberchk(Sort, ['TIME',time, []])
	->	Kind = time
	;isIntervalSort(Sort)
	->	Kind = integer
	;	(ground(Sort)
		->	(spec:sort_element(Sort, _)
			->	true
			;	spec:sortdef(Sort, _)
			),
			!,
			Kind = unspecified
		;	(spec:sortdef(Sort, _)
			;	spec:sort_element(Sort, _),
				\+ spec:sortdef(Sort, _)
			),
			Kind = unspecified
		)
	).

isIntervalSort(interval).
isIntervalSort('TIME_INTERVAL').
set_dynamic_spec :-
	(   spec_pred(Pred),
	    dynamic(spec:Pred),
	    fail
	;   trace_pred(Pred),
	    dynamic(spec:Pred),
	    fail
	;   true
	).

spec_pred(display/2).
spec_pred(display_number_range/4).
spec_pred(constant/2).
spec_pred(start_time/1).
spec_pred(end_time/1).
spec_pred(interval/3).
spec_pred(interval/4).
spec_pred(between/4).
spec_pred(periodic/4).
spec_pred(periodic/5).
spec_pred(cwa/1).
spec_pred(global_lambda/1).
spec_pred(cwa/2).
spec_pred(leadsto/3).
spec_pred(leadsto/4).
spec_pred(sort_element/2).
spec_pred(sortdef/2).
spec_pred(specification/1).
spec_pred(content/1).
spec_pred(denotes/2).
spec_pred(at/3).
spec_pred(model/1).
spec_pred(qterm/1).

set_dynamic_load_results :-
	(trace_pred(Pred),
	    dynamic(load_results:Pred),
	    fail
	;   true
	).


trace_pred(dnr/4).
trace_pred(content/1).
trace_pred(d/2).
trace_pred(atom_trace/3).
trace_pred(times/3).
trace_pred(cwa/1).


member_limit(5).

code_ite(Cond, T, E, Code) :-
	(Cond == true
	->	Code = T
	;Cond == fail
	->	Code = E
	;	Code = (Cond->T;E)
	).

code_member(List, _ ,_) :-
	assert_debug(is_list(List)),
	fail.

code_member([], _Element, fail).

code_member([Member], Element, Code) :-
	!,(ground(Member)
	->	(Element = Member
		->	Code = true
		;	Code = fail
		)
	;	(Element = Member
		->	Code = true
		;	Code = fail
		)
	).
code_member(Members, Element,Code) :-
	Code = member(Element, Members).

expect_integer(Val, Val1) :-
	(integer(Val)
	->	Val1 = Val
	;	Val1 is Val,
		integer(Val1)
	->	true
	;	error('Expected integer, got ~w', [Val])
	).

code_between(Low, Hi, Value, Code) :-
	ground(Value),
	!,
	expect_integer(Value, Value1),
	(ground(Low)
	->	expect_integer(Low,Low1),
		(Value1 < Low1
		->	Code = fail
		;Value1 = Low1
		->	(ground(Hi)
			->	expect_integer(Hi,Hi1),
				(Hi1 >= Low1
				->	Code = true
				;	Code = fail
				)
			;	Code = (Hi >= Low1)
			)
		;	(ground(Hi)
			->	expect_integer(Hi,Hi1),
				(Hi1 >= Value1
				->	Code = true
				;	Code = fail
				)
			;	Code = (Value1 =< Hi)
			)
		)
	;	(ground(Hi)
		->	expect_integer(Hi,Hi1),
			(Value1 > Hi1
			->	Code = fail
			;Value1 = Hi1
			->	Code = (Low1 =< Hi1)
			;	Code = (Low1 =< Value1)
			)
		;	Code = between(Low, Hi, Value1)
		)
	).
code_between(Low, Hi, Value, Code) :-
	(	ground(Low),
		ground(Hi)
	->	expect_integer(Low,Low1),
		expect_integer(Hi, Hi1),
		(Hi1 < Low1
		->	Code = fail
		;	Code = between(Low1, Hi1, Value)
		)
	;	Code = between(Low, Hi, Value)
	).
pre_eval_num(Term, Res) :-
	(pre_eval_num1(Term, Res)
	->      true
	;      Res = Term
	).
pre_eval_num1(Term, Res) :-
	(number(Term)
	->     Res = Term
	;      var(Term)
	->     fail
	;      current_arithmetic_function(Term)
	->     Term =.. [F|Args],
	       maplist(pre_eval_num1, Args, Args1),
	       Term1 =.. [F|Args1],
	       Res is Term1
	;      fail
	).
test_num(Term, Res) :-
	ground(Term),
	(number(Term)
	->	Res = Term
	;	Res is Term
	).


code_between_call1(<,Low1, Hi1, CmpRes, PVar, Code) :-
	CmpRes1 is truncate(CmpRes),
	(CmpRes =:= CmpRes1
	->	CmpRes2 is CmpRes1 -1
	;	CmpRes2 = CmpRes1
	),
	Hi2 is min(CmpRes2, Hi1),
	(Hi2 < Low1
	->	Code = fail
	;	code_between(Low1, Hi2, PVar, Code)
	).
code_between_call1(=<,Low1, Hi1, CmpRes, PVar, Code) :-
	CmpRes1 is truncate(CmpRes),
	Hi2 is min(CmpRes1, Hi1),
	(Hi2 < Low1
	->	Code = fail
	;	code_between(Low1, Hi2, PVar, Code)
	).
code_between_call1(>,Low1, Hi1, CmpRes, PVar, Code) :-
	CmpRes1 is truncate(CmpRes),
	(CmpRes =:= CmpRes1
	->	CmpRes2 is CmpRes1 + 1
	;	CmpRes2 = CmpRes1
	),
	Low2 is max(CmpRes2, Low1),
	(Hi1 < Low2
	->	Code = fail
	;	code_between(Low2, Hi1, PVar, Code)
	).
code_between_call1(>=,Low1, Hi1, CmpRes, PVar, Code) :-
	CmpRes1 is truncate(CmpRes),
	Low2 is max(CmpRes1, Low1),
	(Hi1 < Low2
	->	Code = fail
	;	code_between(Low2, Hi1, PVar, Code)
	).

code_between_call(F,Low,Hi,CmpTerm1,PVar,Code) :-
	test_num(CmpTerm1, CmpRes),
	test_num(Low, Low1),
	test_num(Hi, Hi1),
	code_between_call1(F, Low1, Hi1, CmpRes, PVar, Code),
	!.
code_between_call(F,Low,Hi,CmpTerm1,PVar,Code) :-
	CmpCode =.. [F, PVar, CmpTerm1],
	code_between(Low, Hi, PVar, Code1),
	combine_and([Code1, CmpCode], Code).


code_between_call2(F1,F2,Low,Hi,CmpTerm11,CmpTerm12, PVar,Code) :-
	code_between_call2L(F1,F2,[Low],[Hi],CmpTerm11,CmpTerm12, PVar,Code).
code_between_call2I(F1,F2,CmpTerm11,CmpTerm12, PVar,Code) :-
	code_between_call2L(F1,F2,[],[],CmpTerm11,CmpTerm12, PVar,Code).
code_between_call2L(F1,F2,LowIn,HiIn,CmpTerm11,CmpTerm12, PVar,Code) :-
	add_extrema(F1, CmpTerm11, LowIn, NL1, HiIn, NH1),
	add_extrema(F2, CmpTerm12, NH1, NH2, NL1, NL2),
	test_num_min_max(NL2, max, NumL, NL),
	test_num_min_max(NH2, min, NumH, NH),
	combine_nn(NumL, NL, NLN),
	(	NLN = [LE]
	->	LB = LE,
		Code1 = true
	;	NLN = [LE1,LE2]
	->	Code1 = (LB is max(LE1,LE2))
	;	pr_error('Unexpected comparison args'),
		fail
	),
	combine_nn(NumH, NH, NHN),
	(	NHN = [HE]
	->	HB = HE,
		Code2 = true
	;	NHN = [HE1, HE2]
	->	Code2 = (HB is min(HE1, HE2))
	;	pr_error('Unexpected comparison args'),
		fail
	),
	code_between(LB, HB, PVar, BCode),
	combine_and([Code1, Code2, BCode], Code).

combine_nn(nan, NL, NL) :-
	!.
combine_nn(N, NL, [N|NL]).

test_num_min_max([],_MinMax, nan, []).

test_num_min_max([N1|NR],MinMax,NumVal, Out) :-
	assert_debug(nonvar(NR)),
	(test_num(N1, N11)
	->	test_num_min_max(NR, MinMax, N11, NumVal, Out)
	;	test_num_min_max(NR, MinMax, NumVal, Out1),
		Out = [N1|Out1]
	).
test_num_min_max([], _MinMax, N11, N11, []).

test_num_min_max([N1|NR], MinMax, N11, NumVal, Out) :-
	(test_num(N1, N12)
	->	minmax(MinMax, N12, N11,N2),
		test_num_min_max(NR, MinMax, N2, NumVal, Out)
	;	test_num_min_max(NR, MinMax, N11, NumVal, Out1),
		Out = [N1|Out1]
	).
minmax(max, A, B, R) :-
	R is max(A,B).
minmax(min, A, B, R) :-
	R is min(A,B).
add_extrema(F, Cmp, LowIn, LowOut, HiIn, HiOut) :-
	cmpopfkind(F, M, E),
	(M = <
	->	(E = =
		->	LowOut = [Cmp|LowIn]
		;	LowOut = [Cmp+1|LowIn]
		),
		HiOut = HiIn
	;M = >
	->	(E = =
		->	HiOut = [Cmp|HiIn]
		;	HiOut = [Cmp-1|HiIn]
		),
		LowOut = LowIn
	;	pr_error('Never cmpopkind'),
		fail
	).



var_code_test_inf(true, Call, _Code) :-
	\+ ground(Call),
	!,
	error('Cannot generate infinite numbers'),
	fail.
var_code_test_inf(_, Call, Code) :-
	(ground(Call)
	->	(call(Call)
		->	Code = true
		;	Code = fail
		)
	;	Code = Call
	).


last_interval(LT) :-
	(holds:last_time(LT)
	->	true
	;	error('Probably no trace loaded, no last_interval'),
		fail
	).

code_sort_element(Sort, Element, Code, MustGenerateTF) :-
	assert_debug(nonvar(Sort)),
	assert_debug(var(Element)),
	copy_term(Sort, Sort1),
	(spec:sortdef(Sort, Elements)
	->	(	spec:sortdef(Sort1, Elements1),
			c(Sort1,Elements1) \=@= c(Sort,Elements)
		->	Code = sort_element(Sort, Element)
		;	code_member(Elements, Element, Code)
		)
	;	(memberchk(Sort, [[], time, 'TIME'])
		;isIntervalSort(Sort)
		)
	->	last_interval(LT),
		code_between(0, LT, Element, Code)
	;Sort = between(Low, Hi)
	->	code_between(Low, Hi, Element, Code)
	;	is_same_sort(Sort, real)
	->	var_code_test_inf(MustGenerateTF, number(Element), Code)
	;	is_same_sort(Sort, integer)
	->	var_code_test_inf(MustGenerateTF, integer(Element), Code)
	;bi_sort(Sort, _Kind)
	->	impl_error('Unsupported builtin sort ~w', [Sort])
	;	ground(Sort)
	->	(	member_limit(ML),
			sort_size(Sort, Size),
			Size \= maxinf,
			Size =< ML
		->	sort_elements(Sort, Elements),
			code_member(Elements, Element, Code)
		;	Code = spec:sort_element(Sort, Element)
		)
	;	Code = spec:sort_element(Sort, Element)
	).
sort_elements(Sort, Elements) :-
	bagof1(SE, spec:sort_element(Sort, SE), Elements1),
	list_to_set(Elements1, Elements).

sort_element(Sort, Element) :-
	assert_debug(ground(Sort)),
	(spec:sortdef(Sort, Elements)
	->	(ground(Element)
		->	memberchk(Element, Elements)
		;	member(Element, Elements)
		)
	;memberchk(Sort, [[], time, 'TIME'])
	->	last_interval(LT),
		between(0,LT,Element)
	;isIntervalSort(Sort)
	->	last_interval(LT),
		between(0,LT,Element)
	;bi_sort(Sort, _Kind)
	->	bi_sort_elemente(Sort, Element, _)
	;	spec:sort_element(Sort, Element)
	).
check_sort_element_code(Sort, Element, Code) :-
	ground(Element),
	ground(Sort),
	var(Element),
	!,
	code_sort_element(Sort, Element, Code1, false),
	(	call(Code1)
	->	Code = true
	;	Code = fail
	).
check_sort_element_code(Sort, Element, Code) :-
	ground(Element),
	ground(Sort),
	ground(Element),
	!,
	(check_ground_sort_value(Sort, _Kind, Element)
	->	Code = true
	;	Code = fail
	).

check_sort_element_code(Sort, Element, Code) :-
	code_sort_element(Sort, Element, Code, false).



% May be check (for example find_atom_trace) or
% generate
check_sort_value(Sort, Kind, Val) :-
	ground(Val),
	!,
	check_ground_sort_value(Sort, Kind, Val).
check_sort_value(Sort, Kind, Val) :-
	\+ \+ instantiate_elemente(Sort, Kind, Val, Status),
	instantiate_elemente(Sort, Kind, Val, Status),
	(var(Status)
	->	true
	;	error('Undefined error, warn lourens'),
		!,
		fail
	).

check_ground_sort_value(Sort, Kind, Val) :-
	bi_sort(Sort, Kind),
	!,
	check_bi_sort_element(Sort, Val).
check_ground_sort_value(Sort, Kind, Val) :-
	ensure_sort_kind(Sort, Kind),
	!,sort_element(Sort, Val).

check_bi_sort_element_code(RSort, PVar, number(PVar)) :-
	is_same_sort(RSort, real).
check_bi_sort_element_code(ISort, PVar, integer(PVar)) :-
	is_same_sort(ISort, integer).
check_bi_sort_element_code(between(I1, I2), Val, (Val >= I1,
	Val =< I2)).

check_bi_sort_element(RSort, Val) :-
	is_same_sort(RSort, real),
	!,number(Val).
check_bi_sort_element(ISort, Val) :-
	is_same_sort(ISort, integer),
	!,integer(Val).
check_bi_sort_element(between(I1, I2), Val) :-
	integer(Val),
	Val >= I1,
	Val =< I2.

is_same_sort('INTEGER', integer).
is_same_sort(integer, integer).
is_same_sort('REAL', real).
is_same_sort(real, real).
is_inf_sort(Sort) :-
	is_same_sort(Sort, real),
	!.
is_inf_sort(Sort) :-
	is_same_sort(Sort, integer),
	!.

is_inf_sort(between(_,_)) :-
	!,
	fail.
is_inf_sort(Sort) :-
	flag(dyninf, Old, 0),
	(	sort_element(Sort, _),
		flag(dyninf, I, I + 1),
		I > 100
	->	impl_error(['Cannot detect infinite versus big sorts yet, ',
			    'contact lourens'])
	;       flag(dyninf, _, Old),
		fail
	).


bi_sort(Sort, real) :-
	is_same_sort(Sort, real).

bi_sort(Sort, integer) :-
	is_same_sort(Sort, integer).
bi_sort(between(_,_),integer).

bi_sort_elemente(Sort, _, _) :-
	assert_debug(ground(Sort)),
	fail.

bi_sort_elemente(Sort, _Element, Status) :-
	is_same_sort(Sort, Sort1),
	memberchk(Sort1, [integer,real]),
	!,
	ferrorv(instantiating_infinite_sort, [Sort], Status).


bi_sort_elemente(between(T1, T2), Element, Status) :-
	(integer(T1)
	->	true
	;	ferrorv(between_argument_not_int1, [between(T1, T2)],Status)
	),
	(integer(T2)
	->	true
	;	ferrorv(between_argument_not_int2, [between(T1, T2)],Status)
	),
	(var(Status)
	->	between(T1, T2, Element)
	;	true
	).


denotes(Header, Formula) :-
	spec:denotes(Header, Formula).


unifyHeader([], [], _VLIn, _DenotesVL).
unifyHeader([Arg|Args], [DArg|DArgs], VLIn, DenotesVL) :-
	unifyArg(Arg, DArg, VLIn, DenotesVL),
	unifyHeader(Args, DArgs, VLIn, DenotesVL).

act_form(Act, Form) :-
	(dyn_unify_info(actform(Act, Form))
	->	true
	;	Act = 'Unidentified actual property call',
		Form = 'Unidentified formal property header'
	).

% Arg:actual arg DArg:denotes arg
unifyArg(Arg, DArg, VLIn, DVL) :-
	Env = 'Trying to identify property:~nactual call  :~w, ~nformal header:~w~n',
	(var(DArg)
	->	plvarVarFromVarList(DVL,DArg,DVar,DSort),
		DArg = Arg,
		(some_var_termS(Arg,VLIn,_,Sort, Status)
		->	var(Status),
			(Sort == DSort
			->	true
			;	act_form(Act, Form),
				warning([Env,
					 'formal variable of sort "~w" and actual variable "~w" argument~n of different sort not allowed(yet)'],[Act, Form, Sort, DVar:DSort]),
				fail
			)
		;	isGround(Arg,VLIn,Status)
		->	var(Status),
			transTerm(Arg, VLIn, PreCode, TermOut),
			(	call(PreCode),
				check_ground_sort_value(DSort, _Kind, TermOut)
			->	true
			;	act_form(Act, Form),
				warning([Env,
					 'Actual argument ~w not in formal argument range ~w, IGNORING'],[Act, Form, Arg, DVar:DSort])
			)
		;	isIntervalSort(DSort),
			Arg = interval(_)
		->	true
		;	act_form(Act, Form),
			warning([Env,'denotes argument sorts not checked:~w -> ~w'],
				[Act, Form,Arg, DVar:DSort])
		)
	;ground(DArg)
	->	(	atomic(DArg),
			atomic(Arg)
		->	(some_var_termS(Arg,VLIn,_,Sort, Status),
				var(Status)
			->	act_form(Act, Form),
				error([Env, 'property formal argument ~w must be at least as general as actual argument ~w'],
				      [Act, Form, DArg, Arg:Sort]),
				fail
			;	Arg = DArg
			)
		;	act_form(Act, Form),
			error([Env,'Only simple nonvariable comparison allowed in denotes'], [Act, Form]),
			fail
		)
	;	act_form(Act, Form),
		error([Env,'expected ground or var formal arguments'],[Act, Form]),
		fail
	).



:- dynamic dyn_unify_info/1.

trans_denotes_new(Formula, VLIn, Formula1) :-
	functor(Formula, F, A),
	functor(TermD, F, A),
	flag(den, Old, 0),
	flag(dyntdn, PP, PP + 1),
	(	denotes(TermD, Body),
		flag(den, N, N + 1),
		normaliseDenotes(TermD, Body, DNVL, DArgs, Formula1),
		Formula =.. [F|Args],
		asserta(dyn_unify_info(actform(Formula, TermD))),
		(unifyHeader(Args, DArgs, VLIn, DNVL)
		->	retract(dyn_unify_info(actform(Formula, TermD)))
		;	retract(dyn_unify_info(actform(Formula, TermD))),
			fail
		)
	->	flag(den, _, Old)
	;	flag(den, M, M),
		M > 0,
		numbervars(Formula, 23,_),
		(M = 1
		->	error('property definition ~w does not match ~w',
			   [F/A, Formula])
		;	error('~w denotes ~w definitions, none match for ~w',
			   [M, F/A, Formula])
		),
		fail
	).

/*
normalise_denotes_new(TermD, Body, DArgsR, Formula1) :-
	TermD =.. [_|DArgs],
	normDArgs(DArgs, [], DArgsR, VL),
	rmSatVars(VL, Body, Formula1).
*/


rm_real_args(Term, Term1, VL, RL) :-
	rm_real_args(Term, Term1, [], VL, [], RL).
rm_real_args(Var, Var, VLIn, VLIn, RLIn, RLIn) :-
	var(Var),
	!.
rm_real_args(Term, Var, VLIn, [Var|VLIn], RLIn, [Term|RLIn]) :-
	float(Term),
	!.
rm_real_args(Term, Term, VLIn, VLIn, RLIn, RLIn) :-
	atomic(Term).
rm_real_args(Term, VarTerm, VLIn, VLOut, RLIn, RLOut) :-
	Term =.. [F|Args],
	rm_real_args_list(Args, VarTermArgs, VLIn, VLOut, RLIn, RLOut),
	VarTerm =.. [F|VarTermArgs].
rm_real_args_list([], [], VLIn, VLIn, RLIn, RLIn).
rm_real_args_list([Term|Args], [VarTerm|VarTermArgs], VLIn,VLOut,RLIn,RLOut) :-
	rm_real_args(Term, VarTerm, VLIn, VL1, RLIn, RL1),
	rm_real_args_list(Args, VarTermArgs, VL1,VLOut,RL1,RLOut).
atom_key(Atom, Key) :-
	(ground(Atom)
	->	term_to_atom(Atom, Key1),
		Key = Key1
	;	impl_error('atom_key non ground Atom')
	).


maxsortsize(10000).

:- dynamic dyn_sort_info/2.

sort_info(Sort, Info) :-
	assert_debug(ground(Sort)),
	(bi_sort_info(Sort, Info)
	->	true
	;	dyn_sort_info(Sort, Info)
	->	true
	;	userdefined_sort_info(Sort, Info),
		assertz(dyn_sort_info(Sort, Info))
	),
	!.
bi_sort_info(Sort, si(Sort, Kind, maxinf)) :-
	bi_sort(Sort, Kind),
	is_same_sort(Sort, Sort1),
	member(Sort1, [integer,real]).
bi_sort_info(Sort, si(Sort, Kind, Size)) :-
	Sort = between(I1, I2),
	bi_sort(Sort, Kind),
	Size is I2 - I1 + 1.
userdefined_sort_info(Sort, Info) :-
	ensure_sort_kind(Sort, Kind),
	maxsortsize(MS),
	flag(dfs, P, 0),
	(	sort_element(Sort, _Value),
		flag(dfs, I, I + 1),
		I =< MS,
		fail
	->	flag(dfs, _, P),
		warning('Sort ~w contains >= ~w elements and is considered infinite',
			[Sort, MS]),
		Info = si(Sort, Kind, maxinf)
	;	flag(dfs, Size, P),
		Info = si(Sort, Kind, Size)
	).




is_infinite_sort(Sort) :-
	sort_info(Sort, Info),
	Info = si(Sort, _Kind, Size),
	!,
	Size = maxinf.

sort_size(Sort, N) :-
	sort_info(Sort, Info),
	Info = si(Sort, _Kind, N).


sync_display :-
	flag(sync_display, I, I + 1),
	(I >= 0
	->	send(@display, synchronise),
		flag(sync_display, _, 0)
	;	true
	).


trfloat(I,O) :-
	(integer(I)
	->	O is float(I)
	;	O = I
	).
rmmins(In, Out) :-
	maplist(rmmin, In, Out).

rmmin(_-Data, Data).


instantiate_elemente(Sort, Kind, Element, Status) :-
	bi_sort(Sort, Kind),
	!,
	bi_sort_elemente(Sort, Element, Status).
instantiate_elemente(Sort, Kind, Element, Status) :-
	ensure_sort_kinde(Sort, Kind, Status),
	!,
	sort_element(Sort, Element).
instantiate_elemente(Sort, _Kind, _, _) :-
	impl_error('Uncaught error:Sort ~w not defined', [Sort]).
