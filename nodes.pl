:- module(nodes, [
		  setup_formula_types/2,
		  get_domain/1,
		  checking_enabled/1,
		  node_prop_options/2,
		  test_probability/2
		 ]).

:- use_module(util).
:- use_module(parteditors).
:- use_module(externaleditor).

:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(formframe).
:- use_module(formload).


:- use_module(psprinting).
allow_disj :-
	get_domain(checker),!.

allow_disj :-
	get_option(allform).

allow_pxor :-
	get_domain(leadsto),
	get_option(pxor).



%    DOMAIN:
%    checker or leadsto
%
:- dynamic dyn_domain/1.
:- discontiguous default_prop/2.


ensure_domain(Domain) :-
	(	dyn_domain(Domain1)
	->	(Domain1 == Domain
		->	true
		;	impl_error('Can handle but one domain')
		)
	;	set_domain(Domain)
	).

set_domain(Domain) :-
	(dyn_domain(_Domain1)
	->	impl_error('Loaded domain already')
	;	assertz(dyn_domain(Domain))
	).

get_domain(Domain) :-
	(dyn_domain(Domain1)
	->	Domain = Domain1
	;	impl_error('No domain defined')
	).

:- dynamic checking_enabled/0.

checking_enabled(TF) :-
	(checking_enabled
	->	TF = true
	;	TF = false
	).
toplevel_element(LC, comment, comment_node) :-
	memberchk(LC, [checker,leadsto]),
	enable_comment.

toplevel_element(leadsto, leadsto, leadsto_node).
toplevel_element(leadsto, interval, interval_node).
toplevel_element(leadsto, start_time, start_time_node).
toplevel_element(leadsto, end_time, end_time_node).
toplevel_element(leadsto, global_lambda, global_lambda_node).
toplevel_element(leadsto, model, model_node) :-
	is_local.

toplevel_element(leadsto, cwa, cwa_node).
toplevel_element(leadsto, sort, sort_node).
toplevel_element(leadsto, other, other_node).

toplevel_element(leadsto, constant, constant_def_node).
toplevel_element(checker, constant, constant_def_node).
toplevel_element(checker, sort, sort_node).
toplevel_element(checker, other, other_node).
toplevel_element(checker, property_definition, property_def_node).

setup_formula_types(Domain, CheckerEnabledTF) :-
	assert_debug(memberchk(CheckerEnabledTF, [true,false])),
	assert_debug((CheckerEnabledTF == false;Domain==checker)),
	ensure_domain(Domain),
%	set_prolog_flag(iso, true),
	assert_debug(\+ checking_enabled),
	(CheckerEnabledTF == true
	->	assertz(checking_enabled)
	;	true
	),
	config_tree_frame,
	(Domain == checker,
		CheckerEnabledTF == true
	->	assert_debug(current_module(ttlchecker)),
		ttlchecker:config_checker
	;	true
	),
	(get(@types, member,formula_kind, _)
	->	true
	;	setof(N, formula_object(N), Ns),
		chain_list(L, Ns),
		new(_, type(formula_kind, name_of, L))
	),
	(get(@types, member,formula_class, _)
	->	true
	;	setof(K, formula_class(K), Ks),
		chain_list(L2, Ks),
		new(_, type(formula_class, name_of, L2))
	),
	(get(@types, member,toplevel_kind, _)
	->	true
	;	setof(Le, Nm^toplevel_element(Domain, Nm, Le), Ls),
		chain_list(L3, Ls),
		new(_, type(toplevel_kind, name_of, L3))
	).


expand_minimum_sons(Sons, Sons1) :-
	(expand_minimum_sons1(Sons, Sons1)
	->	true
	;	fatal_fail(expand_minimum_sons(Sons, Sons1))
	).

expand_minimum_sons1([], []).

expand_minimum_sons1([F|RSons], Sons1) :-
	!,
	expand_minimum_son1(F, L1),
	expand_minimum_sons1(RSons, Sons2),
	append(L1, Sons2, Sons1).
expand_minimum_sons1(L, L1) :-
	expand_minimum_son1(L, L1).
expand_minimum_son1(min(N, Kind), L) :-
	!,
	assert_debug(is_proto_kind(Kind)),
	length(L, N),
	uchecklist(=(Kind), L).
expand_minimum_son1(Kind, [Kind]) :-
	assert_debug(is_proto_kind(Kind)).

is_proto_kind(var).
is_proto_kind(formula).
is_proto_kind(pxor_formula).

%formula_data(var, var, []).
formula_data(true,value,[]) :-
	allow_disj.
formula_data(false,value,[]):-
	allow_disj.
formula_data(and,connector, min(2, formula)).
formula_data(or, connector, min(2, formula)) :-
	allow_disj.
formula_data(pxor, connector, min(1, pxor_formula)) :-
        allow_pxor.

formula_data(not, connector, [formula]).
formula_data(implies, connector, [formula, formula]) :-
	allow_disj.


formula_data(forall, quantifier, [min(1,var),formula]).
formula_data(exists, quantifier, [min(1,var),formula]) :-
	allow_disj.

formula_data(atom, atom, []) :-
	get_domain(leadsto).
formula_data(itef, itef, []) :-
	get_domain(checker).
formula_data(condition, condition, []) :-
	true.%allow_disj.
formula_data(property, property, []) :-
	get_domain(checker).
formula_data(holds, holds, []) :-
	get_domain(checker).
formula_data('|=', '|=', []) :-
	get_domain(checker).
formula_data(external, external, []) :-
	is_local.

formula_data(formula, generic, []).
formula_name_kind(Name, Kind) :-
	formula_data(Name, Kind, _).

%	is_kind(Kind, formula).

formula_class(FClass) :-
	formula_name_kind(_Kind, FClass).
formula_kind_sons(Kind, Sons) :-
	formula_data(Kind, _Class, Sons).


formula_object(Kind) :-
	formula_data(Kind, _, _).

formula_name(formula, '[formula]') :-
	!.
formula_name(X, X).

is_kind(var, var) :-
	!.

is_kind(Kind, formula) :-
	member(Kind, [atom,property,holds,'|=',condition,formula,true, false, and, or, exists, forall, not, implies, external, itef]).
is_kind(pxor, formula) :-
        allow_pxor.

remove_without_protest0(formula).
remove_without_protest1(var).
remove_without_protest1(true).
remove_without_protest1(false).



%  Given ContainingSonKind, does Kind support sons such that
%  ContainingSonKind is contained in it?

formula_minimum_containing(Kind, ContainingSonKind, PrefixSons, PostfixSons) :-
	formula_kind_sons(Kind, Sons),
	assert_debug(is_proto_kind(ContainingSonKind)),
	expand_minimum_sons(Sons, Sons1),
	list_pre_postfix(Sons1, ContainingSonKind, PrefixSons,
				 PostfixSons),!.
% CUT: only take first choice


:- pce_begin_class(delay_node, editable_text_node).

tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, tr_pl_term_nil, delay, [delay], Test, Term).

variable(delay, name*, both).



initialise(This) :->
	send(This, send_super, initialise,@delay_editor,
	     @delay_node_recogniser),
	send(This, update_name).


copy_local(This, New:delay_node) :<-
        new(New, delay_node),
	(	member(M, [delay]),
		send(New, M, This?M),
		fail
	;	send(New, update_name)
	).
/*
is_default(This) :->
	get(This, var_name, @nil),
	get(This, sort_name, @nil).
*/
reset1(This, Test:[bool]) :->
	(get(This, delay, @nil)
	->	fail
	;	Test == @on
	->	true
	;	send(This, delay, @nil),
		send(This, modified, reset1(This))
	).
default_prop(delay, 'efgh([time],[time],[time],[time])').






prefix(This, Prefix:char_array) :<-
        (get(This?ifparent, collapsed, @on)
	->	Prefix = ''
	;	Prefix = 'EFGH: '
	).
base_name(This, Name:char_array) :<-
        new(Name, string('%s%s', This?prefix, ?(This, prop_nil_def, delay))).




cut(This, Test:[bool]) :->
	(	Test == @on
	->	true
	;	get(This, delay_cut_tree, CT),
		get(This, copy_tree_to, CT, _),
		send(This, modified, cut(This)),
		send(This, delete_tree)
	).

rename_reset(reset, reset1) :-
	!.
rename_reset(X, X).

:- pce_global(@delay_node_recogniser, mk_delay_node_recogniser).
mk_delay_node_recogniser(R) :-
	new(P, popup),
	(	member(Act, [edit, cut, reset]),
		rename_reset(Act, Act1),
		send(P, append, menu_item(Act,
					   message(@arg1, Act1),
					   @default, @default,
					   message(@arg1, Act1, @on))),
		fail
	;	true %send(P?members?tail, end_group, @on)
	),
        adapt_menu_items(P),
	new(R, popup_gesture(P)).

:- pce_end_class.


:- pce_begin_class(var_node, editable_text_node).

variable(var_name, name*, both).
variable(sort_name, name*, both).
variable(whole_name, name*, both).

tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, whole_name, HN),
	(HN == @nil
	->	get(This, tr_pl_atom_nil, var_name, [var], Test, VN),
		get(This, tr_pl_term_nil, sort_name, [sort], Test, SN),
		Term = VN:SN
	;	assert_debug(get(This, var_name, @nil)),
		assert_debug(get(This, sort_name, @nil)),
		get(This, tr_pl_term_nil, whole_name, [wn], Term)
	).





fill_term(This, Term:prolog) :->
	range_var_condition(Term, VarName, Sort, Op, RHS),
	(       Op == none,
		RHS == true
	->	(VarName == [var]
		->	true
		;	pl_pce(VarName, VarName1),
			send(This, change_var_name, VarName1)
		),
		(Sort == [sort]
		->	true
		;	pl_pce(Sort, Sort1),
			send(This, change_sort_name, Sort1)
		)
	;	pl_pce(Term, WN),
		send(This, change_whole_term, WN)
	).






initialise(This, Ed:any, Rec:[recogniser]) :->
	default(Ed, @var_editor, Ed1),
	default(Rec, @var_node_recogniser, Rec1),
	send(This, send_super, initialise, Ed1, Rec1),
	send(This, update_name).

get_sheet_vals([], _Sheet, []).

get_sheet_vals([AttrName|AttrNames], Sheet, [Val|Vals]) :-
	(get(Sheet, value, AttrName, Val)
	->	true
	;	send(@pce, report, error,
		     'Implementation Error: missing attribute %s',AttrName),
		send(@pce, format, 'Implementation Error: missing attribute %s',AttrName),
		local_trace(aap),
		Val = error
	),
	get_sheet_vals(AttrNames, Sheet, Vals).

apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([variable_name, sort_name, whole_term],
		       Sheet,
		       [VarName, SortName, WholeTerm]),
	get(This, sort_name, SortName1),
	get(This, var_name, VarName1),
	get(This, whole_name, WholeTerm1),
	([VarName, SortName, WholeTerm] = [VarName1, SortName1, WholeTerm1]
	->	true
	;       (Acted == @default,
		WholeTerm == ''
		;Acted \= whole_term, Acted \= @default
		)
	->	(VarName == VarName1
		->	send(This, change_sort_name, SortName)
		;SortName == SortName1
		->	send(This, change_var_name, VarName, @on, ReportVisual)
		;	send(This, change_sort_name, SortName),
			send(This, change_var_name, VarName, @on, ReportVisual)
		)
	;	send(This, change_whole_term, WholeTerm, ReportVisual)
	).


change_var_name(This, Name:char_array,DoUpdate:[bool],ReportVisual:[visual]) :->
	get(This, var_name, Name1),
	combine_reporteds(This, ReportVisual, Msg),
	(	Name == ''
	->	(Name1 == @nil
		->	(DoUpdate \= @off
			->	send(@var_editor, update1, This)
			;	true
			)
		;	send(This, var_name, @nil),
			send(This, modified, change_var_name(This, Name)),
			send(This, update_name),
		        (DoUpdate \= @off
			->	send(@var_editor, update1, This)
			;	true
			)
		)
	;Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	valid_term(Msg, Name, Term)
	->	(Term = [var]
		->	send(This, change_var_name, '', DoUpdate, ReportVisual)
		;atom(Term)
		->	send(This, var_name, Name),
			send(This, modified, change_var_name(This, Name)),
			send(This, whole_name, @nil),
			(	DoUpdate \= @off
			->	send(This, update_name),
				send(@var_editor, update1, This)
			;	true
			)
		;	multi_rec_error(Msg, 'Expected name, got %s', [Name])
		)
	;	(DoUpdate \= @off
		->	send(@var_editor, update1, This)
		;	true
		),
		fail
	).
change_sort_name(This, Name:char_array, DoUpdate:[bool]) :->
	get(This, sort_name, Name1),
	(Name == ''
	->	(Name1 == @nil
		->	true
		;	send(This, sort_name, @nil),
			send(This, sort_update, DoUpdate)
		)
	;	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, sort_name, Name),
		send(This, sort_update, DoUpdate)
	).

sort_update(This, DoUpdate:[bool]) :->
	get(This, sort_name, Name),
	send(This, modified, change_sort_name(This, Name)),
	send(This, whole_name, @nil),
	(	DoUpdate \= @off
	->	send(This, update_name),
		send(@var_editor, update1, This)
	;	true
	).

change_whole_term(This, Name:char_array, ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	get(This, whole_name, Name1),
	(Name == ''
	->	(Name1 == @nil
		->	true
		;	send(This, whole_name, @nil),
			send(This, modified, change_whole_name(This, Name))
		)
	;	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	valid_terml(Msg, Name, Term),
		(range_var_condition(Term , VarName, Sort, Op, _RHS, Msg)
		->	(Op == none
			->	pl_pce(VarName, V1),
				pl_pce(Sort, S1),
				send(This, change_var_name, V1),
				send(This, change_sort_name, S1)
			;	send(This, whole_name, Name),
				send(This, sort_name, @nil),
				send(This, var_name, @nil),
				send(This, update_name),
				send(@var_editor, update1, This),
				send(This, modified, change_whole_name(This, Name))
			)
		;	send(@var_editor, update1, This),
			fail
		)
	).
whole_name1(This, Res:char_array) :<-
        get(This, whole_name, Res1),
	(Res1 == @nil
	->	Res = ''
	;	Res = Res1
	).




copy_local(This, New:var_node) :<-
        new(New, var_node),
	(	member(M, [var_name, sort_name, whole_name]),
		send(New, M, This?M),
		fail
	;	send(New, update_name)
	).

is_default(This) :->
	get(This, var_name, @nil),
	get(This, sort_name, @nil).

reset1(This, Test:[bool]) :->
	(Test == @on
	->	\+ send(This, is_default)
	;	send(This, var_name, @nil),
		send(This, sort_name, @nil),
		send(This, modified, reset1(This)),
		send(This, update_name)
	).


default_prop(var_name, '[var]').

/* Is non default behavior ! */
var_name1(This, Res:char_array) :<-
        get(This, var_name, Res1),
	(Res1 == @nil
	->	(get(This, whole_name, @nil)
		->	default_prop(var_name,Res)
		;	Res = ''
		)
	;	Res = Res1
	).
/* Is non default behavior ! */
sort_name1(This, Res:char_array) :<-
        get(This, sort_name, Res1),
	(Res1 == @nil
	->	(get(This, whole_name, @nil)
		->	default_prop(sort_name, Res)
		;	Res = ''
		)
	;	Res = Res1
	).
base_name(This, Name:char_array) :<-
        (get(This?ifparent, collapsed, @on)
	->	Prefix = ''
	;get_domain(checker)
	->	Prefix = ''
	;	Prefix = 'V: '
	),
	(	get(This, whole_name, WN),
		WN \= @nil
	->	new(Name, string('%s%s',Prefix,WN))
        ;	new(Name, string('%s%s : %s',Prefix,
				 ?(This,prop_nil_def, var_name),
				 ?(This,prop_nil_def, sort_name)))
	).

unimplemented(This, Act:char_array) :->
	send(This, report, error, 'Action %s unimplemented', Act),
	fail.


insert_below_above(This, Below:bool, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, parent, Parent),
		send(Parent, son, new(VN, var_node)),
		send(This, modified, insert_below_above(This, Below, VN)),
		(Below == @on
		->	send(VN, move_after, This)
		;	(get(This, preceding_son, Before)
			->	send(VN, move_after, Before)
			;	send(VN, move_after, @nil)
			)
		)
	).
insert_var_below(This, Test:[bool]) :->
	send(This, insert_below_above, @on, Test).

insert_var_above(This, Test:[bool]) :->
	send(This, insert_below_above, @off, Test).
cut(This, Test:[bool]) :->
	get(This, parent, Parent),
	get(Parent?sons, size, N),
	(	N < 3,
		send(Parent, instance_of, formula_simple_node)
	->	assert_debug(N = 2),
		send(Parent, take_out, Test)
	;	(	Test == @on
		->	true
		;	get(This, var_cut_tree, CT),
			send(This, modified, cut(This)),
			get(This, copy_tree_to, CT, _),
			send(This, delete_tree)
		)
	).


:- pce_end_class.



:- pce_begin_class(formula_node, text_node).
/*
modified_formula(This, Op:name, Res:formula_node) :<-
        get(This, tr_term, Term),
	get(This, copy_tree, Res),
	get(Res, tr_term, Term1),
	(Term == Term1
	->	true
	;	impl_error('Copy mismatch in op ~w :~w  <-> ~w', [Op,Term, Term1]),
		fail
	).
  */
modified_formula(This, Op:name, Res:formula_node) :<-
        get(This, copy_local, Res),
	send(This?sons, for_all, if(message(@arg1, instance_of, formula_node),
				    message(Res, son, ?(@arg1, modified_formula,Op)),
				    message(Res, son, @arg1?copy_tree))),
	get(This, tr_term, Term),
	get(Res, tr_term, Term1),
	(Term == Term1
	->	true
	;	warning('Modification in op ~w :~w  <-> ~w', [Op,Term, Term1])
	).

ltrole_prefix(antecedent, 'A: ').
ltrole_prefix(consequent, 'C: ').

in_ante_conse(This, AC:name) :<-
        (allow_pxor
        ->      true
	;      local_trace(pxor_why)
        ),
        (	get(This, ifparent, P)
	->	(send(P, instance_of, formula_node)
		->	get(P, in_ante_conse, AC)
		;send(P, instance_of, leadsto_node)
		->       get(P, formula_role, This, AC)
		;	AC = none
		)
	;	AC = none
	).

prefix(This, Prefix) :<-
        (	get(This, ifparent, P),
		\+ send(P, instance_of, formula_node),
		\+ get(P, collapsed, @on),
		\+ get_domain(checker)
	->	(send(P, instance_of, leadsto_node)
		->	(	get(P, formula_role, This, AC)
			->	ltrole_prefix(AC, Prefix)
			;	Prefix = 'F '
			)
		;send(P, instance_of, interval_node)
		->	Prefix = 'F '
		;send(P, instance_of, holds_node)
		->	Prefix = 'F '
		;	Prefix = 'F '
		)
	;	Prefix = ''
	).

may_change_kind(This, NewKind:formula_kind) :->
	get(This, kind, Kind),
	(Kind == holds
	->	format('holdsn~n')
	;	true
	),
	Kind \= NewKind,
	NewKind \= var,
	(send(This, compatible_kind, NewKind)
	->	true
	;	send(This, remove_without_protest, 1)
	).


compatible_kind(This, NewKind:formula_kind) :->
	get(This, kind, Kind),
	send(This, update_kind_sons, Kind, NewKind, @on).



has_equivalent_sons(Kind, NewKind, Res) :-
	formula_kind_sons(Kind, Sons),
	formula_kind_sons(NewKind, NewSons),
	equivalent_sons(Sons, NewSons, Res).

update_kind_sons(This, CurrKind:formula_kind,
		 NewKind:formula_kind,Test:[bool]) :->
	formula_kind_sons(CurrKind, Sons),
	formula_kind_sons(NewKind, NewSons),
	(NewSons == Sons
	->	true
	;CurrKind == formula
	->	(Test == @on
		->	true
		;	expand_minimum_sons(NewSons, Sons1),
			(	member(Son, Sons1),
				send(This, son,
				     ?(This,create_proto,Son)),
				fail
			;	true
			)
		)
	;CurrKind == implies,
		memberchk(NewKind, [and, or])
	->	true
	;	memberchk(CurrKind, [and, or]),
		NewKind == implies,
		get(This?sons, size, 2)
	->	true
	;	Test == @on
	->	fail
	;	send(This, report, warning,
		     'Cannot convert, make children compatible %s -> %s',
		     CurrKind, NewKind),
		fail
	).




correct_sons(ok, _OldCount, _This, _Test) :-
	!.
correct_sons(check_n_sons(N1), OldCount, This, Test) :-
	!,
	(	OldCount =:= N1
	->	true
	;	Test == @on
	->	fail
	;	send(This, report, warning,
		     'Cannot convert, make children compatible(2)'),
		fail
	).
correct_sons(add_node(Kind1, Count), _OldCount, This, Test) :-
	!,
	(Test == @on
	->	true
	;	send(This, add_n_trailing, Kind1, Count)
	).
correct_sons(Res, _, _This, _Test) :-
	impl_error('Unexpected kind 2  ~w',[Res]).

add_n_trailing(This, Kind:formula_kind, N:int) :->
	forall(between(1, N,I),
	       (
		       send(This, son, new(FSN, formula_simple_node(Kind))),
		       send(This, modified, add_n_trailing(This, Kind, I, FSN))
	       )
	      ).

equivalent_sons(OldSons, NewSons, Kind) :-
	(equivalent_sons1(OldSons, NewSons, Kind)
	->	true
	;	OldSons = [NewSon|NewSons],
		equivalent_sons1(OldSons, NewSons, Kind1),
		Kind = add_first(NewSon, Kind1)
	;	OldSons = [OldSon|OldSons1],
		equivalent_sons1(OldSons1, NewSons, Kind1),
		Kind = rm_first(OldSon, Kind1)
	).


equivalent_sons1(Sons, Sons, ok) :-
	!.
equivalent_sons1(OldSons, min(N, Kind), Res) :-
	is_list(OldSons),
	forall(member(A, OldSons), A = Kind),
	length(OldSons, N1),
	(N1 >= N
	->	Res = ok
	;	Add is N - N1,
		Res = add_node(Kind, Add)
	),
	!.
equivalent_sons1(min(N, Kind), NewSons, check_n_sons(N1)) :-
	is_list(NewSons),
	length(NewSons, N1),
	N1 >= N,
	forall(member(A, NewSons), A = Kind),
	!.
paste(This, Test:[bool]) :->
	get(This, formula_cut_tree, CT),
	get(CT, root, RT),
	(RT == @nil
	->	(Test == @on
		->	fail
		;	send(This, report, error,
			     'Cannot paste, paste buffer empty'),
			fail
		)
	;	get(This?sons, size, N),
		(N > 0
		->	(Test == @on
			->	fail
			;	send(This, report, error,
				     'Cannot paste, drop node has children'),
				fail
			)
		;	send(This, remove_without_protest, 1)
		->	(Test == @on
			->	true
			;	get(RT, copy_tree, RT1),
				send(This?parent, son, RT1),
				send(RT1, move_after, This),
				send(RT1, update_name),
				send(This, modified, paste(This, RT1)),
				send(This, delete_tree)
			)
		;	Test == @on
		->	fail
		;	send(This, report, error,
				     'Cannot paste, first simplify node'),
			fail
		)
	).


special_formula_data(condition, condition_node).
special_formula_data(atom, atom_node).
special_formula_data(holds, holds_node).
special_formula_data('|=', bareq_node).

special_formula_data(external, external_node).
special_formula_data(property, property_node).
special_formula_data(pxor, pxor_node).
special_formula_data(itef, itef_node).

toplevel_formula(external).

toplevel_change_allowed(This, NewKind:formula_kind, Test:[bool]) :->
	(NewKind == external
	->	get(This, parent, P),
		(	send(P, instance_of, property_def_node)
		->	true
		;	(Test == @on
			->	fail
			;	send(This, report,
				     error,
				     'sub formula cannot have external definition'),
				fail
			)
		)
	;	true
	).

change(This, NewKind:formula_kind, Test:[bool]) :->
	get(This, kind, Kind),
	(	get_domain(leadsto),
		\+ allow_disj,
		get(This, ifparent, Parent),
		send(Parent, instance_of, formula_node),
		get(Parent, kind, not)
	->	(memberchk(NewKind,[atom,formula,condition])
		->	true
		;	(Test == @on
			->	fail
			;	send(This, report,
				     error, 'not may only have atom child'),
				fail
			)
		)
	;	true
	),
	(NewKind == pxor
	->	get(This, in_ante_conse, ACN),
		(ACN == consequent
		->	true
		;	Test \== @on,
			send(This, report, error, 'pxor only allowed in consequent of leadsto rule'),
			fail
		)
	;	true
	),
	(	NewKind == Kind
	->	(Test == @on
		->	fail
		;	send(This, report,
			     warning, 'ignoring %O -> %O', Kind,NewKind),
			fail
		)
	;	NewKind == pxor,
		(	memberchk(Kind, [and, or])
		->	(Test == @on
			->	true
			;	send(This, and_or_to_pxor)
			)
		;	fail
		)
	;	Kind == pxor,
		(	memberchk(NewKind, [and, or])
		->	(Test == @on
			->	true
			;	send(This, pxor_to_and_or, NewKind)
			)
		;	fail
		)
	;	(Kind == holds, NewKind == '|=',
		get(This, tr_bareq, Res)
		;Kind == '|=', NewKind == holds,
		get(This, tr_holds, Res)
		)
	->	(Test == @on
		->	true
		;	send(This?parent, son,Res),
			send(Res, move_after, This),
			send(This, modified, change(This, NewKind, Res)),
			send(This, delete_tree),
			send(Res, update_name)
		)
	;	special_formula_data(NewKind, NewNode)
	->	(send(This, remove_without_protest, 1)
		->	send(This, toplevel_change_allowed, NewKind, Test),
			(	Test == @on
			->	true
			;	send(This?parent, son, new(A, NewNode)),
				send(A, move_after, This),
				send(This, modified, change(This, NewKind, A)),
				send(This, delete_tree)
			)
		;	(Test == @on
			->	fail
			;	send(This, report, error,
				     'Will not change, complex node contents would be lost'),
				fail
			)
		)

	;	special_formula_data(Kind, _)
	->	(Test == @on
		->	true
		;	send(This?parent, son, new(A, formula_simple_node)),
			send(A, move_after, This),
			send(This, modified, change(This, NewKind, A)),
			send(This, delete_tree),
			send(A, update_kind_sons, formula, NewKind),
			send(A, kind, NewKind),
			send(A, update_name)
		)
	;	send(This, update_kind_sons, Kind, NewKind, Test),
		(	Test == @on
		->	true
		;	send(This, kind, NewKind),
			send(This, modified, change(This, NewKind, A)),
			send(This, update_name)
		)
	).

pxor_to_and_or(This, NewKind:name) :->
        send(This?parent, son, new(A, formula_simple_node(NewKind, @off))),
	send(A, move_after, This),
	chain_list(This?sons, Sons),
	psons_to_fsnode(Sons, A),
	send(This, modified, change(This, NewKind, A)),
	send(This, delete_tree),
	send(A, kind, NewKind),
	send(A, update_name).
and_or_to_pxor(This) :->
	send(This?parent, son, new(A, pxor_node(@off))),
	send(A, move_after, This),
	chain_list(This?sons, Sons),
	add_psons(Sons, A),
	send(This, modified, change(This, pxor, A)),
	send(This, delete_tree).


minimum_config(Sons, Sons) :-
	is_list(Sons),
	!.
minimum_config(min(N, Kind), NewSons) :-
	length(NewSons, N),
	uchecklist(=(Kind), NewSons).

copy(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, formula_cut_tree, CT),
		get(This, copy_tree_to, CT, _NN)
	).

cut_tree(This, Test:[bool]) :->
	get(This, parent, Parent),
	(send(Parent, instance_of, formula_node)
	->	true
	;	(Test == @on
		->	fail
		;	send(This, report,
			     warning,'Cannot cut toplevel formula?'),
			fail
		)
	),
	get(Parent, kind, ParentKind),
	(Test == @on
	->	true
	;	(memberchk(ParentKind, [and, or])
		->	get(Parent?sons, size, N),
			(N > 2
			->	get(This, copy_tree_to,
				    This?formula_cut_tree, _),
				send(This, modified, cut_tree(This)),
				send(This, delete_tree)
			;	N = 2
			->	send(This, modified, cut_tree(This)),
				get(This, formula_cut_tree, CT),
				get(This, copy_tree_to, CT, _),
				send(This, delete_tree),
				send(Parent, take_out)
			;	send(This, report, error,
				     'IMPL ERROR: and/or < 2 leaves'),
				fail
			)
		;	new(F, formula_simple_node),
			send(This, modified, cut_tree(This)),
			send(Parent, son, F),
			send(F, move_after, This),
			get(This, copy_tree_to, This?formula_cut_tree, _),
			send(This, delete_tree)
		)
	).


paste_above(This, Test:[bool]) :->
	send(This, paste_below_above, @off, Test).
paste_below(This, Test:[bool]) :->
	send(This, paste_below_above, @on, Test).
paste_below_above(This, Below:bool, Test:[bool]) :->
	get(This, parent, Parent),
	(	get(This?formula_cut_tree, root, Root),
		Root \= @nil,
		send(Parent, instance_of, formula_node),
		get(Parent, kind, ParentKind),
		memberchk(ParentKind, [and, or])
	->	(Test == @on
		->	true
		;	get(Root, copy_tree, PasteNode),
			send(Parent, son, PasteNode),
			send(This, modified, paste_below_above(This, Below, PasteNode)),
			(Below == @on
			->	send(PasteNode, move_after, This)
			;	(get(This, preceding_son, Before)
				->	send(PasteNode, move_after, Before)
				;	send(PasteNode, move_after, @nil)
				)
			)
		)
	;Test == @on
	->	fail
	;	send(This, report,
			     warning,'Canot paste:no value or not and/or'),
		fail
	).

insert_above(This, Test:[bool]) :->
	send(This, insert_below_above, @off, Test).
insert_below(This, Test:[bool]) :->
	send(This, insert_below_above, @on, Test).
insert_below_above(This, Below:bool, Test:[bool]) :->
	get(This, parent, Parent),
	(	send(Parent, instance_of, formula_node),
		get(Parent, kind, ParentKind),
		memberchk(ParentKind, [and, or])
	->	(Test == @on
		->	true
		;	send(Parent, son, new(N, formula_simple_node)),
			send(This, modified, insert_below_above(This, Below, N)),
			(Below == @on
			->	send(N, move_after, This)
			;	(get(This, preceding_son, Before)
				->	send(N, move_after, Before)
				;	send(N, move_after, @nil)
				)
			)
		)
	;Test == @on
	->	fail
	;	send(This, report,
			     warning,'Can only insert_below children of and/or'),
		fail
	).

take_out(This, Test:[bool]) :->
	get(This, kind, Kind),
	assert_debug(is_kind(Kind, formula)),
	% all but at most one son must be simple, i.e. easy to remove
	% if no son, do not allow
	% parent must be compatible with xxx this son
	get(This, sons, Sons),
	get(Sons, size, N),
	(N = 0
	->	(Test == @on
		->	fail
		;	send(This, report,
			     warning,'Cannot take out, you wish to cut?'),
			fail
		)
	;N = 1
	->	get(Sons?head, kind, SonKind),
		(is_kind(SonKind, formula)
		->	(Test == @on
			->	true
			;	get(This?sons, head, FSon),
				send(This?parent, son, FSon),
				send(This, modified, take_out(This)),
				send(FSon, move_after, This),
				send(This, unrelate, FSon),
				send(This, delete)
			)
		;	Test == @on
		->	remove_without_protest0(SonKind)
		;	remove_without_protest0(SonKind)
		->	send(This, delete_all_sons),
			send(This, modified, take_out(This)),
			send(This, kind, formula),
			send(This, update_name)
		;	send(This, report,
			     warning,'Cannot convert, child incompatible(3)'),
			fail
		)
	;	memberchk(Kind, [and, or]),
		get(This, parent, Parent),
		send(Parent, instance_of, formula_node),
		get(Parent, kind, Kind)
	->	(Test == @on
		->	true
		;	send(This?sons, for_all,
			     message(Parent, son, @arg1?copy_tree)),
			send(This, delete_tree)
		)
	;	memberchk(Kind, [exists, forall]),
		N = 2,
		send(Sons?head, is_default)
	->	(Test == @on
		->	true
		;	get(This?sons, nth1, 2, FSon),
			get(FSon, copy_tree, FSonCopy),
			send(This?parent, son, FSonCopy),
			send(FSonCopy, move_after, This),
			send(This, delete_tree)
		)
	;	Test == @on
	->	fail
	;	send(This, report,warning,'Cannot take out node if more than 1 son')
	).
not_remove_without_protestn(Level, Node) :-
	(send(Node, not_remove_without_protest, Level)
	->	fail
	;	true
	).
% NewKind must have one or more children of kind = formula
% Need operation minimum children with >= one formula
% Insert Formula After, if parent is and/or, any formula_node
% Insert Formula Front, if kind is and/or
%
% If allow_disj is false, then not may only be inserted before atom
insert_left(This, NewKind:formula_kind, Test:[bool]) :->
	(	get_domain(leadsto),
		\+ allow_disj,
		get(This, ifparent, Parent),
		send(Parent, instance_of, formula_node),
		get(Parent, kind, not)
	->	(Test == @on
		->	fail
		;	send(This, report,
			     error, 'not may only have atom child'),
			fail
		)
	;	true
	),
	(formula_minimum_containing(NewKind,formula,PrefixSons,PostfixSons)
	->	(	NewKind = not,
			get(This, kind, Kind),
			Kind \= atom,
			\+ allow_disj
		->	(Test == @on
			->	fail
			;	send(This, report,
				     warning, 'Only insert not in front of atom'),
				fail
			)
		;	Test == @on
		->	true
		;	new(NewNode, formula_simple_node(NewKind, @off)),
			send(This?parent, son, NewNode),
			send(NewNode, move_after, This),
			(	member(PSon, PrefixSons),
				send(NewNode, son,
				     ?(This,create_proto,PSon)),
				fail
			;	true
			),
			send(NewNode, move, This),
			send(NewNode, collapsed, @off),
			(	member(PSon, PostfixSons),
				send(NewNode, son,
				     ?(This,create_proto,PSon)),
				fail
			;	true
			),
			send(This, modified, insert_left(This, NewKind, NewNode)),
			send(This, update_name)
		)
	;	Test == @on
	->	fail
	;	send(This, report,
		     warning,'Cannot convert, make children compatible(4)'),
		fail
	).

compatible_formula_name_kind(FormulaKind, Activity, FormulaClass) :-
	formula_name_kind(FormulaKind, FormulaClass),
	(Activity = change
	->	assert_debug(FormulaKind \= var)
		%has_equivalent_sons(formula, FormulaKind, _Res)
	;Activity == insert_left
	->	formula_minimum_containing(FormulaKind, formula, _, _)
	;Activity == take_out
	->	true

				% take_out is delete if leaf => disable
				% take_out if 1 formula child ok, but variables problem
				%             in case of quantifier
				% take_out in case of and/or sometimes ok
				% take_out if no children not ok, is cut/delete
	;	fail
	).
:- pce_end_class.

:- pce_begin_class(condition_node, formula_node).
variable(lhs,        name* := @nil, both).
variable(rhs,        name* := @nil, both).
variable(op,         name* := @nil, both).
variable(whole_name, name* := @nil, both).

kind(_This, Res) :<-
        Res = condition.
tr_term(This, Test:[bool], F:prolog) :<-
        get(This, whole_name, HN),
	(HN == @nil
	->	get(This, tr_pl_atom_nil, op, '[op]', Test, Fn),
		get(This, tr_pl_term_nil, lhs, [term], Test, LHS),
		get(This, tr_pl_term_nil, rhs, [term], Test, RHS),
		F =.. [Fn, LHS, RHS]
	;	assert_debug(get(This, op, @nil)),
		assert_debug(get(This, lhs, @nil)),
		assert_debug(get(This, rhs, @nil)),
		get(This, tr_pl_term_nil, whole_name, [wn], F)
	).
reset(This) :->
	send_list(This, [lhs, rhs, op, whole_name], @nil).

fill_term(This, Term:prolog) :->
	multi_term_condition(Term, IsBinary, []),
	(IsBinary == true
	->	(Term =.. [F, L, R]
		->	(	(F == '[op]'
			 ;	 F = ['op']
			 )
			->	true
			;	cmpopusr(F, F1),
				functor(F1, F2, _)
			->	pl_pce(F2, Fp),
				send(This, change_name, op, Fp)
			;	%send(This, report, error,
				%     'Unrecognised condition operator %s',F)
				format('ERROR:Unrecognised condition operator ~w~n',
				       [F])
			),
			(	L = [term]
			->	true
			;	pl_pce(L, Lp),
				send(This, change_name, lhs, Lp)
			),
			(	R = [term]
			->	true
			;	pl_pce(R, Rp),
				send(This, change_name, rhs, Rp)
			)
		;	impl_error('Unexpected condition error'),
			fail
		)
	;	pl_pce(Term, WN),
		send(This, change_whole_term, WN)
	).
unlink(This) :->
	send(@condition_editor, release_node, This),
	send(This, send_super, unlink).

change_whole_term(This, Name:name, ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	get(This, whole_name, Name1),
	(Name == ''
	->	(Name1 == @nil
		->	true
		;	send(This, whole_name, @nil),
			send(This, modified, change_whole_name(This, Name)),
			send(This, update_name)
		)
	;	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	valid_terml(This, Name, Term),
		(multi_term_condition(Term, IsBinary, Msg )
		->	(	IsBinary == true
			->	Term =.. [OP, LHS, RHS],
				pl_pce(OP, OP1),
				pl_pce(LHS, LHS1),
				pl_pce(RHS, RHS1),
				send(This, valid_name, op, OP1, ReportVisual),
				send(This, valid_name, lhs, LHS1, ReportVisual),
				send(This, valid_name, rhs, RHS1, ReportVisual),
				send(This, change_name, op, OP1),
				send(This, change_name, lhs, LHS1),
				send(This, change_name, rhs, RHS1)
			;	send(This, reset),
				send(This, whole_name, Name),
				send(This, update_name),
				send(This, modified,
				     change_whole_name(This, Name))
			)
		;	send(@condition_editor, update1, This),
			fail
		)
	).
whole_name1(This, Res:char_array) :<-
        get(This, whole_name, Res1),
	(Res1 == @nil
	->	Res = ''
	;	Res = Res1
	).
apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([lhs,rhs,op,whole_term], Sheet, [LHS, RHS, OP, WT]),
	(	(Acted == whole_term,
			WT \== ''
		;	Acted == @default,
			WT \== ''
		)
	->	send(This, change_whole_term, WT)
	;	ignore(send(This, change_name, op, OP, ReportVisual)),
		ignore(send(This, change_name, lhs, LHS, ReportVisual)),
		ignore(send(This, change_name, rhs, RHS, ReportVisual))
	).



valid_name(This,  What:{op,lhs,rhs}, Name:name*,ReportVisual:[visual]):->
	combine_reporteds(This, ReportVisual, Msg),
	valid_condition_name(What, Name, Msg, _Name2).


change_name(This, What:{op,lhs,rhs}, Name:name*, ReportVisual:[visual]) :->
	get(This, What, Name1),
	(Name1 == Name
	->	true
	;Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	combine_reporteds(This, ReportVisual, Msg),
		valid_condition_name(What, Name, Msg, Name2)
	->	send(This, What, Name2),
		send(This, whole_name, @nil),
		send(This, modified, change(What, Name2)),
		send(This, update_name)
	;	combine_reporteds(This, ReportVisual, Msg),
		multi_rec_error(Msg, '%s name %s not valid',[What, Name1]),
		fail
	).
valid_condition_name(What, Name, Msg, Name) :-
	(valid_term(Msg, Name, _)
	->	What \= op
	;	!,fail
	).

valid_condition_name(_What, @nil, _, @nil) :-
	!.
valid_condition_name(op, '[op]', _, @nil) :-
	!.
valid_condition_name(op, Name, _, Name2) :-
	cmpopusr(Name, Name1),
	functor(Name1, Name2, _),
	!.
valid_condition_name(What, Name, Msg, _) :-
	multi_rec_error(Msg, '%s name %s not valid',[What, Name]),
	fail.

update_name(This) :->
	send(This, send_super, update_name),
	send(@condition_editor, update1, This).
initialise(This) :->
	send(This, send_super, initialise, @condition_node_recogniser),
	send(This, update_name).

base_name(This, Name:char_array) :<-
        (get(This, whole_name, WN),
		WN \== @nil
	->	Name = WN
	;	new(Name, string('%s %s %s', ?(This, prop_nil_def, lhs),
			 ?(This, prop_nil_def, op), ?(This, prop_nil_def, rhs)))
	).

copy_local(This, New:condition_node) :<-
        new(New, condition_node),
	(	member(What, [lhs, op, rhs, whole_name]),
		send(New, What, This?What),
		fail
	;	send(New, update_name)
	).
is_default(This) :->
	forall(member(What, [lhs, op, rhs, whole_name]),
	       get(This, What, @nil)
	      ).
name1(This, What:{lhs,op,rhs}, Name) :<-
        (get(This, whole_name, WN),
		WN \= @nil
	->	(What == op
		->	default_prop(op, Name)
		;	Name = ''
		)
        ;	get(This, prop_nil_def, What, Name)
	).



default_prop(lhs, '[term]').
default_prop(rhs, '[term]').
default_prop(op, '[op]').


double_click(This) :->
	send(This, edit).
edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(@condition_editor, setup, This)
	).
remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	send(This, is_default).

:- pce_global(@condition_node_recogniser, mk_condition_node_recogniser).
mk_condition_node_recogniser(R) :-
	new(PQ, popup),
	send(PQ, append, menu_item(edit,
				   message(@arg1, edit),
				   @default, @default,
				   message(@arg1, edit, @on),
				   end_group := @on
			      )),
	add_formula_entries(PQ,R),
	adapt_menu_items(PQ).



:- pce_end_class.

adapt_menu_items(_Menu) :-
        fail,!.
adapt_menu_items(Menu) :-
        %format('MN~n'),
        (menu_items(N)
        ->      get(Menu?members, size, S),
                (S =< N
                ->    true
                ;N = 1
                ->    send(Menu, layout, horizontal)
	        ;     S1 is ceiling(S/N),
		      send(Menu, columns, S1)
                )
        ;       true
        ).

:- pce_begin_class(external_node, formula_node).
tr_term(This, Test:[bool], external(Term):prolog) :<-
	get(This, tr_pl_term_nil, constant0, [external_state], Test, Term).
tr_term_bindings(This, tb(external(Term),B):prolog) :<-
        get(This, tr_pl_term_bindings_nil, constant0, [external_state],
	    tb(Term,B)).
variable(constant0,  char_array*, both).
contains_generic_local(This) :->
	get(This, constant0, '[external_state]').
change_value(This, Val:char_array) :->
	get(This, constant0, Val1),
	(	Val1 \== @nil,
		send(Val1, equal, Val)
	->	true
	;	send(This, constant0, Val),
		send(This, modified, change_value(This, Val)),
		send(This, update_name)
	).
kind(_This, Res) :<-
        Res = external.
initialise(This) :->
	send(This, send_super, initialise, @external_node_recogniser),
	send(This, change_value, '[external_state]'),
	send(This, update_name).
update_name(This) :->
	send(This, send_super, update_name),
	send(@external_editor, update1, This).
unlink(This) :->
	send(@external_editor, release_node, This),
	send(This, send_super, unlink).
base_name(This, Name:char_array) :<-
        new(Name, string('external(%s)',?(This, prop_nil_def, constant0))).
edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(@external_editor, setup, This)
	).
double_click(This) :->
	send(This, edit).






fill_term(This, Term:prolog) :->
	(Term = external
	->	true
	;	send(This, report, error, 'Cannot handle external term'),
		fail
	).





:- pce_global(@external_node_recogniser, mk_external_node_recogniser).
mk_external_node_recogniser(R) :-
	new(PQ, popup),
	send(PQ, update_message,
	if(not(message(@event?receiver?frame?atom_list?members,empty)),
	   message(?(@receiver, member, change_atom),
		   popup, @event?receiver?frame?atom_list))),
	send(PQ, append, menu_item(edit,
				   message(@arg1, edit),
				   @default, @default,
				   message(@arg1, edit, @on),
				   end_group := @on
			      )),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).
remove_without_protest(_This, Level:int) :->
	Level > 0,
	Level =< 1.
:- pce_end_class.


:- pce_begin_class(state_prop_node, formula_node).
variable(state, char_array*, both).

initialise(This, Rec:[recogniser]) :->
	send(This, send_super, initialise, Rec).




change_state(This, Name:char_array, ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	valid_term(Msg, Name, _),
	get(This, state, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, state, Name),
		send(This, modified, change_state(This, Name)),
		send(This, update_name)
	).

unlink(This) :->
	send(This?editor, release_node, This),
	send(This, send_super, unlink).

update_name(This) :->
	send(This, send_super, update_name),
	send(This?editor, update1, This).
state1(This, Name:char_array) :<-
        default_state(DS),
        get(This, val_or_default, state, DS, Name).

default_state('[state]').

double_click(This) :->
	send(This, edit).
edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?editor, setup, This)
	).
remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	send(This, is_default).
variable(atom_name, name*, both).

change_atom_name(This, Name:char_array, ReportVisual:[visual]) :->
	send(This, change_atom_name0, Name, ReportVisual).



base_name(This, Name:char_array) :<-
	new(Name, string('%s%s', This?prefix,This?name1)).
:- pce_end_class.

:- pce_begin_class(bareq_node, state_prop_node).
modified_formula(This, Op:name, Res:formula_node) :<-
        (Op == bareq_to_holds,
		get(This, tr_holds, Res)
	->	true
	;	get(This, get_super, modified_formula, Op, Res)
	).
tr_holds(This, Res:formula_node) :<-
        get(This, atom_name, Name1),
	Name1 \== @nil,
	valid_sprop_term(This, Name1, T),
	convertToDnf(T, T1),
	get(This, state, State),
	tr_holds(T1, State, Res).

tr_holds(or(T1,T2), State, Res) :-
	!,
	tr_holds_and(T1, State, Res1),
	tr_holds(T2, State, Res2),
	new(Res, formula_simple_node),
	send(Res, kind, or),
	send(Res, son, Res1),
	send(Res, son, Res2),
	send(Res, update_name).
tr_holds(T, State, Res) :-
	tr_holds_and(T, State, Res).
tr_holds_and(and(T1,T2), State, Res) :-
	!,
	tr_holds_not(T1, State, Res1),
	tr_holds_and(T2, State, Res2),
	new(Res, formula_simple_node),
	send(Res, kind, and),
	send(Res, son, Res1),
	send(Res, son, Res2),
	send(Res, update_name).
tr_holds_and(T1, State, Res) :-
	tr_holds_not(T1, State, Res).
tr_holds_not(T1, State, New) :-
	(T1 = not(IT)
	->	TF = @off
	;	TF = @on,
		IT = T1
	),
	new(New, holds_node),
	pl_pce(IT, IT1),
	send(New, atom_name, IT1),
	send(New, is_true, TF),
	send(New, state, State),
	send(New, update_name).

change_atom_name0(This, Name:char_array, ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	valid_sprop_term(Msg, Name, _),
	get(This, atom_name, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	atom_to_sprop(Name, Name2),
		send(This, atom_name, Name2),
		send(This, modified, change_atom_name0(This, Name2)),
		send(This, update_name)
	).
initialise(This) :->
	send(This, send_super, initialise, @bareq_node_recogniser).
:- pce_global(@bareq_node_recogniser, mk_bareq_node_recogniser).
mk_bareq_node_recogniser(R) :-
	new(PQ, popup),

	send(PQ, append, menu_item(edit,
				   message(@arg1, edit),
				   @default, @default,
				   message(@arg1, edit, @on),
				   end_group := @on
			      )),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).
editor(_This, E:any) :<-
        E = @bareq_editor.


tr_term(This, Test:[bool], '|='(State, Atom):prolog) :<-
        push_sprop_ops,
        (get(This, tr_pl_term_nil, atom_name, [sprop], Test, Atom)
	->	pop_operators
	;	pop_operators, fail
	),
	get(This, tr_pl_term_nil, state, [state], Test, State).

kind(_This, Res) :<-
        Res = '|=' .

fill_term(This, Term:prolog) :->
	(Term = '|='(State, Prop)
	->	(State == [state]
		->	true
		;	pl_pce(State, State1),
			send(This, change_state, State1)
		),
		(Prop == [sprop]
		->	true
		;	pl_pce(Prop, Atom1),
			send(This, change_atom_name, Atom1)
		)
	;	send(This, report, error, 'Cannot handle |= term'),
		fail
	).


apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([state, state_property], Sheet, [State, Atom]),
	((Acted == @default)
	->	send(This, change_atom_name, Atom, ReportVisual),
		send(This, change_state, State, ReportVisual)
	;Acted == state
	->	send(This, change_state, State, ReportVisual),
		send(This, change_atom_name, Atom, ReportVisual)
	;	send(This, change_atom_name, Atom, ReportVisual),
		send(This, change_state, State, ReportVisual)
	).

copy_local(This, New:bareq_node) :<-
        new(New, bareq_node),
	send(New, atom_name, This?atom_name),
	send(New, state, This?state),
	send(New, update_name).


is_default(This) :->
	get(This, atom_name, @nil),
	get(This, state, @nil).
default_bareq_name('[state] |= [sprop]').

name1(This, Res:char_array) :<-
        (send(This, is_default)
	->	default_bareq_name(Res)
	;	new(Res, string('%s |= %s', This?state1,
				This?atom_name1))
	).
default_sprop_name('[sprop]').

atom_name1(This, Name:char_array) :<-
        default_sprop_name(DN),
	get(This, val_or_default, atom_name, DN, Name).

:- pce_end_class.

:- pce_begin_class(holds_node, state_prop_node).

modified_formula(This, Op:name, Res:formula_node) :<-
        (Op == holds_to_bareq,
		get(This, tr_bareq, Res)
	->	true
	;	get(This, get_super, modified_formula, Op, Res)
	).
tr_bareq(This, Res:bareq_node) :<-
        new(Res, bareq_node),
	send(Res, state, This?state),
	(	get(This, is_true, @on)
	->	send(Res, change_atom_name, This?atom_name)
	;	new(S, string('not(%s)',This?atom_name)),
		send(Res, change_atom_name, S)
	).


change_atom_name0(This, Name:char_array, ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	valid_term(Msg, Name, _),
	get(This, atom_name, Name1),
	ignore(send(This?frame?atom_list, add_name, Name)),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, atom_name, Name),
		send(This, modified, change_atom_name0(This, Name)),
		send(This, update_name)
	).
variable(is_true, bool := @on, both).

kind(_This, Res) :<-
        Res = holds.

editor(_This, E:any) :<-
        E = @holds_editor.
tr_term(This, Test:[bool], holds(State, Atom, TF):prolog) :<-
        get(This, tr_pl_term_nil, atom_name, [atom], Test, Atom),
	get(This, tr_pl_term_nil, state, [state], Test, State),
	get(This, tr_tf, TF).

fill_term(This, Term:prolog) :->
	(Term = holds(State, Atom, TF)
	->	(State == [state]
		->	true
		;	pl_pce(State, State1),
			send(This, change_state, State1)
		),
		(Atom == [atom]
		->	true
		;	pl_pce(Atom, Atom1),
			send(This, change_atom_name, Atom1)
		),
		send(This, change_tf_name, TF)
	;	send(This, report, error, 'Cannot handle holds term'),
		fail
	).



fifo_selected(This, Value:char_array) :->
	send(This, change_atom_name0, Value).

apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([state, atom, truth_value], Sheet, [State, Atom, TruthValue]),
	((Acted == @default;Acted == truth_value)
	->	send(This, change_tf_name, TruthValue),
		send(This, change_atom_name, Atom, ReportVisual),
		send(This, change_state, State, ReportVisual)
	;Acted == state
	->	send(This, change_state, State, ReportVisual),
		send(This, change_tf_name, TruthValue),
		send(This, change_atom_name, Atom, ReportVisual)
	;	send(This, change_atom_name, Atom, ReportVisual),
		send(This, change_tf_name, TruthValue),
		send(This, change_state, State, ReportVisual)
	).


initialise(This) :->
	send(This, send_super, initialise, @holds_node_recogniser).


copy_local(This, New:holds_node) :<-
        new(New, holds_node),
	send(New, atom_name, This?atom_name),
	send(New, is_true, This?is_true),
	send(New, state, This?state),
	send(New, update_name).


is_default(This) :->
	get(This, atom_name, @nil),
	get(This, is_true, @on),
	get(This, state, @nil).



atom_name1(This, Name:char_array) :<-
        default_atom_name(DN),
	get(This, val_or_default, atom_name, DN, Name).
default_atom_name('[atom]').
default_holds_name('holds([state], [atom], true)').
name1(This, Res:char_array) :<-
        (send(This, is_default)
	->	default_holds_name(Res)
	;	new(Res, string('holds(%s, %s, %s)', This?state1,
				This?atom_name1, ?(This, tr_tf)))
	).

tr_tf(This, TF:char_array) :<-
        (get(This, is_true, @off)
	->	TF = false
	;	TF = true
	).

change_tf_bool(This, OnOff:bool) :->
        (get(This, is_true, OnOff)
	->	true
	;	send(This, is_true, OnOff),
		send(This, modified, change_tf_bool(This, OnOff)),
		send(This, update_name)
	).

change_tf_name(This, TF:char_array) :->
        (tfonoff(TF, OnOff)
	->	send(This, change_tf_bool, OnOff)
	;	send(This, report, error, 'Illegal truth value %s', TF),
		fail
	).



:- pce_global(@holds_node_recogniser, mk_holds_node_recogniser).
mk_holds_node_recogniser(R) :-
	new(PQ, popup),
	send(PQ, update_message,
	if(not(message(@event?receiver?frame?atom_list?members,empty)),
	   message(?(@receiver, member, change_atom),
		   popup, @event?receiver?frame?atom_list))),
	send(PQ, append, menu_item(change_atom,
				   message(@pce,format,
				   'CR:%O A1:%O A2:%O A3:%O\n',
				   @receiver, @arg1, @arg2, @arg3),
				   @default,@default,
				   not(message(@event?
					      receiver?frame?atom_list?
					      members,empty)))),
	send(PQ, append, menu_item(edit,
				   message(@arg1, edit),
				   @default, @default,
				   message(@arg1, edit, @on),
				   end_group := @on
			      )),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).

:- pce_end_class.


:- pce_begin_class(property_node, formula_term_node).

initialise(This) :->
	send(This, send_super, initialise, property).

:- pce_end_class.




:- pce_begin_class(atom_node, formula_term_node).

initialise(This) :->
	send(This, send_super, initialise, atom).

:- pce_end_class.



formula_term_node_data(property,
		       [
			class=property_node,
			rec= @property_node_recogniser,
			def=[property],
			editor = @property_editor,
			fifo=property_list
		       ]).
formula_term_node_data(atom,
		       [
			class=atom_node,
			rec= @atom_node_recogniser,
			def=[atom],
			editor = @atom_editor,
			fifo=atom_list
		       ]).
formula_term_kind_member(Kind, Element) :-
	formula_term_node_data(Kind, List),
	member(Element, List).
formula_term_node_member(This, Element) :-
	get(This, kind, Kind),
	formula_term_kind_member(Kind, Element).

:- pce_begin_class(formula_term_node, formula_node).

variable(kind,  name, both).

tr_term(This, Test:[bool], Term:prolog) :<-
	formula_term_node_member(This, def=Default),
	get(This, tr_pl_term_nil, term_name, Default, Test, Term).

contains_generic_local(This) :->
	get(This, term_name, @nil).

editor(This, Editor:dialog) :<-
        formula_term_node_member(This, editor=Editor).


variable(term_name, name*, both).


unlink(This) :->
	send(This?editor, release_node, This),
	send(This, send_super, unlink).
update_name(This) :->
	send(This, send_super, update_name),
	send(This?editor, update1, This).

fifo_selected(This, Value:char_array) :->
	send(This, change_term_name0, Value).
change_term_name(This, Name:char_array) :->
	formula_term_node_member(This, fifo=Fifo),
	ignore(send(This?frame?Fifo, add_name, Name)),
	send(This, change_term_name0, Name).

change_term_name0(This, Name:char_array) :->
	get(This, term_name, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, term_name, Name),
		send(This, modified, change_term_name0(This, Name)),
		send(This, update_name)
	).
initialise(This, Kind:name) :->
	formula_term_kind_member(Kind, rec=Recogniser),
	send(This, send_super, initialise, Recogniser),
	send(This, kind, Kind),
	send(This, update_name).


base_name(This, Name:char_array) :<-
        new(Name, string('%s%s', This?prefix, This?name1)).

copy_local(This, New:extra_node) :<-
        formula_term_node_member(This, class=Class),
        new(New, Class),
	send(New, term_name, This?term_name),
	send(New, update_name).

is_default(This) :->
	get(This, term_name, @nil).
name1(This, Res:char_array) :<-
        get(This, term_name, Res1),
	(Res1 == @nil
	->	formula_term_node_member(This, def=Default),
		term_to_atom(Default, Res)
	;	Res = Res1
	).


double_click(This) :->
	send(This, edit).
edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?editor, setup, This)
	).
remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	send(This, is_default).
reset1(This) :->
	send(This, term_name, @nil).


term_node_recogniser(Kind, Rec) :-
	formula_term_kind_member(Kind, rec=Rec).

:- pce_global(@atom_node_recogniser, mk_atom_node_recogniser).
mk_atom_node_recogniser(R) :-
	mk_term_node_recogniser(atom_list, R).
:- pce_global(@property_node_recogniser, mk_property_node_recogniser).
mk_property_node_recogniser(R) :-
	mk_term_node_recogniser(property_list, R).
mk_term_node_recogniser(ListMember, R) :-
	new(PQ, popup),
	send(PQ, update_message,
	if(not(message(@event?receiver?frame?ListMember?members,empty)),
	   message(?(@receiver, member, change),
		   popup, @event?receiver?frame?ListMember))),
	send(PQ, append, menu_item(change,
				   message(@pce,format,
				   'CR:%O A1:%O A2:%O A3:%O\n',
				   @receiver, @arg1, @arg2, @arg3),
				   @default,@default,
				   not(message(@event?
					      receiver?frame?ListMember?
					      members,empty)))),
	send(PQ, append, menu_item(edit,
				   message(@arg1, edit),
				   @default, @default,
				   message(@arg1, edit, @on),
				   end_group := @on
			      )),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).

:- pce_end_class.

:- pce_begin_class(itef_node, formula_node).
tr_term(This, Test:[bool], itef(Cond, Vars, Form):prolog) :<-
        get(This, sons, Sons),
	chain_list(Sons, SonsL),
        maplist(tr_term(Test), SonsL, Objs),
	append([Cond|Vars], [Form], Objs).




copy_local(_This, NewNode:itef_node) :<-
        new(NewNode, itef_node(@default,@default,@default,@off)).


remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	send(This?sons, for_all, message(@arg1, remove_without_protest, Level)).

kind(_This, itef:char_array) :<-
        true.
base_name(This, Name:char_array) :<-
        new(Name, string('%s%s', This?prefix, 'CONDITIONAL SUBSTITUTION')).

update_name(This) :->
	send(This, send_super, update_name).
	%send(@holds_editor, update1, This).

initialise(This,  Condition:[formula_node], IntoFormula:[formula_node],
	   ConditionalVars:[chain],EnsureSons:[bool]) :->
	send(This, send_super, initialise, @bare_formula_node_recogniser),
	(EnsureSons == @off
	->	true
	;	(Condition == @default
		->	send(This, son, new(formula_simple_node))
		;	send(This, son, Condition)
		),
		(ConditionalVars == @default
		->	send(This, son, new(conditional_var_node))
		;	send(ConditionalVars, for_all, message(This, son, @arg1))
		),
		(IntoFormula == @default
		->	send(This, son, new(formula_simple_node))
		;	send(This, son, IntoFormula)
		)
	).



:- pce_end_class.







:- pce_begin_class(conditional_var_node, var_node).
variable(then, name*, both).
variable(else, name*, both).


local_update(This, Member:name, DoUpdate:[bool]) :->
	get(This, Member, Name),
	send(This, modified, change_local(This, Member, Name)),
	(	DoUpdate \= @off
	->	send(This, update_name),
		send(@conditional_var_editor, update1, This)
	;	true
	).
change_then(This, Name:char_array, DoUpdate:[bool]) :->
	send(This, change_local, then, Name, DoUpdate).
change_else(This, Name:char_array, DoUpdate:[bool]) :->
	send(This, change_local, else, Name, DoUpdate).
change_local(This, Member:{then,else},Name:char_array, DoUpdate:[bool]) :->
	get(This, Member, Name1),
	(Name == ''
	->	(Name1 == @nil
		->	true
		;	send(This, Member, @nil),
			send(This, local_update, Member, DoUpdate)
		)
	;	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, Member, Name),
		send(This, local_update, Member, DoUpdate)
	).

then1(This, Res:char_array) :<-
        get(This, then, Res1),
	(Res1 == @nil
	->	default_prop(then,Res)
	;	Res = Res1
	).
else1(This, Res:char_array) :<-
        get(This, else, Res1),
	(Res1 == @nil
	->	default_prop(else,Res)
	;	Res = Res1
	).
default_prop(then, '[then]').
default_prop(else, '[else]').

apply_all_editor(This, ReportVisual:visual, Sheet:sheet, _Acted:[name]) :->
	get_sheet_vals([then,else, variable_name, sort_name], Sheet,
		       [T,E,V,S]),
	send(This, change_then, T, @off),
	send(This, change_else, E, @off),
	send(This, change_sort_name, S),
	send(This, change_var_name, V, @on, ReportVisual).
tr_term(This, Test:[bool], Term:prolog) :<-
	get(This, tr_pl_term_nil, then, [then], Test, Then),
	get(This, tr_pl_term_nil, else, [else], Test, Else),
	get(This, get_super, tr_term, Test, Var),
	Term = tev(Then, Else, Var).
fill_local(This, Member:{then, else}, P:prolog) :->
	(default_prop(Member, P)
	->	true
	;	pl_pce(P, P1),
		send(This, change_local, Member, P1)
	).

fill_term(This, Term:prolog) :->
	(Term = tev(T, E, Var)
	->	send(This, send_super, fill_term, Var),
		send(This, fill_local, then, T),
		send(This, fill_local, else, E)
	;	send(This, report, error, 'Cannot handle holds term'),
		fail
	).
initialise(This) :->
	send(This, send_super, initialise,@conditional_var_editor,
	     @conditional_var_node_recogniser).
remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	get(This, then, @nil),
	get(This, else, @nil),
	send(This, send_super, remove_without_protest, Level).

copy_local(This, New:conditional_var_node) :<-
        new(New, conditional_var_node),
	(	member(M, [var_name, sort_name, whole_name, then, else]),
		send(New, M, This?M),
		fail
	;	send(New, update_name)
	).
base_name(This, Name:char_array) :<-
	(	get(This, whole_name, WN),
		WN \= @nil
	->	impl_error('Unexpected whole_name')
        ;	new(Name, string('%s OTHERWISE %s INTO %s : %s',
				 ?(This,prop_nil_def, then),
				 ?(This,prop_nil_def, else),
				 ?(This,prop_nil_def, var_name),
				 ?(This,prop_nil_def, sort_name)))
	).
is_default(This) :->
	send(This, send_super, is_default),
	get(This, then, @nil),
	get(This, else, @nil).

reset1(This, Test:[bool]) :->
	send(This, send_super, reset1, Test),
	(Test == @on
	->	\+ send(This, is_default)
	;	(member(Prop, [var_name, sort_name, then, else]),
			send(This, Prop, @nil),
			fail
		;	true
		),
		send(This, modified, reset1(This)),
		send(This, update_name)
	).
insert_below_above(This, Below:bool, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, parent, Parent),
		send(Parent, son, new(VN, conditional_var_node)),
		send(This, modified, insert_below_above(This, Below, VN)),
		(Below == @on
		->	send(VN, move_after, This)
		;	(get(This, preceding_son, Before)
			->	send(VN, move_after, Before)
			;	send(VN, move_after, @nil)
			)
		)
	).
insert_conditional_below(This, Test:[bool]) :->
	send(This, insert_below_above, @on, Test).

insert_conditional_above(This, Test:[bool]) :->
	send(This, insert_below_above, @off, Test).
cut(This, Test:[bool]) :->
	get(This, parent, Parent),
	get(Parent?sons, size, N),
	(	N < 4
		%send(Parent, instance_of, formula_simple_node)
	->	assert_debug(N = 3),
		(send(This, is_default)
		->	send(Parent, take_out, Test)
		;	Test == @on
		->	true
		;	send(This, report, error, 'First reset conditional before removing last one'),
			fail
		)
	;	(	Test == @on
		->	true
		;	get(This, conditional_cut_tree, CT),
			send(This, modified, cut(This)),
			get(This, copy_tree_to, CT, _),
			send(This, delete_tree)
		)
	).
:- pce_end_class.

:- pce_global(@conditional_var_node_recogniser,
              mk_conditional_var_node_recogniser).
mk_conditional_var_node_recogniser(Rec) :-
	new(PQ, popup),
	(	member(Act, [edit,insert_conditional_below,insert_conditional_above,cut,
			     reset]),
		rename_reset(Act, Act1),
		send(PQ, append, menu_item(Act,
					   message(@arg1, Act1),
					   @default, @default,
					   message(@arg1, Act1, @on))),
		fail
	;	true
	),
        adapt_menu_items(PQ),
	new(Rec, popup_gesture(PQ)).


:- pce_begin_class(pxor_node, formula_node).

tr_term(This, Test:[bool], pxor(Objs):prolog) :<-
        get(This, sons, Sons),
	chain_list(Sons, SonsL),
        maplist(tr_term(Test), SonsL, Objs).




copy_local(_This, NewNode:pxor_node) :<-
        new(NewNode, pxor_node(@off)),
	send(NewNode, update_name).



remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	send(This?sons, for_all, message(@arg1, remove_without_protest, Level)).


kind(_This, pxor:char_array) :<-
        true.


base_name(This, Name:char_array) :<-
        new(Name, string('%s%s', This?prefix, 'PXOR')).

update_name(This) :->
	send(This, send_super, update_name).
	%send(@holds_editor, update1, This).
initialise(This, EnsureSons:[bool]) :->
	send(This, send_super, initialise, @bare_formula_node_recogniser),
	(EnsureSons == @off
	->	true
	;	send(This, son, new(pxor_formula_node)),
		send(This, update_name)
	).

:- pce_end_class.
:- pce_global(@bare_formula_node_recogniser, mk_bare_formula_node_recogniser).
mk_bare_formula_node_recogniser(R) :-
	new(PQ, popup),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).
/*
:- pce_global(@pxor_node_recogniser, mk_pxor_node_recogniser).
mk_pxor_node_recogniser(R) :-
	new(PQ, popup),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).
*/

:- pce_begin_class(pxor_formula_node, formula_node).

tr_term(This, Test:[bool], Term:prolog) :<-
        (get(This, probability, @nil)
	->     P1 = @nil
        ;	get(This, tr_pl_term_nil, probability, @nil, P1)
	),
	get(This?formula, tr_term, Test, Term1),
	Term = pxe(P1, Term1).

copy_local(This, Copy:pxor_formula_node) :<-
        local_trace(pof),
        new(Copy, pxor_formula_node(This?probability, This?formula, @off)),
	send(Copy, update_name).

formula(This, F:formula_node) :<-
        get(This, sons, S),
	get(S, head, F).

psons_to_fsnode([], _).

psons_to_fsnode([Son|Sons], A) :-
	send(A, son, Son?formula),
	psons_to_fsnode(Sons, A).

add_psons([], _PXORnode).

add_psons([Son|Sons], PXORnode) :-
	add_pson(Son, PXORnode),
	add_psons(Sons, PXORnode).

add_pson(Son, PXORnode) :-
	get(Son, copy_tree, CSon),
	new(PSon, pxor_formula_node(@default,CSon)),
	send(PXORnode, son, PSon).

remove_without_protest(This, Level:int) :->
	Level > 0,
	Level =< 1,
	%get(This, probability, P),
	%memberchk(P, [@nil, @default]),
	send(This?sons, for_all, message(@arg1, remove_without_protest, Level)).

kind(_This, pxor_formula:char_array) :<-
        true.
variable(probability, name*, get).
probability_real(This, P1:real) :<-
       get(This, probability, P),
       P \== @nil,
       get(@pce, convert, P, real,P1).








check_sum(This, P:real, Editor:any) :->
	get(This?parent, sons, S),
	chain_list(S, L),
	(	sum_prob(L, 0, Sum),
		Sum + P > 1
	->	send(Editor, report, warning, 'Sum probability > 1')
	;	true
	).
sum_prob([], Sum, Sum).
sum_prob([P|Nodes], SumIn, SumOut) :-
	get(P, probability_real, PV),
	number(PV),
	Sum1 is PV + SumIn,
	sum_prob(Nodes, Sum1, SumOut).

test_probability(Editor, P) :-
	(number(P)
	->	test_probability_num(Editor, P)
	;	P == @nil
	->	true
	;	send(P, instance_of, char_array)
	->	get(P, value, Q),
		valid_term(Editor, Q, Term),
		(number(Term)
		->	test_probability_num(Editor, Term)
		;	true
		)
	;	send(Editor, report, error, 'Expected probability, got %s',P),
		fail
	).
test_probability_num(Editor, Term) :-
	(Term =< 1.0,
		Term >= 0.0
	->	true
	;	pl_pce(Term, T1),
		send(Editor, report, error, 'Expected probability 0..1, got %s',T1),
		fail
	).

change_probability(This, P0:[name]*, Editor:any) :->
	default(P0, 0.0, P),
	default(Editor, This, Ed),
	(test_probability(Ed, P)
	->	true
	;	send(@probability_editor, update1, This),
		fail
	),
	get(This, probability, P1),
	(P1 == P
	->	true
	;	send(This, probability, P),
		send(This, modified, p(P))
	).

probability(This, P:name*) :->
	send(This, slot, probability, P),
	send(This, update_name),
	(P == @nil
	->	send(@probability_editor, release_node, This)
	;	send(@probability_editor, update1, This)
	).

base_name(This, Name:char_array) :<-
        get(This, probability, P),
	(P == @nil
	->      Name = 'OTHERWISE'
	;	new(Name, string('Prob %s', This?probability))
	).


initialise(This,  Prob:any, FN1:[formula_node], EnsureSons:[bool]) :->
	send(This, send_super, initialise, @pxor_formula_node_recogniser),
	default(Prob, 0.0, Prob1),
	test_probability(@pce, Prob1),
	send(This, probability, Prob1),
	(EnsureSons == @off
	->	true
	;	(FN1 == @default
		->	new(FN, formula_simple_node),
			send(This, son, FN)
		;	send(This, son, FN1)
		)
	).

popped_pxor_formula_node(insert_p_entry_before, This) :-
	new(N, pxor_formula_node),
	send(This?parent, son, N),
	send(N, move_before, This),
	send(This, modified, popped(insert_p_entry_before)).



popped_pxor_formula_node(insert_p_entry_after, This) :-
	get(This, probability, P),
	(P == @nil
	->	send(This, probability, @default),
		send(This, update_name),
		P1 = @nil
	;	P1 = @default
	),
	send(This, modified, popped(insert_p_entry_after)),
	new(N, pxor_formula_node(P1)),
	send(This?parent, son, N),
	send(N, move_after, This).


popped_pxor_formula_node(change_p, This) :-
	send(This, edit).
popped_pxor_formula_node(change_otherwise, This) :-
	get(This?parent?sons, tail, Tail),
	(Tail == This
	->	send(This, change_probability, @nil)
	;	send(Tail, change_probability, @default),
		send(This, change_probability, @nil),
		send(This, move_after)
	).

popped_pxor_formula_node(move_up, This) :-
	(get(This, preceding_son, Preceding)
	->	send(This, move_before, Preceding),
		(get(This, probability, @nil)
		->	send(This, probability, @default)
		;	true
		)
	;	send(This, report, warning, 'cannot move up first node'),
		fail
	).

popped_pxor_formula_node(rm_entry, This) :-
	get(This, parent, Parent),
	assert_debug(send(Parent,instance_of, pxor_node)),
	get(Parent?sons, size, L),
	(	     L =< 1
	->	send(This, delete_tree),
		get(Parent, parent, PP),
		new(FN, formula_simple_node),
		send(PP, son, FN),
		send(FN, move_after, Parent),
		send(Parent, delete_tree)
	;	get(This, parent, Parent),
		send(This, delete_tree),
		PP = Parent
	),
	send(PP, modified, popped(rm_entry_son)).
double_click(This) :->
	send(This, edit).
edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(@probability_editor, setup, This)
	).
unlink(This) :->
	send(@probability_editor, release_node, This),
	send(This, send_super, unlink).

:- pce_end_class.
:- pce_global(@pxor_formula_node_recogniser, mk_pxor_formula_node_recogniser).
mk_pxor_formula_node_recogniser(R) :-
	new(PQ, popup),
	send_list(PQ, append, [
			       insert_p_entry_before,
			       insert_p_entry_after,
			       change_p,
			       change_otherwise,
			       move_up,
			       rm_entry]),
        adapt_menu_items(PQ),
	send(PQ, message, message(@prolog, popped_pxor_formula_node, @arg1, @arg2)),
	new(R, popup_gesture(PQ)).





:- pce_begin_class(formula_simple_node, formula_node).




variable(kind, formula_kind, both).





tr_formula_kind(formula, Test,This, Term) :-
	!,
	(	Test == @on
	->	fail
	;	send(This, report, warning, 'saving invalid generic formula'),
		Term = [formula]
	).

tr_formula_kind(Kind, Test, This, Term) :-
	quantor(Term, Vars, Kind, SubForm),
	!,
	get(This, sons, ChildrenNodes),
	chain_list(ChildrenNodes, NIn),
	tr_vars(NIn, Test, N1, This, Vars),
	(N1 = [FormNode]
	->	get(FormNode, tr_term, Test, SubForm)
	;	impl_error('Inconsistent leadsto_node')
	).

tr_formula_kind(Kind, Test, This, Term) :-
	formula_data(Kind, _, SonKinds),
	(SonKinds = min(_N, formula)
	;	forall(member(SK,SonKinds), SK = formula)
	),
	!,
	get(This, sons, Sons),
	chain_list(Sons, SonsL),
	maplist(tr_term(Test), SonsL, Args),
	Term =.. [Kind|Args].
tr_formula_kind(Kind, _Test, This, _Term) :-
	!,
	send(This, report, error, 'Unimplemented node save kind %s',
	     Kind),
	fail.

contains_generic_local(This) :->
	get(This, kind, formula).


tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, kind, Kind),
	tr_formula_kind(Kind, Test, This, Term).



base_name(This, Name:char_array) :<-
        get(This, kind, Kind),
	formula_name(Kind, Name1),
	new(Name, string('%s%s', This?prefix,Name1)).


copy_local(This, New:formula_simple_node) :<-
        new(New, formula_simple_node(This?kind, @off)).

initialise(This, Kind:[formula_kind], EnsureSons:[bool]) :->
	default(Kind, formula, Kind1),
	is_kind(Kind1, MasterKind),
	assert_debug(MasterKind == formula),
	send(This, send_super, initialise, @bare_formula_node_recogniser),
	send(This, kind, Kind1),
	(EnsureSons == @off
	->	true
	;MasterKind == formula
	->	send(This, update_kind_sons, formula, Kind1)
	;	true
	),
	send(This, update_name).


remove_without_protest(This, Level:int) :->
	get(This, kind, Kind),
	(remove_without_protest0(Kind)
	->	true
	;	Level >= 1,
		remove_without_protest1(Kind)
	->	true
	;	fail
	).



:- pce_global(@var_node_recogniser, mk_var_node_recogniser).
mk_var_node_recogniser(R) :-
	new(PQ, popup),
	(	member(Act, [edit,insert_var_below,insert_var_above,cut,
			     reset]),
		rename_reset(Act, Act1),
		send(PQ, append, menu_item(Act,
					   message(@arg1, Act1),
					   @default, @default,
					   message(@arg1, Act1, @on))),
		fail
	;	true
	),
        adapt_menu_items(PQ),
	new(R1, popup_gesture(PQ)),
	new(R, handler_group(R1)).
/*
:- pce_global(@formula_node_recogniser_flat, mk_formula_node_recogniser_flat).
mk_formula_node_recogniser_flat(R) :-
	new(PQ, popup),
	add_formula_entries(PQ,R),
        adapt_menu_items(PQ).
*/

add_formula_entries(PQ,R) :-
	(	member(Activity, [change,insert_left]),
		bagof(Name1,
		      Kind^compatible_formula_name_kind(Name1,Activity, Kind),
		      Names1),
		member(Name, Names1),
		concat_atom([Activity, '   ', Name], MName),
		send(PQ, append,
		     menu_item(MName,
			       message(@arg1, Activity, Name),
			       @default, @default,
			       message(@arg1, Activity, Name, @on)
			      )
		    ),
		fail
	;	(	member(Act, [insert_below,insert_above,
				     paste_below,paste_above,take_out, cut_tree,
				     copy,paste]),
			send(PQ, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
			fail
		;	true
		)
	),
	new(R1, popup_gesture(PQ)),
	new(R, handler_group(R1)).


:- pce_end_class.


:- pce_begin_class(toplevel_node, text_node).

toplevel_op(insert_below).
toplevel_op(insert_above).
toplevel_op(paste_below).
toplevel_op(paste_above).
toplevel_op(copy).
toplevel_op(cut).
modify_op(This, Op:name) :->
	send(@pce, format, 'N:%s  Op:%s\n', This?class_name, Op).

cut(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, toplevel_cut_tree, CT),
		get(This, copy_tree_to, CT, Copy),
		send(This, modified, cut(This, Copy)),
		send(This, delete_tree)
	).

paste_contents(This, Test:[bool], N:toplevel_node) :<-
        get(This?toplevel_cut_tree, root, N),
	(N \= @nil
	->	send(This?parent, may_add, N?class_name, Test)
	;	Test == @on
	->	fail
	;	send(This, report, error,
		     'Cut buffer empty'),
		fail
	).
add_below_above(This, Below:bool, NewNode:toplevel_node, What:name) :->
	get(This, parent, Parent),
	send(Parent, son, NewNode),
	W1 =.. [What, This, Below,NewNode],
	send(This, modified, W1),
	(	Below == @on
	->	send(NewNode, move_after, This)
	;	(get(This, preceding_son, Before)
		->	send(NewNode, move_after, Before)
		;	send(NewNode, move_after, @nil)
		)
	).

paste_below_above(This, Below:bool, Test:[bool]) :->
	get(This, paste_contents, Test, CutNode),
	(Test == @on
	->	true
	;	get(CutNode, copy_tree, PasteNode),
		send(This, add_below_above, Below, PasteNode, paste_below_above),
		send(PasteNode, update_name)
	).

insert_below_above(This, Below:bool, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, parent, Parent),
		get(This?frame, domain, Domain),
		send(Parent, son, new(VN, generic_node(Domain))),
		send(This, add_below_above, Below, VN, insert_below_above)
	).

insert_below(This, Test:[bool]) :->
	send(This, insert_below_above, @on, Test).
insert_above(This, Test:[bool]) :->
	send(This, insert_below_above, @off, Test).
paste_below(This, Test:[bool]) :->
	send(This, paste_below_above, @on, Test).
paste_above(This, Test:[bool]) :->
	send(This, paste_below_above, @off, Test).

copy(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, toplevel_cut_tree, CT),
		get(This, copy_tree_to, CT, _)
	).



:- pce_end_class.


/* Horrible copy/paste from editable_text_node */
:- pce_begin_class(editable_toplevel_node, toplevel_node).
variable(editor, single_node_editor, both).

initialise(This, Editor:single_node_editor, R:recogniser) :->
	send(This, send_super, initialise, R),
	send(This, editor, Editor).

edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?editor, setup, This)
	).
double_click(This) :->
	send(This, edit).
unlink(This) :->
	send(This?editor, release_node, This),
	send(This, send_super, unlink).
update_name(This) :->
	send(This, send_super, update_name),
	send(This?editor, update1, This).

:- pce_end_class.

:- pce_begin_class(editable_header_toplevel_node, editable_toplevel_node).
edit_header(This, Test:[bool]) :->
	send(This, edit, Test).

:- pce_end_class.


:- pce_begin_class(generic_node, toplevel_node).
variable(domain, {leadsto,checker}, both).
tr_term(This, Test:[bool], Term:prolog) :<-
        Term = [specification_element],
	(Test == @on
	->	fail
	;	send(This, report,
		     warning,'saving invalid generic_formula node')
	).

base_name(_This, Name:char_array) :<-
        Name = '[specification_element]'.

initialise(This, Domain:{leadsto,checker}) :->
	generic_node_recogniser(Domain, Rec),
	send(This, send_super, initialise, Rec),
	send(This, domain, Domain),
	send(This, collapsed, @nil),
	send(This, update_name).

copy_local(This, New:text_node) :<-
        new(New, generic_node(This?domain)).


change(This, ClassName:name, Test:[bool]) :->
	send(This?parent, may_add, ClassName, Test),
	(Test == @on
	->	true
	;	get(@pce, instance, ClassName, Element),
		send(This?parent, son, Element),
		send(Element, move_after, This),
		send(This, delete_tree)
	).

generic_node_recogniser(leadsto, @generic_node_recogniser_lt).
generic_node_recogniser(checker, @generic_node_recogniser_chk).
:- pce_global(@generic_node_recogniser_chk, mk_generic_node_recogniser_chk).
mk_generic_node_recogniser_chk(R) :-
	mk_generic_node_recogniser(checker,R).
:- pce_global(@generic_node_recogniser_lt, mk_generic_node_recogniser_lt).
mk_generic_node_recogniser_lt(R) :-
	mk_generic_node_recogniser(leadsto,R).

mk_generic_node_recogniser(Domain,R) :-
	new(P, popup),
	(	toplevel_element(Domain,Act, Class),
		send(P, append, menu_item(Act,
					   message(@arg1, change, Class),
					   @default, @default,
					   message(@arg1, change, Class, @on))),
		fail
	;	send(P?members?tail, end_group, @on)
	),
	(	toplevel_op(Act),
		send(P, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
		fail
	;	true
	),
        adapt_menu_items(P),
	new(R, popup_gesture(P)).

:- pce_end_class.






:- pce_begin_class(range_node, editable_text_node).

variable(low, name*, both).
variable(hi, name*, both).
variable(range, name*, both).

tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, range, Range),
	(Range == @nil
	->	get(This, tr_pl_term_nil, low, [time], Test, Lo),
		get(This, tr_pl_term_nil, hi, [time], Test, Hi),
		Term = range(Lo, Hi)
	;	get(This, tr_pl_term_nil, range, [range], Test, Term)
	).




initialise(This) :->
	send(This, send_super, initialise,@range_editor,
	     @range_node_recogniser),
	send(This, update_name).

change_range(This, Name:char_array) :->
	get(This, range, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, range, Name),
		send(This, low, @nil),
		send(This, hi, @nil),
		send(This, modified, change_range(This, Name)),
		send(This, update_name)
	).

change_low(This,  Name:char_array) :->
	send(This, change_bound, low, Name).

change_hi(This,  Name:char_array) :->
	send(This, change_bound, hi, Name).
change_bound(This, What:{low,hi}, Name:char_array) :->
	get(This, What, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, What, Name),
		send(This, modified, change_bound(This, What, Name)),
		send(This, range, @nil),
		send(This, update_name)
	).

copy_local(This, New:range_node) :<-
        new(New, range_node),
	(	member(M, [low, hi, range]),
		send(New, M, This?M),
		fail
	;	send(New, update_name)
	).
is_default(This) :->
	forall(member(M, [low, hi, range]),
	       get(This, M, @nil)
	      ).


reset1(This, Test:[bool]) :->
	(send(This, is_default)
	->	fail
	;	Test == @on
	->	true
	;	forall(member(M, [low, hi, range]),
		       send(This, M, @nil)
		      ),
		send(This, modified, reset1(This)),
		(get(@range_editor, for, This)
		->	send(@range_editor, update_node)
		;	true
		),
		send(This, update_name)
	).


default_range('range([time],[time])').

default_range_elem('[time]').

range1(This, Res:char_array) :<-
        get(This, range, Lo1),
	(	Lo1 == @nil
	->	get(This, low, Lo1),
		get(This, hi, Hi1),
		(	Lo1 == @nil,
			Hi1 == @nil
		->	default_range(Res)
		;	get(This, low1, L1),
			get(This, hi1, H1),
			new(Res, string('range(%s,%s)', L1, H1))
		)
	;	Res = Lo1
	).
low1(This, Res:char_array) :<-
        get(This, low, Lo1),
	(	Lo1 == @nil
	->	default_range_elem(Res)
	;	Res = Lo1
	).
hi1(This, Res:char_array) :<-
        get(This, hi, Lo1),
	(	Lo1 == @nil
	->	default_range_elem(Res)
	;	Res = Lo1
	).
prefix(This, Prefix:char_array) :<-
        (get(This?ifparent, collapsed, @on)
	->	Prefix = ''
	;	Prefix = 'R: '
	).

base_name(This, Name:char_array) :<-
        get(This, range, Range),
	(Range == @nil
	->	new(Name, string('%srange(%s,%s)',This?prefix,
				 This?low1, This?hi1))
	;	new(Name, string('%s%s', This?prefix, Range))
	).




:- pce_global(@range_node_recogniser, mk_range_node_recogniser).
mk_range_node_recogniser(R) :-
	new(P, popup),
	(	member(Act, [edit, reset]),
		rename_reset(Act, Act1),
		send(P, append, menu_item(Act,
					   message(@arg1, Act1),
					   @default, @default,
					   message(@arg1, Act1, @on))),
		fail
	;	true %send(P?members?tail, end_group, @on)
	),
        adapt_menu_items(P),
	new(R, popup_gesture(P)).
:- pce_end_class.






:- pce_begin_class(period_node, editable_text_node).

tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, tr_pl_term_nil, constant0, [time], Test, Term).

variable(constant0, char_array*, both).
functor1(_This, T:name) :<-
        T = period.
default1(_This, T:name) :<-
        T = '[period]'.

copy_local(This, New:period_node) :<-
        new(New, period_node),
	send(New, constant0, This?constant0),
	send(New, update_name).



change_value(This, P:char_array*) :->
	send(This, change_gui_prop, constant0, P).



remove(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This, modified, remove(This)),
		send(This, delete_tree)
	).



:- pce_global(@period_node_recogniser, mk_period_node_recogniser).
mk_period_node_recogniser(R) :-
	new(Pop, popup),
	(	member(Act, [remove,edit]),
		send(Pop, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
		fail
	;	true
	),
        adapt_menu_items(Pop),
	new(R, popup_gesture(Pop)).


initialise(This) :->
	send(This, send_super, initialise, @constant_editor, @period_node_recogniser),
	send(This, update_name).

prefix(This, Prefix:char_array) :<-
        (get(This?ifparent, collapsed, @on)
	->	Prefix = ''
	;	Prefix = 'P: '
	).
base_name(This, Name:char_array) :<-
        new(Name, string('%s%s', This?prefix, ?(This, prop_nil_def, constant0))).


:- pce_end_class.

:- pce_begin_class(other_node, multi_constant_node).
tr_term(This, Test:[bool], Term:prolog) :<-
        impl_error('Unexpected tr_term call'),
        get(This, tr_pl_term_nil, constant0, [any], Test, Term).
tr_term_bindings(This, TB:prolog) :<-
        local_trace(other),
        get(This, tr_pl_term_bindings_nil, constant0, [other], TB).

copy_local(This, New:text_node) :<-
        new(New, other_node),
	send(New, constant0, This?constant0),
	send(New, update_name).

base_name(This, Name:char_array) :<-
        new(Name, string('%s :  %s', 'OTHER', ?(This, prop_nil_def, constant0))).
initialise(This) :->
	send(This, send_super, initialise, other, '[any]').

:- pce_end_class.


:- pce_begin_class(model_node, constant_node).
tr_term(This, Test:[bool], model(Term):prolog) :<-
        get(This, tr_pl_term_nil, constant0, [model], Test, Term).
tr_term_bindings(This, tb(model(Term), B):prolog) :<-
       impl_error('Unexpected tr_term call'),
        get(This, tr_pl_term_bindings_nil, constant0, [model], tb(Term, B)).

copy_local(This, New:text_node) :<-
        new(New, model_node),
	send(New, constant0, This?constant0),
	send(New, update_name).

base_name(This, Name:char_array) :<-
        new(Name, string('%s :  %s', 'MODEL', ?(This, prop_nil_def, constant0))).
initialise(This) :->
	send(This, send_super, initialise, model, '[any]').

:- pce_end_class.

:- pce_begin_class(start_time_node, constant_node).
tr_term(This, Test:[bool], start_time(Term):prolog) :<-
        get(This, tr_pl_term_nil, constant0, [time], Test, Term).
copy_local(This, New:text_node) :<-
        new(New, start_time_node),
	send(New, constant0, This?constant0),
	send(New, update_name).

initialise(This) :->
	send(This, send_super, initialise, start_time, '[time]').
:- pce_end_class.



:- pce_begin_class(end_time_node, constant_node).
tr_term(This, Test:[bool], end_time(Term):prolog) :<-
        get(This, tr_pl_term_nil, constant0, [time], Test, Term).

copy_local(This, New:text_node) :<-
        new(New, end_time_node),
	send(New, constant0, This?constant0),
	send(New, update_name).
initialise(This) :->
	send(This, send_super, initialise, end_time, '[time]').
:- pce_end_class.



:- pce_begin_class(global_lambda_node, constant_node).

tr_term(This, Test:[bool], global_lambda(Term):prolog) :<-
        get(This, tr_pl_term_nil, constant0, [fraction], Test, Term).
copy_local(This, New:text_node) :<-
        new(New, global_lambda_node),
	send(New, constant0, This?constant0),
	send(New, update_name).
initialise(This) :->
	send(This, send_super, initialise, global_lambda, '[real]').
:- pce_end_class.




:- pce_begin_class(cwa_node, multi_constant_node).

tr_term_bindings(This, tb(cwa(Term),B):prolog) :<-
        get(This, tr_pl_term_bindings_nil, constant0, [atom], tb(Term,B)).

tr_term(This, Test:[bool], cwa(Term):prolog) :<-
        get(This, tr_pl_term_nil, constant0, [atom], Test, Term).
copy_local(This, New:text_node) :<-
        new(New, cwa_node),
	send(New, constant0, This?constant0),
	send(New, update_name).
initialise(This) :->
	send(This, send_super, initialise, cwa, '[atom]').
:- pce_end_class.





:- pce_begin_class(multi_constant_node, constant_node).
:- pce_end_class.




:- pce_begin_class(object_node, editable_text_node).
variable(is_sub_sort, bool := @off, both).
variable(object_name, name*, both).
tr_term(This, Test:[bool], Term:prolog) :<-
	get(This, tr_term_nil, object_name, [object], Test, Term1),
	(get(This, is_sub_sort, @on)
	->	Term = 'SUBSORT'(Term1)
	;	Term = Term1
	).



copy_local(This, New:text_node) :<-
        new(New, object_node),
	send(New, object_name, This?object_name),
	send(New, is_sub_sort, This?is_sub_sort),
	send(New, update_name).


copy(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, object_cut_tree, CT),
		get(This, copy_tree_to, CT, _Copy)
	).
cut(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	get(This, object_cut_tree, CT),
		get(This, copy_tree_to, CT, Copy),
		send(This, modified, cut(This, Copy)),
		send(This, delete_tree)
	).
paste_above(This, Test:[bool]) :->
	send(This, paste_below_above, @off, Test).
paste_below(This, Test:[bool]) :->
	send(This, paste_below_above, @on, Test).
paste_below_above(This, Below:bool, Test:[bool]) :->
	get(This, parent, Parent),
	(	get(This?object_cut_tree, root, Root),
		Root \= @nil
	->	(Test == @on
		->	true
		;	get(Root, copy_tree, PasteNode),
			send(Parent, son, PasteNode),
			send(This, modified,
			     paste_below_above(This, Below, PasteNode)),
			(Below == @on
			->	send(PasteNode, move_after, This)
			;	(get(This, preceding_son, Before)
				->	send(PasteNode, move_after, Before)
				;	send(PasteNode, move_after, @nil)
				)
			)
		)
	;Test == @on
	->	fail
	;	send(This, report,
			     warning,'Canot paste:no value or not and/or'),
		fail
	).

insert_above(This, Test:[bool]) :->
	send(This, insert_below_above, @off, Test).
insert_below(This, Test:[bool]) :->
	send(This, insert_below_above, @on, Test).
insert_below_above(This, Below:bool, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?parent, son, new(N, object_node)),
		(	Below == @on
		->	send(N, move_after, This)
		;	(get(This, preceding_son, Before)
			->	send(N, move_after, Before)
			;	send(N, move_after, @nil)
			)
		),
		send(This, modified, insert_below_above(This, Below, N))
	).
toggle_sub_sort(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This, is_sub_sort, This?is_sub_sort?negate),
		send(This, modified, toggle_subsort)
	),
	send(This, update_name).


initialise(This) :->
	send(This, send_super, initialise, @object_editor, @object_node_recogniser),
	send(This, update_name).


:- pce_global(@object_node_recogniser, mk_object_node_recogniser).
mk_object_node_recogniser(R) :-
	new(PQ, popup),
	(	member(Act, [edit,cut,toggle_sub_sort,paste_above,paste_below,
			     insert_above,
			     insert_below,copy]),
		send(PQ, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
		fail
	;	true
	),
        adapt_menu_items(PQ),
	new(R1, popup_gesture(PQ)),
	new(R, handler_group(R1)).

default_prop(object_name, '[object]').
base_name(This, Name:char_array) :<-
	get(This, prop_nil_def, object_name, Name1),
	(get(This, is_sub_sort, @on)
	->	new(Name, string('SUBSORT %s',Name1))
	;	Name = Name1
	).

:- pce_end_class.



:- pce_begin_class(comment_node, toplevel_node).

variable(content, char_array, both).

save_current_stream(This) :->
	format('/*'),
	get(This, content, C),
	get(C,size, S),
	S1 is S -1,
	(	between(0,S1, I),
		get(C, character, I, Char),
		put_byte(Char),
		fail
	;	format('*/~n')
	).

set_content(This, Content:char_array) :->
	(send(Content, equal, This?content)
	->	true
	;	send(This, modified, change_comment(This, Content)),
		send(This, content, Content),
		send(This, update_name)
	).

/*
fill_value(This, Header:prolog) :->
	pl_pce(Header, Header1),
	send(This, change_gui_prop, value, Header1).
fill_header(This, Header:prolog) :->
	pl_pce(Header, Header1),
	send(This, change_gui_prop, header, Header1).
apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([header, value], Sheet, [Header,Value]),
	((Acted == @default;Acted == header)
	->	send(This, change_gui_prop_term, header, Header, ReportVisual),
		send(This, change_gui_prop_term, value, Value, ReportVisual)
	;	send(This, change_gui_prop_term, value, Value, ReportVisual),
		send(This, change_gui_prop_term, header, Header, ReportVisual)
	).
*/
copy_local(This, New:comment_node) :<-
        new(New, comment_node(This?content)),
	send(New, update_name).
initialise(This, Comment:[char_array]) :->
	send(This, send_super, initialise, @toplevel_edit_recogniser),
	default(Comment, '', Comment1),
	send(This, content, Comment1),
	send(This, update_name).
editor(_This, E:comment_editor) :<-
        E = @comment_editor.


base_name(This, Name:char_array) :<-
        new(Name, string('COMMENT\n%s', ?(This, prop_nil_def, content))).

%variable(editor, comment_editor, both).

edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?editor, setup, This)
	).
double_click(This) :->
	send(This, edit).
:- pce_end_class.


:- pce_begin_class(constant_def_node, editable_toplevel_node).
variable(header, name*, both).
variable(value, name*, both).

default_prop(value, '[value]').

tr_term(This, Test:[bool], constant(Header, Value):prolog) :<-
        get(This?sons, size, N),
	assert_debug(N == 0),
	get(This, tr_pl_term_nil, header, [constant_name], Test, Header),
	get(This, tr_pl_term_nil, value, [value], Test, Value).
fill_value(This, Header:prolog) :->
	pl_pce(Header, Header1),
	send(This, change_gui_prop, value, Header1).
fill_header(This, Header:prolog) :->
	pl_pce(Header, Header1),
	send(This, change_gui_prop, header, Header1).
apply_all_editor(This, ReportVisual:visual, Sheet:sheet, Acted:[name]) :->
	get_sheet_vals([header, value], Sheet, [Header,Value]),
	((Acted == @default;Acted == header)
	->	send(This, change_gui_prop_term, header, Header, ReportVisual),
		send(This, change_gui_prop_term, value, Value, ReportVisual)
	;	send(This, change_gui_prop_term, value, Value, ReportVisual),
		send(This, change_gui_prop_term, header, Header, ReportVisual)
	).

copy_local(This, New:text_node) :<-
        new(New, constant_def_node),
	send(New, header, This?header),
	send(New, value, This?value),
	send(New, update_name).
initialise(This) :->
	send(This, send_super, initialise, @constant_def_editor,
	     @toplevel_edit_recogniser),
	send(This, update_name).



base_name(This, Name:char_array) :<-
        new(Name, string('CONSTANT %s=%s', ?(This, prop_nil_def, header),
			 ?(This, prop_nil_def, value))).
:- pce_end_class.




:- pce_begin_class(property_def_node, editable_header_toplevel_node).

variable(header, name*, both).
modify_op(This, Op:name) :->
	(memberchk(Op, [bareq_to_holds, holds_to_bareq])
	->	send(This, modify_formula, Op)
	;	send(This, send_super, modify_op, Op)
	).

modify_formula(This, Op:name) :->
	assert_debug(memberchk(Op, [bareq_to_holds, holds_to_bareq])),
	get(This, formula_node, FN),
	get(FN, modified_formula, Op, FN1),
	send(FN, delete_tree),
	send(This, son, FN1),
	send(This, modified, modify_formula(Op)).



formula_node(This, FN:formula_node) :<-
       (	get(This?sons, size, 1),
	       get(This?sons, head, FN),
	       send(FN, instance_of, formula_node)
       ->      true
       ;       send(This, report, error, 'Invalid property_def node'),
	       fail
       ).
formula(This, Term:prolog) :<-
       get(This, formula_node, FN),
       get(FN, tr_term, Term).


tr_term(This, Test:[bool], denotes(Header, Formula):prolog) :<-
        get(This?sons, size, N),
	assert_debug(N = 1),
	get(This?sons?head, tr_term, Test, Formula),
	get(This, tr_pl_term_nil, header, [property_name], Test, Header).

fill_header(This, Header:prolog) :->
	pl_pce(Header, Header1),
	send(This, change_header, Header1).



copy_local(This, New:text_node) :<-
        new(New, property_def_node(@off)),
	send(New, header, This?header).





initialise(This, AddChildren:[bool]) :->
	property_def_node_recogniser(Rec),
	send(This, send_super, initialise, @property_def_editor, Rec),
	(AddChildren == @off
	->	true
	;	send(This, son, new(formula_simple_node)),
		send(This, collapsed, @off),
		send(This, update_name)
	).

change_header(This, Name:char_array) :->
	ignore(send(This?frame?property_list, add_name, Name)),
	send(This, change_header0, Name).

change_header0(This, Name:char_array) :->
	send(This, change_gui_prop, header, Name).

base_name(This, Name:char_array) :<-
        new(Name, string('PROPERTY DEFINITION %s', ?(This, prop_nil_def, header))).
default_prop(header, '[name]').

check_property_quiet(This, Test:[bool]) :->
	send(This, check_property, @off, Test).
check_property_verbose(This, Test:[bool]) :->
	send(This, check_property, @on, Test).
check_property(This, Verbose:[bool], Test:[bool]) :->
	assert_debug(checking_enabled),
	assert_debug(current_module(ttlchecker)),
	send(This, check, Verbose, Test).

check(This, Verbose:[bool], Test:[bool]) :->
	assert_debug(current_module(ttlchecker)),
	ttlchecker:ttl_check_property(This, Verbose, Test).

:- pce_global(@advanced_check_options, mk_advanced_check_options).
mk_advanced_check_options(D) :-
	new(D, dialog(logging_options)),
	send(D, done_message, message(D, show, @off)),
	send(D, append, new(M, menu(output_what))),
	send_list(M, append,
		  [log_binding_succeeded_exist,
		   log_binding_failed_forall,
		   log_every_binding,
		   mark_formula_by_id_next,
		   log_to_file,
		   log_to_screen
		  ]),
	send(M, layout, vertical),
	send(M, multiple_selection, @on),
	send(M, clear_selection),
	forall(member(Opt, [log_binding_succeeded_exist,
			    log_binding_failed_forall,
			    log_to_file,
			    log_to_screen
			   ]),
	       send(?(M, member, Opt), selected, @on)
	      ),
	send(D, append, new(text_item(file, log))),
	send(D,  append, new(OB,
			     button(ok,
				    and(
					message(@pce, format, 'OK pressed\n'),
					message(D, return, ok))))),
	send(new(button(cancel, message(D, return, @nil))), right, OB).

substitute_properties(This, Test:[bool]) :->
	assert_debug(checking_enabled),
	assert_debug(current_module(ttlchecker)),
	ttlchecker:substitute_properties(This,Test).

check_new2(This, Test:[bool]) :->
	assert_debug(checking_enabled),
	assert_debug(current_module(ttlchecker)),
	(Test == @on
	->	ttlchecker:ttl_check_property(This, @on, Test)
	;	ttlchecker:ttl_check_property2(This, @on, Test)
	).
check_property_advanced(This, Test:[bool]) :->
	assert_debug(checking_enabled),
	assert_debug(current_module(ttlchecker)),
	(Test == @on
	->	ttlchecker:ttl_check_property(This, @on, Test)
	;	D = @advanced_check_options,
		send(@advanced_check_options, expose),
		(	get(D, confirm, Response)
		->	(	Response == @nil
			->	send(This, report, inform, 'check cancelled'),
				send(D, show, @off),
				fail
			;Response == ok
			->	send(D, show, @off),
				get(?(D, member, output_what), selection,
				    Opts),
				chain_list(Opts, OptsList),
				get(?(D,member,file), selection, TIS),
				get(TIS, value, TIF),
				V = advanced([file=TIF|OptsList]),
				format('Verbose= ~w~n', [V]),
				ttlchecker:ttl_check_property(This, V, Test)
			;	send(D, show, @off),
				send(This, report, error, 'Unhandled response %O',
				     Response),
				fail
			)
		;	send(This, report, inform, 'check cancelled'),
			send(D, show, @off),
			fail
		)
	).




property_def_node_recogniser(Rec) :-
	assert_debug(get_domain(checker)),
	(checking_enabled
	->	Rec = @property_def_node_recogniser_checker
	;	Rec = @property_def_node_recogniser
	).

:- pce_global(@property_def_node_recogniser, mk_property_def_node_recogniser).
mk_property_def_node_recogniser(R) :-
	mk_toplevel_recogniser([edit_header], R).
:- pce_global(@property_def_node_recogniser_checker,
	      mk_property_def_node_recogniser_checker).
mk_property_def_node_recogniser_checker(R) :-
	Opts = [edit_header,
		check_property_verbose,
		check_property_quiet,
		check_property_advanced,
		substitute_properties|Last],
	(is_local
	->	Last = [check_new2]
	;	Last = []
	),
	mk_toplevel_recogniser(Opts, R).

:- pce_end_class.














:- pce_begin_class(sort_node, editable_toplevel_node).

variable(sort_name, name*, both).
tr_term(This, Test:[bool], sortdef(Term, Objs):prolog) :<-
	get(This, tr_pl_term_nil, sort_name, [sort_name], Test, Term),
	get(This, sons, Sons),
	chain_list(Sons, SonsL),
	maplist(tr_term(Test), SonsL, Objs).

tr_term(Test, Node, Term) :-
	get(Node, tr_term, Test, Term).

add_object(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This, son, new(O,object_node)),
		send(This, modified, add_object(This,O))
	).




copy_local(This, New:sort_node) :<-
        new(New, sort_node),
	send(New, sort_name, This?sort_name).

initialise(This) :->
	send(This, send_super, initialise, @sort_editor,
	     @sort_node_recogniser),
	send(This, update_name).
base_name(This, Name:char_array) :<-
        new(Name, string('SORT %s', ?(This, prop_nil_def, sort_name))).
%	get(This, name1, Name).
is_default(This) :->
	get(This, sort_name, @nil).
default_prop(sort_name, '[sort]').

:- pce_global(@sort_node_recogniser, mk_sort_node_recogniser).
mk_sort_node_recogniser(R) :-
	mk_toplevel_recogniser([edit,add_object],R).
:- pce_end_class.



:- pce_begin_class(constant_node, editable_toplevel_node).


copy_local(_This, _New:text_node) :<-
        impl_error('Cannot copy generic constant_node'),
	fail.

:- pce_global(@toplevel_edit_recogniser, mk_toplevel_edit_recogniser).
mk_toplevel_edit_recogniser(R) :-
	mk_toplevel_recogniser([edit],R).





variable(constant0, char_array*, both).
variable(functor1, char_array, both).
variable(default1, char_array, both).

default_prop(constant0, '[value]').


initialise(This, Functor:{start_time, end_time, global_lambda, period,cwa,other,model},
	   Default:char_array) :->
	send(This, send_super, initialise, @constant_editor, @toplevel_edit_recogniser),
	send(This, functor1, Functor),
	send(This, default1, Default),
	send(This, update_name).

base_name(This, Name:char_array) :<-
        new(Name, string('%s(%s)', This?functor1, ?(This, prop_nil_def, constant0))).

change_value(This, Val:char_array) :->
	get(This, constant0, Val1),
	(	Val1 \== @nil,
		send(Val1, equal, Val)
	->	true
	;	send(This, constant0, Val),
		send(This, modified, change_value(This, Val)),
		send(This, update_name)
	).



:- pce_end_class.





:- pce_begin_class(interval_node, toplevel_node).

tr_term(This, Test:[bool], Term:prolog) :<-
        get(This, tr_vars_rest_pl_list, Test, res(Vars,N1)),
	(	N1 = [RangeNode|Rest],
		send(RangeNode, instance_of, range_node)
	->	get(RangeNode, tr_term, Test, Range),
		(	Rest = [PNode, FNode],
			send(PNode, instance_of, period_node),
			send(FNode, instance_of, formula_node)
		->	get(PNode, tr_term, Test, Period),
			get(FNode, tr_term, Test, Formula),
			Term = periodic(Vars, Range, Period, Formula)
		;	Rest = [FNode],
			send(FNode, instance_of, formula_node)
		->	get(FNode, tr_term, Test, Formula),
			Term = interval(Vars, Range, Formula)
		;	impl_error('Inconsistent interval_node')
		)
	;	impl_error('Inconsistent interval_node')
	).

copy_local(_This, New:text_node) :<-
        new(New, interval_node(@off)).

initialise(This, AddChildren:[bool]) :->
	send(This, send_super, initialise, @interval_node_recogniser),
	(AddChildren == @off
	->	true
	;	send(This, son, new(range_node)),
		send(This, son, new(formula_simple_node)),
		send(This, collapsed, @off),
		send(This, update_name)
	).


base_name(_This, Name:char_array) :<-
        Name = interval.



add_period(This, Test:[bool]) :->
	get(This, add_period, Test, _).

add_period(This, Test:[bool], PN:period_node*) :<-
	(get(This?sons, find, message(@arg1, instance_of, period_node),PN)
        ->	(Test == @on
		->	fail
		;	send(This, report, error,
			     'Cannot add period:already there'),
			fail
		)
	;	Test == @on
	->	PN = @nil
	;	send(This, son, new(PN, period_node)),
		(	get(This, find,
			    message(@arg1, instance_of, formula_node),FN),
			get(FN, preceding_son, RN),
			assert_debug(send(RN, instance_of, range_node))
		->	send(PN, move_after, RN),
			send(This, modified, add_period(This, PN))
		;	impl_error('inconsistent interval')
		)
	).

remove_period(This, Test:[bool]) :->
	(get(This?sons, find, message(@arg1, instance_of, period_node),PN)
        ->	(Test == @on
		->	true
		;	send(This, modified, remove_period(This,PN)),
			send(PN, delete_tree)
		)
	;Test == @on
	->	fail
	;	send(This, report, error, 'Cannot remove period:not there'),
		fail
	).

add_var_end(This, VN:var_node) :<-
        send(This, son, new(VN,var_node)),
	send(This, modified, add_var_end(This, VN)),
        (get(This?sons, find, not(message(@arg1, instance_of, var_node)),
	     Next),
		get(This?sons, index, Next, NI),
		NI > 1
	->	NI1 is NI - 1,
		get(This?sons, nth1, NI1, PV),
		send(VN, move_after, PV)
	;	send(VN, move_after, @nil)
	).
add_var(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This, son, new(VN,var_node)),
		send(This, modified, add_var(This, VN)),
		send(VN, move_after, @nil)
	).


:- pce_global(@interval_node_recogniser, mk_interval_node_recogniser).
mk_interval_node_recogniser(R) :-
	mk_toplevel_recogniser([add_var,add_period,remove_period], R).


:- pce_end_class.



mk_toplevel_recogniser(Local,R) :-
	new(Pop, popup),
	(	member(Act, Local),
		send(Pop, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
		fail
	;	(Local == []
		->	true
		;	send(Pop?members?tail, end_group, @on)
		)
	),
	(	toplevel_op(Act),
		send(Pop, append, menu_item(Act,
					   message(@arg1, Act),
					   @default, @default,
					   message(@arg1, Act, @on))),
		fail
	;	true
	),
        adapt_menu_items(Pop),
	new(R, popup_gesture(Pop)).





tr_vars([VN|NIn], Test, NOut, This, [Var|Vars]) :-
	send(VN, instance_of, var_node),
	!,
	get(VN, tr_term, Test, Var),
	tr_vars(NIn, Test, NOut, This, Vars).
tr_vars(NIn, _Test, NIn, _This, []).


:- pce_begin_class(leadsto_node, toplevel_node).


update_name(This) :->
	send(This, send_super, update_name),
	send(This?sons, for_all, message(@arg1, update_name)).

update_sons(This) :->
	get(This, sons, Sons),
	%get(Sons, size, S),
	send(Sons, for_all, message(@arg1, update_name)).

son(This, Node:extra_node) :->
	send(This, send_super, son, Node),
	send(This, update_sons).






formula_role(This, Node:formula_node, Which:{antecedent,consequent}) :<-
	get(This?sons, size, N),
	N > 1,
	(	get(Node, following_son, NN),
		send(NN, instance_of, formula_node)
	->	Which = antecedent
	;	get(Node, preceding_son, NN),
		send(NN, instance_of, formula_node)
	->	Which = consequent
	;	fail
	).
%	;	send(@pce, format, 'N:%s I:%s\n', N, I)



tr_term(This, Test:[bool], leadsto(Vars, Ante, Conse, Delay):prolog) :<-
        get(This, tr_vars_rest_pl_list, Test, res(Vars,N1)),
	(N1 = [AnteNode,ConseNode|N2]
	->	true
	;	impl_error('Inconsistent leadsto_node')
	),
	get(AnteNode, tr_term, Test, Ante),
	get(ConseNode, tr_term, Test, Conse),
	(N2 == []
	->	Delay = standard
	;	N2 = [DN]
	->	get(DN, tr_term, Test, Delay)
	;	impl_error('Inconsistent leadsto_node')
	).



copy_local(_This, New:text_node) :<-
        new(New, leadsto_node(@off)).

initialise(This, AddChildren:[bool]) :->
	send(This, send_super, initialise, @leadsto_node_recogniser),
	(AddChildren == @off
	->	true
	;	send(This, son, new(formula_simple_node)),
		send(This, son, new(formula_simple_node)),
		send(This, collapsed, @off),
		send(This, update_name)
	).


base_name(_This, Name:char_array) :<-
        Name = leadsto.



add_var(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This, son, new(VN,var_node)),
		send(VN, move_after, @nil),
		send(This, modified, lt_add_var(This, VN))
	).
add_var_end(This, VN:var_node) :<-
        send(This, son, new(VN,var_node)),
        (get(This?sons, find, not(message(@arg1, instance_of, var_node)),
	     Next),
		get(This?sons, index, Next, NI),
		NI > 1
	->	NI1 is NI - 1,
		get(This?sons, nth1, NI1, PV),
		send(This, modified, add_var_end(This, VN)),
		send(VN, move_after, PV)
	;	send(VN, move_after, @nil)
	).



has_delay(This, Delay:delay_node) :<-
        get(This?sons, tail, Delay),
	send(Delay, instance_of, delay_node).


add_delay(This, Test:[bool]) :->
	(Test == @on
	->	\+ get(This, has_delay, _)
	;	get(This, has_delay, _)
	->	send(This, report, error, 'Delay already present'),
		fail
	;	send(This, son, new(DN,delay_node)),
		send(This, modified, add_delay(This, DN))
	).

paste_delay(This, Test:[bool]) :->
	(Test == @on
	->	\+ get(This, has_delay, _),
		get(This?delay_cut_tree, root, R),
		R \= @nil
	;	get(This, has_delay, _)
	->	send(This, report, error, 'Delay already present'),
		fail
	;	get(This?delay_cut_tree, root, R),
		(R == @nil
		->	send(This, report, error, 'Delay paste buffer empty'),
			fail
		;	get(R, copy_tree, Son),
			send(This, son, Son),
			send(This, modified, paste_delay(This, Son, R))
		)
	).

:- pce_global(@leadsto_node_recogniser, mk_leadsto_node_recogniser).
mk_leadsto_node_recogniser(R) :-
	mk_toplevel_recogniser([add_var, add_delay, paste_delay],R).


:- pce_end_class.


/*
  * For the time being part_node is the root of a specification
  */
:- pce_begin_class(part_node, text_node).

variable(collapsed_name, name, both).
variable(base_name, name, both).

copy_local(This, New:part_node) :<-
        new(New, part_node(This?base_name)).

initialise(This, Domain:{leadsto,checker},Name:char_array, CollapsedName:[char_array]) :->
	part_node_recogniser(Domain, Recogniser),
	send(This, send_super, initialise, Recogniser),
	ensure_domain(Domain),
	default(CollapsedName, Name, CN),
	send(This, base_name, Name),
	send(This, collapsed_name, CN),
	send(This, update_name).





update_name(This) :->
	(get(This, collapsed, @on)
	->	send(This, name, This?collapsed_name)
	;	get(This, base_name, Name),
		send(This, name, Name)
	).

unique_node(Class) :-
	get(@pce, instance, class, Class, P),
	send(P, is_a, constant_node),
	\+ send(P, is_a, multi_constant_node).


may_add(This, Class:name, Test:[bool]) :->
	(unique_node(Class)
	->	(get(This?sons, find, message(@arg1, instance_of, Class), _)
		->	(Test == @on
			->	fail
			;	send(This, report, error, 'Duplicate %s',
				     Class),
				fail
			)
		;	true
		)
	;	true
	).



add1(This, Class:toplevel_kind,Test:[bool]) :->
	get(This, add1, Class, Test, _).

add1(This, Class:toplevel_kind,Test:[bool],Son:extra_node*) :<-
	send(This, may_add, Class, Test),
	(Test == @on
	->	Son = @nil
	;	get(@pce, instance, Class, Son),
		send(This, son, Son),
		send(Son, move_after, @nil),
		send(This, modified, add1(This, Class, Son))
	).
addend(This, Class:toplevel_kind,Son:extra_node*) :<-
	send(This, may_add, Class, @off),
	get(@pce, instance, Class, Son),
	send(This, son, Son),
	send(This, modified, addend(This, Class, Son)).

part_node_recogniser(checker, @part_node_recogniser_checker).
part_node_recogniser(leadsto, @part_node_recogniser_leadsto).
:- pce_global(@part_node_recogniser_checker, mk_part_node_recogniser_checker).
mk_part_node_recogniser_checker(R) :-
	mk_part_node_recogniser(checker, R).
:- pce_global(@part_node_recogniser_leadsto, mk_part_node_recogniser_leadsto).
mk_part_node_recogniser_leadsto(R) :-
	mk_part_node_recogniser(leadsto, R).

mk_part_node_recogniser(Domain, R) :-
        %format('HIER~n'),
	new(P, popup),
	(	toplevel_element(Domain,Act, Class),
		send(P, append, menu_item(Act,
					   message(@arg1, add1, Class),
					   @default, @default,
					   message(@arg1, add1, Class, @on))),
		fail
	;	true %send(P?members?tail, end_group, @on)
	),
        %format('Dan~n'),
        adapt_menu_items(P),
        %format('VERDER~n'),
	new(R, popup_gesture(P)).

:- pce_end_class.



:- pce_begin_class(extra_node, node).


contains_generic_local(_This) :->
	fail.

contains_generic(This) :->
	(send(This, contains_generic_local)
	->	true
	;	get(This?sons, find, message(@arg1, contains_generic),_)
	).

tr_vars_rest_pl_list(This, Test, res(Vars, Nodes):prolog) :<-
        get(This, sons, ChildrenNodes),
	chain_list(ChildrenNodes, NIn),
	tr_vars(NIn, Test, Nodes, This, Vars).


tr_pl_atom_nil(This, ArgName, NilName:prolog, Test:[bool],Term:prolog) :<-
        get(This, ArgName, Name1),
	(Name1 == @nil
	->	(Test == @on
		->	fail
		;	Term = NilName,
			send(This, report, warning,
			     'Using default for argument %s',
			     ArgName)
		)
	;	get(Name1, value, Name),
		valid_term(This, Name, Term),
		(atom(Term)
		->	true
		;	send(This, report,
			     error,'Expected name, not compound:%s', Term),
			local_trace(wh)
		)
	).

prop_nil_def(This, ArgName:name, Name:char_array) :<-
        get(This, ArgName, Name1),
	(Name1 == @nil
	->	(default_prop(ArgName, Name)
		->	true
		;	pr_error('Missing default property for ~w',
			     ArgName),
			Name = 'UNDEFINED'
		)
	;	Name = Name1
	).

val_or_default(This, What:name, Default:name, Res:name) :<-
        get(This, What, W),
	(W == @nil
	->	Res = Default
	;	Res = W
	).


tr_term_nil(This, ArgName:name, NilName:prolog, Test:[bool],Term:prolog) :<-
        get(This, ArgName, Name1),
	(Name1 == @nil
	->	(Test == @on
		->	fail
		;	Term = NilName,
			send(This, report, warning,
			     'Using default for argument %s',
			     ArgName)
		)
	;	get(Name1, value, Name),
		any_term(This, Name, Term)
	).
% Test == @on => silently fail if contains placeholder
tr_pl_term_nil(This, ArgName:name, NilName:prolog, Test:[bool],Term:prolog) :<-
        get(This, ArgName, Name1),
	(Name1 == @nil
	->	(Test == @on
		->	fail
		;	Term = NilName,
			send(This, report, warning,
			     'Using default for argument %s',
			     ArgName)
		)
	;	get(Name1, value, Name),
		valid_term(This, Name, Term)
	).
tr_pl_term_bindings_nil(This, ArgName:name, NilName:prolog, Term:prolog) :<-
        get(This, ArgName, Name1),
	(Name1 == @nil
	->	Term = tb(NilName,[]),
		send(This, report, warning, 'Using default for argument %s',
		     ArgName)
	;	get(Name1, value, Name),
		valid_term_no_bind(This, Name, Term1, B),
		Term = tb(Term1, B)
	).

proto_class_name(formula, formula_simple_node).
proto_class_name(var, var_node).
proto_class_name(pxor_formula, pxor_formula_node).


modified(This, By:prolog) :->
	(get(This, frame, F)
	->	send(F, modified, By),
		send(F, clear_report)
	;	true
	).
delay_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, delay_cut_tree, CT).
var_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, var_cut_tree, CT).
conditional_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, conditional_cut_tree, CT).
delay_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, delay_cut_tree, CT).
formula_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, formula_cut_tree, CT).
object_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, object_cut_tree, CT).
toplevel_cut_tree(This, CT:tree) :<-
        get(This?ensure_frame, toplevel_cut_tree, CT).
ensure_frame(This, F:frame) :<-
        (get(This, frame, F)
	->	true
	;	impl_error('No frame'),
		fail
	).

create_proto(_This, ProtoKind:name, N:extra_node) :<-
        assert_debug(is_proto_kind(ProtoKind)),
	(proto_class_name(ProtoKind, ClassName)
	->	get(@pce, instance, ClassName, N)
	;	impl_error('Unhandled proto kind ~w', [ProtoKind])
	).


initialise(This, G:graphical, R:recogniser) :->
	send(This, send_super, initialise, G),
	send(This, recogniser, R),
	send(This, send_super, collapsed, @nil).

preceding_son(This, Preceding:extra_node) :<-
        get(This?parent?sons, index, This, I),
	I > 1,
	I1 is I - 1,
	get(This?parent?sons, nth1, I1, Preceding).
following_son(This, Preceding:extra_node) :<-
        get(This?parent?sons, index, This, I),
	I1 is I + 1,
	get(This?parent?sons, nth1, I1, Preceding).

copy_tree_to(This, AsSonOrRoot:tree, NewNode:extra_node) :<-
        get(This, copy_tree, NewNode),
	send(AsSonOrRoot, root, NewNode).

copy_tree(This, NewNode:extra_node) :<-
        get(This, copy_local, NewNode),
	send(This?sons, for_all, message(NewNode, son, @arg1?copy_tree)).

double_click(This) :->
	send(This, report, inform, 'No default action for %s\n', This?class?name).


delete_all_sons(This) :->
	repeat,
	(	get(This, sons, Sons),
		(	(Sons == @nil
			->	true
			;	send(Sons, empty)
			->	true
			;	fail
			)
		->	!
		;	send(Sons?head, delete_tree),
			fail
		)
	).

single_recogniser(This, R:recogniser) :<-
        get(This, all_recognisers, RC),
	get(RC, size, N),
	assert_debug(N == 1),
	get(RC, head, R).


move(This, Node:extra_node) :->
	send(This, send_super, move, Node),
	(send(Node, instance_of, formula_node)
	->	send(Node, update_name)
	;	true
	),
	send(This, collapsed, @off).
son(This, Node:extra_node) :->
	"Add a son and normalise"::
	send(This, send_super, son, Node),
/*	(	get(This, window, W),
		W \== @nil
	->	send(W, normalise, Node?image)
	;	true
	),
*/	(send(Node, instance_of, formula_node)
	->	send(Node, update_name)
	;	true
	),
	send(This, collapsed, @off).


ifparent(This, Parent:node) :<-
        get(This, parents, P),
	P \== @nil,
	get(P, size, N),
	N = 1,
	get(P, head, Parent).

parent(This, Parent:node) :<-
        get(This, parents, P),
	P \== @nil,
	get(P, size, N),
	assert_debug(N = 1),
	get(P, head, Parent).

delete_tree(This) :->
	(	get(This, ifparent, P),
		get(P?sons, size, N),
		(	N > 1
		->	(send(P, instance_of, leadsto_node)
			->	send(P, update_sons)
			;	true
			)
		;	send(P, collapsed, @nil)
		)
	->	true
	;	true
	),
	send(This, send_super, delete_tree).

:- pce_end_class.

prop_comment(_Attr, _Comment) :-
	fail.

node_prop_options(Attr, Options) :-
	prop_info(Attr, _Value, _Message, Options).
prop_message(Attr, Value, Message) :-
	prop_info(Attr, Value, Message, _ConfigOptions).
prop_info(Attr, Value, message(@arg1?text, Attr, Value), Options) :-
	member(Attr, [colour, font, background, border, pen]),
	get(@pce, convert, text, class, TextClass),
	get(TextClass, send_method, Attr, Method),
	get(Method, types, Ts),
	get(Ts, size, S),
	assert_debug(S == 1),
	get(Ts, element, 1, Tp),
	get(Tp, name, TypeName1),
	(tr_type(TypeName1, TypeName, Attr, Default)
	->	true
	;	TypeName = TypeName1,
		Default = @default
	),
	(prop_comment(Attr, Comment)
	->	true
	;	get(Method?summary, value, Comment)
	),
	Options = [type(TypeName),comment(Comment)|O1],
	(	get(TextClass, class_variable, Attr, CV),
		get(CV, value, CVal),
		CVal \== @default,
		object(CVal, DefVal)
	->	true
	;	DefVal = Default
	),
	O1 = [default(DefVal)].




tr_type('0..', name, _, 0).

tr_type('[colour|pixmap]', colour, colour, black).
tr_type('[colour|pixmap]*', colour, background, white).



:- pce_begin_class(editable_text_node, text_node).

update_name(This) :->
	send(This, send_super, update_name),
	send(This?editor, update1, This).

variable(editor, single_node_editor, both).

initialise(This, Editor:single_node_editor, R:recogniser) :->
	send(This, send_super, initialise, R),
	send(This, editor, Editor).

edit(This, Test:[bool]) :->
	(Test == @on
	->	true
	;	send(This?editor, setup, This)
	).
double_click(This) :->
	send(This, edit).
unlink(This) :->
	(get(This?editor, for, This)
	->	send(This?editor, dismiss)
	;	true
	),
	send(This, send_super, unlink).
:- pce_end_class.

:- pce_begin_class(text_node, extra_node).

change_root_prop(This, Prop:name, Value:any) :->
	send(@pce, format, 'crp:%s  %O\n', Prop, Value),
	(prop_message(Prop, Value, Message)
	->	send(This, for_some, Message)
	;	send(This, report, warning, 'Cannot set property %s', Prop)
	).

copy_local(This, New:text_node) :<-
        new(New, text_node(This?single_recogniser)).


change_gui_prop_term(This, ArgName:name, Name:char_array,
		     ReportVisual:[visual]) :->
	combine_reporteds(This, ReportVisual, Msg),
	valid_term(Msg, Name, _),
	send(This, change_gui_prop, ArgName, Name).

change_gui_prop(This, ArgName:name, Name:char_array) :->
	get(This, ArgName, Name1),
	(	Name1 \== @nil,
		send(Name1, equal, Name)
	->	true
	;	send(This, ArgName, Name),
		send(This, modified, change_prop(This, ArgName, Name)),
		send(This, update_name)
	).

name(This, N:char_array) :->
        send(This?image, string, N).

update_name(This) :->
	(get(This, collapsed, @on)
	->	send(This, name, This?collapsed_name)
	;	get(This, base_name, Name),
		send(This, name, Name)
	).

collapsed_name(This, Name:char_array) :<-
	new(Name, string('%s',This?base_name)),
	get(This?sons, size, N),
	(N == 0
	->	true
	;	send(Name, append, '('),
		chain_iterate(This?sons,message(Name,append, @arg1?collapsed_name),
				message(Name, append, ', ')),
		send(Name, append, ')')
	).

collapsed(This, OnOff:bool*) :->
	send(This, send_super, collapsed, OnOff),
	send(This, update_name).

text(This, T:text) :<-
        get(This, image, T).

initialise(This, R:recogniser) :->
	send(This, send_super, initialise, new(text('UNDEFINED')),R),
	(	prop_message(Prop, Value, Message),
		get_config(form_config:tree/node/Prop, Value),
		(Value == default
		->	fail
		;	send(Message, forward, This),
			fail
		)
	;	true
	),
	send(This, send_super, collapsed, @nil).



:- pce_end_class.












































































































































































































































