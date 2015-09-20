:- module(parteditors,[]).

:- use_module(library(pce)).
:- use_module(util).
:- use_module(library(lists)).

:- discontiguous sn_editor_info/2.

:- pce_begin_class(single_node_editor, dialog).

add_element(text_item(Name, _NodeAct, _NodeQ),This) :-
	send(This, append,
	     new(TI,text_item(Name, @default,
			      message(This, text_item_act, Name,@arg1)))),
	send(TI, hor_stretch, 100).
add_element(menu(Name, Members, _SelectedMember, _Change), This) :-
	send(This, append, new(M, menu(Name, choice,
				message(This, menu_act, Name, @arg1)))),
	send_list(M, append, Members).
add_element(editor(_Name, Size), This) :-
	send(This, append, new(E,editor)),
	send(E, size, Size).



variable(for,  extra_node*, both).
variable(kind, name, both).

initialise(This, Kind:name) :->
	sn_editor_info(Kind, setup(Header, _NodeClass)),
	send(This, send_super, initialise, Header),
	send(This, kind, Kind),
	send(This, for, @nil),
	(	sn_editor_info(Kind, Element),
		add_element(Element, This),
		fail
	;	true
	),
	send(This, append, button(apply, message(This, apply1))),
	send(This, append, button(ok, message(This, ok))),
	send(This, append, button(cancel, message(This, dismiss))),
	send(This, append, new(reporter)),
	send(This, done_message, message(This, dismiss)),
	send(This, resize_message, message(This, layout, @arg2)).


ok(This) :->
	send(This, apply1),
	send(This, dismiss).

apply_element(text_item(Name, _NodeAct, _NodeQ), This, Error) :-
	(send(This, text_item_act, Name,?(This,member,Name)?selection)
	->	true
	;	Error = error
	).

apply_element(menu(Name, _Members, _SelectedMember, _Change), This, Error) :-
	(send(This, menu_act, Name, ?(This,member,Name)?selection)
	->	true
	;	Error = error
	).

apply_all_member(text_item(Name, _NodeAct, _NodeQ),Name).
apply_all_member(menu(Name, _Members, _SelectedMember, _Change),Name).

apply_all_element(This, Kind, Attr) :-
	sn_editor_info(Kind, Element),
	apply_all_member(Element, Name),
	get(This, member, Name, Member),
	get(Member, selection, Sel),
	get(Sel, value, P),
	new(Attr, attribute(Name, P)).

attr_sheet(This, Sheet:sheet) :<-
        get(This, kind, Kind),
	new(Sheet, sheet),
	(	apply_all_element(This, Kind, Attribute),
		send(Sheet, append, Attribute),
		fail
	;	true
	).

apply_all(This, Acted:[name]) :->
	get(This, attr_sheet, Sheet),
	send(This?for, apply_all_editor, This, Sheet, Acted).


apply1(This) :->
	get(This, kind, Kind),
	(sn_editor_info(Kind, apply_all)
	->	send(This, apply_all)
	;	(	sn_editor_info(Kind, Element),
			apply_element(Element, This, Error),
			Error == error
		->	fail
		;	send(This, report, inform, '')
		)
	).



dismiss(This) :->
	send(This, transient_for, @nil),
	send(This, for, @nil),
	send(This, show, @off).

menu_act(This, Name:name, Selection:char_array) :->
	assert_debug(\+ get(This, for, @nil)),
	get(This, kind, Kind),
	sn_editor_info(Kind, menu(Name, Members, _SelectedMember, Change)),
	assert_debug(memberchk(Selection, Members)),
	(sn_editor_info(Kind, apply_all)
	->	send(This, apply_all,Name)
	;	Change =.. C,
		append(C, [Selection], C1),
		C2 =.. C1,
		send(This?for, C2)
	).

text_item_act(This, Name:name, Selection:char_array) :->
	assert_debug(\+ get(This, for, @nil)),
	get(This, kind, Kind),
	sn_editor_info(Kind, text_item(Name, NodeAct, _NodeQ)),
	(sn_editor_info(Kind, apply_all)
	->	send(This, apply_all,Name)
	;send(Selection, equal, '')
	->	NodeAct =.. N1,
		append(N1, [''], N2),
		N3 =.. N2,
		send(This?for, N3)
	;valid_terml(This, Selection, _Term)
	->	NodeAct =.. N1,
		append(N1, [Selection], N2),
		N3 =.. N2,
		send(This?for, N3),
		send(This, report, inform, ' ')
	;	fail
	).

release_node(This, Node:extra_node) :->
	(get(This, for, Node)
	->	send(This, dismiss)
	;	true
	).

update1(This, Node:extra_node) :->
	(get(This, for, Node)
	->	send(This, update_node)
	;	true
	).

qcall(Object, Method, Q) :-
	Method =.. Args,
	Q =.. [(?), Object|Args].

update_element(text_item(Name, _NodeAct, NodeQ), This) :-
	qcall(This?for, NodeQ, Q),
	send(?(This, member, Name), selection, Q).
update_element(menu(Name, Members, SelectedMember, _Change), This) :-
	get(This?for, SelectedMember, Sel),
	assert_debug(memberchk(Sel, Members)),
	send(?(This, member, Name), selection, Sel).

update_node(This) :->
	get(This, for, For),
	get(This, kind, Kind),
	sn_editor_info(Kind, setup(_Header, NodeClass)),
	assert_debug(get(@pce, convert, For, NodeClass, For)),
	(	sn_editor_info(Kind, Element),
		update_element(Element, This),
		fail
	;	true
	).

setup(This, VN:extra_node) :->
	send(This, for, VN),
	send(This, report, inform, ''),
	send(This, transient_for, VN?frame),
	get(This, kind, Kind),
	sn_editor_info(Kind, setup(_Header, NodeClass)),
	assert_debug(get(@pce, convert, VN, NodeClass, VN)),
	send(This, create),
	send(This, expose),
	send(This, update_node).




:- pce_end_class.

mk_sn_part_editor(Kind, Object) :-
	assert_debug(ground(Kind)),
	assert_debug(sn_editor_info(Kind, _)),
	new(Object, single_node_editor(Kind)).




/******************************

  CONDITIONAL VAR EDITOR

*******************************/
sn_editor_info(conditional_var_editor, setup('CONDITIONAL VARIABLE EDITOR',
					     conditional_var_node)).
sn_editor_info(conditional_var_editor, apply_all).

sn_editor_info(conditional_var_editor, text_item(then, change_then, then1)).
sn_editor_info(conditional_var_editor, text_item(else, change_else, else1)).

sn_editor_info(conditional_var_editor, text_item(variable_name,change_var_name,
				     var_name1)).
sn_editor_info(conditional_var_editor, text_item(sort_name, change_sort_name,
				     sort_name1)).

:- pce_global(@conditional_var_editor, mk_sn_conditional_var_editor).
mk_sn_conditional_var_editor(Object) :-
	mk_sn_part_editor(conditional_var_editor, Object).

  
               /************************
	       *
	       *   V A R   E D I T O R
	       *
	       ************************/

sn_editor_info(var_editor, setup('Variable Editor', var_node)).
sn_editor_info(var_editor, apply_all).


sn_editor_info(var_editor, text_item(variable_name,change_var_name,
				     var_name1)).
sn_editor_info(var_editor, text_item(sort_name, change_sort_name,
				     sort_name1)).
sn_editor_info(var_editor, text_item(whole_term, change_whole_term, whole_name1)).
:- pce_global(@var_editor, mk_sn_var_editor).
mk_sn_var_editor(Object) :-
	mk_sn_part_editor(var_editor, Object).




               /************************
	       *
	       *   A T O M   E D I T O R
	       *
	       ************************/
sn_editor_info(atom_editor, setup('Atom Editor', atom_node)).
sn_editor_info(atom_editor, text_item(name,change_term_name,name1)).
:- pce_global(@atom_editor, mk_sn_atom_editor).
mk_sn_atom_editor(Object) :-
	mk_sn_part_editor(atom_editor, Object).

               /************************
	       *
	       *   P R O P E R T Y    E D I T O R
	       *
	       ************************/
sn_editor_info(property_editor, setup('Property Editor', property_node)).
sn_editor_info(property_editor, text_item(name,change_term_name,name1)).
:- pce_global(@property_editor, mk_sn_property_editor).
mk_sn_property_editor(Object) :-
	mk_sn_part_editor(property_editor, Object).



               /************************
	       *
	       *   P R O P E R T Y   D E F   E D I T O R
	       *
	       ************************/
sn_editor_info(property_def_editor, setup('Property Definition Editor', property_def_node)).
sn_editor_info(property_def_editor, text_item(header,change_header,
					      prop_nil_def(header))).
:- pce_global(@property_def_editor, mk_sn_property_def_editor).
mk_sn_property_def_editor(Object) :-
	mk_sn_part_editor(property_def_editor, Object).



               /************************
	       *
	       *   C O N S T A N T   D E F   E D I T O R
	       *
	       ************************/
sn_editor_info(constant_def_editor,
	       setup('Constant Editor', constant_def_node)).
sn_editor_info(constant_def_editor, text_item(header,change_gui_prop(header),
					      prop_nil_def(header))).
sn_editor_info(constant_def_editor, text_item(value,change_gui_prop(value),
					      prop_nil_def(value))).
sn_editor_info(constant_def_editor, apply_all).

:- pce_global(@constant_def_editor, mk_sn_constant_def_editor).
mk_sn_constant_def_editor(Object) :-
	mk_sn_part_editor(constant_def_editor, Object).




               /************************
	       *
	       *   S O R T   E D I T O R
	       *
	       ************************/
sn_editor_info(sort_editor, setup('Sort Editor', sort_node)).
sn_editor_info(sort_editor, text_item(sort_name,
				      change_gui_prop(sort_name),
				      prop_nil_def(sort_name))).


:- pce_global(@sort_editor, mk_sn_sort_editor).
mk_sn_sort_editor(Object) :-
	mk_sn_part_editor(sort_editor, Object).



               /************************
	       *
	       *   O B J E C T   E D I T O R
	       *
	       ************************/
sn_editor_info(object_editor, setup('Object Editor', object_node)).
sn_editor_info(object_editor, text_item(object_name,
					change_gui_prop(object_name),
					prop_nil_def(object_name))).


:- pce_global(@object_editor, mk_sn_object_editor).
mk_sn_object_editor(Object) :-
	mk_sn_part_editor(object_editor, Object).




               /************************
	       *
	       *   D E L A Y   E D I T O R
	       *
	       ************************/
sn_editor_info(delay_editor, setup('Delay Editor', delay_node)).
sn_editor_info(delay_editor, text_item(delay,change_gui_prop(delay),prop_nil_def(delay))).


:- pce_global(@delay_editor, mk_sn_delay_editor).
mk_sn_delay_editor(Object) :-
	mk_sn_part_editor(delay_editor, Object).




               /************************
	       *
	       *   R A N G E   E D I T O R
	       *
	       ************************/
sn_editor_info(range_editor, setup('Range Editor', range_node)).
sn_editor_info(range_editor, text_item(range,change_range,range1)).
:- pce_global(@range_editor, mk_sn_range_editor).
mk_sn_range_editor(Object) :-
	mk_sn_part_editor(range_editor, Object).




sn_editor_info(constant_editor, setup('Constant Editor',
				      'constant_node|period_node')).
sn_editor_info(constant_editor,
	       text_item(constant,change_gui_prop(constant0),
			 prop_nil_def(constant0))).

:- pce_global(@constant_editor, new(constant_editor)).
:- pce_begin_class(constant_editor, single_node_editor).
initialise(This) :->
	send(This, send_super, initialise, constant_editor).
setup(This, VN:'constant_node|period_node') :->
	send(This, send_super, setup, VN),
	sn_editor_info(constant_editor, setup(Header,_)),
	get(VN, functor1, FN),
	concat_atom([Header, ' for ', FN], H1),
	send(This, label, H1),
	send(?(This, member, constant), label, FN).

:- pce_end_class.


:- pce_global(@constant_editor, new(constant_editor)).

sn_editor_info(comment_editor, setup('Comment Editor',
				      'comment_node')).
sn_editor_info(comment_editor,
	       text_item(constant,change_gui_prop(content),
			 prop_nil_def(content))).



:- pce_global(@comment_editor, mk_comment_editor).
mk_comment_editor(E) :-
	new(E, comment_editor),
	send(E, open),
	send(E, show, @off).

tst1 :-
	new(P, tst1),
	send(P, open).

:- pce_begin_class(tst1, frame).

initialise(This) :->
	send(This, send_super, initialise,test),
	send(This, append, new(V,view)),
	send(new(D, dialog), below, V),
	send(D, append, button(apply, message(This, apply1))),
	send(D, append, button(ok, message(This, ok))),
	send(D, append, button(cancel, message(This, dismiss))),
	send(D, append, new(reporter)),
	send(This, done_message, message(This, dismiss)),
	send(D, resize_message, message(D, layout, @arg2)).

:- pce_end_class.

:- pce_begin_class(comment_editor, frame).

initialise(This) :->
	send(This, send_super, initialise, comment_editor),
	send(This, append, new(V,view)),
	send(new(D, dialog), below, V),
	send(D, append, button(apply, message(This, apply1))),
	send(D, append, button(ok, message(This, ok))),
	send(D, append, button(cancel, message(This, dismiss))),
	send(D, append, new(reporter)),
	send(This, done_message, message(This, dismiss)),
	send(D, resize_message, message(D, layout, @arg2)).

variable(for, comment_node*, both).

setup(This, VN:comment_node) :->
	send(This, for, VN),
	send(This, report, inform, ''),
	send(This, transient_for, VN?frame),
	send(?(This, member, view)?text_buffer, contents, VN?content),
	send(This, create),
	send(This, expose).

 
dismiss(This) :->
	send(This, transient_for, @nil),
	send(This, for, @nil),
	send(This, show, @off).
ok(This) :->
	send(This, apply1),
	send(This, dismiss).
apply1(This) :->
	send(This?for, set_content, ?(This, member, view)?text_buffer?contents).



/*
setup(This, VN:'comment_node') :->
	send(This, send_super, setup, VN),
	sn_editor_info(comment_editor, setup(Header,_)),
	concat_atom([Header], H1),
	send(This, label, H1),
	send(?(This, member, constant), label, comment).
*/
:- pce_end_class.




sn_editor_info(probability_editor, setup('Probability Editor',
				      'pxor_formula_node')).
sn_editor_info(probability_editor,
	       text_item(probability,change_probability,
			 probability)).

:- pce_global(@probability_editor, new(probability_editor)).
:- pce_begin_class(probability_editor, single_node_editor).
initialise(This) :->
	send(This, send_super, initialise, probability_editor).
setup(This, VN:'pxor_formula_node') :->
	send(This, send_super, setup, VN),
	sn_editor_info(probability_editor, setup(Header,_)),
	get(VN, probability, FN),
	concat_atom([Header], H1),
	send(This, label, H1),
	send(?(This, member, probability), selection, FN).

:- pce_end_class.




               /************************
	       *
	       *   H O L D S   E D I T O R
	       *
	       ************************/
sn_editor_info(holds_editor, setup('Holds Editor',holds_node)).
sn_editor_info(holds_editor, text_item(state,change_state,state1)).
sn_editor_info(holds_editor, text_item(atom,change_atom_name,atom_name1)).
sn_editor_info(holds_editor, menu(truth_value,
				  [true, false],
				  tr_tf, change_tf_name)).
sn_editor_info(holds_editor, apply_all).

:- pce_global(@holds_editor, mk_sn_holds_editor).
mk_sn_holds_editor(Object) :-
	mk_sn_part_editor(holds_editor, Object).





               /************
		 * B A R E Q  E D I T O R
	       *
	       ************************/
sn_editor_info(bareq_editor, setup('|= Editor',bareq_node)).
sn_editor_info(bareq_editor, text_item(state,change_state,state1)).
sn_editor_info(bareq_editor, text_item(state_property,change_atom_name,atom_name1)).

sn_editor_info(bareq_editor, apply_all).

:- pce_global(@bareq_editor, mk_sn_bareq_editor).
mk_sn_bareq_editor(Object) :-
	mk_sn_part_editor(bareq_editor, Object).





               /************************
	       *
	       *   C O N D I T I O N    E D I T O R
	       *
	       ************************/
sn_editor_info(condition_editor, setup('Condition Editor',condition_node)).
sn_editor_info(condition_editor, text_item(lhs,change_name(lhs),name1(lhs))).
sn_editor_info(condition_editor, menu(op,
				  CmpOps,
				  name1(op), change_name(op))) :-
	bagof(CmpOp, cmpopd(CmpOp), CmpOps).
cmpopd(Op) :-
	cmpopusr(Op1),
	functor(Op1, Op, _).
cmpopd('[op]').
sn_editor_info(condition_editor, apply_all).

sn_editor_info(condition_editor, text_item(rhs,change_name(rhs),name1(rhs))).
sn_editor_info(condition_editor, text_item(whole_term, change_whole_term,
					   whole_name1)).
:- pce_global(@condition_editor, mk_sn_condition_editor).
mk_sn_condition_editor(Object) :-
	mk_sn_part_editor(condition_editor, Object).












