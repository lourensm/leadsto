:- module(externaleditor, []).
:- use_module(library(pce)).
:- use_module(util).
:- use_module(library(lists)).
:- pce_global(@external_editor, mk_external_editor).

mk_external_editor(Editor) :-
	new(Editor, external_editor).

:- pce_begin_class(external_editor, dialog).
variable(for,  external_node*, both).
initialise(This) :->
	send(This, send_super, initialise, 'External Property Editor'),
	send(This, for, @nil),
	send(This, append, new(TI,text_item('property name'))),
	send(TI, name, property),
	send(This, append, new(TE,text_item(external,''))),
	send(TI, editable, @off),
	send(TE, editable, @on),
	send(TE, message, message(This, apply1)),
	send(This, append, button(apply, message(This, apply1))),
	send(This, append, button(ok, message(This, ok))),
	send(This, append, button(cancel, message(This, dismiss))),
	send(This, append, new(reporter)),
	send(This, done_message, message(This, dismiss)),
	send(This, resize_message, message(This, layout, @arg2)).
update_label(This) :->
	get(This, for, For),
	(For \== @nil
	->	get(For, parent, PD),
		get(PD, prop_nil_def, header, Name),
		send(?(This, member, property), selection, Name),
		send(?(This, member, external), selection, For?constant0)
	;	send(?(This, member, 'property name'), selection,
					   'UNSPECIFIED'),
		send(?(This, member, external), selection, 'UNSPECIFIED')
	).
ok(This) :->
	send(This, apply1),
	send(This, dismiss).
apply1(This) :->
	send(This?for, change_value, ?(This, member, external)?selection),
	send(This, report, inform, '').
dismiss(This) :->
	send(This, transient_for, @nil),
	send(This, for, @nil),
	send(This, show, @off).
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

update_node(This) :->
	get(This, for, For),
	send(This, update_label),
	send(This, report, inform, 'update_node %O', For).
setup(This, VN:extra_node) :->
	send(This, for, VN),
	send(This, transient_for, VN?frame),
	send(This, create),
	send(This, expose),
	send(This, update_node).
:- pce_end_class.
