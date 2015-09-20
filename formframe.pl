:- module(formframe,
	  [
           menu_items/1,
	   lteditor/0,
	   add_tree_frame_modified_hook/3,
	   config_tree_frame/0
	  ]).

:- use_module(util).
:- use_module(formload).
:- use_module(library(pce)).
:- use_module(library(pce_helper)).
:- use_module(library(pce_report)).
:- use_module(library(help_message)).

:- use_module(psprinting).
:- use_module(ltversion).

:- use_module(library(toolbar)).
:- use_module(library(find_file)).
:- use_module(nodes).
:- use_module(library(lists)).
:- use_module(logutil).
:- use_module(util).

:- require([get/3,listen/2,listen/3]).


:- [form_config].

:- pce_global(@finder, new(finder)).



:- pce_global(@tree_frame, new(tree_frame)).

:- multifile user:version/2.

user:version(lteditor,1).


config_tree_frame :-
	(ensure_loaded_config(form_config:_ConfigFile)
	->	true
	;	pce_config:load_key(form_config, LeadsToEditorCnf),
		pce_config:load_file(LeadsToEditorCnf, LT)
	->	fatal_error('Configuration file ~w probably corrupted, you may try to remove it and restart', [LT])
	;	impl_error('Unexpected error loading configuration'),
	        fail
	),
	(get_option(gui(color(node, Color)))
	->	(set_config(form_config:tree/node/background, Color)
		->	true
		;	format('FAILED:~w~n',
			       [set_config(form_config:tree/node/color, Color)])
		)
	;	true
	).



savealone_nsa(P) :-
	my_autoload_all,
        qsave_program(P, [goal=lteditor, autoload=true]),
        halt.

savealone(P):-
        savealone1(P),
        halt.
savealone1(P) :-
	my_autoload_all,
	qsave_program(P, [goal=lteditor, autoload=true, stand_alone=true]).
savealonefeditor(P):-
        savealonefeditor1(P),
        halt.
savealonefeditor1(P):-
	my_autoload_all,
	qsave_program(P, [goal=checkereditor, autoload=true, stand_alone=true]).
savealonefeditor_nsa(P):-
	my_autoload_all,
	qsave_program(P, [goal=checkereditor, autoload=true]),
	halt.
lteditor :-
	catch_fatal_halt(lteditor1, lteditor_exception).

checkereditor :-
	catch_fatal_halt(checkereditor1, checkereditor_exception).

log_entry(formload,'Loading specification file', formload, on).

lteditor1 :-
	lteditoropt([]).
lteditoropt(Options) :-
	setup_logging(lteditor, [util,formframe]),
	setupmainnew('LEADSTO EDITOR', lteditor,
		     [os(util,[wd,debugging,log]),os(formframe, lttree),
		      os(formframe, ftree)|Options], File),
	setup_formula_types(leadsto,false), % after reading options -allform
	new(P, tree_frame(leadsto)),
	send(P, open),
	(File == []
	->	true
	;	send(P, load_file, File)
	),
	update_wd.

lttest :-
	File = 'tsto.lt',
	lteditoropt([file(File)]).



checkereditor1 :-
	setup_logging(ttleditor, [util,formframe]),
	setupmainnew('TTL EDITOR', ttleditor,
		     [os(util,[wd,debugging,log]),os(formframe, ftree)], File),
	setup_formula_types(checker,false), % after reading options -allform
	new(P, tree_frame(checker)),
	send(P, open),
	(File == []
	->	true
	;	send(P, load_file, File)
	),
	update_wd.

local_option(ftree,arg('-gui', Arg,
			set_term_option_bind(Arg, TermArg, gui(TermArg),
					     gui_option_syntax), 'GUITERM'),
	     'Specify     -gui \'color(node,red)\' for red nodes', []).
local_option(lttree, single('-allform', set_option(allform)),
	     'Enable all logical connectors', []).
local_option(lttree, single('-pxor', set_option(pxor)),
	     'Enable probabilistic exclusive or', []).


:- pce_global(@form_tree_recogniser, make_form_tree_recogniser).
make_form_tree_recogniser(R) :-
	PopNode = @event?receiver?node,
	new(E1, click_gesture(left, '', double,
			      message(PopNode, double_click))),
	new(R, handler_group(E1)).







/*
  Order is important: last added will be first
  No duplicates
  */

:- pce_begin_class(fifo_set, popup).


initialise(This) :->
	send(This, send_super, initialise),
	send(This, message, message(@arg2, fifo_selected, @arg1)).

add_name(This, E:name) :->
	(get(This, member, E, P)
	->	send(This, delete, P)
	;	true
	),
	send(This, prepend, menu_item(E)),
	send(This, cleanup).

cleanup(This) :->
	send(This, truncate, 10).

truncate(This, N:int) :->
	repeat,
	    get(This, members, M),
	    get(M, size, S),
	    (S =< N
	    ->	    !
	    ;	    get(M, tail, Mn),
		    send(This, delete, Mn),
		    fail
	    ).

:- pce_end_class.





:- pce_begin_class(tree_frame, frame).



variable(domain,   {leadsto,checker},   both).

find_property(This, P:name, Prop:property_def_node) :<-
       get(This?spec_tree?root?sons, find,
	   and(message(@arg1, instance_of, property_def_node),
	       message(P, equal, @arg1?header)),
	   Prop).

reporta(This, LogKind:name,
	Kind:{status, inform, progress, done, warning, error, fatal},
       Format:[char_array], Args:any ...) :->
	(Kind == status,
		Format == ''
	->	true
	;do_log(LogKind)
	->	(	kind_prefix(Kind, Txt)
		->	send(@pce, format, '\n'),
			send(@pce, format, '%s', Txt)
		;	true
		),
		(is_list(Args)
		->	Call =.. [format, Format |Args],
			send(@pce, Call)
		;	send(@pce, send_vector, format, Format, Args)
		),
		send(@pce, format, '\n')
	;	true
	),
	(is_list(Args)
	->	Call1 =.. [report, Kind, Format|Args],
		send(?(This, member, quit_reporter), Call1)
	;	send(?(This, member, quit_reporter), send_vector, report,
		     Kind, Format, Args)
	).

report(This, Kind:{status, inform, progress, done, warning, error, fatal},
       Format:[char_array], Args:any ...) :->
	(Kind == status,
		Format == ''
	->	true
	;	(	kind_prefix(Kind, Txt)
		->	send(@pce, format, '\n'),
			send(@pce, format, '%s', Txt)
		;	true
		),
		(is_list(Args)
		->	Call1 =.. [format, Format | Args],
			send(@pce, Call1)
		;	send(@pce, send_vector, format, Format, Args)
		),
		send(@pce, format, '\n')
	),
	(is_list(Args)
	->	Call2 =.. [report, Kind, Format| Args],
		send(?(This, member, quit_reporter), Call2)
	;	send(?(This, member, quit_reporter), send_vector, report,
		     Kind, Format, Args)
	).

set_unmodified(This, _By:prolog) :->
	(get(This, is_modified, @off)
	->	true
	;	send(This, is_modified, @off),
		send(This, update_modified_look)
	).

:- dynamic tree_frame_modified_hook/3.

add_tree_frame_modified_hook(Frame, ModifiedData, Hook) :-
	assertz(tree_frame_modified_hook(Frame, ModifiedData, Hook)).

modified(This, OP:prolog) :->
	(	tree_frame_modified_hook(This, OP, Hook),
		ignore(call(Hook)),
		fail
	;	true
	),
	(get(This, is_modified, @on)
	->	true
	;	send(This, is_modified, @on),
		send(This, update_modified_look)
	).

variable(object_cut_tree, tree, both).
variable(formula_cut_tree, tree, both).
variable(var_cut_tree, tree, both).
variable(conditional_cut_tree, tree, both).
variable(toplevel_cut_tree, tree, both).
variable(delay_cut_tree, tree, both).
variable(atom_list, fifo_set, both).
variable(property_list, fifo_set, both).
variable(is_modified, bool:= @off, both).
variable(file_name, char_array*, both).
variable(spec_tree,    tree,  both).
variable(checking_enabled, bool, both).

picture(This, P:picture) :<-
        get(This, member, picture, P).


resource(printer,       image,  image('16x16/print.xpm')).
resource(floppy,        image,  image('16x16/save.xpm')).
resource(open,          image,  image('16x16/open.xpm')).

file_item(This, FI:text_item) :<-
        get(?(This, member, control_dialog), member, file, FI).

update_modified_look(This) :->
	get(This, modified_label, Label),
	(get(This, is_modified, @on)
	->	send(Label, colour, red)
				%send(This, icon_label, string('%s%s','>',L))
		%send(Label, background, colour(white))
	;	send(Label, colour, Label?background),
		send(Label, background, This?background)
		%send(This, icon_label, L)
	).
modified_label(This, L:label) :<-
	get(?(This, member, control_dialog), member, modified, L).

domain_header(leadsto, 'Leads To Editor', _).
domain_header(checker, 'TTL Checker', @on) :-
	!.

domain_header(checker, 'TTL Editor', _).
initialise(This, Domain:{leadsto,checker},CheckingEnabled:[bool]) :->
	domain_header(Domain, Header, CheckingEnabled),
	send(This, send_super, initialise, Header),
	send(This, domain, Domain),
	ignore(util:send(This, icon, resource(tree_icon))),
	default(CheckingEnabled, @off, CheckingEnabled1),
	send(This, checking_enabled, CheckingEnabled1),
	send(This, append, new(D, dialog)),
	send(D, name, control_dialog),
	send(D, pen, 0),
	send(D, gap, size(5, 5)),
	send(D, append, new(MB, menu_bar)),
	send(new(T, text_item(file,'')), right, MB),
	send(T, alignment, right),
	send(T, editable, @off),
	send(new(ModLabel, label(modified, '  Modified')), right, T),
	send(ModLabel, width, 8),
	send(ModLabel, font, @helvetica_bold_14),
	send(D, append, new(tool_bar(This))),
	send(This, fill_menu_bar),
	send(This, fill_tool_bar),
	send(This, done_message, message(This, quit)),
	send(new(P,ps_tree_picture), below, D),
	send(new(D2, dialog), below, P),
	send(D2, name, quit_reporter),
	send(D2, resize_message, message(D2, layout, @arg2)),
	send(D2, append, new(DB,button(quit,message(This, quit)))),
	(fail,CheckingEnabled == @on
	->	send(new(DB2, button(interrupt)), right, DB)
	;	DB2 = DB
	),
	send(DB, reference, point(0,DB?height)),
	send(DB2, reference, point(0,DB2?height)),
	send(new(reporter), right, DB2),
	send(This, object_cut_tree, new(tree)),
	send(This, formula_cut_tree, new(tree)),
	send(This, var_cut_tree, new(tree)),
	send(This, conditional_cut_tree, new(tree)),
	send(This, toplevel_cut_tree, new(tree)),
	send(This, atom_list, new(fifo_set)),
	send(This, property_list, new(fifo_set)),
	%send(P, display, TCT, point(600, 200)),
	send(This, delay_cut_tree, new(tree)),
	new(RN, part_node(Domain,'ROOT')),
	send(P, display, new(RT,tree(RN))),
	send(RT, direction, list),
	send(RT, node_handler, @form_tree_recogniser),
	send(This, spec_tree, RT),
	send(This, update_modified_look),
	(	member(TreeAttr, [level_gap, link_gap, neighbour_gap]),
		(	get_config(form_config:tree/TreeAttr, VT1)
		->	send(This, tree_attrs, TreeAttr, VT1)
		;	true
		),
		listen(This, set_config(form_config:tree/TreeAttr, VT2),
		       send(This, tree_attrs, TreeAttr, VT2)),
		fail
	;	true
	),
	(	link_attr(LinkAttr, LineAttr),
		(	get_config(form_config:tree/LinkAttr, VL1)
		->	send(This, line_attrs, LineAttr, VL1)
		;	true
		),
		listen(This, set_config(form_config:tree/LinkAttr, VL2),
		       send(This, line_attrs, LineAttr, VL2)),
		fail
	;	true
	),
	(get_config(form_config:tree/background, PBg)
	->	send(P, background, PBg)
	;	true
	),
	listen(This, set_config(form_config:tree/menu_items, PBg1),
	       send(@prolog, set_menu_items, PBg1)),
	listen(This, set_config(form_config:tree/background, PBg1),
	       send(This?picture, background, PBg1)),
	(   get_config(form_config:history/geometry/main_window, Geometry)
	->  send(This, geometry, Geometry)
	;   true
	),
	listen(This, set_config(form_config:tree/node/Prop, Val),
	       send(This, node_prop, Prop, Val)).
menu_items(N) :-
        get_config(form_config:tree/menu_items, N1),
        (number(N1)
        ->     N = N1
        ;      atom(N1),
               atom_to_term(N1, N, []),
               number(N)
        ),
	N < 1000,
	N >= 1.
user:set_menu_items(N) :-
	format('WARNING:Unimplemented set menu items ~w~n', [N]),
        format('Please save the menu_entries setting file and restart~n'),
        format('for the option to be handled.~n').
line_attrs(This, LineAttr:name, Value:any) :->
	(	tree_name(TreeMember),
		get(This, TreeMember, Tree),
		send(Tree?link?line, LineAttr, Value),
		send(Tree, redraw),
		fail
	;	true
	).
tree_attrs(This, TreeAttr:name, Value:any) :->
	(	tree_name(TreeMember),
		get(This, TreeMember, Tree),
		send(Tree, TreeAttr, Value),
		send(Tree, redraw),
		fail
	;	true
	).
link_attr(link_pen, pen).
link_attr(link_colour, colour).
tree_name(formula_cut_tree).
tree_name(var_cut_tree).
tree_name(toplevel_cut_tree).
tree_name(delay_cut_tree).
tree_name(spec_tree).

node_prop(This, Prop:name, Val:any) :->
	forall(tree_name(Attr),
	       change_tree_node_prop(This, Attr, Prop, Val)
	      ).
change_tree_node_prop(This, Attr, Prop, Color) :-
	get(This, Attr, Tree),
	assert_debug(send(Tree, instance_of, tree)),
	get(Tree, root, Root),
	(Root == @nil
	->	true
	;	assert_debug(send(Root, instance_of, text_node)),
		send(Root, change_root_prop, Prop, Color)
	).

resize(This) :->
	get(This, geometry, G),
	set_config(form_config:history/geometry/main_window, G),
	send(This, send_super, resize).


unlink(This) :->
	unlisten(This),
	get(This, geometry, Geometry),
	send(This, send_super, unlink),
	set_config(form_config:history/geometry/main_window, Geometry).


:- dynamic dyn_interrupted/0.

reset_interrupted :-
	retractall(dyn_interrupted).
sync_interrupted :-
	sync_display,
	retract(dyn_interrupted),
	retractall(dyn_interrupted).

interrupt(This) :->
	send(This, report, warning, 'checking interrupted'),
	(dyn_interrupted
	->	true
	;	assertz(dyn_interrupted)
	).

clear(This) :->
	(get(This, is_modified, @on)
	->	(send(@display,
		     confirm, 'Specification is modified, modifications will be lost, clear ok?')
		->	true
		;	send(This, report, inform, 'Clear cancelled'),
			fail
		)
	;	true
	),
	send(This?spec_tree, delete_all_sons).

modify_nodes(This, NodeOp:name) :->
	(get(This, is_modified, @on)
	->	(send(@display,
		     confirm, 'Specification is modified, result before transformation %s will be lost, continue?', NodeOp)
		->	true
		;	send(This, report, inform, 'Clear cancelled'),
			fail
		)
	;	true
	),
	send(This?spec_tree?root?sons, for_some, message(@arg1, modify_op, NodeOp)).
empty(This) :->
	get(This?spec_tree?sons, size, S),
	S = 0.

clear_report(This) :->
	send(This, report, status, '').

set_file_name(This, F:char_array*) :->
	send(This, file_name, F),
	(F == @nil
	->	F1 = ''
	;	F1 = F
	),
	send(This?file_item, selection, F1).


print(This) :->
	send(This, clear_report),
	get(This, picture, P),
	(get(@pce, convert, win_printer, class, _)
	->	get(This, picture, P),
		print_canvas(P)
	;	print_postscript(This, P)
	).

domain_extension(leadsto, 'Leads To (*.lt)','.lt').
domain_extension(checker, 'Formula (*.fm)','.fm').

confirm_lose_modifications(This) :->
	(get(This, is_modified, @on)
	->	(send(@display,
		     confirm, 'Specification is modified, modifications will be lost, load ok?')
		->	true
		;	send(This, report, inform, 'Load cancelled'),
			fail
		)
	;	true
	).

load(This) :->
	send(This, clear_report),
	send(This, confirm_lose_modifications),
	get(This, domain, Domain),
	domain_extension(Domain, Description, Ext),
	get(@finder, file, @on, tuple(Description,Ext), File),
	send(This, load_file, File).
/*reload(This) :->
	get(This, file_name, F),
	(F == @nil
	->	send(This, report, inform, 'No file loaded'),
		send(This, load)
	;	send(This, confirm_lose_modifications),
		send(This, load_file, F)
	).
*/
load_file(This, File:char_array) :->
	send(This, set_file_name, File),
	send(This, reporta,formload,status, 'Loading specification from "%s"...', File),
	send(This, modified, loading_new_spec),
	(load_spec(File, This)
	->	send(This, set_unmodified, load_file(File)),
		send(This, reporta,formload,inform, 'Loaded specification from "%s"', File)
	;	send(This, set_file_name, @nil)
	).

confirm_save(This, F:char_array) :->
	(send(@display,
	     confirm,'Current specification not generated by lteditor;\nall comment and layout info from "%s" will get lost by saving, ok?', F)
	->	true
	;	send(This, report, inform, 'Save cancelled'),
		fail
	).

save(This) :->
	send(This, clear_report),
	get(This, file_name, F),
	(F == @nil
	->	send(This, save_as)
	;	(lteditorspecloaded(_F1)
		->	true
		;	send(This, confirm_save, F)
		),
		send(This, save_file, F)
	).

save_file(This, File:name) :->
	send(file(File), backup),
	send(This, report, inform, 'Saving specification in "%s"...', File),
	send(This, set_file_name, File),
	(	save_spec(File, This)
	->	send(This, set_unmodified, save(File)),
	send(This, report, inform, 'Saved specification in "%s"...', File)
	;	send(This, set_file_name, @nil),
		fail
	).

save_as(This) :->
	send(This, clear_report),
	get(This, domain, Domain),
	domain_extension(Domain, Description, Ext),
	get(@finder, file, @off, tuple(Description,Ext), File),
	(	lteditorspecloaded(_F1)
	->	true
	;	get(This, file_name, FL),
	        FL \== @nil,
		get(file(FL), absolute_path, P1),
		get(file(File), absolute_path, P2),
		send(P1, equal, P2)
	->	send(This, confirm_save, File)
	;	true
	),
	send(This, save_file, File).



confirm_modified(This) :->
	(get(This, is_modified, @on)
	->	send(@display,
		     confirm, 'Specification is modified, quit anyhow?')
	;	true
	).


quit(This) :->
	send(This, confirm_modified),
	send(This, free),
	halt.



fill_tool_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, tool_bar, TB),
	send_list(TB, append,
		  [tool_button(load,
				resource(open),
				load),
		   gap,		% skip a little
/*		   tool_button(reload,
				resource(reopen),
				reload,not(This?file_name == @nil)),
		   gap,stupid: is for leadsto
		   */
		   tool_button(save,
				resource(floppy),
				save),
		   gap,
		   tool_button(print,
			       resource(printer),
			       print)
		  ]).
do_settings :-
	(	current_prolog_flag(version, V),
		\+ is_win32,
		V < 50106
	->	format('WARNING:editing a colour will make application crash!~n')
	;	true
	).


fill_menu_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, menu_bar, MB),
	send_list(MB, append,
		  [ new(File, popup(file)),
		    new(Edit, popup(edit))
		  ]),
	new(UM, message(@arg1?frame, clear_report)),
	send_list([File, Edit], update_message, UM),
	(do_settings
	->	send(MB, append, new(Settings, popup(settings))),
		send(Settings, update_message, UM),
		send(Settings, append,  menu_item(preferences,
					  message(This, preferences),
					  'Preferences ...'))
	;	true
	),
	send_list(File, append,
		  [ menu_item(load,
			      message(This, load)),
		    /*
		    menu_item(reload,
			      message(This, reload),
			      @default, @default, not(This?file_name == @nil)),
		      */
		    menu_item(save,
			      message(This, save),
			      @default, @default, not(This?file_name == @nil)),
		    menu_item('save as..',
			      message(This, save_as)),
		    @postscript_popup,
		    menu_item(print,
			      message(This, print))
		  ]),
	(get(This, checking_enabled, @on)
	->	send(File, append, menu_item('trace management...',
			      message(@prolog, checker_trace_management1,
				      This))
		    )
	;	true
	),
	send_list(File, append, [
		    menu_item(quit,
			      message(This, quit))
		  ]),
	(get(This, domain, checker)
	->	send_list(Edit, append,
		  [menu_item('holds to |=',
			      message(This, modify_nodes, holds_to_bareq),
			     @default, @default,
			     not(message(This, empty))),
		   menu_item('|= to holds',
			      message(This, modify_nodes, bareq_to_holds),
			     @default, @default,
			     not(message(This, empty)))
		   ]
		 )
	;	true
	),
	send_list(Edit, append,
		  [menu_item(clear,
			      message(This, clear), @default, @default,
			     not(message(This, empty)))
		   ]
		 ).


preferences(This) :->
        %trace,
	edit_config(form_config:This).


checker_trace_management1(This) :-
	assert_debug(current_module(ttlchecker)),
	ttlchecker:checker_trace_management(This, show).


:- pce_end_class.
:- pce_begin_class(ps_tree_picture, ps_picture).
variable(pageoffset, int, both).

clear_dev(This) :->
	send(This, send_super, clear_dev),
	send(This, pageoffset, 0),
	(	get_config(form_config:tree/background, PBg)
	->	send(This, background, PBg)
	;	true
	).


activate_print(This) :->
	(send(This, print_to_screen)
	->	send(This, report, error, 'No advanced print to screen supported'),
		fail
	;	true
	),
	send(This, setup_pages),
	get(This?frame?spec_tree?root, sons, Sons),
	send(Sons, for_all, message(This, print_node, @arg1)),
	get(This, pageoffset, PO),
	(PO is 0
	->	true
	;	send(This, print_page)
	).
max_x(This, W:int) :<-
	get(This, width, W),
	format('No printout width yet, simply copied from visible width ~w\n',
	       [W]).
print_node(This, Node:node) :->
	get(Node, copy_tree, NodeCopy),
	new(T, tree(NodeCopy)),
	send(T, direction, list),
	get(T, height, H),
	get(This, pageoffset, PO),
	(PO is 0
	->	Dist = 0
	;	Dist = 10
	),
	get(This, page_length_pixels, PLP),
	(H + Dist + PO > PLP
	->	(PO is 0
		->	send(This, display, T, point(0, 0)),
			send(This, print_page)
		;	send(This, print_page),
			send(This, display, T, point(0, 0)),
			(H >= PLP
			->	send(This, print_page)
			;	send(This, pageoffset, H)
			)
		)
	;	send(This, display, T, point(0, PO+Dist)),
		send(This, pageoffset, PO + H + Dist)
	).




:- pce_end_class.

