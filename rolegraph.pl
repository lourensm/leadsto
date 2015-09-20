:- module(rolegraph,[
		     ensure_no_graph/0,
		     do_show_graphs/5,
		     testrg/0,
		     do_graphs/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(pce_tagged_connection)).
:- use_module(library(lists)).
:- use_module(library(pce_report)).
:- use_module(library(help_message)).
:- use_module(library(toolbar)).
:- use_module(util).
:- use_module(psprinting).
:- use_module(library(find_file)).
:- pce_global(@finder, new(finder)).

:- require([listen/2,listen/3]).
:- discontiguous resource/3.

/*
  display(GraphTag, graph_term(Atom, SourceVar, DestVar)).
  */
graph_display(Tags, graph_term(Atom, SourceVar, DestVar)) :-
	algo:display(Tag, graph_term(Atom, SourceVar, DestVar)),
	memberchk(Tag, Tags),!.

node(GD, Traces, Node) :-
	member(SD, [source, destination]),
	node(SD, GD, Traces, Node).

node(SD, GD, Traces, Node) :-
	(SD == source
	->	Node = S
	;	Node = D
	),
	member(graph_term(Atom, S, D),GD),
	Atom = AtomKey,
	member(AtomKey-_Data, Traces).


arc(GD, Traces, Src, Dest, At) :-
	member(graph_term(Atom, Src, Dest),GD),
	member(Atom-Data, Traces),
	member(range(At1, _, true), Data),
	trfloat(At1, At).

:- pce_global(@graphframe, new(graphframe)).

local_option(rolegraph, arg('-graphview', Arg, set_option(graphview(Arg)), 'GRAPHVIEWTAG'),
	     'Display trace graph according to GRAPHVIEWTAG',[]).
local_option(rolegraph, arg('-graphinit', Arg, set_option(graphinit(Arg)), 'INITIALPOSFILE'),
	     'Get initial node layout from INITIALPOSFILE',[]).
graph_tags(Tags) :-
	setof(V,get_option(graphview(V)),Tags).
do_graphs :-
	graph_tags(_Tags).

do_show_graphs(File,Traces, PictureFrame, ST, ET) :-
	(	graph_tags(Tags)
	->	show_graphs(File,Tags, Traces, PictureFrame, ST, ET)
	;	warning('No graphview options, still will show graph'),
		show_graphs(File,[], Traces, PictureFrame, ST, ET)
	).

show_graphs(File,Tags, Traces, PictureFrame, ST, ET) :-
	(bagof(O, graph_display(Tags, O), GD)
	->	error('Unimplemented:~w',
		      [show_graphs(Tags, 'Traces', PictureFrame)])
	;	warning('No graph_display spec for GraphView ~w using default',
		       [Tags]),
		GD = [graph_term((output(X)|communication_from_to(X, Y, _, _)),
				 X, Y)/*,
		      graph_term((input(X)|communication_from_to(Y, X, _, _)),
					X, Y)*/],
		(setof(Node, X^Y^GD^node(GD, Traces, Node), Nodes)
		->	format('NODES:~w~n', [Nodes]),
			show_graphs_ui(File,Tags, GD, Nodes, Traces, ST, ET,
				       PictureFrame)
		;	error('No roles found in trace matching graph_display')
		)
	).

show_graphs_ui(File,_Tags, GD, Nodes, Traces, ST, ET, PictureFrame) :-
	setof(T-arc(Src,Dest,T),GD^arc(GD, Traces, Src, Dest, T),
			      Arcs1),
	rmmins(Arcs1, Arcs),
	format('Arcs:~n'),
	(	member(arc(Src,Dest,T), Arcs),
		format('~w   ~w -> ~w~n', [T, Src, Dest]),
		fail
	;	true
	),
	(PictureFrame == []
	->	PF = @nil
	;	PF = PictureFrame
	),
	send(@graphframe, boss, PF),
	send(@graphframe, show, @on),
	send(@graphframe, set_file, File),
	send(@graphframe?picture, setup_graph(Nodes, Arcs, ST, ET)).

ensure_no_graph :-
	(object(@graphframe)
	->	send(@graphframe, cleanup)
	;	true
	).

testrg :-
	new(GV, graphviewer),
	send(GV, open),
	send(GV, arc(aap, noot, aapnoot)),
	send(GV, node(zus, circle(30))),
	send(GV, node(jet, circle(30))),
	send(GV, layout).




/*	send(GV, arc(G, From:from=name, To:to=name,
    Label:label=[name]*,
    Pen:pen=[int],
    Colour:colour=[colour],
    Arrows:arrows=[{first,second,both}])
	*/

/*
  Ideeen:
  als aantrekking of afstoting beide klein zijn tussen twee nodes,
  maakt hun onderlinge positie nauwelijks uit.
  Dus initieel zorgen we gewoon dat de onderlinge afstand > no interaction
  distance.
  */

undoable :-
	\+ get(@grapher_undo, '_value', @nil).

:- pce_global(@grapher, make_grapher).

:- pce_global(@grapher_undo,
		  new(var('chain*', grapher_undo, @nil))).
:- pce_global(@grapher_app, make_grapher_app).

make_grapher_app(A) :-
	new(A, application(graphviewer)),
	send(A, kind, service).

make_grapher(G) :-
	send(new(G, graphviewer), open),
	send(G, wait).

:- pce_begin_class(graphframe, frame).
variable(node_diameter, int := 30, both).
variable(node_comm_factor, real := 0.33, both).

variable(node_fill_colour, colour := red, both).

variable(link_unit_width,  int := 4, both).
variable(link_min_len_strength, real := 5, both).
variable(link_min_len, real := 10, both).
variable(link_max_len, real := 30, both).

variable(boss, frame*, both).

variable(half_time_fraction,  real := 0.02, both).


variable(reporter_dialog, dialog, both).
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
		->	Call1 =.. [format, Format|Args],
			send(@pce, Call1)
		;	send(@pce, send_vector, format, Format, Args)
		),
		send(@pce, format, '\n')
	),
	(is_list(Args)
	->	Call =.. [report, Kind, Format|Args],
		send(?(This, reporter_dialog), Call)
	;	send(?(This, reporter_dialog), send_vector,report, Kind,
		     Format, Args)
	).

quit(This) :->
	get(This, boss, B),
	(B == @nil
	->	halt
	;	send(B, quit)
	).

resource(printer,       image,  image('16x16/print.xpm')).

picture(This, P:graph_picture) :<-
        get(This, member, graph_picture, P).
save_as_initial(This) :->
	send(This?picture, save_as_initial).

cleanup(This) :->
	send(This, set_file, ''),
	send(This?picture, cleanup).

file_item(This, FI:text_item) :<-
        get(?(This, member, control_dialog), member, file, FI).

file_name(This, F:char_array) :<-
        get(This?file_item, selection, F),
	F \= '',
	F \= @nil.


set_file(This, File:name) :->
	send(This?file_item, selection, File).

fill_menu_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, menu_bar, MB),
	send_list(MB, append,
		  [ new(File, popup(file)),
		    %new(_Edit, popup(edit))
		    new(Settings, popup(settings))
		  ]),
	send_list(File, append,
		  [ menu_item(layout,
			      message(This, layout),
			      'Layout..'),
		    menu_item(save_as_initial,
			      message(This, save_as_initial),
			      'Save As Initial Position..',
			      @default),
		     menu_item(load_initial,
			      message(This?picture, load_initial),
			      'Load Initial Position..',
			      @default),
		    menu_item(auto_arrange_initial,
			      message(This?picture, auto_arrange_initial),
			      'Auto Arrange Initial Position..',
			      @default),
		    menu_item(run_arcs,
			      message(This?picture, run_arcs),
			      'Show communication..',
			      @default),
		    @ps_picture_popup,
		    menu_item(quit,
			      message(This, quit))
		  ]),
	send(Settings, append, menu_item(preferences,
					  message(This, preferences),
					  'Preferences ...')).
preferences(This) :->
	edit_config(lt_config:This).



resource(reopen,          image,  image('16x16/refresh.xpm')).			      
fill_tool_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, tool_bar, TB),
	send_list(TB, append,
		  [ /*tool_button(load,
				resource(open),
				'load and run'),
		    gap,	% skip a little
		    */tool_button(reset,
				resource(reopen),
				'Reset',
				message(This, trace_loaded)),
		    gap,	% skip a little
		    tool_button(print,
				resource(printer),
				print)
		  ]).
reset(This) :->
	send(This?picture, reset).
layout(This) :->
	send(This?picture, layout, @on).

print(This) :->
	get(This, picture, P),
	(get(@pce, convert, win_printer, class, _)
	->	get(This, picture, P),
		print_canvas(P)
	;	print_postscript(This, P)
	).

initialise(This) :->
	send(This, send_super, initialise, 'Communication Analysis Tool'),
	ignore(util:send(This, icon, resource(tree_icon))),
	send(This, size, size(1024, 768)),
	send(This, append, new(D, dialog)),
	send(D, name, control_dialog),
	send(D, pen, 0),
	send(D, gap, size(5, 5)),
	send(D, append, new(MB, menu_bar)),
	send(new(T, text_item(file,'')), right, MB),
	send(T, alignment, right),
	send(T, editable, @off),
	send(D, append, new(tool_bar(This))),
	send(This, fill_menu_bar),
	send(This, fill_tool_bar),
	send(This, done_message, message(This, quit)),
	send(new(P,graph_picture), below, D),
	send(P, ver_stretch, 100),
	send(new(D3, dialog), below, P),
	send(D3, append, new(TI, text_item(time))),
	send(This, time_item, TI),
	send(new(BStep, button(step, message(This?picture, step))),
	     right, TI),
	send(new(BLeap, button(leap, message(This?picture, leap))),
	     right, BStep),
	send(new(BReset, button(reset, message(This?picture, reset))),
	     right, BLeap),
	send(This, stepbutton, BStep),
	send(This, leapbutton, BLeap),
	send(This, resetbutton, BReset), 
	send(new(D2, dialog), below, D3),
	send(This, reporter_dialog, D2),
	send(D2, resize_message, message(D2, layout, @arg2)),
	send(D2, append, new(DB,button(quit,message(This, quit)))),
	send(DB, reference, point(0,DB?height)),
	send(new(reporter), right, DB),
	send(P, mode, unloaded),
	(	graph_config_attr2(What, Val),
		(	get_config(lt_config:graph/What/Val, VT1)
		->	send(This, config2, What, Val, VT1)
		;	true
		),
		listen(This, set_config(lt_config:graph/What/Val, VT2),
		       send(This, config2, What, Val, VT2)),
		fail
	;	true
	),
	(	graph_config_attr1(What),
		(	get_config(lt_config:graph/What, VT1)
		->	send(This, config1, What, VT1)
		;	true
		),
		listen(This, set_config(lt_config:graph/What, VT2),
		       send(This, config1, What, VT2)),
		fail
	;	true
	),
	(get_config(lt_config:graph/background, PBg)
	->	send(P, background, PBg)
	;	true
	),
	listen(This, set_config(lt_config:graph/background, PBg1),
	       send(This?picture, background, PBg1)),
	(   get_config(lt_config:history/geometry/graph_window, Geometry)
	->	send(This, geometry, Geometry)
	;	true
	).
config1(This, Attr1, Value) :->
	(graph_config_attr1(Attr1, simple(ClassSend))
	->	send(This, ClassSend, Value)
	;	send(This, report, error, 'Unhandled config')
	).
config2(This, Attr1, Attr2, Value) :->
	(graph_config_attr2(Attr1, Attr2, simple(LinkNode,ClassVar))
	->	send(This, ClassVar, Value),
		send(This, update_graphicals, LinkNode, Attr2)
	;	send(This, report, error, 'Unhandled config')
	).
linknodeclass(link, graph_link).
linknodeclass(node, graph_node).

update_graphicals(This, LinkNode:{link,node},Attr2:name) :->
	(linknodeclass(LinkNode, Class)
	->	send(This?picture?graphicals, for_all,
		     if(message(@arg1,instance_of,Class),
			message(@arg1, update_config, Attr2)))
	;	send(This, report, error, 'Unhandled config')
	).
graph_config_attr1(A1) :-
	graph_config_attr1(A1,  _).
graph_config_attr1(half_time_fraction,simple(half_time_fraction)).

graph_config_attr2(A1, A2) :-
	graph_config_attr2(A1, A2, _).

graph_config_attr2(link, unit_width,simple(link,link_unit_width)).
graph_config_attr2(link, min_length, simple(link,link_min_len)).
graph_config_attr2(link, min_length_strength,
		   simple(link,link_min_len_strength)).
graph_config_attr2(link, max_length, simple(link, link_max_len)).
graph_config_attr2(node, diameter,simple(node, node_diameter)).
graph_config_attr2(node, communication_factor,simple(node, node_comm_factor)).
graph_config_attr2(node, fill_colour,simple(node, node_fill_colour)).

graph_config_attr2(node, pen_colour, simple(node, node_pen_colour)).
graph_config_attr2(node, pen, simple(node, node_pen)).

variable(node_pen_colour, colour := blue, both).
variable(node_pen, [int] := @default, both).
resize(This) :->
	get(This, geometry, G),
	set_config(lt_config:history/geometry/graph_window, G),
	send(This, send_super, resize).

unlink(This) :->
	unlisten(This),
	get(This, geometry, Geometry),
	send(This, send_super, unlink),
	set_config(lt_config:history/geometry/graph_window, Geometry).

variable(stepbutton, button, both).
variable(leapbutton, button, both).
variable(resetbutton, button, both).
variable(time_item, text_item, both).
variable(time, real := 0, both).

set_time(This, T:real) :->
	send(This?time_item, selection, T),
	send(This, time, T),
	send(This, flush).


graph_mode(This, Mode:{steppable,busy,ended,unloaded}) :->
	(Mode == unloaded
	->	send(This?time_item, selection, '')
	;	true
	),
	(Mode == steppable
	->	StepStatus = active
	;	StepStatus = inactive
	),
	send(This?stepbutton, status, StepStatus),
	send(This?leapbutton, status, StepStatus),
	send(This?resetbutton, status, StepStatus).

:- pce_end_class.

:- pce_begin_class(arc_template, object).
variable(from, name, both).
variable(to, name, both).
variable(time, real, both).
variable(strength, real, both).
variable(arrows, {none,first,second,both} := none, both).

initialise(This, F:name, T:name, Time:real, Strength:real) :->
	send(This, send_super, initialise),
	send(This, from, F),
	send(This, to, T),
	send(This, time, Time),
	send(This, strength, Strength).


:- pce_end_class.

:- pce_begin_class(graph_node_action, object).

variable(name, char_array, both).
variable(position, point, both).
variable(do_create, bool, both).

initialise(This, N:char_array, Pos:point, Create:bool) :->
	send(This, send_super, initialise),
	send(This, do_create, Create),
	send(This, name, N),
	send(This, position, Pos).


:- pce_end_class.

:- pce_begin_class(graph_node_data, object).

variable(position, point, both).
variable(name, char_array, both).

diff_node(This, GN:graph_node, GA:graph_action) :<-
        (send(This?position, equal, GN?position)
	->	fail
	;	new(GA, graph_node_action(This?name, GN?position, @off)),
		send(This, position, GN?position)
	).

initialise(This, GN:graph_node) :->
	send(This, send_super, initialise),
	send(This, name, GN?name),
	send(This, position, GN?position).

:- pce_end_class.

/* incremental representation of movie */
:- pce_begin_class(graph_state_container, object).
variable(nodes, hash_table, both).
variable(connections, hash_table := new(hash_table), both).
variable(current_graph_time_slice, graph_time_slice:= @nil, both).

initialise(This, T:real) :->
	send(This, send_super, initialise),
	send(This?hash_table, new(hash_table)),
	send(This, setup, T).

setup(This, T:real) :->
	send(This, current_graph_time_slice, new(graph_time_slice(T, @nil))).
next_graph_time_slice(This, T:real) :->
	get(This, current_graph_time_slice, OGTS),
	send(This, current_graph_time_slice, new(graph_time_slice(T, OGTS))).

	
check_node(This, GN:graph_node, Time:real) :->
	send(This, check_new_time, Time),
	send(This, check_node_current, GN:graph_node, This?current_graph_time_slice).
check_node_current(This, GN:graph_node, GTS:graph_time_slice) :->
	(get(This?nodes, member, GN?name, GN1)
	->	(get(GN1, diff_node, GN, GNAction)
		->	send(GTS, add_action, GNAction)
		;	true
		)
	;	send(GTS, add_action, new(graph_node_action(GN?name,GN?position, @on))),
		send(This?nodes, append, GN?name, new(graph_node_data(GN)))
	).


check_new_time(This, Time:real) :->
	(	get(This, current_graph_time_slice, CTS),
		CTS \== @nil
	->	get(CTS, time, T1),
		(T1 is Time
		->	true
		;T1 < Time
		->	send(This, next_graph_time_slice, Time)
		;	send(@pce, format, 'ERROR:wrng time seq\n'),
			local_trace(timeseq)
		)
	;	send(@pce, format, 'ERROR:wrng time seq\n'),
		local_trace(aap)
	).

:- pce_end_class.

:- pce_begin_class(graph_time_slice, object).
initialise(This, T:real, Prev:graph_time_slice*) :->
	send(This, send_super, initialise),
	send(This, time, T),
	send(This, prev, Prev),
	send(This, next, @nil),
	(Prev == @nil
	->	true
	;	send(Prev, next, This)
	).

variable(actions, chain := new(chain), both).

variable(prev, graph_time_slice*, both).
variable(next, graph_time_slice*, both).
variable(time, real, both).

:- pce_end_class.



:- pce_begin_class(graph_picture, ps_picture).
variable(nodes,	    hash_table := new(hash_table), get,
	 "Id --> node table").
variable(new_nodes, chain := new(chain),           get,
	 "Nodes added since last ->layout").
variable(layouting, bool := @off, get,
	 "Layout is in progress").
variable(mode,      {steppable,busy,ended,unloaded}* := @nil,
	 get, "Mode of operation").

mode(This, Mode:{steppable, busy, ended, unloaded}) :->
	get(This, mode, M),
	(M == Mode
	->	true
	;	send(This, slot, mode, Mode),
		send(This?frame, graph_mode, Mode)
	),
	(Mode == steppable
	->	get(This?parcs, size, S),
		get(This, arc_no, An),
		(An >= S
		->	send(This, mode, ended)
		;	true
		)
	;	true
	).


rmarcs(This) :->
	send(This?nodes, for_all, message(@arg2, disconnect)).


arc(G, From:from=name, To:to=name,
    Label:label=[name]*,
    Pen:pen=[int],
    Colour:colour=[colour],
    Arrows:arrows=[{first,second,both}]) :->
	get(G, arc, From, To, Label, Pen, Colour, Arrows, _).

arc(G, From:from=name, To:to=name,
    Label:label=[name]*,
    Pen:pen=[int],
    Colour:colour=[colour],
    Arrows:arrows=[{first,second,both}], C:connection) :<-
	"Add an arc with parameters"::
	get(G, node, From, @on, FN),
	get(G, node, To, @on, TN),
	get(FN, connect, TN, C),
	(   Arrows \== @default,
	    \+ get(C, from_node, FN)
	->  reverse_arrows(Arrows, Arrs)
	;   Arrs = Arrows
	),
	if_provided(C, label,  Label),	% textual or graphics label
	if_provided(C, pen,    Pen),	% thickness of the line
	if_provided(C, colour, Colour),	% colour of the line
	if_provided(C, arrows, Arrs).	% arrows at its ends
if_provided(_, _, @default) :- !.
if_provided(Obj, Method, Value) :-
	Msg =.. [Method,Value],
	(   undoable
	->  get(Obj, Method, Old),
	    send(@grapher_undo, prepend,
		 message(Obj, Method, Old))
	;   true
	),
	send(Obj, Msg).

first_time_nodes([], _Arcs, []).

first_time_nodes([Node|Nodes], Arcs, [T-Node|TNodes]) :-
	(first_occ(Arcs, Node, T)
	->	true
	;	local_trace(first_occ),
		first_occ(Arcs, Node, T)
	),
	first_time_nodes(Nodes, Arcs, TNodes).
first_occ([arc(S,D, T)|_], Node, T) :-
	(Node = S;Node = D),
	!.
first_occ([_|Arcs], Node, T) :-
	first_occ(Arcs, Node, T).

	
	
sort_creation_time(Nodes, Arcs, SortedNodes) :-
	first_time_nodes(Nodes, Arcs, TNodes),
	sort(TNodes, TNodes1),
	rmmins(TNodes1, SortedNodes).

initialise(This) :->
	send_super(This, initialise, 'RoleInteraction Viewer'),
	send(This, pnodes, []).

%	send(This, mode, unloaded).

trace_loaded(This) :->
	get(This, pnodes, P),
	P \== [].

variable(parcs, chain := new(chain), both).
variable(arc_no, int* := @nil, both). % nth0

variable(pnodes, prolog, both).

cleanup1(This) :->
	send(This?old_arcs, clear),
	send(This?new_nodes, clear),
	send(This?nodes, clear),
	send(This, clear).
cleanup(This) :->
	send(This, cleanup1),
	send(This?parcs, clear),
	send(This, pnodes, []),
	send(This?new_nodes, clear),
	send(This?nodes, clear),
	send(This, clear),
	send(This, mode, unloaded).


save_as_initial1(This, File:char_array) :->
	new(F, file(File)),
	send(F, open, write),
	send(F, format, 'size(%s,%s).\n', This?picture?width, This?picture?height),
	send(This?nodes, for_all, message(@arg2, save_node, F)),
	send(F, close).




load_initial(This) :->
	Description = 'Node Layout', Ext = '.nl',
	get(@finder, file, @on, tuple(Description,Ext), File),
	send(This, load_initial_file(File)).
load_initial_file(This, File:char_array) :->	    
	open(File, read, S1),
	repeat,
	read_term(S1, Term,[variable_names(Vars)]),
	bind_bindings(Vars),
	(Term == end_of_file
	->	!
	;	Term = size(_,_)
	->	%send(This?picture, size, Term),
		fail
	;	Term = node(Name, X, Y)
	->	(get(This?nodes, member, Name, N)
		->	send(N, position, point(X,Y)),
			send(This, flush),
			fail
		;	send(This, report, error, 'Unrecognised node %s',Name),
			fail
		)
	;	!,
		term_to_atom(Term, Atom), 
		send(This, report, error, 'Unrecognised entry %s', Atom),
		fail
	),
	close(S1).


save_as_initial(This) :->
	Description = 'Node Layout', Ext = '.nl',
	get(@finder, file, @off, tuple(Description,Ext), File),
	send(file(File), backup),
	send(This, report, inform, 'Saving Layout in "%s"...', File),
	send(This, save_as_initial1, File).

reset(This) :->
	send(This, setup_graph1).
variable(st, real, both).
variable(et, real, both).

setup_graph(This, Nodes:prolog, Arcs:prolog, ST:real, ET:real) :->
	send(This, cleanup),
	(	member(arc(F1, T1, Time1),Arcs),
		member(arc(T1, F1, Time2),Arcs)
	->	format('Found duplicate:~w~n~w~n',[arc(F1, T1, Time1),
						   arc(T1, F1, Time2)])
	;	true
	),
	(	member(arc(F,T, Time), Arcs),
		send(This?parcs, append, new(arc_template(F, T, Time, 0))),
		fail
	;	true
	),
	sort_creation_time(Nodes, Arcs, SortedNodes),
	send(This, pnodes, SortedNodes),
	send(This, st, ST),
	send(This, et, ET),
	send(This, setup_graph1).


	
set_state(This, T:real) :->
	send(This?frame, set_time, T).



run_arcs(This) :->
	send(This, mode, busy),
	send(This, rmarcs),
	send(This, flush),
	send(This?arc_queue, clear),
	send(This?parcs, for_all, message(This, current_arc, @arg1)),
	send(This, mode, ended).

variable(arc_queue, chain:= new(chain), both).
variable(old_arcs, chain := new(chain), both).

current_arc(This, Arc:arc_template) :->
	get(This, arc_no, AN),
	send(This, arc_no, AN + 1),
	(get(This, all_arcs_shown, @on)
	->	send(This?graphicals, for_all,
		     if(message(@arg1, instance_of, graph_connection),
			message(@arg1, free))),
		send(This, all_arcs_shown, @off)
	;	true
	),
	get(Arc, from, F),
	get(Arc, to, T),
	get(Arc, time, Time),
	get(This, arc, F, T, C),
	send(C, arrow_at, T),
	send(C, set_comm, Time),
	format('T:~w, ArcNo:~w  ~w -> ~w~n', [Time, AN, F, T]),
	send(This?arc_queue, add, C),
	send(This,remove_old_arcs),
	send(This, set_state, Time),
	send(This?arc_queue, for_all, message(@arg1, update_time, Time)),
	send(This?nodes, for_all, message(@arg2, update_time, Time)),
	send(This, layout, @on),
	send(This, restore_old_arcs),
	send(This, flush),
	send(@display, synchronise).




remove_old_arcs(This) :->
	send(This?old_arcs, for_all, message(This, rm_old_arc, @arg1)).

rm_old_arc(This, Arc:arc_template) :->
	(	(get(This, find_arc, Arc, C)
		->	true
		;	get(This, find_arc_rev, Arc, C)
		->	true
		;	fail
		)
	->	(send(This?arc_queue, member, C)
		->	(send(This?old_arcs, delete, Arc)
			->	true
			;	local_trace(oeps)
			)
		;	send(C, free)
		)
	;	send(@pce, format, 'WARNING:missing arc %s - %s\n',
		     Arc?from, Arc?to)
	).

find_arc_rev(This, Arc:arc_template, C:graph_connection) :<-
	get(This, node, Arc?from, @off, TN),
	get(This, node, Arc?to, @off, FN),
	get(FN, image, FI),
	get(TN, image, TI),
	get(FI, connected, TI, @default, @default, @default, C).
find_arc(This, Arc:arc_template, C:graph_connection) :<-
	get(This, node, Arc?from, @off, FN),
	get(This, node, Arc?to, @off, TN),
	get(FN, image, FI),
	get(TN, image, TI),
	get(FI, connected, TI, @default, @default, @default, C).
restore_old_arcs(This) :->
	send(This?old_arcs, for_all, message(This, arc_grey, @arg1)),
	send(This, flush),
	sleep(0.5).




half_time(This, T:real) :<-
        get(This, st, ST),
	get(This, et, ET),
	get(This?frame, half_time_fraction, F),
	T is (ET-ST)*F.

arc_grey(This, Arc:arc_template) :->
	get(This, arc, Arc?from, Arc?to, C),
	send(C, arrows, Arc?arrows),
	send(C, time, Arc?time),
	send(C, colour, grey).



arc_t(This, Arc:arc_template, Anim:bool) :->
	send(This, arc, Arc?from, Arc?to, @default),
	(Anim == @on
	->	send(This, flush),
		send(This, layout, @on)
	;	true
	).


variable(all_arcs_shown, bool := @off, both).

setup_graph1(This) :->
	send(This, mode, busy),
	send(This, cleanup1),
	get(This, pnodes, SortedNodes),
	(	member(Node, SortedNodes),
		new(C, circle(This?node_diameter)),
		send(This, node(Node, C)),
		send(C, fill_pattern, This?node_fill_colour),
		send(C, colour, This?node_pen_colour),
		send(C, pen, This?node_pen),
		fail
	;	true
	),
	(get_option(graphinit(File))
	->	send(@graphframe?picture, load_initial_file(File))
	;	send(This, layout)
	),
	send(This?parcs, for_all, message(This, arc_t, @arg1, @off)),
	(get_option(graphinit(File))
	->	send(@graphframe?picture, load_initial_file(File))
	;	send(This, layout, @on)
	),
	send(This, arc_no, 0),
	send(This, set_state, This?st),
	send(This, all_arcs_shown, @on),
	send(This, mode, steppable).


step(This) :->
	send(This, mode, busy),
	get(This?parcs, nth0, This?arc_no, Arc),
	send(This, current_arc, Arc),
	send(This, mode, steppable).
leap(This) :->
	send(This, mode, busy),
	get(This, arc_no, No),
	get(This?parcs, size, S),
	S1 is S - 1,
	(	between(No, S1, I),
		get(This?parcs, nth0, I, Arc),
		send(This, current_arc, Arc),
		fail
	;	send(This, arc_no, S)
	).

auto_arrange_initial(This) :->
	send(This, layout),
	send(This?parcs, for_all, message(This, arc_t, @arg1, @on)).




node(G, Name:label=name, Img:image=[image|graphical]) :->
	"Find/create a new node"::
	get(G, node, Name, @on, Img, _Node).

node(G, Name:label=name, Create:create=[bool], Img:image=[image|graphical],
     Node:graph_node) :<-
	"Find/create a new node"::
	get(G, nodes, Nodes),
	(   get(Nodes, member, Name, Node)
	->  (   Img == @default
	    ->	true
	    ;	send(Node, image, Img)
	    )
	;   Create == @on
	->  get(G, create_node, Name, Img, Node),
	    send(G, append, Node),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(Node, destroy))
	    ;	true
	    )
	).

create_node(_G, Name:label=name, Img:image=[image|graphical],
	    Node:graph_node) :<-
	"Create a new node from with given label"::
	new(Node, graph_node(Name, Img)).
append(G, N:'name|graph_node') :->
	"Display node at computed position"::
	(   atom(N)
	->  get(G, create_node, N, Node)
	;   Node = N
	),
	send(G, place_random, Node),
	send(G, display, Node),
	get(Node, name, Name),
	send(G?nodes, append, Name, Node),
	send(G?new_nodes, append, Node).

deleted_node(G, N:graph_node) :->
	"Node was deleted; update <-nodes"::
	get(N, name, Name),
	send(G?nodes, delete, Name),
	send(G?new_nodes, delete_all, N).
clear_g(_This, G:graphical) :->
	send(G, destroy).

clear(This) :->
	send(This?graphicals, for_all, message(This, clear_g, @arg1)),
	send(This?graphicals, clear),
	send_super(This, clear).

place_random(G, N:graphical) :->
	"Place N at random location (first in middle)"::
	get(N?area, size, size(W, H)),
	get(G, visible, area(X, Y, PW, PH)),
	(   send(G?graphicals, empty)
	->  GX is X +(PW-W)//2,
	    GY is Y +(PH-H)//2
	;   B is 10,			% Border
	    GX is X + B + random(PW-W-2*B),
	    GY is Y + B + random(PH-H-2*B)
	),
	send(N, set, GX, GY).

layout(D, All:all=[bool], Animate:animate=[bool]) :->
	"Produce automatic layout"::
	send(D, slot, layouting, @on),
	call_cleanup(layout(D, All, Animate),
		     send(D, slot, layouting, @off)).

layout(D, All, Animate) :-
	new(Nodes, chain),
	send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, graph_node),
		message(Nodes, append, @arg1))),
	get(D, visible, Area),
	(   All == @on
	->  MoveOnly = @default,
	    send(D, save_positions, Nodes)
	;   get(D, new_nodes, MoveOnly),
	    send(D, save_positions, MoveOnly)
	),
	(   MoveOnly \== @default,
	    send(MoveOnly, empty)
	->  true
	;   Animate == @off
	->  send(Nodes?head, layout, 2, 40,
		 iterations := 200,
		 area := Area,
		 network := Nodes,
		 move_only := MoveOnly)
	;   Steps = 50,			% Animated move
	    Interations is 200//50,
	    (	between(1, Steps, _),
		send(Nodes?head, layout, 2, 40,
		     iterations := Interations,
		     area := Area,
		     network := Nodes,
		     move_only := MoveOnly),
		(   get(D, request_compute, @nil)
		->  true		% No object has been moved
		;   send(D, flush),
		    sleep(0.01),
		    fail
		)
	    ;	true
	    )
	->  true
	),
	send(D?new_nodes, clear).

save_positions(_D, For:chain) :->
	"Save positions if undoable"::
	(   undoable
	->  chain_list(For, List),
	    (	member(Gr, List),
		get(Gr, position, P),
		send(@grapher_undo, prepend, message(Gr, position, P)),
		fail
	    ;	true
	    )
	;   true
	).

compute(D) :->
	"Incorporate layout of new nodes"::
	(   get(D, layouting, @off),
	    get(D, new_nodes, New),
	    \+ send(New, empty)
	->  send(D, layout, animate := @off)
	;   true
	),
	send_super(D, compute).

:- pce_end_class.

:- pce_begin_class(graph_node(name), device,
		   "Node in a graph").

variable(time, real := 0, both).
variable(strength, real := 0, both).
set_comm(This, T:real, _:{from,to}) :->
	get(This, half_time, HT),
	get(This, time, T1),
	get(This, strength, S),
	NewS is S*exp((T1-T)/HT *log(2)) + 1.0,
	send(This, strength, NewS),
	send(This, time, T).
half_time(This, T:real) :<-
        get(This?device, half_time, T).
update_time(This, _T:real) :->
	send(This, update_config, graph).


update_config(This, _What:name) :->
	get(This,image, Im),
	(send(Im, instance_of, circle)
	->	get(This?device, time, T1),
		get(This, half_time, HT),
		get(This, strength, S),
		get(This, time, T),
		Fact is S*exp((T-T1)/HT *log(2)),
		get(This?device, node_diameter, Diam),
		get(This?device, node_comm_factor, NCF),
		D1 is Diam + Diam*Fact*NCF,
		send(Im, diameter, D1),
		send(Im, fill_pattern, This?device?node_fill_colour),
		send(This, colour, This?device?node_pen_colour),
		send(This, pen, This?device?node_pen)
	;	send(This?device, report, error, 'Unhandled image')
	).

variable(highlight, bool := @off, get, "Selected state").

save_node(This, File:file) :->
	send(File, format, 'node(%s, %s).\n', This?name, This?position).
:- pce_global(@graph_node_format, make_graph_node_format).

 

	


make_graph_node_format(F) :-
	new(F, format(horizontal, 1, @on)),
	send(F, row_sep, 0),
	send(F, adjustment, vector(center)).

:- pce_global(@graph_north_handle, new(handle(w/2, 0, graph, north))).
:- pce_global(@graph_south_handle, new(handle(w/2, h, graph, south))).
:- pce_global(@graph_west_handle,  new(handle(0, h/2, graph, west))).
:- pce_global(@graph_east_handle,  new(handle(w, h/2, graph, east))).

initialise(N, Name:name, Image:[image|graphical]) :->
	"Create from Name and Image"::
	send_super(N, initialise),
	send(N, name, Name),
	send(N, format, @graph_node_format),
	(   Image == @default
	->  get(N, default_image, Img)
	;   send(Image, instance_of, image)
	->  new(Img, bitmap(Image))
	;   Img = Image
	),
	send(N, prepare_image, Img),
	send(N, display, Img),
	send(N, display, text(Name)).

device(N, Dev:device*) :->
	"Chance device (admin)"::
	(   Dev == @nil,
	    get(N, device, Old),
	    send(Old, instance_of, graph_picture)
	->  send(Old, deleted_node, N)
	;   true
	),
	send_super(N, device, Dev).




default_image(_N, Img:graphical) :<-
	"Default node image"::
	new(Img, circle(7)),
	send(Img, pen, 2).

prepare_image(_N, Img:graphical) :->
	"Prepare image for creating connections"::
	send_list(Img, handle,
		  [ @graph_north_handle,
		    @graph_south_handle,
		    @graph_west_handle,
		    @graph_east_handle
		  ]),
	send(Img, name, image).

image(N, Img:graphical) :->
	get(N, image, Old),
	(   undoable
	->  send(@grapher_undo, prepend,
		 message(N, image, Old))
	;   true
	),
	send(Old, device, @nil),
	(   get_chain(Old, connections, List),
	    member(C, List),
	    get(C, from, From),
	    get(C, to, To),
	    (	Old == From
	    ->	send(C, relate, Img, To)
	    ;	send(C, relate, From, Img)
	    ),
	    fail
	;   true
	),
	send(N, prepare_image, Img),
	send(N, display, Img),
	send(Img, hide).		% make top one

:- pce_group(part).

image(N, Img:graphical) :<-
	get(N, member, image, Img).

label(N, Label:text) :<-
	get(N, member, text, Label).

:- pce_group(connect).

disconnect(This) :->
	send(This?image, disconnect).

connect(N, To:graph_node, C:graph_connection) :<-
	"Return existing/create connection"::
	(   get(N, connected, To, C)
	->  true
	;   new(C, graph_connection(N, To))
	).

connect(N, To:graph_node, Label:[name]) :->
	"Create connection with attributes"::
	get(N, connect, To, C),
	send(C, label, Label).

connected(N, To:graph_node, Link:[link], FN:[name], TN:[name],
	  C:graph_connection) :<-
	"Find connection between two nodes"::
	get(N, image, FromImg),
	get(To, image, ToImg),
	get(FromImg, connected, ToImg, Link, FN, TN, C).

:- pce_group(selected).

selected(N, Val:bool) :<-
	get(N, highlight, Val).

selected(N, Val:bool) :->
	"Pretty selected visualisation"::
	get(N, selected, Old),
	(   Val == Old
	->  true
	;   send(N, slot, highlight, Val),
	    send(N?graphicals, for_all,
		 message(@arg1, selected, Val)),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(N, selected, Old))
	    ;	true
	    )
	).

:- pce_group(event).

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).
:- pce_global(@graph_node_popup, make_graph_node_popup).
	      
make_graph_node_recogniser(G) :-
	new(C, move_gesture(left)),
	new(P, popup_gesture(@receiver?popup)),
	new(G, handler_group(P, C)).

make_graph_node_popup(P) :-
	Node = @arg1,
	new(P, popup),
	send_list(P, append,
		  [ menu_item(delete,
			      message(Node, destroy))
		  ]).

event(N, Ev:event) :->
	(   send_super(N, event, Ev)
	->  true
	;   send(@graph_node_recogniser, event, Ev)
	).

popup(_, Popup:popup) :<-
	"Popup menu for the node"::
	Popup = @graph_node_popup.

:- pce_end_class(graph_node).





		 /*******************************
		 *	       LINK		*
		 *******************************/

:- pce_global(@graph_link, new(link(graph, graph, @default,
				    graph_connection))).

:- pce_begin_class(graph_connection, tagged_connection,
		   "Connection between two nodes").

arrow_at(This, N:name) :->
	get(This, arrows, Arrows),
	(get(This?from_node, name, N)
	->	add_arrow(Arrows, first, Arr)
	;	get(This?to_node, name, N)
	->	add_arrow(Arrows, second, Arr)
	;	send(This?frame, report, error, problem1),
		fail
	),
	send(This, arrows, Arr).
add_arrow(none, X, X) :-
	!.
add_arrow(both, _, both) :-
	!.
add_arrow(@default, X, X) :-
	!.
add_arrow(X, X, X) :-
	!.
add_arrow(first, second, both).
add_arrow(second, first, both).


update_config(_This, _What:name) :->
	format('Unimplemented update_config link~n').


variable(ideal_length, int := 30, both).

variable(time, real := 0, both).

variable(strength, real := 0, both).

/*
  ideal_length:
  force_attract = 2048.0 * C1 * log(d/C2)/d
  */
rmifold :- true.
half_time(This, T:real) :<-
        get(This?device, half_time, T).

set_comm(This, T:real) :->
	get(This, half_time, HT),
	get(This, time, T1),
	get(This, strength, S),
	NewS is S*exp((T1-T)/HT *log(2)) + 1.0,
	send(This, strength, NewS),
	send(This, time, T),
	send(This?from_node, set_comm, T, from),
	send(This?to_node, set_comm, T, to).



arc_template(This, Arc:arc_template) :<-
        new(Arc, arc_template(This?from_node?name,
			      This?to_node?name, This?time, This?strength)).
update_look(This, Fact:real) :->
	get(This?device, link_unit_width, UW),
	Width is round(UW*Fact),
	(Width < 2
	->	send(This, colour, black)
	;Width > UW
	->	send(This, colour, purple)
	;	send(This, colour, red)
	).



update_time(This, T:real) :->
	get(This, time, T1),
	get(This, half_time, HT),
	get(This, strength, S),
	Fact is S*exp((T1-T)/HT *log(2)),
	get(This?device, link_unit_width, UW),
	Width is round(UW*Fact),
	get(This, device, Dev),
	send(Dev?new_nodes, add, This?from),
	send(Dev?new_nodes, add, This?to),
	send(This, update_look, Fact),
	%send(This?from_node, update_time, T),
	%send(This?to_node, update_time, T),
	(Width < 2
	->	(rmifold
		->	get(This, arc_template, AT),
			send(Dev?old_arcs, append, AT),
			send(AT, arrows, This?arrows),
			send(This, free)
		;	send(This?device?arc_queue, delete_all, This)
		)
	;	true
	),
	(rmifold, Width < 2
	->	true
	;	send(This, pen, Width),
		get(This?device, link_min_len_strength, MDS),
		get(This?device, link_min_len, MIND),
		get(This?device, link_max_len, MAXD),
		% EffectFactor at 1/unit_width 30
		% EffectFactor at min_lenance_strength width 5
		(Fact >= MDS
		->	IL = MIND
		;	IL is (MAXD-MIND)*(MDS - Fact)/MDS + MIND
		),
		send(This, ideal_length, IL)
	).


variable(highlight, bool := @off, get, "Selected state").
variable(saved_pen, int*,	  get, "Pen saved over selection").

variable(label_font, font := italic, both).

initialise(C, From:graph_node, To:graph_node,
	   Link:[link], FH:[name], TH:[name]) :->
	"Create connection between two graph nodes"::
	default(Link, @graph_link, TheLink),
	get(From, image, IF),
	get(To, image, TF),
	send_super(C, initialise, IF, TF, TheLink, FH, TH).

label(C, Label:[name|graphical]*) :->
	"Label the arc"::
	(   Label == @default		% @default: leave as is
	->  true
	;   Label == @nil		% @nil: no label
	->  send(C, tag, @nil)
	;   atom(Label)			% atom: opaque italic text
	->  get(C, label_font, Font),
	    send(C, tag, new(T, text(Label, center, Font))),
	    send(T, background, @default)
	;   send(C, tag, Label)		% graphical: use as label
	).

label(C, Label:'name|graphical*') :<-
	"Current label"::
	get(C, tag, Tag),
	(   Tag == @nil
	->  Label = @nil
	;   get(Tag, class_name, text)	% dubious.  Should _know_ it is
	->  get(Tag, string, Label)	% a default text
	;   Label = Tag
	).

:- pce_group(selection).

selected(C, Val:bool) :<-
	get(C, highlight, Val).

selected(C, Val:bool) :->
	"Pretty selected visualisation"::
	get(C, selected, Old),
	(   Val == Old
	->  true
	;   send(C, slot, highlight, Val),
	    (	Val == @on
	    ->	get(C, pen, Pen),
		send(C, slot, saved_pen, Pen),
		NewPen is Pen + 1,
		send_super(C, pen, NewPen)
	    ;	get(C, saved_pen, Pen),
		send_super(C, pen, Pen)
	    ),
	    (	get(C, tag, Tag),
		Tag \== @nil
	    ->	send(Tag, selected, Val)
	    ;	true
	    ),
	    (	undoable
	    ->	send(@grapher_undo, prepend,
		     message(C, selected, Old))
	    ;	true
	    )
	).

pen(C, P:'0..') :->
	"Set pen (consider selection)"::
	send(C, slot, saved_pen, P),
	(   get(C, highlight, @on),
	    NP is P + 1
	;   NP = P
	),
	send_super(C, pen, NP).


		 /*******************************
		 *               C		*
		 *******************************/

from_node(C, N:graph_node) :<-
	"Graph-node at `from' side"::
	get(C, from, Img),
	Img \== @nil,
	get(Img, device, N).
	
to_node(C, N:graph_node) :<-
	"Graph-node at `to' side"::
	get(C, to, Img),
	Img \== @nil,
	get(Img, device, N).

:- pce_end_class(graph_connection).

