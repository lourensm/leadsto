:- module(trace_manager,
	  [unique_trace_manager/1,
	   trace_manager/2,
	   trace_management/4,
	   add_command_line_traces/2,
	   add_trace_file/1,
	   all_traces/2
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(util).

% Loaded traces: info:what file are they from, what content info
%                trace_name:what name to associate with trace
% Each time we (re)load a trace, we recompile all traces, or we
% set compiled @off
% Simple load_trace should present a default name for each consecutive trace.

local_option(trace_management, arg('-trace', Arg,add_trace_file(Arg),'FILE'),
	     'Load trace file FILESPEC',[]).
local_option(trace_management, arg('-traces', Arg,add_trace_file_wild(Arg),'FILE'),
	     'Load trace file(s) FILESPEC here ? and * are interpreted as wildcard codes',[]).
:- pce_begin_class(trace_manager, frame).

:- dynamic dyn_trace_manager/2.

variable(parent_frame, frame, both).

trace_manager(Frame, M) :-
	dyn_trace_manager(Frame, M).

empty(This) :->
	send(This?list_browser?dict?members, empty).

initialise(This, F:frame) :->
	send(This, send_super, initialise, 'Checker Trace Management'),
	send(This, parent_frame, F),
	new(D, dialog),
	send(This, append, D),
	send(D, name, main_dialog),
	send(D, append, new(LB,list_browser)),
	send(new(AB, button(add_trace)), below, LB),
	send(new(SB, button(save_compacted_traces)), right, AB),
	send(new(_DB, button(dismiss)), right, SB),
	send(new(report_dialog), below, D),
	send(new(DG1, dialog_group(selected_trace_info)), right, LB),
	%send(new(TI,text_item(trace_name, '')), right, LB),
	send(DG1, append, new(TI,text_item(trace_name, ''))),
	send(TI, message, message(This, change_trace_name, @arg1)),
	send(new(FI,text_item(trace_file_name, '')), below, TI),
	send(FI, length, 50),
	send(new(SI,text_item(spec_file_name, '')), below, FI),
	send(new(DI,text_item(trace_date, '')), below, SI),
	send_list([TI, FI, SI, DI], editable, @off),
	send(new(BR,button(remove_trace,message(This,remove_trace))),below,DI),
	send(BR, active, @off),
	%send(new(LB,list_browser),left, TI),
	send(LB, select_message, message(This, item_selected, @arg1)),
	%send(new(AB, button(add_trace)), below, LB),
	%send(new(DB, button(dismiss)), right, AB),
	send(This, done_message, message(This, dismiss)),
	%send(new(reporter), right, DB),
	assertz(dyn_trace_manager(F, This)).

list_browser(This, LB:list_browser) :<-
        get(?(This, member, main_dialog), member, list_browser, LB).
selection_same_name(This, Into:char_array) :->
	get(This?list_browser, selection, TI),
	send(TI?key, equal, Into).

change_trace_name(This, Into:char_array) :->
	get(This?list_browser, selection, TI),
	(send(TI?key, equal, Into)
	->	true
	;	get(TI, model, M),
		M \== @nil
	->	send(This, report, error, 'Cannot change name of set of traces'),
		fail
	;	send(This, test_trace_name, Into),
		send(TI, key, Into),
		send(This, invalidate_info)
	).
/*
report_super(This, Kind:name, Format:[char_array],Args:any...) :->
	send(This, send_super, report, Kind, Format, Args).
*/
report(This, Kind:name, Format:[char_array],Args:any...) :->
	get(This, status, Status),
	M =.. [report, Kind, Format | Args],
	(memberchk(Status, [window, full_screen, open])
	->	true
	;	%object(Args, P),
		%send(@pce, format, 'O:%O\n', Args),
		send(@pce, M)
		%send(@pce, send_super_vector, report, Kind, Format, Args)
	),
	send_super(This, M).

%	send(This,  send_vector, report_super, Kind, Format, Args).
selected_trace_info(This, I:dialog_group) :<-
        get(?(This, member, main_dialog), member, selected_trace_info, I).

item_selected(This, Item:trace_info) :->
	send(This, report, inform , ''),
	get(This, selected_trace_info, DG1),
	send(?(DG1, member, remove_trace), active, @on),
	send(Item, update_selected_trace_info, DG1).



remove_trace(This) :->
	get(This?list_browser, selection, Sel),
	Sel \== @nil,
	send(This?list_browser, selection, @nil),
	send(Sel, lock_object, @on),
	send(This?list_browser, delete, Sel),
	send(This, update_unselected_trace_info),
	(	trace_info_trace(Sel, TraceName),
		retractall(holds:atom_trace(TraceName,_,_,_)),
		retractall(holds:times(TraceName,_,_,_)),
		retractall(holds:cwa(TraceName,_)),
		fail
	;	retractall(trace_model_trace(Sel, _))
	),
	send(Sel, lock_object, @off),
	send(This, invalidate_info).

trace_info_trace(Info, Trace) :-
	get(Info, model, Model),
	(Model == @nil
	->	get(Info, key, Trace)
	;	trace_model_trace(Info, Trace)
	).

update_unselected_trace_info(This) :->
	get(This, selected_trace_info, DG),
	send(?(DG, member, trace_name), selection, ''),
	send(?(DG, member, trace_name), editable, @off),
	send(?(DG, member, trace_file_name), selection, ''),
	send(?(DG, member, spec_file_name), selection, ''),
	send(?(DG, member, trace_date), selection, ''),
	send(?(DG, member, remove_trace), active, @off).

unique_trace_manager(M) :-
	unique_trace_manager(_Frame, M).

unique_trace_manager(Frame, M) :-
	(dyn_trace_manager(Frame, M)
	->	(dyn_trace_manager(Frame1, M1),
			(Frame1 \= Frame
			;M1 \= M
			)
		->	impl_error('Multiple trace_manager')
		;	true
		)
	;	fail
	).

all_traces(This, Traces) :-
	(bagof(Trace, some_trace(This, Trace), Traces)
	->	true
	;	Traces = []
	).

some_trace(This, Trace) :-
	get(This?list_browser?dict, members, Infos),
	chain_list(Infos, InfoL),
	info_trace(InfoL, Trace).

add_trace(This) :->
	get(@finder, file, @on, tuple('Trace', '.tr'), '.',
		    TraceFile),
	send(This, load_trace, TraceFile).

next_trace_name(This, Next:[int], Name:name) :<-
        default(Next, 1, N1),
	new(N, string('trace%s', N1)),
	get(N, value, M),
	(send(This, test_trace_name, M, @on)
	->	Name = M
	;	get(This, next_trace_name, N1 + 1, Name)
	).

:- dynamic dyn_add_trace_file/1.

trace_management(Frame,Show,Constructor, M) :-
	(trace_manager(Frame, M)
	->	(Show == show
		->	send(M, expose)
		;	true
		)
	;	new(M, Constructor),
		(Show == show
		->	send(M, show, @on)
		;	true
		)
	).

add_command_line_traces(Frame, Constructor) :-
	send(@display, synchronise),
	(	dyn_add_trace_file(_)
	->	trace_management(Frame,noshow, Constructor, M),
		(	retract(dyn_add_trace_file(Arg)),
			send(M, load_trace_p, Arg),
			fail
		;	true
		)
	;	true
	).

add_trace_file_wild(Arg) :-
	expand_file_name(Arg, List),
	uchecklist(add_trace_file, List).

add_trace_file(Arg) :-
	format('Command line trace file ~w~n', [Arg]),
	assertz(dyn_add_trace_file(Arg)).
load_trace_p(This, FS:name) :->
	send(This, report, progress, 'loading trace from %s...',FS),
	(exists_file(FS)
	->	send(This, load_trace, FS),
		send(This, report, done, 'DONE',FS)
	;	send(This, report, error, 'Trace file %s does not exist', FS),
		fail
	).
load_trace(This, TraceFile:name) :->
	send(This, report, inform, 'Loading trace %s', TraceFile),
	get(This, next_trace_name, TN),
	Name = TN,
	send(This, test_trace_name, Name),
	send(This?list_browser, append,trace_info(This, TraceFile)),
	send(This, invalidate_info).


trace_info(This, Name:name, Trace:trace_info) :<-
        get(This?list_browser?dict, find, message(@arg1?key, equal,Name),
	    Trace).

invalidate_info(_This) :->
	true.

	
test_trace_name(This, Name:name, Test:[bool]) :->
	(get(This?list_browser?dict, find,
	     message(@arg1?key, equal,Name), _)
	->	(Test == @on
		->	fail
		;	send(This, report, error, 'trace label already present'),
			fail
		)
	;	true
	).

unlink(This) :->
	get(This, parent_frame, F),
	(retract(dyn_trace_manager(F, This))
	->	true
	;	impl_error('checker_trace_manager not registered')
	),
	send(This, send_super, unlink).

dismiss(This) :->
	send(This, show, @off).












info_trace([Info|_Infos], Trace) :-
	trace_info_trace(Info, Trace).
info_trace([_Info|Infos], Trace) :-
	info_trace(Infos, Trace).




	







	
:- pce_end_class.


:- pce_begin_class(trace_info, dict_item).
variable(file, name, both).
variable(content_read, bool:= @off, both).
variable(spec_file_name, name*, both).
variable(trace_date, name*, both).
variable(atom_traces_loaded, bool:= @off, both).
variable(model, prolog* := @nil, both).
variable(model_size, int := 1, both).

update_selected_trace_info(This, DG:dialog_group) :->
	send(?(DG, member, trace_name), selection, This?key),
	send(?(DG, member, trace_name), editable, @on),
	send(?(DG, member, trace_file_name), selection, This?file),
	send(?(DG, member, spec_file_name), selection, 'UNKNOWN'),
	send(?(DG, member, trace_date), selection, 'UNKNOWN'),
	(get(This, content_read, @on)
	->	get(This, spec_file_name, SF),
		(SF == @nil
		->	true
		;	send(?(DG, member, spec_file_name), selection, SF)
		),
		get(This, trace_date, TD),
		(TD == @nil
		->	true
		;	send(?(DG, member, trace_date), selection, TD)
		)
	;	send(This, reread, DG)
	).



reread(This, DG:dialog_group*) :->
	(DG == @nil
	->	true
	;	send(?(DG, member, spec_file_name), selection, 'UNKNOWN'),
		send(?(DG, member, trace_date), selection, 'UNKNOWN')
	),
	get(This, file, File),
	open(File,read, R, []),
	repeat,
	read_term(R, Term, []),
	(	Term == end_of_file
	->	!,close(R)
	;	(get_content(Term, This, DG)
		->	fail
		;	!,close(R)
		)
	).

load_atom_traces(This) :->
	(get(This, atom_traces_loaded, @on)
	->	true
	;	get(This, key, TraceName),
		assert_debug(\+ holds:atom_trace(TraceName, _Key, _Atom, _Ranges)),
		get(This, file, File),
		open(File, read, R, []),
		repeat,
		read_term(R, Term, []),
		(	Term == end_of_file
		->	!,close(R)
		;	get_trace_info(Term, This),
			fail
		),
		send(This, atom_traces_loaded, @on)
	).


:- dynamic holds:atom_trace/4.
:- dynamic holds:times/4.
:- dynamic holds:cwa/2.

trace_entry(atom_trace(Key, Atom, Ranges),TraceName,
	    atom_trace(TraceName, Key, Atom, Ranges)).
trace_entry(times(TSetup, THandled, ET), TraceName,
	    times(TraceName,TSetup, THandled, ET)).
trace_entry(cwa(Atom), TraceName, cwa(TraceName, Atom)).
ignore_trace_entry(content(_)).
ignore_trace_entry(d(_,_)).
get_trace_info(Info, _This) :-
	ignore_trace_entry(Info),
	!.
get_trace_info(Info, This) :-
	get(This, model, M),
	(M == @nil
	->	get(This, key, TraceName),
		trace_entry(Info, TraceName, Data)
	;	trace_entry(_, TraceName, Info),
		Data = Info
	),
	!,holds:assertz(Data).
get_trace_info(trace(Trace), This) :-
	get(This, model, M),
	M \== @nil,
	!,
	(trace_model_trace(_Other, Trace)
	->	error('Multiple identical traces ~w',[Trace]),
		fail
	;	holds:atom_trace(Trace, _Key, _Atom, _Ranges)
	->	error('Multiple identical traces ~w',[Trace]),
		fail
	;	true
	),
	assertz(trace_model_trace(This, Trace)).

get_trace_info(Info, _This) :-
	warning('Unhandled trace entry ~w', Info).

:- dynamic trace_model_trace/2.

get_content(content(source(file(FileName, Parts))),This,DG) :-
	!,
	(	memberchk(path(Path), Parts)
	->	F = Path
	;	F = FileName
	),
	(DG == @nil
	->	true
	;	send(?(DG, member, spec_file_name), selection, F)
	),
	send(This, spec_file_name, F).

get_content(content(run(RunData)),This,DG) :-
	!,
	(	memberchk(date(Date), RunData)
	->	(DG == @nil
		->	true
		;	send(?(DG, member, trace_date), selection, Date)
		),
		send(This, trace_date, Date)
	;	true
	).
get_content(content(_), _This, _DG).

get_content(model(M, Size), This, _DG) :-
	!,
	send(This, model, M),
	send(This, model_size, Size).




initialise(This, Mgr:trace_manager, File:name) :->
	send(This, send_super, initialise, undefined),
	send(This, file, File),
	send(This, reread, @nil),
	get(This, model, M),
	(M == @nil
	->	get(Mgr, next_trace_name, Name)
	;	term_to_atom(M, Name)
	),
	send(This, key, Name).




:- pce_end_class.

