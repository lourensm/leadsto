:- module(simtool,
	  []).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(util).
:- use_module(library(pce_helper)).
:- use_module(library(pce_report)).
:- use_module(library(help_message)).
:- use_module(library(toolbar)).
:- use_module(library(find_file)).
:- pce_global(@finder, new(finder)).
:- use_module(psprinting).

:- use_module(showtrace).
:- use_module(algo).
:- use_module(rolegraph).
%:- use_module(fv).

:- require([listen/2,listen/3]).

:- multifile version/2.
:- discontiguous version/2.

:- [lt_config].

version(simtool, 2).

savealone(P):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(P, [goal=simtool:go, autoload=true, stand_alone=true,
			  global=20000]),
	halt.
savealone_nsa(P):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(P, [goal=go, autoload=true, global=20000]),
	halt.
go :-
	catch_fatal_halt(runsetup('LEADSTO', leadsto, File),
			 leadsto_exception),
        set_option(pxor),
	(fail,is_local
	->	%set_prolog_flag(debug_on_error,true),
		go1(File)
	;	catch_fatal_halt(go1(File), leadsto_exception)
	).

go1(File) :-
	%runsetup('LEADSTO', leadsto, File),
	(get_option(noshow)
	->	(File == []
		->	fatal_error('leadsto -noshow requires argument file in GUI version')
		;	format('Option -noshow => no GUI~n'),
			ensure_loaded_config(lt_config:_ConfigFile),
			readrunspec(File)
		)
	;	go2(File)
	).
go2(File) :-
	ensure_loaded_config(lt_config:_ConfigFile),
	new(ST, sim_tool),
	(File == []
	->	send(ST, open),
		(get_option(displaytrace)
		->	send(ST, error, displaytrace_no_file)
		;	true
		)
	;	get_option(displaytrace)
	->	send(ST, open),
		%local_trace(do),
		send(ST, load_trace_file, File)
	;	send(ST, open),
		(	send(ST, load_run_sim_file, File)
		->	true
		;	finalhalt(1)
		),
		send(ST, open)
	).
/*
	(   fv_lt_initialisation(ST)
	->  true
	;   true
	).
*/



:- pce_begin_class(sim_tool, frame).

resource(printer,  image,  image('16x16/print.xpm')).
resource(floppy,   image,  image('16x16/save.xpm')).
resource(open,     image,  image('16x16/open.xpm')).
resource(reopen,   image,  image('16x16/refresh.xpm')).

variable(loaded_kind,   {trace, sim}*, both).


quit(This) :->
	send(This, free),
	halt.

reload(This) :->
	(	get(This, loaded_kind, ST),
		ST \== @nil,
		get(This, file_name, File)
	->	(send(This, reset_loaded, @on)
		->	send(This, load_file, File)
		;	send(This, report, error, 'Cannot unload current simulation, please restart')
		)
	;	send(This, report, error, 'No file loaded, cannot reload'),
		fail
	).

load(This) :->
	(send(This, reset_loaded, @on)
	->	get(@finder, file, @on, chain(tuple('Leads To','.lt'),
					      tuple('Trace', '.tr')), '.',
		    File),
		send(This, load_file, File)
	;	send(This, report, error, 'Cannot unload current simulation, please restart')
	).
load_file(This, File:name) :->
	(atom_concat(_, '.lt', File)
	->	send(This, load_run_sim_file, File)
	;	atom_concat(_, '.tr', File)
	->	send(This, load_trace_file, File)
	;	send(This, report, error, 'IMPL ERROR:unrecognised fie extension for %s', File)
	).
print(This) :->
	get(This, picture, P),
	(get(@pce, convert, win_printer, class, _)
	->	get(This, picture, P),
		print_canvas(P)
	;	print_postscript(This, P)
	).


file_item(This, FI:text_item) :<-
        get(?(This, member, control_dialog), member, file, FI).

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

initialise(This) :->
	send(This, send_super, initialise, 'Leads To Simulation Tool'),
	ignore(util:send(This, icon, resource(tree_icon))),
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
	send(new(P,show_trace), below, D),
	send(P, ver_stretch, 100),
	send(new(D2, dialog), below, P),
	send(This, reporter_dialog, D2),
	send(D2, resize_message, message(D2, layout, @arg2)),
	send(D2, append, new(DB,button(quit,message(This, quit)))),
	send(DB, reference, point(0,DB?height)),
	send(new(reporter), right, DB),
	(   get_config(lt_config:history/geometry/main_window, Geometry)
	->	send(This, geometry, Geometry)
	;	true
	).

variable(reporter_dialog, dialog, both).

resize(This) :->
	get(This, geometry, G),
	set_config(lt_config:history/geometry/main_window, G),
	send(This, send_super, resize).

unlink(This) :->
	unlisten(This),
	get(This, geometry, Geometry),
	send(This, send_super, unlink),
	set_config(lt_config:history/geometry/main_window, Geometry).

reset_loaded(This, Test:[bool]) :->
	get(This, loaded_kind, Kind),
	(Kind == @nil
	->	assert_debug(\+ currently_loaded(_, _))
	;	currently_loaded(Kind, _File)
	->	(reset_algo(Test)
		->	(Test == @on
			->	true
			;	send(This, set_file, '', @nil)
			)
		;Test == @on
		->	fail
		;	send(This, report, error, 'Could not unload existing trace')
		)
	;	fatal_error('Loaded kind mismatch')
	),
	(Test == @on
	->	true
	;	send(This?picture, clear)
	).




may_load_sim(This) :->
	send(This, reset_loaded, @on).



may_load_trace(This) :->
	send(This, reset_loaded, @on).

load_only_sim(This, Test:[bool]) :->
	send(This, reset_loaded, Test),
	(Test == @on
	->	true
	;	get(@finder, file, @on, '.lt', '.', File),
		send(This, load_only_sim_file, File)
	).

load_only_sim_file(This, File:name, Test) :->
	send(This, reset_loaded, Test),
	(Test == @on
	->	true
	;	send(This, set_file, File, sim),
		send(?(This, member, show_trace), adjust_height, @on),
		send(This, report, progress, 'Loading specification %s...',
		     File),
		load_simulation(File),
		send(This, report, done, 'Loaded specification %s.', File)
	).

show_communication_graph(This) :->
	loaded_traces(_Kind1, File1, Traces, StartTime, EndTime),
	do_show_graphs(File1, Traces, This, StartTime, EndTime).

run_loaded_simulation(This, Test:[bool]) :->
	(sim_status(File, loaded)
	->	(Test == @on
		->	true
		;	send(This, report,progress,'Running specification...'),
			(   runshowspec(This)
			->
			    send(This, report, done, 'Simulation completed')
			;   send(This, report,error,'Running specification failed')
			)
		)
	;	Test == @on
	->	fail
	;	algo:dyn_sim_status(File, _)
	->	send(This, report, error, 'Can only run once'),
		fail
	;	send(This, report, error, 'No simulation loaded'),
		fail
	).



load_run_sim_file(This, File:name) :->
	send(This, reset_loaded),
	send(This, set_file, File, sim),
	send(?(This, member, show_trace), adjust_height, @on),
	send(This, report,progress,'Loading and running specification %s...',
	    File),
	run_simulation(File, This),
	send(This, report, done, 'Simulation completed').

load_run_sim(This) :->
	send(This, report,inform,''),
	send(This, reset_loaded),
	get(@finder, file, @on, '.lt', '.', File),
	send(This, set_file, File, sim),
	send(?(This, member, show_trace), adjust_height, @off),
	send(This, report,progress,'Loading and running specification %s...',
	    File),
	run_simulation(File, This),
	send(This, report, done, 'Simulation completed').



load_trace_file(This, File:name) :->
	send(This, reset_loaded),
	send(This, load_trace_file1, File).

load_trace_file1(This, File:name) :->
	send(This, set_file, File, trace),
	send(This, report,progress,'Loading trace %s...',
	    File),
	load_show(File, This),
	send(This, report, done, 'show completed').
load_trace(This) :->
	send(This, reset_loaded),
	get(@finder, file, @on, '.tr', '.', File),
	send(This, load_trace_file1, File).



file_name(This, F:char_array) :<-
        get(This?file_item, selection, F),
	F \= '',
	F \= @nil.


set_file(This, File:name, Kind:{trace,sim}*) :->
	send(This, loaded_kind, Kind),
	send(This?file_item, selection, File).

picture(This, P:show_trace) :<-
        get(This, member, show_trace, P).



fill_tool_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, tool_bar, TB),
	send_list(TB, append,
		  [ tool_button(load,
				resource(open),
				'load and run'),
		    gap,	% skip a little
		    tool_button(reload,
				resource(reopen),
				'Reload and run',
				and(This?file_name \== '',
				    This?loaded_kind \== @nil)),
		    gap,	% skip a little
		    tool_button(print,
				resource(printer),
				print)
		  ]).
fill_menu_bar(This) :->
	get(This, member, control_dialog, D),
	get(D, member, menu_bar, MB),
	send_list(MB, append,
		  [ new(File, popup(file)),
		    %new(_Edit, popup(edit))
		    new(Settings, popup(settings))
		  ]),
	send_list(File, append,
		  [ menu_item(load_sim,
			      message(This, load_run_sim),
			      'Load and Run Simulation..',
			      @default,
			      message(This, may_load_sim)),
		    menu_item(load_trace,
			      message(This, load_trace),
			      'Load and Show Trace..',
			      @default,
			      message(This, may_load_trace)),
		    menu_item(reload_run_show,
			      message(This, reload),
			      'Reload and Run/Show..',
			      @default,
			      and(This?file_name \== '',message(This, reset_loaded, @on))),
		    menu_item(load_only_sim,
			      message(This, load_only_sim),
			      'Load Simulation..',
			      @default,
			      message(This, load_only_sim, @on)),
		    menu_item(run_loaded_simulation,
			      message(This, run_loaded_simulation),
			      'Run loaded simulation..',
			      @default,
			      message(This, run_loaded_simulation, @on)),
/*
		    menu_item(show_communication_graph,
			      message(This, show_communication_graph),
			      'Show Communication Graph..',
			      @default,
			      message(@prolog, trace_loaded)),
		    menu_item(show_fleet_visualisation,
			      message(@prolog,fv_extend_lt, This),
			      'Show Fleet Visualisation..',
			      @default,
			      message(@prolog, fv_lt_extension)),
*/
		    @ps_picture_popup,
		    menu_item(quit,
			      message(This, quit))
		  ]),
	send(Settings, append, menu_item(preferences,
					  message(This, preferences),
					  'Preferences ...')),
	send(Settings, append, menu_item(logging,
			      message(This, logging),
			      'Logging...')).

preferences(This) :->
        %trace,
	edit_config(lt_config:This).

logging(_This) :->
	send(@log_settings, expose).

:- pce_end_class.
