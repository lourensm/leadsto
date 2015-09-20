:- module(algo,
	  [
	   trace_loaded/0,
	   loaded_traces/5,
	   tr_basic_element/3,
	   run_ops/1,
	   efgh0/5,
	   runshowspec/1,
	   sim_status/2,
	   load_simulation/1,
	   readrunspec/1,
	   reset_algo/1,
	   currently_loaded/2,
	   default_value/2,
	   ensure_handled_time/1,
	   ensure_setup_time/1,
	   runsetup/3,
	   run_simulation/2,
	   load_show/2,
	   missing_range1/4,
	   atom_trace/3,
	   lt_rule/6
	  ]).

:- use_module(library(lists)).
:- use_module(psprinting).
:- use_module(varutil).
:- use_module(logutil).
:- use_module(modelchecking).
:- use_module(rolegraph).
%:- use_module(fv).
:- op(1001, yfx, lor).
:- spec:op(1001, yfx, lor).
%:- spec:op(1001, yfx, '|').

:- multifile user:version/2.

:- dynamic display_number_range/4.
:- dynamic display/2.

/*
  Allow:
  constant(name, value)
  effect: at certain places the constant will be substituted.

  leadsto(Ranges, Antecedent, Consequent, Delay).
  Ranges : [PrologVar:sortspec,..],
  Antecedent : literal and|or literal
  Consequent: literal
  Delay: number or efgh(E, F, G, H)

  at(Ranges, time, AndAntecedent).
  between(Ranges, time, time, AndAntecedent).
*/
/*
  ALGORITHM

  HandledTime:
  There are a number of pending tasks stored in dyn_wait(TimeA, Task)
  facts. These tasks may have no effect past HandledTime.
  Some of these tasks may lead to firing of rules.

  ALGO:
      Do a STEP until handled time > end_time

  STEP:
      Perform pending fire actions.
      Next Handled Time := first possible time that any pending
                           Task could lead to change.
      Update HandledTime
      Cleanup traces: remove atom traces that cannot affect the future
      unless explicitly added to.
      If New HandledTime < end_time
           inspect all pending activities with TimeA < New HandledTime
  Do not store CWA and unknown, it is allowed, but not obliged.
  If T <= HandledTime we may calculate default intervals.
  There is also a TSetup:
  Below that time everything is unknown, unless explicitly set by
  interval rules.


  Pending Activities;

  WAIT VAR: Placeholder for rule containing variables for those
  instantiations of variables that are not explicitly separately
  handled.
  wait_var(HT,TMin,FV,FVL,LitData,ToDoAnte, AnteHolds,THolds,ConseRId,PV,
           Delay,Id)
  We know AnteHolds holds between TMin and THolds.
  AnteHolds contains ground Antecedent part that holds.
  LitData is current literal info.
  ConseRId is consequent info.
  Id seems to link wait_var entry with instantiated current literals.
  */
:- use_module(pce_stuff).

:- use_module(util).

:- use_module(ltversion).

:- dynamic
	dyn_leadsto/5,
	dyn_cwa/1,
	dyn_start_time/1,dyn_end_time/1, dyn_atom_trace/3,
	dyn_atom_trace_backup/3.

:- dynamic dyn_setup_time/1, dyn_handled_time/1.

user:version(simalgo, 11).


local_option(ltsim, arg('-savetrace', Arg,set_option(savetrace(Arg)),'FILE'),
	     'Save trace in file FILE',[]).
local_option(ltsim, single('-nosavetrace', set_option(nosavetrace)),
	     'Do not (automatically save trace',[]).
local_option(ltsim, single('-displaytrace', set_option(displaytrace)),
	     'Last argument should be saved trace: load this trace',[]).
local_option(ltsim, single('-uncheckhandled', set_option(uncheckhandled)),
	     '(DEBUGGING(old buggy algorithm))',[]) :-
             check_handled_enabled.
local_option(ltsim, single('-checkhandled', set_option(checkhandled)),
	     '(BUGFIXING)',[]) :-
             \+ check_handled_enabled.
local_option(ltsim, single('-noroundoff', set_option(exact_atom_id)),
	     'Do not round real numbers',[]).
local_option(ltsim, single('-pxor', set_option(pxor)),
	     'Enable probabilistic or',[]).
local_option(ltsim, single('-modelchecking', set_option(modelchecking)),
	     'Modelchecking research',[]).
local_option(ltsim, single('-pc', setprof(cumulative)),'Debugging:Profiling(cumulative)',
	     [help_sort(back(10))]).
local_option(ltsim,single('-pp', setprof(plain)),
	     'Debugging:Profiling(plain)',
	     [help_sort(back(10))]).
local_option(ltshow, arg('-addterm', Arg, set_option(addterm(Arg)), 'SPECTERM'),
	     'Add LTTERM to lt specification',[]).
local_option(ltshow, single('-noshow', set_option(noshow)),
	     'Do not show trace in picture at all',[]).

local_option(ltshow, arg('-view', Arg, set_option(view(Arg)), 'VIEWTAG'),
	     'Display trace according to VIEWTAG',[]).
local_option(ltshow, arg('-ps', Arg, set_option(ps(Arg)), 'FILE'),
	     'Save picture of trace as postscript in FILE',[]).

local_option(ltshow, arg('-tracerangepixels', Arg, set_option(ltshow(rangepixels(Arg))), 'PIXELNUMBER >= 20 (default 100)'),
	     'MINIMUM width in pixels of trace time range',[]).
local_option(ltshow, arg('-paging', Arg, set_option(ltshow(paging(Arg))),
			 'a4:split into A4 chunks, number:h/w'),
	     'MINIMUM width in pixels of trace time range',[]).
local_option(ltshow, arg('-tracepicturepixels', Arg, set_option(ltshow(picturepixels(Arg))), 'PIXELNUMBER >= 200 (default 1000)'),
	     'MINIMUM width in pixels of trace atom label+time range',[]).


savealone(Name):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(Name, [goal=run, autoload=true, stand_alone=true,global=25000]),
	halt.

savealone_nsa(Name):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(Name, [goal=run, autoload=true, global=25000]),
	halt.




run :-
	catch_fatal_halt(runsetup('LTBARE', ltbare, File), ltbare_exception),
	(is_local
	->	debug,
		set_prolog_flag(debug_on_error, true),
		debug,
		readrunspec(File)
	;	catch_fatal_halt(readrunspec(File), ltbare_exception)
	).


runsetup(Header, PgmName, File) :-
	setup_logging(PgmName, [algo,util,formframe]),
	%set_prolog_flag(iso, true),
	setupmainnew(Header, PgmName, [os(util,[wd,constant,log,debugging]),
				       os(algo,[ltsim,ltshow]),
				       os(rolegraph,[rolegraph]),
				       os(fv,[fv])], File).









run_simulation(File) :-
	run_simulation(File, []).


:- dynamic dyn_sim_status/2.
sim_status(X,Y) :-
	dyn_sim_status(X,Y).


run_simulation(File, Frame) :-
	load_simulation(File),
	runshowspec(Frame).


set_add_term(Spec,Arg) :-
	atom_to_term(Arg, Term, Bindings),
	(var(Arg)
	->	error('unrecognised addterm suboption ~w', [Arg])
	;	set_add_term1(Term, Spec, Arg, Bindings)
	).
simple_add_term(tr,display(_,_)).
simple_add_term(tr,display_number_range(_Atom, _Var, _Label, _VarLabel)).
simple_add_term(lt,start_time(_ST)).
simple_add_term(lt,end_time(_ET)).
simple_add_term(lt,cwa(_)).
simple_add_term(lt,global_lambda(_Lambda)).
simple_add_term(lt,constant(_Name, _Value)).

set_add_term1(Term, Spec, _Arg, _Bindings) :-
	simple_add_term(Spec1,Term),
	member(Spec1, Spec),
	handle_term(Term),!.
set_add_term1(_Term, _Spec, Arg, _Bindings) :-
	error('Could not handle -addterm ~w', [Arg]).


add_cmd_terms(List) :-
	(	get_option(addterm(Arg)),
		set_add_term(List, Arg),
		fail
	;	true
	).

load_module_fatal1(File, spec, _FErrorTag) :-
	open(File, read, S),
	repeat,
	spec:read(S, Term),
	(   Term == end_of_file
	->  !
	;   %format('R:~w~n', [Term]),
	    spec:assertz(Term),
	    fail
	),
	close(S).

load_simulation(File) :-
	add_import_module(spec, lists, true),
	assert_debug(\+ dyn_sim_status(_,_)),
	assert_debug(\+ currently_loaded(_, _), load_sim1),
	spec:op(700,xfx,<=),
	spec:discontiguous(leadsto/4),
	spec:discontiguous(leadsto/3),
	spec:discontiguous(interval/4),
	spec:discontiguous(interval/3),
        spec:discontiguous(qterm/1),
	set_dynamic_spec,
	(load_module_fatal1(File, spec, initial_load_spec)
	->	assertz(dyn_currently_loaded(sim, File))
	;	fatal_error('Could not load simulation specification "~w"',
			    [File]),
		clear_module(spec),
		fail
	),
	load_cmd_constants,
	reset_constant_use,
	reset_sorts,
	(predicate_property(spec:sortdef(_,_), interpreted)
	->	true
	;	spec:assertz((sortdef(_,_):- fail))
	),
	open(File, read, _, [alias(spec)]),
	repeat,
	spec:read(spec, Term),
	(	(Term == end_of_file
		->	!,update_wd
		;	flag(ht1, HT1, HT1 + 1),
			send(@display, synchronise),
			alog(readterm, '~w:~w~n', [HT1,handle_term(Term)]),
			handle_term(Term)
		->	fail
		;	impl_error('handle_term(~w) failed', [Term]),
			fail
		)
	->	update_sorts,
		assertz(dyn_sim_status(File, loaded)),
		close(spec),
		add_cmd_terms([lt,tr]),
		update_wd,!
	;	fail
	).

readrunspec(File) :-
	(File == []
	->	(is_win32
		->	(get_option(displaytrace)
			->	Ext = tuple('LTo Trace', '.tr')
			;	Ext = chain(tuple('LTo Spec', '.lt'),'.pl')
		        ),
			win_get_file(File1,Ext)
		;	File1 = 'spec/spec1.lt'%'default.pl'
		)
	;	File1 = File
	),
	protocol(log),
	(	get_option(displaytrace)
	->	load_show(File1)
	;	run_simulation(File1)
	),
	noprotocol.
:- debug(algo).
runshowspec(Picture) :-
	(retract(dyn_sim_status(File, loaded))
	->	true
	;	impl_error('Trying to run, no specification loaded'),
		fail
	),
	assertz(dyn_sim_status(File, running)),
	doprof,
	debug(algo, 'running', []),
	(runspec(Picture)
	->	endprof,
		showprof
	;	endprof,
		showprof,
		fail
	),
	(retract(dyn_sim_status(File, running))
	->	assertz(dyn_sim_status(File, done))
	;	impl_error('Running specification inconsistency'),
		fail
	),
	debug(algo, 'show results', []),
	show_results(File,Picture).



atom_trace(AtomKey, Atoma, AtomTrace) :-
	dyn_atom_trace(AtomKey, Atoma, AtomTrace).
atom_trace(AtomKey, Atoma, AtomTrace) :-
	dyn_atom_trace_backup(AtomKey, Atoma, AtomTrace).

/*
display_number_range(Atom, Var, Label, VarLabel) :-
	predicate_property(load_results:display_number_range(_, _, _, _),
			   interpreted),
	load_results:display_number_range(Atom, Var, Label, VarLabel).
*/

%savetrace :-
%	savetrace([],[]).



savetracesetup(File,Frame, Telling) :-
	(get_option(nosavetrace)
	->	fail
	;	get_option(savetrace(File))
	->	true
	;	File = 'trace.tr'
	),
	version_details(_, App),
	(currently_loaded(sim, SourceFile)
	->	true
	;	impl_error('Cannot save trace, no sim loaded'),
		(Frame == []
		->	true
		;	send(Frame, report, error,
		             'Cannot save trace, no sim loaded')
		)
	),
	(telling(Telling)
	->	true
	;	Telling = fail(0)
	),
	(tell_error(File)
	->	true
	;	Frame == []
	->	fail
	;	pl_pce(File, FX),
		send(Frame, report, error, 'Could not open trace file "%s" for writing', FX),
		fail
	),
	portray_clause(content(type(savedtrace(SourceFile)))),
	portray_clause(content(generator(App))),
	source_details(SourceFile, Details),
	portray_clause(content(source(Details))),
	get(new(date)?string, value, Current),
	(bagof(constant(Name,Value),cmd_constant(Name, Value), Constants)
	->	portray_clause(content(cmd_constants(Constants)))
	;	true
	),
	portray_clause(content(run([date(Current)]))),
	(	display_number_range(P, Q, R, S),
		portray_clause((dnr(P, Q, R, S))),
		fail
	;	true
	),
	(	display(V,W),
		portray_clause((d(V,W))),
		fail
	;	true
	).
trace_entry([],In, In) :-
	!.
trace_entry(Trace, In, Out) :-
	In =..[F|Args],
	Out =.. [F,Trace|Args].

portray_trace_entry(Trace, Term) :-
	trace_entry(Trace, Term, T),
	portray_clause(T).
savetrace1(Trace) :-
	findall(AtomKey-Atoma-AtomTrace,
	       atom_trace(AtomKey, Atoma, AtomTrace),
	       Traces),
	(   current_prolog_flag(float_format, OF)
	->  true
	;   true%format('No float_format~n')
	),
	(old_atom_key_id
	->      true
	;       set_prolog_flag(float_format, '%.17g')
	),
	(	member(AtomKey-Atoma-AtomTrace, Traces),
		fill_trace1(AtomTrace, AtomKey, Atoma, TT),
		portray_trace_entry(Trace,TT),
		fail
	;	cwa(Atom),
		portray_trace_entry(Trace,cwa(Atom)),
		fail
	;	ensure_handled_time(THandled),
		ensure_setup_time(TSetup),
		end_time(ET),
		portray_trace_entry(Trace,times(TSetup, THandled, ET))
	),
	(   var(OF)
	->  true
	;   set_prolog_flag(float_format, OF)
	).

savetrace(Frame) :-
	savetracesetup(File,Frame, Telling),
	!,
	savetrace1([]),
	told,
	(	Telling = fail(0)
	->	true
	;	tell(Telling)
	),
	format('SAVED TRACE IN ~w~n',[File]).
savetrace(_Frame).

fill_trace1(AtomTrace, AtomKey, Atoma, atom_trace(AtomKey, Atoma, TrTrace)) :-
	default_value(Atoma, FU),
	end_time(ET),
	fill_ranges(AtomTrace, ET, FU, TrTrace),
	!.
fill_trace1(AtomTrace, AtomKey, Atoma, TT) :-
	fatal_fail(fill_trace1(AtomTrace, AtomKey, Atoma, TT)).



fill_ranges([], ET, FU, TrTrace) :-
	fill_ranges1([], ET, FU, TrTrace).

fill_ranges([range(Tlo,Thi,TF)|AT], ET, FU, TrTrace) :-
	(cmp_ge(Tlo, ET)
	->	fill_ranges(AT, ET, FU, TrTrace)
	;cmp_ge(Thi, ET)
	->	min_new(ET, Tlo, ET1),
		fill_ranges(AT, ET1, FU, TrTrace1),
		TrTrace = [range(Tlo,Thi,TF)|TrTrace1]
	;	fill_ranges1([range(Tlo,Thi,TF)|AT], ET, FU, TrTrace)
	).

fill_ranges1([], PrevTlo, FU, TrTrace) :-
	ensure_setup_time(TStart),
	(cmp_gt(PrevTlo, TStart)
	->	TrTrace = [range(TStart,PrevTlo,FU)]
	;	TrTrace = []
	).

fill_ranges1([range(Tlo,Thi,TF)|AT], PrevTlo, FU, TrTrace) :-
	(cmp_gt(PrevTlo, Thi)
	->	ensure_setup_time(TStart),
		(cmp_ge(Thi, TStart)
		->	TrTrace = [range(Thi,PrevTlo,FU),
				   range(Tlo,Thi,TF)|TrTrace1]
		;	cmp_ge(TStart, PrevTlo)
		->	TrTrace = [range(Thi,PrevTlo,unknown),
				   range(Tlo,Thi,TF)|TrTrace1]
		;	TrTrace = [range(TStart,PrevTlo,FU),
				   range(Thi,TStart,unknown),
				   range(Tlo,Thi,TF)|TrTrace1]
		),
		fill_ranges1(AT, Tlo, FU, TrTrace1)
	;cmp_lt(PrevTlo, Thi)
	->	impl_error('Strange interval s')
	;	TrTrace = [range(Tlo,Thi,TF)|TrTrace1],
		fill_ranges1(AT, Tlo, FU, TrTrace1)
	).

trace_loaded :-
	currently_loaded(Kind1, _File1),
	(       Kind1 == trace,
		lrat(_Atoma, _TrTrace),
		start_time(_StartTime),
		end_time(_EndTime)
	->	true
	;	Kind1 == sim,
		atom_trace(_, _, _)
	->	start_time(_),
		end_time(_)
	;	fail
	).


loaded_traces(Kind1, File1, Traces, StartTime, EndTime) :-
	assert_debug(var(Kind1)),
	assert_debug(var(Traces)),
	assert_debug(var(StartTime)),
	assert_debug(var(EndTime)),
	currently_loaded(Kind1, File1),
	(Kind1 == trace
	->	bagof(Atoma-TrTrace,
		      lrat(Atoma, TrTrace),
		      Traces),
		start_time(StartTime),
		end_time(EndTime)
	;Kind1 == sim
	->	bagof(Atoma-AtomTrace,
		      AtomKey^atom_trace(AtomKey, Atoma, AtomTrace),
		      Traces),
		start_time(StartTime),
		end_time(EndTime)
	;	fail
	).



show_results(File,PictureFrame) :-
	get_option(noshow),
	!,
	(	PictureFrame == []
	->	(do_graphs
		->	(bagof(Atoma-AtomTrace, AtomKey^atom_trace(AtomKey, Atoma, AtomTrace),
			       Traces)
			->	start_time(ST),
				end_time(ET),
				do_show_graphs(File,Traces, [], ST, ET)
			;	warning('No atoms set at all, NO TRACE'),
				user_forced_halt
			)
		;	format('~nSpecified no show, will exit~n~n'),
			user_forced_halt
		)
	;	send(PictureFrame, report, error, '-noshow option conflict with show'),
		fail
	).
show_results(_File,_PictureFrame) :-
	dyn_model(_Model),
	!,
	warning('Cannot show multiple traces yet'),
	finalhalt(0).

show_results(File,PictureFrame) :-
	ensure_no_graph,
	(bagof(Atoma-AtomTrace, AtomKey^atom_trace(AtomKey, Atoma, AtomTrace),
	       Traces)
	->	show_traces(File,Traces, PictureFrame)
	;	warning('No atoms set at all, NO TRACE'),
		show_traces(File,[], PictureFrame)
	).
lrat(_Atoma, _TrTrace) :-
	\+ predicate_property(load_results:atom_trace(_A, _, _T),interpreted),
	!,
	fwarning(load_trace(load, no_atom_trace)),
	fail.
lrat(Atoma, TrTrace) :-
	load_results:atom_trace(_AtomKey, Atoma, TrTrace).

:- dynamic dyn_currently_loaded/2.

reset_algo(Test) :-
	(currently_loaded(Kind, File)
	->	(Kind == trace
		->	(Test == @on
			->	true
			;	reset_trace_info
			)
		;Kind == sim
		->	(Test == @on
			->	true
			;	reset_sim_info
			)
		;	impl_error('Unknown loaded file ~w of kind ~w',
				   [File, Kind])
		)
	;	true
	).
test_reset_sim_info :-
	dyn_sim_status(File, _Status),
	dyn_currently_loaded(sim, File).

sim_entry(E) :-
	sim_entry(E,_).


sim_entry(display(_View, _What),spec).
sim_entry(display_number_range(_Atom, _Var, _Label, _VarLabel),spec).
sim_entry(dyn_model(_),spec).
sim_entry(dyn_interval(_),spec).
sim_entry(dyn_start_time(_ST),spec).
sim_entry(dyn_end_time(_ST),spec).
sim_entry(dyn_handled_time(_THandled),run).
sim_entry(dyn_setup_time(_),run).
sim_entry(dyn_cwa(_Atom),spec).
sim_entry(dyn_lambda(_Lambda),spec).
sim_entry(dyn_leadsto(_I, _Vars, _LitDisConj, _AndLiterals, _Delay),spec).
sim_entry(dyn_atom_trace(_AtomKey, _Atom1, _AtomTrace1),run).
sim_entry(dyn_atom_trace_backup(_AtomKey, _Atom, _AtomTrace),run).
sim_entry(dyn_wait_var(_Id,_IdTerm,_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_T),run).
sim_entry(dyn_wait1(_T, _Activity),run).
sim_entry(dyn_res1(_),run).
sim_entry(dyn_lt_rule(_Id, _AnteLits,_ConseLits,_PVOutC,_Delay,_RId),run).
sim_entry(dyn_schedule_fire(_ConseRId, _T3, _T4),run).
sim_entry(util:dyn_sort_info(_Sort, _Info),run).

check_no_sim_info :-
	(	dyn_sim_status(File, _Status);
		dyn_currently_loaded(sim, File);
		(sim_entry(Entry),
		call(Entry)
		);
		(current_key(Key),
		(recorded(Key, dyn_wait_var(Key,_IdTerm,_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,
					  _K,_HT))
		;	recorded(Key, dyn_wait1(_, _Activity))
		))
	->	impl_error('Junk left from simulation')
	;	true
	).

reset_run_info :-
	(	sim_entry(Entry,run),
		assert_debug(predicate_property(Entry, dynamic)),
		retractall(Entry),
		fail
	;	true
	),
	(	recwait,
		current_key(Key),
		(integer(Key)
		->	recorded(Key, dyn_wait1(_HT, _Activity),Ref)
		;	Id = Key,
			recorded(Id, dyn_wait_var(Id,_IdTerm,_A,_B,_C,_D,_E,_F,
						  _G,_H,_I,_J,_K,_HT2), Ref)
		),
		erase(Ref),
		fail
	;	true
	).


reset_sim_info :-
	(	dyn_sim_status(File, _Status),
		dyn_currently_loaded(sim, File)
	->	retract(dyn_sim_status(_File, _Status1)),
		retract(dyn_currently_loaded(sim, File))
	;	fail
	),
	(	sim_entry(Entry),
		assert_debug(predicate_property(Entry, dynamic)),
		retractall(Entry),
		fail
	;	true
	),
	(	current_key(Key),
		(integer(Key)
		->	recorded(Key, dyn_wait1(_HT1, _Activity),Ref)
		;	Id = Key,
			recorded(Id, dyn_wait_var(Id,_IdTerm,_A,_B,_C,_D,_E,_F,
						  _G,_H,_I,_J,_K,_HT), Ref)
		),
		erase(Ref),
		fail
	;	true
	),
	clear_module(spec).
currently_loaded(Kind, File) :-
	(dyn_currently_loaded(sim, File)
	->	true
	;	spec:predicate_property(P, interpreted),
		\+ spec:predicate_property(P, imported_from(_)),
		spec:clause(P,_)
	->	impl_error('clear not done:~w', [P])
	;	fail
	),
	!,
	Kind = sim.


currently_loaded(Kind, File) :-
	dyn_currently_loaded(trace, File),
	!,
	Kind = trace.
currently_loaded(_Kind, _) :-
	predicate_property(load_results:P, interpreted),
	\+ predicate_property(load_results:P, imported_from(_)),
	format(' CL:~w~n', [P]),
	fatal_error('Uncleaned junk from trace:~w', [P]).
currently_loaded(_,_) :-
	assert_debug(\+ display_number_range(_,_,_,_)),
	fail.
reset_trace_info :-
	assert_debug(load_results:times(_TSetup, _THandled, _ET)),
	(retract(dyn_currently_loaded(trace, File))
	->	format('Removing trace source ~w from database~n', [File])
	;	assert_debug(dyn_currently_loaded(trace, _File))
	),
	retractall(dyn_handled_time(_)),
	retractall(dyn_setup_time(_)),
	retractall(dyn_end_time(_)),
	retractall(display_number_range(_,_,_,_)),
	retractall(display(_,_)),
	clear_module(load_results).



load_tr_terms :-
	(	get_option(addterm(Arg)),
		atom_to_term(Arg, Term, _Bindings),
		(	var(Arg)
		->	error('unrecognised addterm suboption ~w', [Arg])
		;	(simple_add_term(tr,Term)
			->	assertz(Term)
			;	error('Unhandled option -addterm ~w', [Term])
			)
		),
		fail
	;	true
	).

load_results(File, Traces) :-
	assert_debug(\+ currently_loaded(_,_), load_sim),
	assertz(dyn_currently_loaded(trace, File)),
	set_dynamic_load_results,
	load_module_fatal(File, load_results, load_trace(load, start),
			  load_trace(load, end),
			  load_trace(load, error)),
	update_wd,
	(     predicate_property(load_results:dnr(P,Q,R,S),interpreted),
	      load_results:dnr(P,Q,R,S),
              assertz(display_number_range(P,Q,R,S)),
              fail
        ;     true
        ),
	(     predicate_property(load_results:d(V,W),interpreted),
	      load_results:d(V,W),
              assertz(display(V,W)),
              fail
        ;     true
        ),
	load_tr_terms,
	bagof(Atoma-TrTrace,
	      lrat(Atoma, TrTrace),
	      Traces),
	(	predicate_property(load_results:times(_, _, _),interpreted),
		load_results:times(TSetup, THandled, ET)
	->	assert_debug(\+ dyn_handled_time(_)),
		assertz(dyn_handled_time(THandled)),
		assert_debug(\+ dyn_setup_time(_)),
		assertz(dyn_setup_time(TSetup)),
		assert_debug(\+ dyn_end_time(_)),
		assertz(dyn_end_time(ET))
	;	ferror(load_trace(load, no_time_info)),
		clear_module(load_results),
		fail
	),
	ensure_limits.



ensure_limits :-
	(dyn_handled_time(_)
	->	true
	;	assertz(dyn_handled_time(200))
	),
	(dyn_setup_time(_)
	->	true
	;	assertz(dyn_setup_time(0))
	),
	(dyn_end_time(_)
	->	true
	;	assertz(dyn_end_time(200))
	).



load_show(File) :-
	load_show(File, []).

load_show(File, PictFrame) :-
	(load_results(File, Traces)
	->	show_traces(File,Traces, PictFrame)
	;	warning('No atoms set at all, NO TRACE'),
		show_traces(File,[], PictFrame)
	).




:- ensure_loaded(showtrace).
show_traces(File,Traces) :-
	show_traces(File,Traces, []).

show_traces(File1,Traces, PictureFrame) :-
	start_time(ST),
	end_time(ET),
	(PictureFrame == []
	->	new(Pict, show_trace)
	;	get(PictureFrame, picture, Pict)
	),
	(get_option(ltshow(paging(Paging)))
	->	true
	;	Paging = default
	),
	ensure_no_graph,
	send(Pict, display_traces, Traces, ST, ET, Paging),
	(PictureFrame == []
	->	send(Pict, open)
	;	true
	),
	(do_graphs
	->	do_show_graphs(File1,Traces, PictureFrame, ST, ET)
	;	true
	),
	(get_option(ps(File))
	->	(PictureFrame == []
		->	Frame = Pict
		;	Frame = PictureFrame
		),
		postscript_file(File, Pict),
		(PictureFrame == []
		->	true
		;	send(PictureFrame, report,
			     inform,'Saved picture in file `%s''\n', File)
		)
	;	true
	).




start_time(ST) :-
	(dyn_start_time(ST)
	->	true
	;	ST = 0
	).
end_time(ST) :-
	(dyn_end_time(ST)
	->	true
	;	ST = 200
	).

handle_term(display(View, What)) :-
	!,
	assertz(display(View, What)).
handle_term(display_number_range(Atom, Var, Label, VarLabel)) :-
	!,
	assertz(display_number_range(Atom, Var, Label, VarLabel)).


handle_term(constant(Name, Value)) :-
	!,
	check_constant(Name, Value).

handle_term(model(Term)) :-
	!,
	check_set_model(Term).






handle_term(start_time(ST)) :-
	(retract(dyn_start_time(_ST))
	->	error('Duplicate start_time')
	;	true
	),
	assertz(dyn_start_time(ST)),!.
handle_term(end_time(ST)) :-
	(retract(dyn_end_time(_ST))
	->	error('Duplicate end_time')
	;	true
	),
	assertz(dyn_end_time(ST)),!.
handle_term(at(Vars, Time, AndLiterals)) :-
	!,
	create_unit_range(Time, T1, T2),
	initialise_interval(range(T1, T2), Vars, AndLiterals).
handle_term(interval(Vars, Range, AndLiterals)) :-
	is_list(Vars),
	!,
	initialise_interval(Range, Vars, AndLiterals).
handle_term((interval(Vars, Range, AndLiterals):- Call)) :-
	is_list(Vars),
	!,
	(	spec:call(Call),
		initialise_interval(Range, Vars, AndLiterals),
		fail
	;	true
	).

handle_term(interval(Vars, T1, T2, AndLiterals)) :-
	!,
	initialise_interval(range(T1, T2), Vars, AndLiterals).
handle_term((interval(Vars, T1, T2, AndLiterals):- Call)) :-
        !,
	(	spec:call(Call),
		initialise_interval(range(T1, T2), Vars, AndLiterals),
		fail
	;	true
	).


handle_term(between(Vars, T1, T2, AndLiterals)) :-
	!,
	initialise_interval(range(T1, T2), Vars, AndLiterals).
handle_term(interval(T1, T2, AndLiterals)) :-
	!,
	initialise_interval(range(T1, T2), [], AndLiterals).
handle_term(periodic(Vars, Range, P, AndLiterals)) :-
	is_list(Vars),
	!,
	initialise_interval_periodic(Range,P,Vars,AndLiterals).
handle_term(periodic(Vars, T1, T2, P, AndLiterals)) :-
	!,
	initialise_interval_periodic(range(T1, T2),P,Vars,AndLiterals).
handle_term(periodic(T1, T2, P, AndLiterals)) :-
	!,
	initialise_interval_periodic(range(T1, T2), P, [], AndLiterals).
handle_term(between(T1, T2, AndLiterals)) :-
	!,
	initialise_interval(range(T1, T2), [], AndLiterals).


handle_term(cwa(Atom)) :-
	var(Atom),
	!,
	(dyn_cwa(Atom)
	->	error('Duplicate cwa entry')
	;	assertz(dyn_cwa(Atom))
	).
handle_term(cwa(F/A)) :-
	!,
	(	atom(F),
		integer(A),
		A >= 0
	->	functor(CAtom, F, A),
		(dyn_cwa(CAtom)
		->	error('Duplicate cwa entry')
		;	assertz(dyn_cwa(CAtom))
		)
	;	error('cwa(F/A) arguments incorrect:~w', [F/A])
	).
handle_term(cwa(Atom)) :-
	!,
	(dyn_cwa(Atom)
	->	error('Duplicate cwa entry')
	;	assertz(dyn_cwa(Atom))
	).

:- dynamic dyn_lambda/1.

handle_term(global_lambda(Lambda)) :-
	!,
	(dyn_lambda(_Lambda)
	->	error('Duplicate Lambda')
	;	assertz(dyn_lambda(Lambda))
	).

handle_term(qterm(Term1)) :-
	!,
	(var_containing_constant(Class, X, Term1)
	->	var_containing_constant(Class, Y, Term2),
		(valid_term_no_bind0(@pce, X, Y, _Bindings)
		->	handle_term(Term2)
		;	fatal_error('Unrecognised qterm')
		)
	;	fatal_error('Unrecognised qterm')
	).


handle_term(cwa(_Vars, _Atom)) :-
	!,
	fatal_error('Unsupported cwa spec').



handle_term(leadsto(LitDisConj, AndLiterals, Delay)) :-
	!,
	flag(dyn_leadsto, I, I + 1),
	assertz(dyn_leadsto(I, [], LitDisConj, AndLiterals, Delay)).
handle_term(leadsto(Vars, LitDisConj, AndLiterals, Delay)) :-
	!,
	flag(dyn_leadsto, I, I + 1),
	assertz(dyn_leadsto(I, Vars, LitDisConj, AndLiterals, Delay)).
handle_term(Term) :-
	spec_pred_header(Term),
	!.
handle_term((Term:- _)) :-
	spec_pred_header(Term),
	!.
handle_term(Term) :-
	warning('Unrecognised entry in specification :~w', [Term]),
	(   nonvar(Term),
	    functor(Term, F, A),
	    trace_pred(F/A)
	->  warning('Probably loaded trace file instead of leadsto specification')
	;   true
	).
handle_term((:-op(_,_,_))).
:- dynamic dyn_model/1.

check_set_model(Model) :-
	(ground(Model)
	->	(dyn_model(M1)
		->	error('Multiple model specifications ~w - ~w',
			      [M1,Model])
		;	assertz(dyn_model(Model))
		)
	;	error('Model specification ~w contains prolog variable(s)',
		      [Model]),
		fail
	).

spec_pred_header(sort_element(_,_)).
spec_pred_header(sortdef(_,_)).
spec_pred_header(specification(_)).
spec_pred_header(content(_)).


global_lambda(Lambda) :-
	dyn_lambda(Lambda).



create_unit_range(TimeA, TimeA, TimeA + 1).

tr_basic_element(Element, VarsInst, Value) :-
	tr_basic_elemente(Element, VarsInst, Value,Status),
	var(Status).

tr_basic_elemente(Element, _VarsInst, _, Status) :-
	\+ ground(Element),
	!,
	ferrorv(prolog_var, [Element], Status).



tr_basic_elemente(Element, VarsInst, Value,Status) :-
	atom(Element),
	var_inst_from_var_liste(VarsInst, Element, _Sort, _Kind, Value,
				 Status),
	!.
tr_basic_elemente(Element, VarsInst, Value, Status) :-
	spec_constant(Element, E1),
	!,
	test_recursive_constante(Element, E1, Status),
	(var(Status)
	->	(tr_basic_elemente(E1, VarsInst, Value,Status)
		->	free_recursive_constant(Element)
		;	free_recursive_constant(Element),
			fail
		)
	;	free_recursive_constant(Element)
	).

tr_basic_elemente(Element, _VarsInst, Element,_Status) :-
	atomic(Element),!.

tr_basic_elemente(ArExpr, VarsInst, Value, Status) :-
	functor(ArExpr, F, A),
	number_ar_op(F, A),
	!,
	ArExpr =.. [F|Args],
	maplist(tr_basic_element0(Status,VarsInst), Args, TrArgs),
	(uchecklist(number, TrArgs)
	->	Op =.. [F|TrArgs],
		Value is Op
	;	ferrorv(num_op_args_non_num, [ArExpr], Status)
	).
tr_basic_elemente(Term, VarsInst, Value, Status) :-
	Term =.. [F|Args],
	maplist(tr_basic_element0(Status, VarsInst), Args, TrArgs),
	Value =.. [F|TrArgs].

number_ar_op(+,2).
number_ar_op(-,2).
number_ar_op(*,2).
number_ar_op(/,2).
number_ar_op(mod,2).
number_ar_op(max, 2).
number_ar_op(min, 2).

number_ar_op(exp, 1).
number_ar_op(-,1).


tr_basic_element0(Status,VarsInst, Arg, TrArg) :-
	tr_basic_elemente(Arg, VarsInst, TrArg,Status).

var_inst_from_var_list(VarsIn, Var,Sort,Kind,Value) :-
	var_inst_from_var_liste(VarsIn, Var,Sort,Kind,Value,Status),
	var(Status).

var_inst_from_var_liste([v(Var, Value1, Sort1, Kind1)|_],Var,Sort,Kind,
			Value,Status) :-
	!,
	(	Sort = Sort1,
		Value = Value1,
		Kind = Kind1
	->	true
	;	ferrorv(var_sort_mismatch,[Var:Sort], Status)
	).

var_inst_from_var_liste([_|VarsIn], Var,Sort,Kind,Value,Status) :-
	var_inst_from_var_liste(VarsIn, Var,Sort,Kind,Value,Status).

var_inst_to_var_list(Var, Element, Sort, Kind, VarsIn, VarsOut) :-
	var_inst_to_var_liste(Var,Element,Sort, Kind, VarsIn, VarsOut,Status),
	var(Status).

var_inst_to_var_liste(Var, Element, Sort, Kind, VarsIn, VarsOut,Status) :-
	(var_inst_from_var_liste(VarsIn, Var, _Sort, _Kind, _Value, Status)
	->	ferrorv(dupl_var_def, [Var], Status)
	;	VarsOut = [v(Var, Element, Sort, Kind)|VarsIn]
	).



tr_range(range(T1I, T2I), VarsInst, T1, T2) :-
	!,
	tr_basic_element(T1I, VarsInst, T1),
	tr_basic_element(T2I, VarsInst, T2),
	((number(T1);T1 == mininf)
	->	true
	;	error('First element in range not a number:~w', [T1I]),
		fail
	),
	((number(T2);T2 == maxinf)
	->	true
	;	error('Second element in range not a number:~w', [T2I]),
		fail
	).
tr_range(Range, _, _, _) :-
	error('Expected range(T1,T2) got ~w', Range),
	fail.
instantiate_element(Sort, Kind, Element) :-
	instantiate_elemente(Sort, Kind, Element, Status),
	var(Status).



instantiate_vars(Vars, InstVars) :-
	instantiate_varse(Vars, InstVars, Status),
	(var(Status)->true;!,fail).


instantiate_varse(Vars, InstVars, Status) :-
	instantiate_varse(Vars, [], InstVars, Status).

instantiate_varse([], VarsDone, VarsDone, _).
instantiate_varse([Element|Vars], VarsDone, VarsInst, Status) :-
	instantiate_var(Element, VarsDone, Vars1, Status),
	instantiate_varse(Vars, Vars1, VarsInst, Status).

instantiate_var(Var:Sort, VarsIn, VarsOut,Status) :-
	atom(Var),
	!,
	tr_basic_elemente(Sort, VarsIn, Sort1, Status),
	instantiate_var(Var, Sort1, VarsIn, VarsOut,Status).
instantiate_var(Var, _VarsIn, _VarsOut, Status) :-
	ferrorv(not_a_variable, [Var], Status).




instantiate_var(Var, Sort, VarsIn, VarsOut, Status) :-
	instantiate_elemente(Sort, Kind, Element, Status),
	var_inst_to_var_list(Var, Element, Sort, Kind, VarsIn, VarsOut).



initialise_interval_p(Interval, P, Vars, Form1) :-
	(	instantiate_vars(Vars, VarsInst),
		tr_range(Interval, VarsInst, T1, T2),
		tr_basic_element(P, VarsInst, P1),
		(	number(P1),
			P1 > 0
		->	true
		;	error('period should be number > 0, got ~w',
			      [P]),
			fail
		),
		initialise_interval_p1(T1, T2, P1, VarsInst, Form1),
		fail
	;	true
	).
set_model_checking_p_rules(Form, T1, T2, P1, VarsInst) :-
	tr_simple(Form, Form1),
	!,
	set_model_checking_p_rules(Form1, T1, T2, P1, VarsInst).
set_model_checking_p_rules(and(F1,F2), T1, T2, P1, VarsInst) :-
	!,
	set_model_checking_p_rules(F1, T1, T2, P1, VarsInst),
	set_model_checking_p_rules(F2, T1, T2, P1, VarsInst).
set_model_checking_p_rules(not(F1), T1, T2, P1, VarsInst) :-
	set_model_checking_p_rule(F1, false, T1, T2, P1, VarsInst).
set_model_checking_p_rules(F1, _T1, _T2, _P1, _VarsInst) :-
	reserved(F1),
	!,
	fatal_error('reserved logical form ~w not allowed in consequent',
		    [F1]).
set_model_checking_p_rules(F1, T1, T2, P1, VarsInst) :-
	set_model_checking_p_rule(F1, true, T1, T2, P1, VarsInst).

prep_model_checking_p_rules(Form1, T1, T2, P1, VarsInst) :-
	assertz(dyn_prep_model_checking_p_rules(Form1, T1, T2, P1, VarsInst)).

get_model_checking_p_rules :-
	(	retract(dyn_prep_model_checking_p_rules(Form1, T1, T2, P1,
							VarsInst)),
		set_model_checking_p_rules(Form1, T1, T2, P1, VarsInst),
		fail
	;	true
	).

initialise_interval_p1(T1, T2, P1, VarsInst, Form1) :-
	model_checking,
	!,
	set_range_form(T1, T2, VarsInst, Form1),
	prep_model_checking_p_rules(Form1, T1, T2, P1, VarsInst).

initialise_interval_p1(T1, T2, P1, VarsInst, Form1) :-
	set_range_form(T1, T2, VarsInst, Form1),
	T1p is T1 + P1,
	end_time(ET),
	(cmp_le(T1p, ET)
	->	T2p is T2 + P1,
		initialise_interval_p1(T1p, T2p, P1, VarsInst, Form1)
	;	true
	).
set_range_form(T1, T2, VarsInst, Form) :-
	tr_simple(Form, Form1),
	!,
	set_range_form(T1, T2, VarsInst, Form1).
set_range_form(T1, T2, VarsInst, and(F1,F2)) :-
	!,
	set_range_form(T1, T2, VarsInst, F1),
	set_range_form(T1, T2, VarsInst, F2).
set_range_form(T1, T2, VarsInst, not(F1)) :-
	!,
	set_range_atom(T1, T2, VarsInst, F1, false).

set_range_form(_T1, _T2, _VarsInst, F1) :-
	reserved(F1),
	!,
	fatal_error('reserved logical form ~w not allowed in consequent',
		    [F1]).
set_range_form(T1, T2, VarsInst, F1) :-
	set_range_atom(T1, T2, VarsInst, F1, true).



% This is very sloppy, as initialise_interval1 will reinspect everything
% again, but, well...
% We probably cannot save the process up to now.
% W.r.t. the global ranges: we ostart out with a set of IVars that
% should be also initialise local vars. As soon as we encounter a range
% having one of these local/initialised vars as variable, we should
% add the code.
% So we start out with only IVars, initially [].
% During analysis we gather GVars, those range variables that are
% - not in IVars
% - have no  condition(later no longer?)
% After init_rarange, all IVars are handled by CodeOut, we do not need
% to do instantiate_ivar

:- dynamic dyn_interval/1.

initialise_interval(Interval, Vars, Form) :-
	assertz(dyn_interval(i(Interval, Vars, Form))).
initialise_interval_periodic(Interval, P, Vars, Form) :-
	assertz(dyn_interval(i(Interval, P, Vars, Form))).
setup_rt_intervals :-
	(	dyn_interval(I),
		setup_rt_interval(I),
		fail
	;	true
	).
setup_rt_interval(i(Interval, Vars, Form)) :-
	initialise_interval_run(Interval, Vars, Form).
setup_rt_interval(i(Interval, P, Vars, Form)) :-
	initialise_interval_periodic_run(Interval, P, Vars, Form).

% callback:invalid_time_info(TimeInfo1)
initialise_interval_run(Interval, Vars, Form) :-
	init_interval_callbacks(Interval, Vars, [Form], TimeInfo1, Vars1,
				Forms2,
				invalid_vars(Vars1),
				invalid_interval(TimeInfo1),
				initialise_interval1l(TimeInfo1,Vars1,Forms2)).
initialise_interval_periodic_run(Interval, P, Vars, Form) :-
	init_interval_callbacks(t(Interval, P), Vars, [Form], TimeInfo1,
				Vars1, Forms2,
				invalid_vars(Vars1),
				invalid_period(TimeInfo1),
				initialise_interval_pcl(TimeInfo1,Vars1,
							Forms2)).
initialise_interval1l(TimeInfo1, Vars1,Forms2) :-
	(	nonvar(TimeInfo1),
		is_list(Forms2),
		Forms2 = [Form2]
	->	true
	;	impl_error('Callback range')
	),
	initialise_interval1(TimeInfo1,Vars1,Form2).

initialise_interval_pcl(TimeInfo1, Vars1, Forms2) :-
	(	nonvar(TimeInfo1),
		TimeInfo1 = t(Interval, P),
		is_list(Forms2),
		Forms2 = [Form1]
	->	true
	;	impl_error('Callback range')
	),
	initialise_interval_p(Interval, P, Vars1, Form1).
invalid_period(TimeInfo1) :-
	(	nonvar(TimeInfo1),
		TimeInfo1 = t(Interval, P)
	->	true
	;	impl_error('Callback range')
	),
	(invalid_interval(Interval)
	->	true
	;	tr_basic_element(P, [], P1),
		(	number(P1),
			P1 > 0
		->	fail
		;	error('period should be number > 0, got ~w',[P]),
			fail
		)
	).
invalid_vars(Vars1) :-
	contains_inf_vars(Vars1).

invalid_interval(TimeInfo1) :-
	tr_range(TimeInfo1, [], T1, T2), % variant?
	cmp_ge(T1,T2),
	warning('Empty range in initialisation rule').

init_interval_callbacks(TimeInfo, Vars, Forms, TimeInfo1, Vars1, Forms2,
			InValidVars, InvalidTimeInfo,
			ActPreInstantiated) :-
	init_interval_callbacks(TimeInfo, Vars, Forms, TimeInfo1, Vars1,
				Forms2,
				InValidVars, InvalidTimeInfo,
				ActPreInstantiated, Status),
	(var(Status)
	->	true
	;	fatal_error('Got error -> exit')
	).


init_interval_callbacks(TimeInfo, Vars, Forms, TimeInfo1, Vars1, Forms2,
			InValidVars, InvalidTimeInfo,
			ActPreInstantiated, Status) :-
	init_vars_f(Vars, [], TimeInfo, Forms, TimeInfo1,
		    Forms1, CodeOut, IGOut, Res),
	(Res == error
	->	Status = error
	;IGOut == error
	->	Status = error
	;	Res = ok
	->	flag(dexp, O, 0),
		(	reverse(CodeOut, CodeOut1),
			uchecklist(inst_code, CodeOut1),
			IGOut = ds_ig(Vars1, _IVars),
			(	call(InValidVars)
			->	true
			;	(call(InvalidTimeInfo)
				->	true
				;	maplist(expandq2, Forms1, Forms2),
					call(ActPreInstantiated),
					flag(dexp, O1, O1 + 1)
				),
				fail
			)
		->	flag(dexp, _N, O) % Rule ignored
		;	flag(dexp, N, O),
			(	N = 1
			->	true
			;	N = 0
			->	warning(
					'Leadsto rule or interval has no instances')
			;	alog(leadsto, ' ~w INSTANCES~n', [N])
			)
		)
	;	impl_error('Unrecognised result')
	).



contains_inf_vars(Vars1) :-
	(	memberchk(Var:Sort, Vars1),
		is_infinite_sort(Sort)
	->	error('Infinite domain variable ~w not allowed:ignoring rule',
		      [Var:Sort])
	;	fail
	).


init_vars_f(Vars, IVarsIn, TTIn, FormsIn, TTOut, FormsOut, CodeOut, IGOut,
	   Res) :-
	init_vars_f_step(Vars,IVarsIn,TTIn,FormsIn,TT1,Forms1,Code1,
			IG1, Res1),
	assert_debug(nonvar(Res1);IG1 == error),
	(IG1 == error
	->	IGOut = error,
		Res = error
	;	Res1 == error
	->	Res = error
	;Res1 = add(VarName, Sort)
	->	var_inst_to_var_list(VarName, _Value, Sort, undefined,
					     IVarsIn, IVars1),
		init_vars_f(Vars,IVars1,TTIn,FormsIn,TTOut,FormsOut,CodeOut,
			    IGOut, Res)
	;	Res = Res1,
		CodeOut = Code1,
		TTOut = TT1,
		FormsOut = Forms1,
		IGOut = IG1
	).


expandq11L([], _LVars, IGIn, [], IGIn).
expandq11L([Form|Forms], LVars, IGIn, FormsOut, IGOut) :-
	expandq11(Form, LVars, IGIn, Form1, IG1),
	(IG1 == error
	->	IGOut = error
	;IG1 = ds_ig(_, _)
	->	assert_debug(IG1 == IGIn),
		expandq11L(Forms, LVars, IG1, FormsOut1, IGOut),
		FormsOut = [Form1|FormsOut1]
	;	IGOut = IG1
	).

init_vars_f_step(Vars,IVarsIn,TTIn,FormsIn,TTOut,FormsOut,CodeOut,IGOut,Res):-
	initvars2q(Vars, IVarsIn, CodeOut, Res1),
	(Res1 = gvars(GVars, LV1)
	->	subst_inst_vars_r(TTIn, LV1, GVars, TT1, Res2),
	        assert_debug(nonvar(Res2)),
		(Res2 == error
		->	Res = error
		;Res2 = not_ground(VN, S, _GVars1b)
		->	Res = add(VN, S)
		;Res2 = ground
		->	IG1 = ds_ig(GVars, LV1),
			expandq11L(FormsIn, LV1, IG1, Forms1, IG2),
			(IG2 == error
			->	IGOut = error,
				Res = error
			;IG2 = ig_added(ds_ig(_Vars,_IVars), VNF, SF, _Vars1F)
			->	subst_inst_vars_r(SF, LV1, GVars, SF1, Res3),
				(Res3 == error
				->	IGOut = error,
					Res = error
				;Res3 == ground
				->	Res = add(VNF, SF1)
				;Res3 = not_ground(VNr, Sr, _GVars1a)
				->	Res = add(VNr, Sr)
				;	Res = Res3
				)
			;	IGOut = IG2,
				Res = ok,
				FormsOut = Forms1,
				TTOut = TT1
			)
		;	Res = Res2
		)
	;	Res = Res1
	).

% initvars2q(Vars, IVarsIn, CodeOut, Res)
% Analyse leadsto/interval global variable list.
% Leave some global variables as they are.
% Mark others as instantiatable, and add code to CodeOut for them.
% IVarsIn is a list of variables occurring in Vars that must be
% instantiated anyway.
% Res = gvars(GVars, LV1), CodeOut => ok
% Res = error -> gave error-message, ignore other results
initvars2q(Vars, IVarsIn, CodeOut, Res) :-
	initvars2q_step(Vars,IVarsIn,Code1,Res1),
	(Res1 = add(VarName, Sort)
	->	var_inst_to_var_list(VarName, _Value, Sort, undefined,
					     IVarsIn, IVars1),
		initvars2q(Vars, IVars1, CodeOut, Res)
	;	Res = Res1,
		CodeOut = Code1
	).

initvars2q_step(Vars,IVarsIn,CodeOut,Res) :-
	initvars2q_step1(Vars,IVarsIn,[],[],IVars1,GVars1,Code1,Res1),
	(Res1 == ok
	->	Res = gvars(GVars1, IVars1),
		CodeOut = Code1
	;	Res = Res1
	).

add_inst(VarName, Sort, RFormat, RArgs, Res) :-
	(is_infinite_sort(Sort)
	->	atom_concat('Cannot instantiate variable ~w of infinite sort:',
			     RFormat,NF),
		error(NF, [VarName:Sort|RArgs]),
		Res = error
	;	Res = add(VarName, Sort)
	).


initvars2q_step1([],IVarsIn,GVarsIn,CodeIn,IVarsIn,GVarsIn, CodeIn,ok).
initvars2q_step1([Var|Vars],IVarsIn,GVarsIn,CodeIn,IVarsOut,GVarsOut,
		 CodeOut,Res) :-
	range_var_condition(Var, VarName, Sort, Op, CTerm),
	subst_inst_vars_r(Sort, IVarsIn, GVarsIn, Sort1, Res1),
	(Res1 = not_ground(VN1, S1, _GVars1a)
	->	add_inst(VN1, S1, 'in sort of range ~w', [Var],Res)
	;	Res1 == ground
	->	(Op == none
		->	assert_debug(CTerm == true),
			(var_inst_from_var_list(IVarsIn,VarName, SortA,
						_Kinda, PVar)
			->	Code1 = [ds_rc(Op,Sort1,PVar,CTerm)|CodeIn],
				initvars2q_step1(Vars,IVarsIn,GVarsIn,
						 Code1,IVarsOut, GVarsOut,
						 CodeOut,Res)
			;	initvars2q_step1(Vars,IVarsIn,
						 [VarName:Sort1|GVarsIn],
						 CodeIn,IVarsOut,GVarsOut,
						 CodeOut,Res)
			)
		;	subst_inst_vars_r(CTerm, IVarsIn,GVarsIn,CTerm1,Res2),
			(Res2 = not_ground(VN1, S1, _GVars1b)
			->	add_inst(VN1, S1, 'in condition of range ~w',
					 [Var],Res)
			;Res2 == ground
			->	(var_inst_from_var_list(IVarsIn,VarName, SortA,
							_Kindb, PVar)
				->	Code1 = [ds_rc(Op,Sort1,PVar,CTerm1)
						| CodeIn],
					initvars2q_step1(Vars,IVarsIn,GVarsIn,
							 Code1,IVarsOut,
							 GVarsOut,CodeOut,Res)
				;	add_inst(VarName, Sort1,
						 ' constrained variable in range:~w',
						 [Var],Res)
				)
			;	Res = Res2
			)
		)
	;	Res = Res1
	).




initialise_interval1(Interval, Vars, Form) :-
	tr_simple(Form, Form1),
	!,
	initialise_interval1(Interval, Vars, Form1).

initialise_interval1(Interval, Vars, and(Lit1,Lit2)) :-
	!,
	initialise_interval1(Interval, Vars, Lit1),
	initialise_interval1(Interval, Vars, Lit2).

initialise_interval1(Interval, Vars, Lit) :-
	!,
	(	instantiate_vars(Vars, VarsInst),
		tr_range(Interval, VarsInst, T1, T2),
		set_range_lit(T1, T2, VarsInst, Lit),
		fail
	;	true
	).




set_range_lit(T1, T2, VarsInst, Lit) :-
	Lit = not(Atom),
	!,
	set_range_atom(T1, T2, VarsInst, Atom, false).
set_range_lit(T1, T2, VarsInst, Lit) :-
	 set_range_atom(T1, T2, VarsInst, Lit, true).

set_range_atom(T1, T2, VarsInst, Atom, TF) :-
	(reserved(Atom)
	->	error('Used reserved term, expected atom:~w', [Atom]),
		fail
	;	true
	),
	tr_basic_element(Atom, VarsInst, Atom1),
	set_range_atom1(T1, T2, Atom1, TF, []).


/* Default situation:
do we mark handled instances of antecedent atoms
*/
check_handled_enabled :-
	true.

do_check_handled :-
	(check_handled_enabled
	->      \+ get_option(uncheckhandled)
	;	get_option(checkhandled)
	).




old_atom_key_id :-
	\+ get_option(exact_atom_id).


fix1 :-
	(	do_check_handled
	;
		is_local
	).





set_range_atom1(T1, T2, Atom1, TF, By) :-
	assert_debug(ground(Atom1)),
	(reserved(Atom1)
	->	error('Used reserved term, expected atom:~w', [Atom1]),
		fail
	;	true
	),
	assert_debug(memberchk(TF, [true, false])),
	set_range_atom11(T1, T2, Atom1, TF, By).
set_range_atom11(_T1, _T2, Atom1, false, _By) :-
	cwa(Atom1),
	!,
	warning('Making cwa atom ~w explicitly false seems useless',
		[Atom1]).

set_range_atom11(T1, T2, Atom1, TF, By) :-
	atom_key(Atom1, AtomKey),
	(old_atom_key_id
	->      true
	;       Atom1a = Atom1
	),
	(retract(dyn_atom_trace(AtomKey, Atom1a, AtomTrace))
	->	true% assert_debug(Atom1a == Atom1)
	;retract(dyn_atom_trace_backup(AtomKey, Atom1a, AtomTrace))
	->	true % assert_debug(Atom1a == Atom1)
	;	AtomTrace = [],
	        (ground(Atom1a)
		->     true
		;      Atom1a = Atom1
		)
	),
	flag('SET', I, I + 1),
	(By == []
	->	alog('SET','~w:SET ~w (~w,~w) = ~w~n', [I,Atom1, T1, T2, TF])
	;	alog('SET','~w:SET ~w (~w,~w) = ~w (by ~w)~n', [I,Atom1, T1, T2, TF,
								By])
	),
	assert_debug(ground(Atom1a)),
	set_range_atom_list(AtomTrace, Atom1a, range(T1, T2), TF, AtomTrace1),
	assertz(dyn_atom_trace(AtomKey, Atom1a, AtomTrace1)).




cwa(Atom) :-
	dyn_cwa(Atom).


default_value(Atom, false) :-
	dyn_cwa(Atom),
	!.
default_value(_Atom, unknown).





find_atom_trace(Atom, AtomTrace) :-
	(ground(Atom)
	->	atom_key(Atom, AtomKey),
		dyn_atom_trace(AtomKey, _Atoma, AtomTrace)
	;	dyn_atom_trace(AtomKey, Atom, AtomTrace)
	).
/*	;       rm_real_args(Atom, Atom1, Args, Insts),
	        dyn_atom_trace(AtomKey, Atom1, AtomTrace),
	        term_to_atom(Args, T),
	        term_to_atom(Insts, T1),
	        T == T1
	).
*/
overlapping_ground_range1(Atom, TIn, Range, Ranges) :-
	find_atom_trace(Atom, AtomTrace),
	overlapping_range_rest1(AtomTrace, Atom, TIn, maxinf,Range, [],Ranges).

/*
  For backtracking over variants of Atom,
  Will fail if no entry is available.

  It may well be that TMin < HandledTime

  Given lit(Atom,PN) and time TMin,
  give result whether Lit holds starting from that time then
  true(Tlo, Thi, _Cont)
  or if at TMin Lit has conflicting i.e. FT or explicit unknown
  value, fail(TN, By)
  or if at TMin Lit is blank, return blank.

  This variant excludes entries FV from FVL

  If O2 = true(Tlo, THi, Cont) then Tlo must be < THandled otherwise
  point to preceding interval THi must be >= THandled
  If O2 = fail(TN, By) then TN > THandled and all previous actions are taken
  If O2 = blank then at THandled

  Problem: How far back do we go? What have we handled before?
  Assume we are not interested in intervals starting before TMin.
  */
find_min_range(Atom, PN, FV, FVL, TMin, O2) :-
	(	FV == [],
		FVL == [[]]
	->	fail
	;	find_atom_trace(Atom, AtomTrace),
		\+ memberchk(FV, FVL),
		overlapping_range_rest1(AtomTrace, Atom, TMin, maxinf, Range, [], Ranges),
		% Range overlaps TMin or ends at TMin
		find_min_range_rest(Range, Ranges, Atom, PN, TMin, O2)
	).

find_min_range(Atom, PN, TMin, O2) :-
	overlapping_ground_range1(Atom, TMin, Range, Ranges),
	find_min_range_rest(Range, Ranges, Atom, PN, TMin, O2).

find_min_range_rest(Range, Ranges, Atom, PN, TMin, O2) :-
	Range = range(Tlo, Thi, _),
	assert_debug(cmp_le(TMin,Thi)),
	assert_debug(cmp_lt(Tlo,TMin)),
	default_value(Atom, CWAVal),
	tr_o1_o2(Range,Ranges,PN,Atom,CWAVal,TMin, O2).







/* Find range ending at TIn
 Before TSetup, no cwa, After THandled, no cwa

   Must be able to deal with TIn deeper in AtomTrace
   RangesIn, RangesOut : ranges after range searched for.
*/
overlapping_range_rest1([], Atom, TIn, LastTime, Range, RangesIn, RangesIn) :-
	!,
	ensure_handled_time(THandled),
	ensure_setup_time(TSetup),
	(cmp_ge(TIn, LastTime)
	->	(cmp_eq(TIn,LastTime)
		->	(cmp_le(TIn, THandled)
			->	(cmp_le(TIn, TSetup)
				->	Range = range(mininf, TSetup, unknown)
				;	default_value(Atom, UF),
					Range = range(TSetup, LastTime, UF)
				)
			;	Range = range(THandled, LastTime, blank)
			)
		;	impl_error('TIn =:= LastTime')
		)
	;	cmp_gt(TIn, THandled)
	->	Range = range(THandled, LastTime, blank)
	;	cmp_gt(TIn, TSetup)
	->	default_value(Atom, UF),
		min_new(LastTime, THandled, ET),
		Range = range(TSetup, ET, UF)
	;	Range = range(mininf, TSetup, unknown)
	),
	Range = range(F, _, _),
	assert_debug(cmp_lt(F,TIn), 'F < TIn').
overlapping_range_rest1([range(Tr1,Tr2,TFUR)|AtomTrace], Atom, TIn, LastTime,
			Range,RangesIn, RangesOut) :-
	assert_debug(cmp_ge(LastTime,TIn)),
	(cmp_le(TIn, Tr1)
	->	overlapping_range_rest1(AtomTrace, Atom,TIn,Tr1,Range,
					[range(Tr1,Tr2,TFUR)|RangesIn], RangesOut)
	;cmp_le(TIn,Tr2)
	->	Range = range(Tr1,Tr2,TFUR),
		RangesOut = RangesIn
	;	% between Tr2 and LastTime defaultvalue
		ensure_handled_time(THandled),
		(cmp_lt(THandled, TIn)
		->	TStart is max(THandled, Tr2),
			Range = range(TStart, LastTime, blank)
		;	min_new(THandled, LastTime, TEnd),
			default_value(Atom, UF),
			Range = range(Tr2, TEnd, UF)
		),
		RangesOut = RangesIn
	),
	Range = range(F, F1, _),
	assert_debug(cmp_gt(F1, F)),
	assert_debug(cmp_lt(F,TIn), 'F < TIn 2').



ensure_not_empty_range(Atom, T1, T2) :-
	(cmp_lt(T1,T2)
	->	true
	;	fatal_error('Empty range ~w - ~w for atom ~w', [T1, T2, Atom])
	).

% We prefer the most recent, i.e. last range at head
set_range_atom_list([], Atom, range(T1, T2), TFU, [range(T1, T2, TFU)]) :-
	ensure_not_empty_range(Atom, T1, T2).


set_range_atom_list([range(Tr1,Tr2,TFUR)|Rest],Atom,range(T1,T2),TFU,
		    NewTrace):-
	ensure_not_empty_range(Atom, T1, T2),
	(T1 > Tr2
	->	NewTrace = [range(T1, T2,TFU), range(Tr1, Tr2, TFUR)|Rest]
	;T1 =:= Tr2
	->	(TFU == TFUR
		->	NewTrace = [range(Tr1, T2, TFUR)|Rest]
		;	NewTrace = [range(T1, T2, TFU),
				    range(Tr1, Tr2, TFUR)|Rest]
		)
	;T1 >= Tr1
	->	warn_multi_set_or_conflict(Atom,TFUR,TFU,Tr1,Tr2,T1,T2),
		(TFU == TFUR
		->	T2n is max(Tr2, T2),
			NewTrace = [range(Tr1, T2n, TFUR)|Rest]
		;	impl_error('conflict not detected')
		)
	;T2 > Tr1
	->	warn_multi_set_or_conflict(Atom,TFUR,TFU,Tr1,Tr2,T1,T2),
		set_range_atom_list(Rest, Atom,range(T1,Tr1),TFU,NewTrace1),
		(NewTrace1 = [range(Tr1n,Tr2n,TFUR)|Rest1],
			Tr2n =:= Tr1
		->	true
		;	fatal_error('Problem in  set_range')
		),
		Trnn2 is max(Tr2, T2),
		NewTrace = [range(Tr1n,Trnn2,TFUR)|Rest1]
	;	set_range_atom_list(Rest,Atom,range(T1, T2), TFU, NewTrace1),
		set_range_atom_list(NewTrace1, Atom, range(Tr1, Tr2), TFUR,
				    NewTrace)
	),
	assert_debug((NewTrace = [range(_,_,V)|_],atom(V))).
warn_multi_set_or_conflict(Atom,TFUR,TFU,Tr1,Tr2,T1,T2) :-
	(TFU == TFUR
	->	(do_log(multifire)
		->	warning('Multiple setting of atom ~w ~w was ~w, added ~w',
			[Atom,TFU, range(Tr1,Tr2),range(T1,T2)])
		;	true
		)
	;	fatal_error('Conflict setting of atom ~w ~w:~w, added ~w:~w',
			[Atom,TFUR, range(Tr1,Tr2),TFU,range(T1,T2)])
	).


tr_arg0(E1, E) :-
	tr_arg_prolog1(E1, [], E, Inst,
		       ds_ta([], [], [], [], []),
		       ds_ta(VOut, PVOut, PreOps, PostOps, PostConds)),
	(	Inst == inst,
		VOut == [],
		PVOut == [],
		PreOps == [],
		PostOps == [],
		PostConds == []
	->	true
	;	fatal_error('Expected simple ground value, got ~w',[E1])
	).

efgh0(efgh(E,F,G,H), E, F, G, H) :-
	assert_debug(E >=0),
	assert_debug(F >=0),
	assert_debug(G >=0),
	assert_debug(H >=0),
	assert_debug((H > 0;G = 0)),
	assert_debug(E+F+G+H>0),
	!.
efgh0(Delay, _, _, _, _) :-
	impl_error('Illegal internal delay ~w', [Delay]).

efgh(T1, E, F, G, H) :-
	tr_arg0(T1, T2),
	(efgh1(T2, E, F, G, H)
	->	true
	;T1 \= T2
	->	fatal_error(['Expected <DELAY>:efgh(<N>,<N>,<N>,<N>)|unit,',
			     'got ~w, translate into ~w'],[T1,T2])
	;	fatal_error(['Expected <DELAY>:efgh(<N>,<N>,<N>,<N>)|unit,',
			     'got ~w'],[T1])
	).

valid_efgh(E,F,G,H) :-
	number(E),
	number(F),
	number(G),
	number(H),
	E >= 0,
	F >= 0,
	G >= 0,
	H >= 0,
	E + F + G + H > 0,
	(	H >0;G=0).

efgh1(efgh(E,F,G,H), E, F, G, H) :-
	(valid_efgh(E,F,G,H)
	->	true
	;	fatal_error('Delays must all be numbers >= 0, not all 0, if H = 0 then G must be 0, GOT ~w', [efgh(E,F,G,H)])
	),
	!.
efgh1(Other, E, F, G, H) :-
	efgh2(Other, E, F, G, H).

efgh2(unit, 1, 1, 1, 1).
efgh2(standard, 0, 0, 1, 1).

setup_delay(setup_maxfg, F, G, NewDelay) :-
	NewDelay is F + G.
setup_delay(setup_maxg, _F, G, G).

leadsto_max_value(Option, Delay) :-
	flag(delay, Old, 0),
	(	dyn_leadsto(_RId,_Vars, _LitDisConj, _AndLiterals, Delay1),
		efgh0(Delay1, _E, F, G, _H),
		setup_delay(Option, F, G, NewDelay),
		flag(delay, Cur, max(Cur, NewDelay)),
		fail
	;	true
	),
	flag(delay, Delay, Old).

reload_time(TStart) :-
	start_time(TStart).




option(_) :-
	fail.

do_setup_time(TStart, TSetup) :-
	(	member(Option, [setup_maxg, setup_maxfg]),
		option(Option)
	->	fatal_error('Option ~w temporarily unsupported, email lourens@cs.vu.nl', [Option]),
		leadsto_max_value(Option, Delay)
	;	Delay = 0
	),
	reload_time(TStart),
	TSetup is TStart - Delay,
	assert_debug(\+ dyn_setup_time(_)),
	assertz(dyn_setup_time(TSetup)).



/*
  dyn_setup_time(TSetup), dyn_handled_time(THandled)
  You should never as for values before TSetup.
  All Atoms that have no explicit trace entry before THandled have value
  false if cwa unknown otherwise.
  */
setup_unknown_or_cwa(TStart, TSetup) :-
	assertz(dyn_handled_time(TStart)),
	assert_debug(TStart >= TSetup).

ensure_setup_time(T) :-
	(dyn_setup_time(T)
	->	true
	;	fatal_fail(dyn_setup_time(T))
	).

/*
ensure_reload_time(THandled) :-
	ensure_handled_time(THandled).
*/
set_handled_time(THandled) :-
	%flag(ddht, P, P + 1),
	%alog(ht,'~w:HANDLED TIME -> ~w~n', [P, THandled]),
	alog(ht,'HANDLED TIME -> ~w~n', [THandled]),
	assert_debug(ground(THandled)),
	retractall(dyn_handled_time(_)),
	assertz(dyn_handled_time(THandled)).

ensure_handled_time(THandled) :-
	(dyn_handled_time(THandled)
	->	true
	;	fatal_fail(dyn_handled_time(THandled))
	).

tr_model_arg(Var:SortName, Var:Sort, Size) :-
	(reserved(Var)
	->	error('model variable name is a reserved name: ~w',
		      [Var]),
		fail
	;	sort_info(SortName, si(Sort, _Kind, Size))
	->	true
	;	error('Undefined sort ~w in model argument ~w',
		      [Var:SortName]),
		fail
	).
mul_inf(0,maxinf,0) :-
	!.
mul_inf(maxinf,0,0) :-
	!.
mul_inf(maxinf, _, maxinf) :-
	!.
mul_inf(_, maxinf, maxinf) :-
	!.
mul_inf(X, Y, XY) :-
	XY is X*Y.

tr_model_args([], [], 1).

tr_model_args([Arg|Args], [ModelVar|ModelVars], Size) :-
	tr_model_arg(Arg, ModelVar, Size1),
	tr_model_args(Args, ModelVars, Size2),
	mul_inf(Size1,Size2,Size).



instantiate_model_vars([], [], [],_Status).

instantiate_model_vars([Constant:Sort|ModelVars], [Constant|Constants],
		       [Instance|Instances],Status) :-
	instantiate_elemente(Sort, _, Instance, Status),
	(var(Status)
	->	instantiate_model_vars(ModelVars, Constants,Instances,Status)
	;	true
	).

setup_models(ModelVars, TraceName) :-
	(	instantiate_model_vars(ModelVars, Constants, Instances,Status),
		(var(Status)
		->	ModelInstance =.. [TraceName|Instances],
			assertz(dyn_next_model(ModelInstance,Constants,Instances)),
			fail
		;	error('Instantiating models failed')
		)
	->	fail
	;	true
	).

runmodel(TraceTell, ModelInstance,ModelConstants,ModelInstances) :-
	setup_model_constants(ModelConstants, ModelInstances),
	runspec1,
	savemodelspec_cleanup(TraceTell,ModelInstance),
	cleanup_model_constants(ModelConstants, ModelInstances).
savemodelspec_cleanup(TraceTell,ModelInstance) :-
	telling(Old),
	tell(TraceTell),
	portray_trace_entry(ModelInstance, trace),
	(savetrace1(ModelInstance)
	->	tell(Old)
	;	tell(Old),
		fail
	),
	reset_run_info.



setup_model_constants([],[]).

setup_model_constants([Constant|ModelConstants],
		      [ModelInstance|ModelInstances]) :-
	check_constant(Constant,ModelInstance),
	spec:asserta(constant(Constant, ModelInstance)),
	setup_model_constants(ModelConstants, ModelInstances).
cleanup_model_constants([],[]).

cleanup_model_constants([Constant|ModelConstants], [ModelInstance|ModelInstances]) :-
	spec:retract(constant(Constant, ModelInstance)),
	cleanup_model_constants(ModelConstants, ModelInstances).
runspec(Pict) :-
	dyn_model(Model),
	!,
	(do_graphs
	->	error('Cannot (yet) combine multiple traces with communication visualisation'),
		fail
	;	true
	),
	Model =.. [TraceName|Args],
	tr_model_args(Args, ModelVars, Size),
	(Size =:= 0
	->	warning('Model specification ~w has no instances', [Model])
	;	(Size == maxinf
		->	error('Model specification ~w defines an infinite number of traces', [Model]),
			fail
		;	alog(mayorstats,'Model ~w defines ~w traces~n',
			     [Model, Size])
		),
		setup_models(ModelVars, TraceName),
		savetracesetup(File,Pict, Telling),
		portray_clause(model(Model,Size)),
		telling(NewTelling),
		tell(Telling),
		(	retract(dyn_next_model(ModelInstance,ModelConstants,
					       ModelInstances)),
			runmodel(NewTelling,ModelInstance,ModelConstants,
				 ModelInstances),
			fail
		;	telling(OldT),
			tell(NewTelling),
			told,
			format('SAVED TRACES IN ~w~n',[File]),
			tell(OldT)
		)
	).
runspec(Pict) :-
	runspec1,
	savetrace(Pict).

correct_start_end_times :-
	uchecklist(correct_entry,
		  [(dyn_start_time(ST),ST),(dyn_end_time(ET),ET)]).

correct_entry((Entry,Term)) :-
	copy_term((Entry,Term),(Entry1,Term1)),
	(Entry
	->	tr_basic_element(Term, [], Term1),
		(Term1 = Term
		->	true
		;	retract(Entry),
			assertz(Entry1)
		)
	;	true
	).



runspec1 :-
	debug(algo, runspec1, []),
	do_setup_time(TStart, TSetup),
	debug(algo, runspec2, []),
	correct_start_end_times, % ugly!
	debug(algo, runspec3, []),
	setup_rt_intervals,
	debug(algo, runspec4, []),
	setup_unknown_or_cwa(TStart, TSetup),
	debug(algo, runspec5, []),
	alog('leadsto', '~nAnalysis and instantiation of leadsto rules~n~n', []),
	alog('DOnontrleads', '  pre-instantiated rules will be output preceded by a prefix instance(InstanceId, RuleId)~n', []),
	alog('leadsto', '~n~n', []),
	flag(ppp, Old, 0),
	(	flag(ppp, P, P + 1),
		debug(algo, 'LT~w', [P]),
	        dyn_leadsto(RId,Vars, LitDisConj, AndLiterals, Delay),
		setup_leadsto(TStart,Vars, LitDisConj, AndLiterals, Delay,RId),
		fail
	;	true
	),
	flag(ppp, _, Old),
	debug(algo, runspec6, []),
	get_model_checking_p_rules,
	alog('leadsto', 'END of Analysis leadsto rules~n~n', []),
	setup_atom_state_boundaries,
	debug(algo, runspec_rest, []),
	runspec_rest.


:- dynamic dyn_schedule_fire/3.

handle_fired :-
	(	retract(dyn_schedule_fire(ConseRId, T3, T4)),
		fire(ConseRId, T3, T4),
		fail
	;	true
	).
fire(ds_cr(ConseLits, RInfo),T3, T4) :-
	!,
	flag(ds_cr, I, I + 1),
	alog(deb, 'fire:~w', [I]),
	fire_conse(ConseLits, RInfo, T3, T4).
fire(ds_ppp(PXOR, RInfo,P), T3, T4) :-
	!,
	isolate_pxor_branch(PXOR, P, ConseLits),
	fire_conse(ConseLits, RInfo, T3, T4).
fire(What, _T3, _T4) :-
	fatal_error('Unrecognised fire action:~w', [What]).

/*
  We have ds_pxor(P, PVOutC, ConseLits, R)
  with R ds_pxor(P, PVOutC, ConseLits, R) or ds_pr(PVOutC, ConseLits)

  We have normalised Pis:

  P1: L, (1-P1):R
  => P1:L1, (1 - P1):P2:L2, (1-P1)*(1-P2):P3:L3, ... (1-P1)*(1-P2)*(1-Pn):R
  So we have a sequence
  P < P1 => L1
  P < P1 + (1-P1)*P2 = P1 + P2 - P1*P2 = P2 + P1*(1 - P2) => L2
  P < P1 + (1-P1)*P2 + (1-P1)*(1 - P2)*P3 => L3
  P < P2 + P1*(1 - P2) + (1-P1)*(1 - P2)*P3 =
      P2 + (1-P2)*(P1 + (1 - P1)*P3)
      P2 + (1 - P2)* (P3
  We have a PDone: The offset of chances not followed, P > PDone
  We have a current factor: (1-P1)*(1-P2)*(1-Pk)
  */
isolate_pxor_branch(PXOR, P, ConseLits) :-
	isolate_pxor_branch1(PXOR, P, ConseLits, 1, 0).
isolate_pxor_branch1(ds_pxor(Pk, _PVOutC, ConseLits1,R), P, ConseLits, PFactor, PDone) :-
	PLeft is PFactor*Pk + PDone,
	(PLeft < P
	->	PFactor1 is (1.0 - Pk)*PFactor,
		isolate_pxor_branch1(R, P, ConseLits, PFactor1, PLeft)
	;	ConseLits = ConseLits1
	).
isolate_pxor_branch1(ds_pr(_PVOutC, ConseLits), _P, ConseLits, _, _).

fire_conse([], _RInfo, _T3, _T4).
fire_conse([ds_litd(Atom, PN, PreOps, PostOps, PostConds)|ConseLits],
	   RInfo,T3,T4) :-
	(	run_ops(PreOps),
		run_ops(PostOps),
		(	run_ops(PostConds)
		->	fire_lit(RInfo, Atom, PN, T3, T4)
		;	true
		),
		fire_conse(ConseLits, RInfo, T3, T4),
		fail
	;	true
	).
fire_lit(RInfo, Atom, PN, T1, T2) :-
	pntf(PN, TF),
	set_range_atom1(T1, T2, Atom, TF, RInfo).

:- dynamic dyn_wait_var/14.

:- dynamic dyn_handled_wait_var_instance/4.






/* First for all HTI trace entries compare with current
	*/
runspec_rest :-
	end_time(TEnd),
	repeat,
	    set_state,
	    (model_checking_stop
	    ->	    !
	    ;	    flag(htsfailed, HTS, HTS + 1),
		    (	    handled_time_step(RT)
		    ->	    true
		    ;	    impl_error('htsfailed  ~w',[HTS])
		    ),
		    cmp_ge(RT, TEnd),
		    handle_fired,
		    !
	    ).



handled_time_step(ResultTime) :-
	ensure_handled_time(HT),
	flag(hts, I, I + 1),
	get_time(T1),
	flag(bti, BTI, BTI + 1),
	(	do_log(pr_traces)
	->	pr_traces(handled_time_step_start(HT))
	;	true
	),
	(do_log(pr_waits)
	->	pr_waits(handled_time_step_start(HT))
	;	true
	),
	assert_debug(var(ResultTime)),
	handle_fired,
	first_possible_activity_result(ResultTime),
	alog(resulttime, '  RESULTTIME(~w):~w~n', [BTI,ResultTime]),
	(cmp_gt(ResultTime, HT)
	->	true
	;	fatal_error('No progress, probably used too much 0 time parameters...~n')
	),
	assert_debug(cmp_gt(ResultTime, HT)),
	cleanup_traces(HT, ResultTime),
	set_handled_time(ResultTime),
	end_time(TEnd),
	(cmp_ge(ResultTime, TEnd)
	->	true
	;	update_activity_times(ResultTime)
	),
	(do_log(pr_waits)
	->	pr_waits(handled_time_step_end(ResultTime))
	;	true
	),
	get_time(T2),
	T is T2 - T1,
	(do_log(time(hts(I)))
	->	alog(time(hts(I)), 'TIME HANDLED_TIME_STEP(~w, ~w)=~w~n',
			[I, ResultTime, T])
	;	true
	),
	send(@display, synchronise).


%	show_profile(100),
%	break.



cleanup_traces(OldHT, NextHT) :-
	(	dyn_atom_trace(AtomKey, Atom, AtomTrace),
		(to_backup_trace(Atom, AtomTrace, OldHT, NextHT)
		->	backup_atom_trace(AtomKey, Atom, AtomTrace)
		;	true
		),
		fail
	;	true
	).
backup_atom_trace(AtomKey, Atom, AtomTrace) :-
	alog(bua, 'BU:~w~n', [backup_atom_trace(AtomKey, Atom, AtomTrace)]),
	retract(dyn_atom_trace(AtomKey, Atom, AtomTrace)),
	assert_debug(ground(AtomKey)),
	assertz(dyn_atom_trace_backup(AtomKey, Atom, AtomTrace)).



%
%  If a trace is not visible for rules, we may remove the trace
%  OldHT, NewHT: we just handled all changes between OldHT, NewHT
%
to_backup_trace(Atom, AtomTrace, OldHT, NewHT) :-
	(AtomTrace = [range(Tlo, Thi, TFU)|_]
	->	to_backup_trace1(Atom, Tlo, Thi, TFU, OldHT, NewHT)
	;	impl_error('Unrecognised trace type')
	).
to_backup_trace1(Atom, _Tlo, Thi, TFU, OldHT, NewHT) :-
	assert_debug(memberchk(TFU, [true,false])),
	cmp_le(Thi, OldHT),
	(cmp_le(Thi, OldHT)
	->	true
	;	cmp_ge(Thi, NewHT)
	->	fail
	;	cmp_lt(Thi, NewHT),
		\+ cwa(Atom)
	->	true
	;	fail
	).

/*
to_backup_trace1(Atom, _Tlo, Thi, TFU, OldHT, _NewHT) :-
	cmp_le(Thi, OldHT),
	(cmp_lt(Thi, OldHT)
	->	default_value(Atom, V),
		(V == unknown
		->	true
		;	to_backup_default_false(Atom)
		)
	;	TFU == unknown
	->	true
	;	default_value(Atom, V),
		V == TFU
	->	to_backup_default_false(Atom)
	;	fail
	).
*/
to_backup_default_false(Atom) :-
	warning('Problem reclaiming cwa value for ~w', [Atom]).

optvar :-
	true.
set_wait1_var(T, Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K) :-
	recwait,!,
	assert_debug(atom(Id)),
	assert_debug(ground(IdTerm)),
	(optvar
	->	assert_debug(\+ recorded(Id, dyn_wait_var(Id,IdTerm,_A,_B,_C,_D,_E,
						     _F,_G,_H,_I,_J,_K,_T))),
		recordz(Id,dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,T)),
		(	do_log(setwait)
		->	flag(setwait, If, If + 1),
			alog(setwait, '  ~w:set_wait1a(~w, ~w)',
			     [If, T, w(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,T)])
		;	true
		)
	;	impl_error('Unexpected1'),
		set_wait1(T, _Activity)
	).
set_wait1_var(T, Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K) :-
	assert_debug(atom(Id)),
	assert_debug(ground(IdTerm)),
	(optvar
	->	assert_debug(\+ dyn_wait_var(Id,IdTerm,_A,_B,_C,_D,_E,
						     _F,_G,_H,_I,_J,_K,_T)),
		assertz(dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,T)),
		(	do_log(setwait)
		->	flag(setwait, If, If + 1),
			alog(setwait, '  ~w:set_wait1a(~w, ~w)',
			     [If, T, w(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,T)])
		;	true
		)
	;	impl_error('Unexpected1'),
		set_wait1(T, _Activity)
	).

set_wait1(T, Activity) :-
	(optvar
	->	assert_debug(Activity \= wait_var(_Id1,_IdTerm1,_A,_B,_C,_D,_E,
						  _F,_G,_H,_I,_J,_K))
	;	true
	),
	ensure_handled_time(HT),
	(cmp_ge(T, HT)
	->	true
	;	Activity = wait_var(_Id2,_IdTerm2,_,_,_,_,_,_,_,_,_,_,_)
	->	true
	;	impl_error('set_wait1HT')
	),
	(recwait
	->	hash_time(T, Key),
		recordz(Key, dyn_wait1(T, Activity))
	;	assertz(dyn_wait1(T, Activity))
	),
	(do_log(setwait)
	->	flag(setwait, If, If + 1),
		alog(setwait, '  ~w:set_wait1b(~w, ~w)', [If, T, Activity])
	;	true
	).


recwait.

hash_time(T, T1) :-
	(integer(T)
	->	T1 = T
	;	hash_term(T, T1)
	).

% Order not unimportant for GC
retract_wait1(HT, Activity) :-
	recwait,
	var(Activity),
	!,
	impl_error(retract_waitisvar),
	assert_debug(optvar),
	assert_debug(nonvar(HT)),
	(	hash_time(HT, HTK),
		recorded(HTK, dyn_wait1(HT, Activity),Ref),
		erase(Ref)
	;	Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K),
		assert_debug(ground(Id)),
		recorded(Id, dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,HT),
			 Ref),
		erase(Ref)
	).
retract_wait1(HT, Activity) :-
	optvar,
	var(Activity),
	!,
	(	retract(dyn_wait1(HT, Activity))
	;	Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K),
		retract(dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,HT))
	).

retract_wait1(HT, Activity) :-
	optvar,
	Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K),
	!,
	(recwait
	->	assert_debug(atom(Id)),
		recorded(Id, dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,HT),
			 Ref),
		erase(Ref)
	;	retract(dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,HT))
	).
retract_wait1(HT, Activity) :-
	(recwait
	->	assert_debug(nonvar(HT)),
		hash_time(HT, HTK),
		recorded(HTK, dyn_wait1(HT, Activity), Ref),
		erase(Ref)
	;	retract(dyn_wait1(HT, Activity))
	).
retract_wait1(Ref) :-
	erase(Ref).

get_wait1(HT, Activity, Ref) :-
	optvar,
	var(Activity),
	!,
	assert_debug(var(HT)),
	(recwait
	->	(var(HT)
		->	current_key(Key),
			(integer(Key)
			->	recorded(Key,dyn_wait1(HT, Activity),Ref)
			;	Key = Id,
				recorded(Id,dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,
							 G,H,I,J,
							 K,HT),Ref),
				Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K)
			)
		;	impl_error(nonvar1)
		)
	;	(	clause(dyn_wait1(HT, Activity),true, Ref)
		;	Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K),
			clause(dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,HT),true,
			       Ref)
		)
	).
get_wait1(HT, Activity, Ref) :-
	(	optvar,
		Activity = wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K)
	->	(recwait
		->	(var(Id)
			->	current_key(Id)
			;	true
			),
			recorded(Id,dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,
						 K,HT),Ref)
		;	clause(dyn_wait_var(Id,IdTerm,A,B,C,D,E,F,G,H,I,J,K,
					    HT),true,Ref)
		)
	;	(recwait
		->	(var(HT)
			->	current_key(Key),
				integer(Key)
			;	hash_time(HT, Key)
			),
			recorded(Key, dyn_wait1(HT, Activity), Ref)
		;	clause(dyn_wait1(HT, Activity),true, Ref)
		)
	).

/* Make sure all activities are at or past ResultTime
BEWARE: it could very well be that we are dependent on
   values < ResultTime.
*/

ltest(W) :-
	(	is_local,
		ensure_handled_time(HT),
		HT =:= 81
	->	Activity = wait_var(_Id,IdTerm,_,_,_,_FVL,_,_,_,_,_,_,_),
		IdTerm = k(r(12), (stimulus|occurs)),
		(get_wait1(_T, Activity,_Ref)
		->	format('WAITVARPRESENT:~w~n',[W])
		;	format('WAITVARLOST:~w~n',[W]),
			trace
		)
	;	true
	).



update_activity_times(ResultTime) :-
	(	dyn_handled_wait_var_instance(_IdV,_IdVTerm,_FVV,_)
	->	impl_error('Unhandled wait_var_instance')
	;	true
	),
	assert_debug(\+ dyn_rm_gc_wait_vars(_,_,_,_,_,_,_,_)),
	alog(update_act_times, '   UPDATE_activity_times:~w~n',[ResultTime]),
	update_activity_times1(ResultTime),
	postponed_rm_gc,
	alog(update_act_times,'   END UPDATE_activity_times~n').
update_activity_times1(ResultTime) :-
	recwait,
	!,
	(	get_wait1(T, Activity,Ref),
		(	do_log(rmv),
			Activity = wait_var(_Id,IdTerm,_,_,_,FVL,_,_,_,_,_,_,_)
		->	flag(rmv1, I, I + 1),
			alog(rmv1, 'update_wait_var:T:~w,~w  ~w (~w)~n', [T,I,IdTerm, FVL]),
			\+ \+ pr_wait(Activity, _)
		;	true
		),
		cmp_lt(T, ResultTime),
		flag(uatid, UAT, UAT + 1),
		alog(db(uatid), 'UATID:~w~n', [UAT]),
		alog(db(uat),'  UAT:~w:~w  i:~w~n',[T, Activity, UAT]),
		retract_wait1(Ref),
		update_activity_time(Activity, T),
		fail
	;	true
	).
update_activity_times1(ResultTime) :-
	repeat,
	(	get_wait1(T, Activity,Ref),
		(	do_log(rmv),
			Activity = wait_var(_Id,IdTerm,_,_,_,FVL,_,_,_,_,_,_,_)
		->	flag(rmv1, I, I + 1),
			alog(rmv1, 'update_wait_var:~w  ~w (~w)~n', [I,IdTerm, FVL])
		;	true
		),
		cmp_lt(T, ResultTime)
	->
		flag(uatid, UAT, UAT + 1),
		alog(db(uatid), 'UATID:~w~n', [UAT]),
		alog(db(uat),'  UAT:~w:~w  i:~w~n',[T, Activity, UAT]),
		retract_wait1(Ref),
		local_trace(first),
		update_activity_time(Activity, T),
		fail
	;	!
	).


update_activity_time(Activity, T) :-
	!,
	%retract_wait1(T, Activity),
	(update_activity_time1(Activity, T)
	->	true
	;	fatal_fail(update_activity_time1(Activity, T))
	).

%AnteHolds holds between TMin and THolds, but THolds - TMin < G
update_activity_time1(wait_true(TMin, THolds, AnteHolds, ConseRId, Delay),
		      THolds) :-
	/* only thing we can do is follow all AnteHolds */
	/* A shame because we are now forgetting that we know everything
	   succeeds upto THolds */
	reverse(AnteHolds,AnteHolds1),
	chk_not_inv(AnteHolds1),
	Removed = wait_true(TMin, THolds, AnteHolds, ConseRId, Delay),
	update_new_true_range(AnteHolds1, TMin, maxinf, [],ConseRId, Delay, Removed).


/*
  THolds: antecedent holds upto THolds
  THandled:time upto which consequent has fired
  */
update_activity_time1(wait_fired(THolds, THandled, AnteHolds,
				ConseRId, Delay), THolds) :-
	/*
	  Two cases:
	  1)antecedent holds longer
	  2)antecedent holds no longer
	  */
	reverse(AnteHolds, AnteHolds1),
	chk_not_inv(AnteHolds1),
	Removed = wait_fired(THolds, THandled, AnteHolds,
				ConseRId, Delay),
	update_lits_fired(AnteHolds1, THolds, THandled, [], ConseRId,
			  Delay, maxinf, Removed).







/*
  wait_var:
  wait_var(HT,TMin,FV, FVL, LitData, ToDoAnte, AnteHolds,THolds, ConseRId,
  PV,Delay)
  We know that AnteHolds holds from TMin to THolds. We put the
  wait_var on handled_time, because from then on new antecedent elements
  could become true. NO, we put wait_var to a later time: The
  */
/*
  We know AnteHolds holds in TMin, THolds.
  Now look for lit(Atom,PN)
  Predicate is almost the same as setup_lt_notground:
  Only difference is rm_handled_var(Rs1, FVL, Rs)

  TMin seems irrelevant now as we expect wait_var to deal only with
  as yet blank/unknown literals



  */
update_activity_time1(wait_var(Id, IdTerm, HT,TMin,FV, FVL, LitData,
			      ToDoAnte,
			      AnteHolds,THolds, ConseRId,PV,Delay), HT1) :-
	assert_debug(HT1 == HT),
	assert_debug(uchecklist(var, FV)),
	% TODO: should really start after last instantiated elem of FVL
	% or something like that
	% ensure_handled_time(TH),
	Removed = wait_var(Id,IdTerm,HT,TMin,FV, FVL, LitData,ToDoAnte,
			   AnteHolds,THolds, ConseRId,PV,Delay),
	assert_debug(ground(AnteHolds)),
	setup_lt_notground_fv(HT, FV, FVL, LitData, ToDoAnte,AnteHolds,THolds,
			      ConseRId,PV,Delay,Id,IdTerm,Removed).

update_activity_time1(Activity, T) :-
	fatal_fail(update_activity_time1(Activity, T)).

rm_handled_var([], _FVL, []).
rm_handled_var([R1|Rs], FVL, RsOut) :-
	R1 = ds_or(_Atom, FV, _O2),
	(memberchk(FV, FVL)
	->	RsOut = RsOut1
	;	RsOut = [R1|RsOut1]
	),
	rm_handled_var(Rs, FVL, RsOut1).








/* Rule has fired up to THoldsPrev earlier. Now we know that the antecedent
   up to certain point extended true range to THoldsNew, (but this time is
   smaller than HandledTime: so, check AnteHolds and possibly increase
   firingtime. But there is a gap, other place deals with set_wait1
   */
check_continue_fire([], THoldsPrev, THandled,THoldsNew,ConseRId) :-
	ensure_handled_time(HT),
	assert_debug(cmp_lt(THoldsNew, HT)),
	T3 = THandled,
	T4 is T3 + THoldsNew - THoldsPrev,
	schedule_fire(ConseRId, T3, T4).

check_continue_fire([ds_lh(lit(Atom,PN),_Id,_IdTerm)|AnteHolds], THoldsPrev,
		    THandled,THoldsNew,ConseRId) :-
	ensure_handled_time(HT),
	assert_debug(cmp_lt(THoldsNew, HT)),
	find_min_range_ground(Atom, PN, THoldsPrev, O2),
	(O2 = true(_Tlo, Thi, _Cont)
	->	min_new(THoldsNew, Thi, THoldsNew1),
		check_continue_fire(AnteHolds, THoldsPrev, THandled,
				    THoldsNew1,ConseRId)
	;	true
	).



/*
  update_lits_fired called by wait_fired activity
  THoldsPrev: antecedent known to hold upto THolds
  THandled:time upto which consequent has fired

  Running arg:
  HoldLits: those Lits hold (extra) between  THoldsPrev and THoldsNew
  */
update_lits_fired([], THoldsPrev, THandled, HoldLits, ConseRId, Delay,
		  THoldsNew, Removed) :-
	%efgh0(Delay, _E, _F, G, H),
	ensure_handled_time(HT),
	(model_checking
	->	THoldsNew1 = HT
	;	THoldsNew1 = THoldsNew
	),
	T3 = THandled,
	T4 is T3 + THoldsNew1 - THoldsPrev,
	(cmp_lt(T3, T4)
	->	schedule_fire(ConseRId, T3, T4)
	;	true
	),
	chk_inv(HoldLits),
	assert_debug((\+ model_checking;cmp_lt(THoldsPrev,HT))),
	assert_debug(cmp_ge(THoldsNew1, HT)),
	chk_removed(Removed),
	setup_lt_wait_fired(THoldsNew1,T4,HoldLits,ConseRId,Delay,Removed).

/*
  AnteHolds is in reverse order. Probably not so wise as as soon as some entry
  fails, we need to retract all wait_vars following.
  */
update_lits_fired([ds_lh(lit(Atom,PN),Id, IdTerm)|AnteHolds], THoldsPrev,
		  THandled,HoldLits,ConseRId, Delay, THoldsNew, Removed) :-
	reverse(HoldLits, HoldLits1),
	append(HoldLits1, [ds_lh(lit(Atom,PN),Id,IdTerm)|AnteHolds],
	       L1),
	chk_not_inv(L1),
	ensure_handled_time(HT),
	assert_debug(cmp_gt(HT, THoldsPrev), ht1),
	assert_debug(cmp_ge(THoldsNew, HT), ht2),
	% "because next time is past old time"
	find_min_range_ground(Atom, PN, THoldsPrev, O2),
	(O2 = true(Tlo, Thi, _Cont)
	->	assert_debug(cmp_le(Tlo, THoldsPrev)),
		assert_debug(cmp_gt(Thi, THoldsPrev)),
		min_new(THoldsNew, Thi, THoldsNew1),
		(cmp_ge(THoldsNew1, HT)
		->	update_lits_fired(AnteHolds, THoldsPrev, THandled,
				  [ds_lh(lit(Atom, PN),Id,IdTerm)|HoldLits],
				  ConseRId, Delay, THoldsNew1, Removed)
		;	check_continue_fire(AnteHolds, THoldsPrev, THandled,
					    THoldsNew1, ConseRId),
			find_min_range_ground(Atom, PN, Thi, O3),
			(O3 = fail(TEnd3, _By3)
			->	update_retrace1(TEnd3, Atom, PN, Id, IdTerm,
						AnteHolds, THoldsPrev,
						HoldLits, ConseRId, Delay,
						THoldsNew, Removed)
			;O3 = blank
			->	impl_error('blank impsble:cmp_lt(THoldsNew1,HT)')
			;	impl_error('true after true')
			)
		)
	;O2 = blank
	->	% Lit is blank at end of prev fired true range
		impl_error('because next time is past old time'),
		setup_lt_wait_g(Tlo, Atom, PN,Id,IdTerm,AnteHolds, THoldsPrev,
				HoldLits, ConseRId, Delay, THoldsNew, Removed)
	% Doesn't seem to occur, probably always called with handled_time
	% equal to THoldsPrev
	;O2 = fail(TEnd, _By)
	->	assert_debug(cmp_gt(TEnd, THoldsPrev)),
		% Lit failed at end of prev fired true range
		update_retrace1(TEnd, Atom, PN, Id, IdTerm, AnteHolds,
				THoldsPrev,HoldLits, ConseRId, Delay,
				THoldsNew, Removed)
	;	impl_error('Unrecognised O2:~w', [O2])
	).

% update_retrace1(TFail, Atom, PN, Id, LitsToHold, THoldsPrev,HoldLits, ConseRId,
%		Delay,THoldsNew,Removed)
% We know HoldLits hold (before and) between  THoldsPrev and THoldsNew.
% We know THoldsNew >= THandledNew
% We know lit(Atom, PN) failed at THoldsPrev till TFail
% We know LitsToHold also hold up to THoldsPrev, they are ground
% HoldLitsRev, lit(Atom, PN), LitsToHold represent ground instance
% of whole antecedent
update_retrace1(TFail, Atom, PN, Id, IdTerm,LitsToHold, THoldsPrev,HoldLits,
		ConseRId,Delay,THoldsNew,Removed) :-
	chknotandyes(LitsToHold, Atom, PN, Id,IdTerm, HoldLits,
		     update_retrace11),
	assert_debug(cmp_gt(TFail, THoldsPrev)),
	(cmp_lt(TFail, THoldsNew)
	->	find_min_range_ground(Atom, PN,TFail, O2),
		(O2 = true(Tlo, Thi, _Cont)
		->	assert_debug(cmp_gt(Tlo, THoldsPrev)),
			assert_debug(cmp_eq(Tlo, TFail)),
			(	cmp_lt(Tlo, THoldsNew)
			->	min_new(Thi, THoldsNew,TN),
				update_new_true_range(LitsToHold, Tlo, TN,
						      [ds_lh(lit(Atom,PN),Id,
							     IdTerm)|HoldLits],
						      ConseRId, Delay,Removed)
			;	update_mid_true_range(Tlo, Thi, Atom, PN, Id,
						      IdTerm,LitsToHold,
						      HoldLits, ConseRId,Delay,
						      Removed)
			)
		;O2 = blank
		->	setup_lt_wait_g(TFail, Atom, PN, Id, IdTerm,LitsToHold,
					THoldsPrev, HoldLits, ConseRId, Delay,
					THoldsNew,Removed)
		;	impl_error('Fail after fail ~w', [O2])
		)
	;       fix1
	->      setup_lt_wait_g(TFail, Atom, PN, Id, IdTerm,LitsToHold,
					THoldsPrev, HoldLits, ConseRId, Delay,
					THoldsNew,Removed)
	;	update_new_range(TFail, Atom, PN, Id, IdTerm, LitsToHold,
				 HoldLits,ConseRId, Delay, Removed)
	).

% update_new_range(TFail, Atom, PN, Id, LitsToHold, HoldLits, ConseRId, Delay, Removed)
% HoldLitsInv, lit(Atom, PN), LitsToHold are instantiated antecedent of rule
% that used to hold.
% We know HoldLits held up to some time below TFail, useless
% We just noticed that lit(Atom, PN) failed at TFail
/* We know lit(Atom, PN) failed just at TFail. HoldLits were true somewhere
   before TFail
*/
update_new_range(TFail, Atom, PN, Id, IdTerm, LitsToHold, HoldLits, ConseRId,
		 Delay, Removed) :-
/* We know HoldLits were true somewhere =< Tlo
   We know lit(Atom,PN) is true between Tlo and Thi
   BUG:
   We should first try to reuse wait_var entries
*/
	%chk_not_inv(LitsToHold),
	%chk_inv(HoldLits),
	reverse(HoldLits, HoldLits1),
	append(HoldLits1, [ds_lh(lit(Atom,PN),Id,IdTerm)|LitsToHold], OLits),
	chk_not_inv(OLits),
	update_new_true_range(OLits, TFail, maxinf,
			      [],ConseRId,Delay, Removed).

update_mid_true_range(Tlo, Thi, Atom, PN, Id,IdTerm, LitsToHold, HoldLits,
		      ConseRId, Delay,Removed) :-
	local_trace(mid),
	append(HoldLits, LitsToHold, OLits),
	update_new_true_range(OLits, Tlo, Thi, [ds_lh(lit(Atom,PN),Id,IdTerm)],
			      ConseRId, Delay, Removed).


% update_new_true_range(LitsToHold, Tlo, Thi,AnteHolds,ConseRId,Delay,Removed)
% Whole antecedent is instantiated, used to be true previous THandled.
% We checked AnteHolds to hold at least up to THandled.
% We know LitsToHold held from Tlo to previous handled time
% Removed: the activity that has been removed.

% Tlo is also measure of timepoint before which antecedent does not
% hold.
% Seems to be essential that rule is not busy firing.??


update_new_true_range([], Tlo, Thi, AnteHolds, ConseRId, Delay, Removed) :-
	setup_lt_conse(AnteHolds, Tlo, Thi, ConseRId, Delay,Removed).

update_new_true_range([ds_lh(lit(Atom,PN),Id,IdTerm)|LitsToHold], Tlo, Thi,
		      AnteHolds,ConseRId, Delay, Removed) :-
	ensure_handled_time(HT),
	assert_debug(cmp_ge(Thi, HT)),
	chknotandyes(LitsToHold, Atom, PN, Id, IdTerm,AnteHolds, new_true1),
	find_min_range_ground(Atom, PN, Tlo, O2),
	(O2 = true(Tlo1, Thi1, _Cont)
	->	(cmp_lt(Thi1, HT)
		->	find_min_range_ground(Atom, PN, Thi1, O3),
			(O3 = blank
			->	setup_lt_wait_g(Thi1, Atom, PN, Id, IdTerm,
						LitsToHold, Tlo, AnteHolds,
						ConseRId, Delay,Thi, Removed)
			;O3 = fail(TEnd, _By1)
			->	update_retrace1(TEnd, Atom, PN, Id, IdTerm,
						LitsToHold,Tlo,AnteHolds,
						ConseRId, Delay, Thi, Removed)
			;	impl_error('true after true?')
			)
		;	assert_debug(cmp_le(Tlo1, Tlo)),
				% We allow part of ante to start before
			min_new(Thi, Thi1, TN),
			update_new_true_range(LitsToHold, Tlo, TN,
					      [ds_lh(lit(Atom,PN),Id,IdTerm)|
					      AnteHolds],ConseRId,Delay,
					      Removed)
		)
	;O2 = blank
	->	setup_lt_wait_g(Tlo, Atom, PN, Id, IdTerm,LitsToHold,Tlo,
				AnteHolds, ConseRId, Delay, Thi, Removed)
	;O2 = fail(TEnd, _By2)
	->	update_retrace1(TEnd, Atom, PN, Id, IdTerm, LitsToHold, Tlo,
				AnteHolds, ConseRId, Delay, Thi, Removed)
	;	impl_error('O2?~w', [O2])
	).









%  setup_lt_wait_g(TFail, Atom, PN, Id, LitsToHold, THoldsPrev, HoldLits, ConseRId,
%  Delay, THoldsNew, Removed)
%  At TFail lit(Atom, PN) is blank, TFail < THandled
%  HoldLits are satisfied from THoldsPrev to THoldsNew >= THandled

setup_lt_wait_g(TFail, Atom, PN, Id, IdTerm, LitsToHold, _THoldsPrev,
		HoldLits, _ConseRId, _Delay, _THoldsNew, Removed) :-
	chknotandyes(LitsToHold, Atom, PN, Id, IdTerm,HoldLits,
		     setup_lt_wait_g1),
	(is_local
	->	ensure_handled_time(HT),
		assert_debug(cmp_ge(TFail, HT))
	;	true
	),
	rm_gc_wait_vars(Atom,PN,Id, IdTerm,LitsToHold, HoldLits, [], Removed).


:- dynamic dyn_res1/1, dyn_no_progress_wait/1.


first_possible_activity_result(RT) :-
	ensure_handled_time(HT),
	(dyn_res1(_)
	->	impl_error('Old res1')
	;	assertz(dyn_res1(maxinf))
	),
	(dyn_no_progress_wait(_)
	->	impl_error('old progress_wait')
	;	true
	),
	(	get_wait1(T, Activity,_Ref),
		dyn_res1(RT1),
		cmp_lt(T, RT1), % no result < T expected
		activity_min_result_time(Activity, RT3),
		(cmp_lt(RT3, RT1)
		->	(cmp_le(RT3, HT)
			->	assertz(dyn_no_progress_wait(Activity))
			;	once(retract(dyn_res1(RT1))),
				assertz(dyn_res1(RT3))
			)
		;	true
		),
		fail
	;	once(retract(dyn_res1(RTI))),
		(bagof(Wait, retract(dyn_no_progress_wait(Wait)), Waits)
		->	handle_zero_delay(Waits, RTI, RT)
		;	RT = RTI
		)
	).
/*

  First case : we have one wait_var 0,0,0,H>0 but the antecedent is not
  true and not the consequent of another 0,0,0,* rule.

  We could first simply only allow 0,0,0,H>0 depending on antecedent literals
  that are not consequents of 0,0,0,H'>0 rules or that are known
  already for (HT, HT+EPS) and possibly also not on cwa literals.
  Or if on cwa literals then they should not become true by another
  consequent firing with zerodelay.

  If we have a wait_var waiting on any normal literal, we are done, we
  have a delay that is the previous minimal delay: because the normal literal is
  still not ok and will change at least after next ht?

  First isolate all consequents of zero delay rules. Then we can select
  those rules that are part of the zero delay set.
*/
handle_zero_delay(Waits, RTIn, RT) :-
	(Waits == []
	->	RT = RTIn
	;	impl_error('Zero delay rules depending on zero delay rules not handled')
	).
/*
  Probably better to work one argument at a time. So, First process
  the LitData entry. And analyse all Conclusions. Those LitDataEntries that
  also occur as Consequents or as cwa remain special.
  For those cases proceed into the wait_var LitsToHod
  */
% filter out any non zero delay instances of ToDoAnte
% Let (sub)procedure(s) guarantee valid wait_var storage
% Two aspects: need to gather new nonzero ht result,
/*
filter_non_zero_ante_instances(Wait, NewZeroWaits, AWIn, AWOut) :-
	Wait = wait_var(Id,IdTerm, HT, TMin, FV, FVL, LitData,
				  ToDoAnte, AnteHolds,THolds, ConseRId,
				  PV,Delay),
	remove_non_zero_ante_instances(ToDoAnte, FVL, Wait, NewZeroWaits, AWIn, AWOut,
				       FVL1).

remove_non_zero_ante_instances([], FVLIn, Wait, NewZeroWaits, AWIn, AWOut,
			       FVLIn) :-
	Wait = wait_var(Id,IdTerm, HT, TMin, FV, FVL, LitData,
				  ToDoAnte, AnteHolds,THolds, ConseRId,
				  PV,Delay),
	NewZeroWaits = [wait_var(Id,IdTerm, HT, TMin, FV, FVLIn, LitData,
				  ToDoAnte, AnteHolds,THolds, ConseRId,
				  PV,Delay)].
remove_non_zero_ante_instances([LitData|ToDoAnte], FVLIn, Wait, NewZeroWaits, AWIn,
			       AWOut, FVLIn) :-
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	run_ops(PreOps),
	free_variables(Atom, FV),
	bagof1(ds_at(Atom, FV, AtomTrace),
	       find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, FVL),
	       Traces),
	remove_non_applicable_traces(Traces, Traces1, FVL, FVL1)
	find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds),
	free_variables(Atom, FV),
analyse_partition_wait_vars([Wait|Waits], DataIn, DataOut) :-
	analyse_partition_wait_var(Wait, DataIn, Data1),
	analyse_partition_wait_vars(Waits, Data1, DataOut).
analyse_partition_wait_var(Wait, DataIn, DataOut) :-
	Wait = wait_var(_Id,_IdTerm, HT, _TMin, FV, FVL, LitData,
				  ToDoAnte, AnteHolds,THolds, ConseRId,
				  _PV,Delay),
	efgh0(Delay, E, _F, G, H),
	assert_debug(FVL \== [[]]),
	assert_debug(0 =:= G*G+E*E),
	(ConseRId = ds_cr(ConseLits,RInfo),
		ConseLits = [ds_litd(_,_,_,_,_)|_]
	->	true
	;ConseRId = ds_ppp(ConseLits, RInfo,_P)
	->	true
	;	impl_error('schedule_fire arg error')
	),
	gather_conse_info(ConseLits, DataIn, Data1),
	gather_antes_info(
	tst_consep(ConseRId, ConseRId1),

remove_normal_ante_wait_vars([Wait|Waits], RTIn, RTOut, WaitsAll, Waits1) :-
	remove_normal_ante_wait_var(Wait, RTIn, RT1, WaitsAll, WaitsLeft),
	remove_normal_ante_wait_vars(Waits, RT1, RTOut, WaitsAll, Waits2),
	append(WaitsLeft, Waits2, Waits1).
*/
/* _AnteHolds holds from TMin to THolds
   We know AnteHolds holds between TMin and THolds.
  AnteHolds contains ground Antecedent part that holds.
  LitData is current literal info.
  ConseRId is consequent info.

   We need to propagate the wait_var past handled_time, preferably
   past RTIn.
   We have an entry hanging on some first atom.
   If this atom is non cwa related and does not occur as the consequent of
   some zero delay leadsto rule, simply skip this entry, (but check that
   the atom is nonmatching/unknown past HT, past RTIn)

   Q:how to extract info wrt zero delay rules from set of wait_vars
   All wait_var entries must be gathered:
   a)for their set of conclusion atoms
   b)for the set of antes tha match a zero delay conclusion
   */
/*
remove_normal_ante_wait_var(wait_var(_Id,_IdTerm, HT, _TMin, FV, FVL, LitData,
				  ToDoAnte, AnteHolds,THolds, ConseRId,
				  _PV,Delay),RTIn,RTOut,WaitsAll,WaitsLeft) :-
	!,
	local_trace(zd),
	ensure_handled_time(HT),
	assert_debug(cmp_le(HT, THolds)),
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	assert_debug(ground(PreOps)),
	assert_debug(uchecklist(var,FV)),
	% We could assert: no LitData entry has a matching true value
	% between HT and RTIn, moreover, the entry should be unknown
	bagof1(ds_at(Atom, FV, AtomTrace),
	       find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, FVL),
	       Traces),
	check_no_missed_traces(Traces, Atom, PN,FV, FVL),

	impl_error('Zero delay rules not handled').
remove_normal_ante_wait_var(_Wait, _WaitsAll,_WaitsLeft) :-
	impl_error('Zero delay rules not handled').
*/

/* AnteHolds holds between TMin and THolds, but THolds - TMin < G */
activity_min_result_time(wait_true(TMin, THolds, _AnteHolds, _ConseRId,
				    Delay), RT) :-
	!,efgh0(Delay, E, _F, G, _H),
	RT is TMin + G + E,
	alog(wt, 'WT:TH:~w   TMIN:~w RT:~w~n', [THolds, TMin, RT]).


/*
  Rule just fired up to THandled
  */
activity_min_result_time(wait_fired(_THolds, THandled, _AnteHolds, _Conse,
				    _Delay), RT) :-
	!,
	RT = THandled.

	%efgh0(Delay, E, _F, G, H),
	%RT is THandled -G + E + H.

activity_min_result_time(wait_var(_Id,_IdTerm, HT, _TMin, _FV, FVL, _LitData,
				  _ToDoAnte, _AnteHolds,_THolds, _ConseRId,
				  _PV,Delay),RT) :-
	!,efgh0(Delay, E, _F, G, _H),
	%LitData = ds_litd(Atom, PN, _PreOps, _PostOps, _PostConds),
	(FVL == [[]]
	->	RT = maxinf
	;	RT is HT + G + E
	).
activity_min_result_time(Activity, RT) :-
	impl_error('Unimplemented:~w',
		   [activity_min_result_time(Activity,RT)]).





invalid_delay(TimeInfo1) :-
	assert_debug(ground(TimeInfo1)),
	tr_arg0(TimeInfo1, Delay1),
	(efgh2(Delay1, _,_,_,_)
	->	fail
	;	Delay1 = efgh(E,F,G,H),
		valid_efgh(E,F,G,H)
	->	fail
	;	error('Delays must all be numbers >= 0, not all 0, if H = 0 then G must be 0 and E <= F, GOT ~w', [Delay1])
	).

test_simplify :-
	repeat,
	    write('Testterm:'),
	    read(Term),
	    (memberchk(Term, [end_of_file, stop, end])
	    ->    !
	    ;     simplify_term(Term, [], Term1,Status),
	          format('Result:~w, Status:~w~n', [Term1, Status]),
	          fail
	    ).
setup_lt_internal(TStart, Vars1, Ante2, Conse2, Delay1, RId) :-
	end_time(TEnd),
/*	(is_local
	->     simplify_term(pxor(0.5, and(b, pxor(0.5, c, true)), d),[],
			     _Tt,_StatusT),
	       test_simplify
	;      true
	),
	*/
	simplify_term(Ante2, Vars1, Ante1,Status),
	var(Status),
	simplify_term(Conse2, Vars1, Conse1,Status),
	var(Status),
	efgh(Delay1, E, F, G, H),
	Delay = efgh(E, F, G, H),
	(	is_true(Conse1, Vars1, Status)
	->	(var(Status)
		->	warning('Ignoring rule with true consequent')
		;	warning('Ignoring rule')
		)
	;	is_true(Ante1, Vars1, Status)
	->	(nonvar(Status)
		->	warning('Ignoring rule')
		;	is_false(Conse1, Vars1, Status)
		->	(var(Status)
			->	error('Derived false at setup_leadsto')
			;	warning('Ignored rule')
			)
		;	true
		),
		formula_variables(Conse1, Vars1, Vars2),
		set_range_conse(Conse1, Vars2, TStart, TEnd),
		(model_checking
		->	warning('Interpreting trivial rules as intervals')
		;	true
		)
	;	is_false(Ante1, Vars1,Status)
	->	(var(Status)
		->	warning('Ignoring rule with false antecedent')
		;	warning('Ignoring rule with error')
		)
	;	setup_nontrivial_leadsto(Vars1, Ante1,Conse1,Delay,RId)
	).


setup_lt_internalL(TimeInfo1, Vars1,Forms2, TStart, RId) :-
	(	nonvar(TimeInfo1),
		is_list(Forms2),
		Forms2 = [Ante2,Conse2]
	->	true
	;	impl_error('Callback range')
	),
	setup_lt_internal(TStart, Vars1, Ante2, Conse2, TimeInfo1, RId).

/*
  A new leadsto rule should start holding from TStart.
  This implies that global handled_time should be <= TStart
  */
setup_leadsto(TStart,Vars, Ante, Conse, Delay, RId) :-
	alog('leadsto', 'RId ~w:~w~n',[RId,leadsto(Vars, Ante,
						 Conse, Delay)]),
	init_interval_callbacks(Delay, Vars, [Ante,Conse], TimeInfo1, Vars1,
				Forms2,
			fail, invalid_delay(TimeInfo1),
			setup_lt_internalL(TimeInfo1, Vars1,Forms2, TStart,RId)).



setup_leadsto(TStart,Vars, LitDisConj, Conse, Delay) :-
	fatal_fail(setup_leadsto(TStart,Vars, LitDisConj, Conse,Delay)).


/*
  select at least one, at most all elements from list
  */
random_select_parts([], _) :-
	impl_error('random_select_parts empty list').
random_select_parts(Terms3, Terms4) :-
	length(Terms3, L),
	I is random(L),
	random_select_parts_ensureI(Terms3, I, Terms4).
random_select_parts_ensureI([Term|Terms3], 0, [Term|Terms3N]) :-
	!,
	random_select_parts_eo(Terms3, Terms3N).
random_select_parts_ensureI([Term|Terms3], I, Res) :-
	I1 is I - 1,
	random_select_parts_ensureI(Terms3, I1, Terms3N),
	(halfchance
	->	Res = [Term|Terms3N]
	;	Res = Terms3N
	).
random_select_parts_eo([], []).

random_select_parts_eo([Term|Terms3], Res) :-
	random_select_parts_eo(Terms3, Res1),
	(halfchance
	->	Res = [Term|Res1]
	;	Res = Res1
	).
halfchance :-
	0 =:= random(2).

set_range_conse(Term, Vars, TStart, TEnd) :-
	make_disj_normal(Term, Term2),
	tr_dis_normal(Term2, Terms3),
	random_select_parts(Terms3, Terms4),
	uchecklist(set_range_lit(TStart, TEnd, Vars), Terms4).
set_range_lit_vars(Vars, TStart, TEnd, Lit) :-
	formula_variables(Lit, Vars, Vars1),
	(	instantiate_vars(Vars1, VarsInst),
		set_range_lit(TStart, TEnd, VarsInst, Lit),
		fail
	;	true
	).


connector(and(T1, T2), and, T1, T2).
connector(or(T1, T2), or, T1, T2).


is_ground(Term, Vars) :-
	formula_variables(Term, Vars, VOut),
	!,
	VOut = [].

formula_variables(Term, Vars, VOut) :-
	formula_variables(Term, [], Vars, [], VOut).

formula_variables(not(T1), QVars, Vars, VIn, VOut) :-
	!,formula_variables(T1, QVars, Vars, VIn, VOut).
formula_variables(T, QVars, Vars, VIn, VOut) :-
	connector(T, _Kind, T1, T2),
	!,formula_variables(T1, QVars, Vars, VIn, V1),
	formula_variables(T2, QVars, Vars, V1, VOut).
formula_variables(Term, QVars, Vars, VIn, VOut) :-
	quantor(Term, Ranges, _Kind, T),
	!,range_variables(Ranges, QVars, Vars, QVarsN, VIn, V1),
	formula_variables(T, QVarsN, Vars, V1, VOut).
formula_variables(Atom, QVars, Vars, VIn, VOut) :-
	arg_variables(Atom, QVars, Vars, VIn, VOut).
range_variables([], QVars, _Vars, QVars, VIn, VIn).
range_variables([Range|Ranges], QVars, Vars, QVarsN, VIn, VOut) :-
	range_variables1(Range, QVars, Vars, QVars1, VIn, V1),
	range_variables(Ranges, QVars1, Vars, QVarsN, V1, VOut).
range_variables1(Range, QVars, Vars, QVars1, VIn, VOut) :-
	range_var_condition(Range, VarName, Sort, _Op, ConditionTerm),
	ensure_new_var(VarName, QVars, 'range variable reused'),
	ensure_new_var(VarName, Vars, 'global variable reused in quantor'),
	arg_variables(ConditionTerm, QVars, Vars, VIn, VOut),
	ensure_new_var(VarName, QVars, 'range variable reused'),
	ensure_new_var(VarName, Vars, 'global variable reused in quantor'),
	var_to_var_list(VarName, Sort, QVars, QVars1).


vars_from_ranges([], VarsIn, VarsIn).
vars_from_ranges([Range|Ranges], VIn, VOut) :-
	var_from_range(Range, VIn, V1),
	vars_from_ranges(Ranges, V1, VOut).
var_from_range(Range, VIn, VOut) :-
	range_var_condition(Range, VarName, Sort, _Op, _ConditionTerm),
	ensure_new_var(VarName, VIn, 'variable reused for range'),
	var_to_var_list(VarName, Sort, VIn, VOut).



arg_variables(Arg, _QVars, _Vars, _VIn, _VOut) :-
	\+ ground(Arg),
	!,fatal_error('arg_variables not ground').

arg_variables(Arg, QVars, Vars, VIn, VOut) :-
	atom(Arg),
	(var_in_var_list(Vars, Arg, Sort)
	->	!,var_to_var_list(Arg, Sort, VIn, VOut)
	;var_in_var_list(QVars, Arg, _Sort)
	->	!,VOut = VIn
	;	fail
	).
arg_variables(Arg, QVars, Vars, VIn, VOut) :-
	spec_constant(Arg, Arg1),
	test_recursive_constant(Arg, Arg1),
	!,
	(arg_variables(Arg1, QVars, Vars, VIn, VOut)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).
arg_variables(Arg, _QVars, _Vars, VIn, VIn) :-
	atomic(Arg),
	!.
arg_variables(V:Sort, QVars, Vars, VIn, VOut) :-
	(same_var_from_var_list(QVars, V, Sort)
	->	VOut = VIn
	;same_var_from_var_list(Vars, V, Sort)
	->	ensure_var_in_var_list(VIn, V, Sort, VOut)
	;	VOut = VIn
	).
arg_variables(Arg, QVars, Vars, VIn, VOut) :-
	Arg =.. [_|Args],
	args_variables(Args, QVars, Vars, VIn, VOut).
args_variables([], _QVars, _Vars, VIn, VIn).
args_variables([Arg|Args], QVars, Vars, VIn, VOut) :-
	arg_variables(Arg, QVars, Vars, VIn, V1),
	args_variables(Args, QVars, Vars, V1, VOut).




setup_nontrivial_leadsto(Vars, AnteFormula, ConseFormula, Delay, RId) :-
	flag(setup_nontrivial_leadsto, Id, Id + 1),
	alog('DOnontrleads','instance(~w,~w):~w~n',
	     [Id, RId, leadsto(Vars, AnteFormula, ConseFormula, Delay)]),
	/*
	  algonew did: create NLT_rule,
	  setup handled_time
	  We need to check whether rules are open already, have instantiations
	  otherwise hang them at the correct TimeStruct
	*/
	setup_lt(AnteFormula, ConseFormula, Vars, Delay, Id, RId).


:- dynamic dyn_lt_rule/6.
lt_rule(Id, AnteLits, ConseLits, PVOutC, Delay, RId) :-
	dyn_lt_rule(Id, AnteLits, ConseLits, PVOutC, Delay, RId).

rule(Id) :-
	(dyn_lt_rule(Id, AnteLits, ConseLits, PVOutC, Delay, RId)
	->	format('~w~n', [lt_rule(Id, AnteLits, ConseLits, PVOutC, Delay)]),
		(dyn_leadsto(RId, Vars, LitDisConj, AndLiterals, Delay1)
		->	true
		;	impl_error('Missing rule source ~w', [RId])
		),
		format('~nFROM:~n~w~n',
		       [leadsto(RId, Vars, LitDisConj, AndLiterals, Delay1)])
	;	warning('No rule with id ~w~n', [Id])
	).

/*
  Follow the Antecedent formula. Maintain:
  1)Analysis-time: We should not start looking before TAnalysis
  2)Is antecedent open at startup time
  3)Since when is Ante open
  Result: hang in rule at certain time:
  Depending on pos or neg and the atom expressions we depend on.

  We do need to have the exhaustive dependency on atoms at the first possible
  time. If no atom holds there, we may reanalyse the whole formula for
  a later time.

  So: Determine TFirst: The first time at or after TAnalysis where
  Antecedent could start holding.
  Determine Atoms: the atoms that would trigger some part of the
  Antecedent to become true.
  Determine Continuation: the rest of the formula that should hold.
  */
enable_pxor :-
	is_local,!.
enable_pxor :-
	get_option(pxor).
tr_pxor_all(In, Out) :-
	(tr_pxor(In, I1)
	->      tr_pxor_all(I1, Out)
	;       Out = In
	).

/*
  LATER
  pxorn(c(P1, F1, c(P2, F2, r(F3))))
  probabilistic exclusive or normalised
  pxorn(pxor(P1, F1, pxor(P2, F2, F3)),
        pxorn(c(P1, F1, c((1-P1)*P2, F2, r(F3)))))
  P1 + (1 - P1)*P2 + (1-P1)*(1-P2)
  pxorn(pxor(P1, pxor(P2, F1, F2), F3),
        pxorn(c(P1*P2, F1, c(P1*(1 - P2), F2, r(F3)))))
  */
tr_pval(Val, P1) :-
	(Val == default
	->	P1 = 0.5
	;	number(Val)
	->	P1 = Val
	;	atom(Val1),
		atom_to_term(Val1, P1, []),
		number(P1),
		0 =< P1,
		P1 =< 1.0
	->	true
	;	error('Expected probability, got ~w', [Val]),
		fail
	).

tr_pxorr([E1, E2], P2, pxor(P, T1, T2)) :-
	!,
	(	E1 = pxe(Val, T1),
		E2 = pxe(nil, T2)
	->	tr_pval(Val, P1),
		P is P2*P1
	;	error('Expected pxor got ~w', [pxor([E1,E2])]),
		fail
	).
tr_pxorr([E1, E2|R], P2, pxor(P, T1, T2)) :-
	(	E1 = pxe(Val, T1)
	->	tr_pval(Val, P1),
		P is P2*P1,
		P22 is 1 - P,
		tr_pxorr([E2|R], P22, T2)
	;	error('Expected pxor got ~w', [pxor([E1,E2])]),
		fail
	).
tr_pxor(pxor(L),PXor) :-
	!,
	tr_pxorl(L, 1.0, PXor).

tr_pxor(pxor(P1, T1, T2), Res) :-
	\+ number(P1),
	pre_eval_num1(P1, P2),
	(number(P2)
	->	(P2 =:= 0.0
		->	Res = T2
		;P2 =:= 1.0
		->	Res = T1
		;	Res = pxor(P2, T1, T2)
		)
	;	Res = pxor(P2, T1, T2)
	).

tr_pxor(pxor(_P1, T1, T2), T1) :-
	T1 == T2.
tr_pxor(pxor(P1, TF, T2), pxor(P2, T2, TF)) :-
	memberchk(TF, [true, false]),
	\+ memberchk(T2, [true, false]),
	pre_eval_num(1.0 - P1, P2).
tr_pxor(pxor(P1, pxor(P2, A1, A2), A3),pxor(P12, A1,pxor(P112, A2, A3))) :-
	pre_eval_num(P1*P2, P12),
	pre_eval_num(P1*(1.0-P2),P112).
tr_pxor(and(pxor(P1, A1, A2), A3),
	pxor(P1, and(A1, A3), and(A2, A3))).
tr_pxor(and(A1, pxor(P1, A2, A3)), pxor(P1, and(A1, A2), and(A1, A3))).
tr_pxor(and(and(A1, A2), A3), and(A1, and(A2, A3))).
tr_pxor(not(or(A1, A2)), and(not(A1), not(A2))).
tr_pxor(not(not(A1)), A1).
tr_pxor(p(P1, A), pxor(P1, A, true)).

tr_pxor(and(A1, A2), and(A11, A2)) :-
	tr_pxor(A1, A11).
tr_pxor(and(A1, A2), and(A1, A22)) :-
	tr_pxor(A2, A22).
tr_pxor(not(A), A1) :-
	tr_pxor(A, A1).
tr_pxor(pxor(P, A1, A2), pxor(P, A11, A2)) :-
	tr_pxor(A1, A11).
tr_pxor(pxor(P, A1, A2), pxor(P, A1, A21)) :-
	tr_pxor(A2, A21).
tr_pxor(or(A1, A2), or(A11, A2)) :-
	tr_pxor(A1, A11).
tr_pxor(or(A1, A2), or(A1, A22)) :-
	tr_pxor(A2, A22).


/* PDone is Probability already dealt with.
   The probability should be multiplied:
   P1: a1, P2:a2, P3:a3
   P1:a1  -- ( P2*(1-P1) -- P3*(1 - P2)*(1 - P2)
   0.3, 0,2, 0.4, 0.1
   = (p1 ( p2 ( p3 p4)))
   (1 - p1)*p2 = P2
   p2 = P2/(1-p1)
   (1 - p1)*(1-p2)*p3 = P3
   p3 = P3/((1-p1)*(1-p2)) = P3/((1 - P1)(1 - P2/(1- p1)) =
                             P3/(1 - P1 - P2)
   p1, (1 -p1)*p2, (1 - p1)*(1-p2)*p3, (1-p1)(1-p2)(1-p3)
   p1 + p2 - p1p2 + p3 - p1p3 - p2p3 + p1p2p3 + (1 - p1 - p2 + p1p2)(1-p3)
  = 1
   So we need the chance left (1 - p1)*(1 - p2)*..
   */
tr_pxorl([], _PLeft, true) :-
	!.

tr_pxorl([pxe(Val, T1), pxe(@nil, T2)], PLeft, pxor(P, T1, T2)) :-
	!,pre_eval_num(Val/PLeft, P).

tr_pxorl([pxe(Val, T1)|L], PLeft, pxor(P, T1, T2)) :-
	pre_eval_num(PLeft-Val, PLeft1),
	pre_eval_num(Val/PLeft, P),
	tr_pxorl(L, PLeft1, T2).

conse_r_element(LHS, VOut, PVOut, PVOutC, ConseLits) :-
	DInC = ds_d(ConseLits1, VOut, PVOut),
        code_form(LHS, pos, DInC, DOutC),
	DOutC = ds_d([], VOutC, PVOutC),
	rm_true_ds_litd(ConseLits1, ConseLits),
	(	VOutC == []
	->	true
	;	fatal_error('Rule contains variables missing from ante/conse')
	).
conse_r_from_antep(pxor(P, LHS, RHS), VOut, PVOut,
		   ds_pxor(P, PVOutC, ConseLits, R)) :-
	!,
	conse_r_element(LHS, VOut, PVOut, PVOutC, ConseLits),
	conse_r_from_antep(RHS, VOut, PVOut, R).
conse_r_from_antep(LHS, VOut, PVOut, ds_pr(PVOutC, ConseLits)) :-
	!,
	conse_r_element(LHS, VOut, PVOut, PVOutC, ConseLits).

code_conse(Conse, VOut, PVOut, Id, ConseRId, invalidPV) :-
	enable_pxor,
	tr_pxor_all(Conse, Conse1),
	Conse1 = pxor(_,_,_),
	!,
	conse_r_from_antep(Conse1, VOut, PVOut, DSPXOR),
	ConseRId = ds_pcr(DSPXOR, ds_ri(Id)).
code_conse(Conse, VOut, PVOut, Id, ConseRId, PVOutC) :-
	conse_r_element(Conse, VOut, PVOut, PVOutC, ConseLits),
	ConseRId = ds_cr(ConseLits, ds_ri(Id)).
setup_lt(Ante, Conse, Vars, Delay, Id, RId) :-
	DIn = ds_d(AnteLits1, Vars, []),
	code_form(Ante, pos, DIn, DOut),
	DOut = ds_d([], VOut, PVOut),
	rm_true_ds_litd(AnteLits1, AnteLits),
	(VOut == []
	->      code_conse(Conse, VOut, PVOut, Id, ConseRId, PVOutC),
		assertz(dyn_lt_rule(Id, AnteLits,ConseRId,PVOutC,Delay,RId)),
	        ensure_setup_time(TSetup),
		TMin = TSetup,
		% instead of mininf otherwise problem with invariant
		setup_lt_normed(AnteLits, [], TMin, maxinf, ConseRId, PVOutC,
				Delay,initial)
	;	error(['Rule contains variables missing from ante',
		       '(       use local forall):~w  in~n~w'],
			    [VOut,leadsto(Vars, Ante, Conse, Delay)])
	),!.
setup_lt(Ante, Conse, Vars, Delay, Id, RId) :-
	format('FAILED:~n'),
	trace,setup_lt(Ante, Conse, Vars, Delay, Id, RId).

rm_true_ds_litd(LitsIn, LitsOut) :-
	assert_debug(nonvar(LitsIn),rt1),
	rm_true_ds_litd(LitsIn, [], LitsOut).
rm_true_ds_litd([], Last, LitsOut) :-
	(Last == []
	->	LitsOut = []
	;	LitsOut = [Last]
	).

rm_true_ds_litd([L|LIn], Last, LitsOut) :-
	assert_debug(nonvar(LIn),rt2),
	L = ds_litd(Atom, PN, PreOpsOut, PostOpsOut, PostCondsOut),
	(Atom == true
	->	assert_debug(PN == pos),
		(Last == []
		->	impl_error('initial condition not handled')
		;	Last = ds_litd(AtomL, PNL, PreOpsOutL,PostOpsOutL,
				       PostCondsOutL),
			append(PostOpsOutL, PreOpsOut, PostOpsOutL1),
			(PostOpsOut == []
			->	true
			;	impl_error('Special case not implemented, e-mail lourens@cs.vu.nl please')
			),
			append(PostCondsOutL, PostCondsOut, PostCondsOutN),
			Last1 = ds_litd(AtomL,PNL,PreOpsOutL,PostOpsOutL1,
					PostCondsOutN),
			rm_true_ds_litd(LIn, Last1, LitsOut)
		)
	;	(Last == []
		->	LitsOut = LitsOut1
		;	LitsOut = [Last|LitsOut1]
		),
		rm_true_ds_litd(LIn, L, LitsOut1)
	).

/*
  Again: follow the conjunction thru literals.
  Assume LHS has been handled and is true
  Analysis of Lit in the middle: There are already a number of results for LHS.
  One such result has Some prolog vars instantiated and has antecedent up to
  then true starting at least at TIn, upto THolds.

  So, we start out with a partially instantiated PVList, with LHS instantiated,
  Also with THolds

  setup_lt_normed(AnteTodo, AnteHolds, TMin, TMax, ConseRId, PVOutC, Delay)

  We know AnteTodo holds in range(TMin, TMax), we need to continue with
  AnteHolds.

  Addition: We need the FV: free variable instantiation of the last
  true Literal
  */
setup_lt_normed([], AnteHolds, TMin, THolds, ConseRId, _PV, Delay, Removed) :-
	!,setup_lt_conse(AnteHolds, TMin, THolds, ConseRId, Delay, Removed).

setup_lt_normed([LitData|ToDoAnte], AnteHolds, TMin, THolds, ConseRId, PV,
		Delay,Removed) :-
	!,
	chk_inv(AnteHolds),
	LitData = ds_litd(_Atom, _PN, PreOps, _PostOps, _PostConds),
	run_ops(PreOps),
	setup_lt_notground(LitData, ToDoAnte, AnteHolds, TMin, THolds,
			   ConseRId, PV, Delay,Removed).

lambda_delay(Delay, D) :-
	efgh0(Delay, E, F, _G, _H),
	(E = F
	->	D = E
	;global_lambda(Lambda)
	->	D is E + Lambda*(F - E)
	;	randomf(Fact),
		D is E + (F - E) * Fact
	).
schedule_fire(ConseRId, T3, T4) :-
	(ConseRId = ds_cr(ConseLits,RInfo),
		ConseLits = [ds_litd(_,_,_,_,_)|_]
	->	true
	;ConseRId = ds_ppp(ConseLits, RInfo,_P)
	->	true
	;	impl_error('schedule_fire arg error')
	),
	alog(schedfire, '  SCHEDULE FIRE: ~w - ~w  : ~w  (~w)~n',
	       [T3, T4, ConseLits, RInfo]),
	(cmp_lt(T3, T4)
	->	true
	;	impl_error('Zero interval fire')
	),
	assertz(dyn_schedule_fire(ConseRId, T3, T4)).

/*
  We know AnteHolds holds between (TMin, THolds)

  From t1 to t2 ante holds then from
  t1 + g + (e,f) = t3 to t3 + t2 - t1 - g + h
  */
o :- fail.

tr_consep(ds_pcr(DSPXOR, Id),ds_ppp(DSPXOR,Id,P)) :-
	enable_pxor,
	!,
	randomf(P).
tr_consep(C, C).





/*
  If model_checking then do not fire past
  */
setup_lt_conse(AnteHolds, TMin, THolds, ConseRId, Delay,Removed) :-
	chk_inv(AnteHolds),
	efgh0(Delay, _E, _F, G, H),
	(model_checking
	->	ensure_handled_time(HT),
		min_new(THolds, HT, THolds1)
	;	THolds1 = THolds
	),
	chk_removed(Removed),
	(	cmp_ge(THolds1, TMin + G)
	->	lambda_delay(Delay, D),
		T3 is TMin + G + D,
		% T4 is T3 + THolds - TMin - G + H,
		% T4 is TMin + G + D + THolds -TMin - G + H
		% T4 is THolds + D + H
		(THolds1 == maxinf
		->	T4 = maxinf
		;	T4 is THolds1 + D + H
		),
		tr_consep(ConseRId, ConseRId1),
		schedule_fire(ConseRId1, T3, T4),
		THandled = T4,
		setup_lt_wait_fired(THolds1,THandled,AnteHolds,ConseRId1,Delay,
				    Removed)
	;	setup_lt_wait_true(TMin,THolds1,AnteHolds,ConseRId, Delay)
	).


% Need unique Id per ground antes and rule.
% Could be unique per variable instances
id_from_curr_holds_rule(AnteHolds, ConseRId, Key, KeyTerm) :-
	assert_debug(nonvar(ConseRId)),
	(ConseRId = ds_cr(_ConseLits, ds_ri(RId))
	->    true
	;     ConseRId = ds_pcr(_DSPXOR, ds_ri(RId))
	->    true
	;     fatal_error('Unrecognised conse:~w', [ConseRId])
	),
	key_from_rid_ante(AnteHolds, RId, KeyTerm, Key).

key_from_rid_ante(AnteHolds, RId, KeyTerm, Key) :-
	(AnteHolds == []
	->	KeyTerm = r(RId)
	;	AnteHolds = [ds_lh(lit(At,_PN), _SuperId, SuperTerm)|_],
		KeyTerm = k(SuperTerm, At)
	),
	my_term_to_atom(KeyTerm, Key),!.



rule_from_id_term(r(R), r(R), []) :-
	!.
rule_from_id_term(k(SuperTerm, At), R, [At|Atoms]) :-
	rule_from_id_term(SuperTerm, R, Atoms).

/* Would start with r(RId) -> k(r(RId),At) -> k(k(.. */
chk_not_inv(L) :-
	(chk_not_invf(L)
	->	true
	;	fatal_fail(chk_not_inv(L))
	).
chk_not_inv(Rest, At, IdTerm):-
	(chk_not_invf(Rest, At, IdTerm)
	->	true
	;	fatal_fail(chk_not_inv(Rest, At, IdTerm))
	).


chk_not_invf([]) :-
	!.
chk_not_invf([ds_lh(lit(At,_PN), SuperId, SuperTerm)|Rest]) :-
	SuperTerm = r(_),
	my_term_to_atom(SuperTerm, SuperId),
	chk_not_invf(Rest, At, SuperTerm),!.


chk_not_invf([], _, _) :-
	!.
chk_not_invf([ds_lh(lit(At1,_PN), SuperId1,SuperTerm1)|Rest], At, SuperTerm) :-
	SuperTerm1 = k(SuperTerm, At),
	my_term_to_atom(SuperTerm1, SuperId1),
	chk_not_invf(Rest, At1, SuperTerm1),!.





chk_inv([]) :-
	!.

chk_inv([ds_lh(lit(_At,_PN), _SuperId, SuperTerm)|Rest]) :-
	chk_inv1(Rest, SuperTerm),
	!.
chk_inv(F) :-
	fatal_fail(chk_inv(F)).

chk_inv1([], SuperTerm) :-
	(SuperTerm = r(_RId)
	->	true
	;	impl_error('root anteholds')
	).
chk_inv1([ds_lh(lit(At1,PN1), SuperId1,SuperTerm1)|Rest], SuperTerm) :-
	(	my_term_to_atom(SuperTerm1,SuperId1),
		SuperTerm = k(SuperTerm1, At1)
	->	chk_inv(Rest)
	;	fatal_fail(chk_inv1([ds_lh(lit(At1,PN1), SuperId1,SuperTerm1)|Rest], SuperTerm)),
		impl_error('int anteholds: ~w',
			   [k(SuperId1, At1)])
	).

/*
   We know AnteHolds holds in range(TMin, TMax).
   FV: already separated out ground instances
   */
setup_lt_notground(LitData, ToDoAnte, AnteHolds, TMin, THolds, ConseRId,
		   PV, Delay, Removed) :-
	chk_inv(AnteHolds),
	LitData = ds_litd(Atom, _PN, _PreOps, _PostOps, _PostConds),
	free_variables(Atom, FV),
	id_from_curr_holds_rule(AnteHolds, ConseRId, Id, IdTerm),
	setup_lt_notground_fv(TMin, FV, [], LitData, ToDoAnte, AnteHolds,
			      THolds, ConseRId,PV,Delay, Id, IdTerm, Removed).

/*
  We know AnteHolds is true between TStart and THolds
  We have possibly removed some wait_var entry and need to restore it.
  This wait_var should have time = THandled: so handle all
  intermediate effects.

  DEBUGGING: I think wait-var is set always at new HandledTime
             while overlapping_ground_range_po looks at TStart:

  Problem1: It could well be that THolds <= THandled, then
            we need to refocus on AnteHolds
  Problem2: TStart may well be, always always is < THandled
            find_min_range
  */

setup_lt_notground_fv(TStart, FV, FVL, LitData, ToDoAnte, AnteHolds,THolds,
		      ConseRId,PV,Delay, Id, IdTerm, Removed) :-
	% Id is id of LitData wait_var Id
	flag(uatid, UAT, UAT),
	assert_debug(ground(AnteHolds)),
	ensure_handled_time(HT),
	cmp_gt(HT, THolds),
	!,
	reverse(AnteHolds, AnteHolds1),
	chk_inv(AnteHolds),
	get_new_tholds(AnteHolds1, [], TStart, maxinf, THolds,  FV, FVL,
		       LitData, ToDoAnte, ConseRId,PV,Delay,Id,IdTerm,Removed).


	/*
	  We know AnteHolds holds up to THolds, but do not
	  know enough about T <= THandled

	  */
setup_lt_notground_fv(TStart, FV, FVL, LitData, ToDoAnte, AnteHolds,THolds,
		      ConseRId,PV,Delay, Id, IdTerm, Removed) :-
	chk_inv(AnteHolds),
	ensure_handled_time(HT),
	assert_debug(uchecklist(var, FV)),
	assert_debug(cmp_ge(THolds, HT)),
	assert_debug(THolds == maxinf;AnteHolds\=[]),
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	assert_debug(ground(PreOps)),
	assert_debug(uchecklist(var,FV)),
	findall(ds_at(Atom, FV, AtomTrace),
	       find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, FVL),
	       Traces),
	assert_debug(uchecklist(var,FV)),
	(do_log(fdho)
	->	flag(fdho, IF, IF + 1),
		alog(fdho, 'FDHO:~w~n', [IF])
	;	true
	),
	filter_defaults_handle_others(Traces, FV,TStart, PN, ToDoAnte,
				      AnteHolds, THolds, ConseRId,PV,Delay, Id,
				      IdTerm, FVL2,Removed),
	append(FVL, FVL2, FVL3),
	add_default_cwa(Atom,PN,TStart,PostOps,PostConds,FV,FVL3,FVL4,
			ToDoAnte, AnteHolds,THolds,ConseRId,PV,Delay, Id,
			IdTerm, Removed),
	(	retract(dyn_handled_wait_var_instance(Id,IdTerm,FV,PN1)),
		assert_debug(PN == PN1),
		(	PN == neg,
			cwa(Atom)
		->	true
		;	ensure_bu_atom(Atom,Id,IdTerm,FV)
		),
		fail
	;	true
	),
	assert_debug(uchecklist(var, FV)),
	setup_lt_notground1default(FV, FVL4,LitData,ToDoAnte, AnteHolds,
				   TStart,THolds,ConseRId,PV,Delay, Id,
				   IdTerm).
filter_defaults_handle_others([], _FV, _TStart, _PN, _ToDoAnte, _AnteHolds,
			      _THolds, _ConseRId,_PV,_Delay, _Id, _IdTerm, [],
			      _Removed).

filter_defaults_handle_others([ds_at(Atom, FV, AtomTrace)|Traces], FV1, TStart,
			      PN,
			      ToDoAnte, AnteHolds, THolds, ConseRId,PV,Delay,
			      Id,IdTerm,FVL,Removed) :-
	assert_debug(uchecklist(var, FV1)),
	(fail_filter_handle(TStart, FV, FV1, Atom, PN, ToDoAnte, AnteHolds,
			    THolds, ConseRId,PV,Delay, Id,IdTerm,AtomTrace,
			    Removed)
	->	FVL = [FV|FVL1]
	;	FVL = FVL1
	),
	filter_defaults_handle_others(Traces, FV1, TStart, PN, ToDoAnte,
				      AnteHolds, THolds, ConseRId,PV,Delay,
				      Id,IdTerm,FVL1,Removed).

ensure_bu_atom(Atom,Id,IdTerm,FV) :-
	assert_debug(ground(Atom)),
	atom_key(Atom, AtomKey),
	(dyn_atom_trace_backup(AtomKey, Atom, _AtomTrace)
	->	true
	;	flag(ensure_bu_atom, I, I),
		impl_error('Unaccounted handled_wait_var_instance ~q',
			   [atom_trace_backup(Atom,Id,IdTerm,FV,I)])
	).

find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV,FVL) :-
	find_atom_trace(Atom, AtomTrace),
	\+ memberchk(FV, FVL),
	run_ops(PostOps),
	run_ops(PostConds).


find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds) :-
	find_atom_trace(Atom, AtomTrace),
	run_ops(PostOps),
	run_ops(PostConds).

/*
  fail unless we have an entry that is true up to or past THandled
  in which case we create a separate entry or if we have an entry that
  fails past THandled for which we also create an entry

  Take care: We must also deal with atoms having entries for
  cwa such that the trace would prevent cwa firing.

  We know AnteHolds holds between TIn and THolds. Anything before
  TIn has been dealt with.

  TODO: ensure that Conse is instantiated to FV
  */
fail_filter_handle(_TIn, FV, FV1, _Atom, _PN, _ToDoAnte, _AnteHolds, _THolds,
		   _ConseRId,
		   _PV,_Delay, Id, IdTerm, _AtomTrace, _Removed) :-
	assert_debug(ground(FV)),
	(is_local
	->     assert_debug(uchecklist(var, FV1))
	;      true
	),
	retract(dyn_handled_wait_var_instance(Id,IdTerm,FV,_PN1)),
	!,
	(is_local
	->	warning('ignoring handled instance ~w', hwvi(Id,IdTerm,FV)),
		fail
	;	fail
	).

fail_filter_handle(TIn, FV, FV1, Atom, PN, ToDoAnte, AnteHolds, THolds,
		   ConseRId,
		   PV,Delay, Id, IdTerm, AtomTrace, Removed) :-
	reverse(AtomTrace, AT),
	fail_filter_handleR(AT,FV,FV1,TIn, Atom, PN, ToDoAnte, AnteHolds,
			    THolds, ConseRId, PV, Delay, Id, IdTerm, mininf,
			    Removed).



fail_filter_handleR([],FV,FV1, TIn, Atom, PN, ToDoAnte, AnteHolds,
		    THolds,ConseRId,PV,Delay,Id,IdTerm,LastTraceTHi,Removed) :-
	assert_debug(cmp_le(LastTraceTHi, TIn)),
	default_value(Atom, DefFU),
	missing_range1(TIn, maxinf,DefFU, Range),
	fail_filter_handleRR(FV,FV1,Range, [], TIn, Atom, PN, ToDoAnte,
			     AnteHolds, THolds,ConseRId, PV,Delay, Id, IdTerm,
			     Removed).

/*
  AT sorted increasing Time. LastTraceTHi is entry for Thi of inspected
  previous range.
  So we have in range LastTraceTHi -> Tlo1 empty entry
  */
fail_filter_handleR([range(Tlo1, Thi1, TFUB1)|AT],FV,FV1,TIn, Atom, PN,
		    ToDoAnte,AnteHolds, THolds,ConseRId, PV,Delay, Id,
		    IdTerm, _LastTraceTHi, Removed) :-
	(cmp_le(Thi1, TIn)
	->	fail_filter_handleR(AT,FV,FV1,TIn, Atom, PN, ToDoAnte,
				    AnteHolds, THolds,ConseRId, PV,Delay, Id,
				    IdTerm, Thi1, Removed)
	;	(cmp_lt(TIn, Tlo1)
		->	default_value(Atom, DefFU),
			missing_range1(TIn, Tlo1,DefFU, Range),
			AT1 = [range(Tlo1, Thi1, TFUB1)|AT]
		;	Range = range(Tlo1, Thi1, TFUB1),
			AT1 = AT
		),
		fail_filter_handleRR(FV,FV1,Range, AT1, TIn, Atom, PN,
				     ToDoAnte,  AnteHolds, THolds,ConseRId,
				     PV,Delay, Id, IdTerm, Removed)
	).
fail_filter_handleRR(FV,FV1,range(Tlo, Thi, TFUB), AT, TIn, Atom, PN, ToDoAnte,
		     AnteHolds, THolds,ConseRId, PV,Delay,Id,IdTerm,Removed) :-
	assert_debug(cmp_le(Tlo, TIn)),
	assert_debug(cmp_gt(Thi, TIn)),
	ensure_handled_time(HT),
	assert_debug(uchecklist(var, FV1)),
	(pntf(PN, TFUB)
	->	(cmp_lt(Thi, HT)
		->	\+ \+ (FV1 = FV,
				      check_fire_isolated(TIn, Thi,
							  ToDoAnte,
							  ConseRId,Delay)
			),
			fail_filter_handleR(AT, FV,FV1,Thi, Atom, PN, ToDoAnte,
					    AnteHolds,THolds,ConseRId, PV,
					    Delay, Id, IdTerm,Thi, Removed)
		;	min_new(Thi, THolds, THoldsNew),
			\+ \+ (
				copy_term(Removed, Removed1),
			FV1 = FV,
			setup_lt_normed(ToDoAnte,
					[ds_lh(lit(Atom, PN),Id,IdTerm)|AnteHolds],
					TIn, THoldsNew,
					ConseRId, PV, Delay, Removed1)
			      )
		)
	;	cmp_lt(Thi, HT)
	->	fail_filter_handleR(AT, FV,FV1,Thi, Atom, PN, ToDoAnte,
				    AnteHolds,
				    THolds,ConseRId, PV,Delay, Id, IdTerm, Thi,
				    Removed)
	;	fail
	/* We know in range(Tlo, Thi, TFUB) this instance explicitly
	   fails. We do not need to analyse it thru wait_var, but we may if
	   we are not dealing with cwa, which we are not.
	   */
	).


/*
  We need to propagate to or past THandled
  We know AnteHoldsDone holds between TStart and THoldsNew
  We know AnteOldToDo holds between TStart and THolds

  As soon as we know some AnteHolds fails before THandled,
  we add a possible isolated_fire event and
  continue
  from the next possible start of ground.
  As soon as we end with either
  range true possibly passing through THandled
  or we end with a fail followed by a blank
  */


get_new_tholds([], AHDone, TStart, THoldsNew, _THolds, FV, FVL,
	       LitData, ToDoAnte, ConseRId,PV,Delay, Id, IdTerm, Removed) :-
	setup_lt_notground_fv(TStart, FV, FVL, LitData, ToDoAnte, AHDone,
			      THoldsNew,ConseRId,PV,Delay, Id, IdTerm,Removed).

get_new_tholds([ds_lh(lit(Atom,PN),Id1,IdTerm1)|AnteHolds], AHDoneOut, TStart,
	       THoldsNew, Tholds, FV, FVL, LitData, ToDoAnte, ConseRId,PV,
	       Delay, Id, IdTerm, Removed) :-
	chknotandyes(AnteHolds, Atom, PN,Id1,IdTerm1,AHDoneOut,get_new_tholds),
	chk_not_inv(AnteHolds, Atom, IdTerm1),
	find_min_range_ground(Atom, PN, Tholds, O2),
	ensure_handled_time(HT),
	(O2 = true(Tlo1, Thi1, _Cont)
	->	assert_debug(cmp_le(Tlo1, Tholds)),
		min_new(Thi1, THoldsNew, THoldsNew1),
		(cmp_ge(Thi1, HT)
		->	AHDoneOut1 = [ds_lh(lit(Atom,PN),Id1,IdTerm1)|
				     AHDoneOut],
			get_new_tholds(AnteHolds, AHDoneOut1,
				       TStart, THoldsNew1,Tholds, FV, FVL,
				       LitData, ToDoAnte, ConseRId,PV,Delay,
				       Id, IdTerm, Removed)
		;	assert_debug(cmp_le(Tlo1, Tholds)),
			AHDoneOut1 = [ds_lh(lit(Atom,PN),Id1,IdTerm1)|
				     AHDoneOut],
			get_new_tholds(AnteHolds, AHDoneOut1, Tlo1, THoldsNew1,
				       Tholds,FV, FVL,
				       LitData, ToDoAnte, ConseRId,PV,Delay,
				       Id,IdTerm,Removed)
		)
	;O2 = fail(TFail,_)
	->	% fails from Tholds to TFail
		assert_debug(cmp_gt(TFail, Tholds),
			     'definition of THolds here'),
		(cmp_lt(TFail, THoldsNew)
		->	find_min_range_ground(Atom, PN, TFail, O22),
			(O22 == blank
			->	check_isolated_fire_rest(TStart, Tholds,
							 LitData, ToDoAnte,
							 ConseRId,PV,Delay),
				rm_gc_wait_vars_th(Atom,PN,Id1,IdTerm1,AnteHolds,
						AHDoneOut,ToDoAnte,
						Id, IdTerm, Removed)
			;O22 = true(Tl22,Th22, _)
			->	check_isolated_fire_rest(TStart, Tholds,
							 LitData, ToDoAnte,
							 ConseRId,PV,Delay),
				(	cmp_lt(Tl22, THoldsNew)
				->	min_new(Th22, THoldsNew, THoldsNew1),
					AHDoneOut1 = [ds_lh(lit(Atom,PN),Id1,
							    IdTerm1)|
						     AHDoneOut],
					get_new_tholds(AnteHolds, AHDoneOut1,
						       Tl22,
						       THoldsNew1,
						       Tholds,FV, FVL,
						       LitData, ToDoAnte,
						       ConseRId,PV,
						       Delay,Id,IdTerm,Removed)
				;	rm_gc_wait_vars_th(Atom,PN,Id1,IdTerm1,
							AnteHolds,
							AHDoneOut, ToDoAnte,
							Id, IdTerm, Removed)
				)
			;	impl_error('fail after fail')
			)
		;	rm_gc_wait_vars_th(Atom,PN,Id1, IdTerm1, AnteHolds,
					   AHDoneOut,ToDoAnte, Id, IdTerm,
					   Removed)
		)
	;	impl_error('Unexpected blank before THandled')
	).
/* We know the last Literal checked of AnteHolds does not hold right after THolds,
   but we need to check whether the interval TStart - Tholds may fire
   We know that rest of AnteHolds also hold */
check_isolated_fire_rest(Tstart, Tholds, LitData,ToDoAnte,ConseRId,PV,Delay) :-
	efgh0(Delay, _E, _F, G, _H),
	(cmp_ge(Tholds - Tstart, G)
	->	check_isolated_fire_rest1(Tstart, Tholds, LitData, ToDoAnte,
					  ConseRId,PV,Delay)
	;	true
	).
check_isolated_fire_rest1(Tstart, Tholds, LitData, ToDoAnte, ConseRId,PV,
			  Delay) :-
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	run_ops(PreOps),
	free_variables(Atom, FV),
	(	find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, []),
		reverse(AtomTrace, AT),
		handle_isolated(AT, mininf, Tstart, Tholds, Atom, PN, ToDoAnte, ConseRId,PV,
				Delay),
		fail
	;	do_default_cwa_isolated_no_trace(Atom,PN,Tstart,PostOps,PostConds,
			ToDoAnte, Tholds,ConseRId,PV,Delay)
	).
do_default_cwa_isolated_no_trace(Atom,PN,Tstart,PostOps,PostConds,
			ToDoAnte, Tholds,ConseRId,PV,Delay) :-

	(	PN = neg,
		cwa(Atom),
		(	ground(Atom)
		->	run_ops(PostOps),run_ops(PostConds),
			efgh0(Delay, _E1, _F1, G, _H1),
			ensure_handled_time(HT),
			min_new(HT, Tholds, Thi),
			ensure_setup_time(Tsetup),
			max_new(Tsetup, Tstart, Tlo),
			cmp_ge(Thi - Tlo, G),
			check_isolated_fire_restl(ToDoAnte, Tlo, Thi, ConseRId,
						  PV,Delay)
		;	efgh0(Delay, _E, _F, G, _H),
			ensure_handled_time(HT),
			min_new(HT, Tholds, Thi),
			ensure_setup_time(Tsetup),
			max_new(Tsetup, Tstart, Tlo),
			cmp_ge(Thi - Tlo, G),
			run_ops(PostOps),run_ops(PostConds),
			\+ find_atom_trace(Atom, _AtomTrace),
			check_isolated_fire_restl(ToDoAnte, Tlo, Thi, ConseRId,
						  PV,Delay),
			fail
		;	true
		)
	->	true
	;	true
	).

handle_isolated([], LastTHi, Tstart, Tholds, Atom, PN, ToDoAnte, ConseRId,PV,
					Delay) :-
	PN = neg,
	cwa(Atom),
	cmp_lt(LastTHi, Tholds),
	efgh0(Delay, _E, _F, G, _H),
	ensure_handled_time(HT),
	min_new(HT, Tholds, Thi),
	max_new(Tstart, LastTHi, Tlo),
	cmp_gt(Thi, Tlo),
	cmp_ge(Thi - Tlo, G),
	check_isolated_fire_restl(ToDoAnte, Tlo, Thi, ConseRId,PV,Delay).

handle_isolated([range(Tlo1, Thi1, TFUB1)|AT], LastTHi, Tstart, Tholds, Atom,
		PN, ToDoAnte, ConseRId,PV,Delay) :-
	(cmp_le(Thi1, Tstart)
	->	handle_isolated(AT, Thi1,  Tstart, Tholds, Atom, PN, ToDoAnte,
				ConseRId,PV,Delay)
	;	(	cmp_lt(Tstart, Tlo1),
			PN = neg,
			cwa(Atom),
			max_new(LastTHi, Tstart, TIn),
			missing_range1(TIn, Tlo1,false, Range),
			Range = range(Tlo2, Thi2, TFUB1),
			check_isolated_fire_restl(ToDoAnte, Tlo2, Thi2,
						  ConseRId,PV,Delay)
		;	pntf(PN, TFUB1),
			max_new(Tlo1, Tstart, Tlo2),
			min_new(Thi1, Tholds, Thi2),
			cmp_lt(Tlo2, Thi2),
			check_isolated_fire_restl(ToDoAnte, Tlo2, Thi2,
						  ConseRId,PV,Delay)
		;	cmp_lt(Thi1, Tholds),
			handle_isolated(AT, Thi1, Tstart, Tholds, Atom, PN,
					ToDoAnte, ConseRId,PV,Delay)
		)
	).
check_isolated_fire_restl(ToDoAnte, Tlo2, Thi2, ConseRId,PV,Delay) :-
	efgh0(Delay, _E, _F, G, _H),
	cmp_ge(Thi2 - Tlo2, G),
	check_isolated_fire_restl1(ToDoAnte, Tlo2, Thi2, ConseRId,PV,Delay).

check_isolated_fire_restl1([], Tlo, Thi, ConseRId,_PV,Delay) :-
	efgh0(Delay, _E, _F, G, H),
	assert_debug(cmp_ge(Thi - Tlo, G)),
	lambda_delay(Delay, D),
	T3 is Tlo + G + D,
				% T4 is T3 + THolds - TMin - G + H,
				% T4 is TMin + G + D + THolds -TMin - G + H
				% T4 is THolds + D + H
	T4 is Thi + D + H,
	tr_consep(ConseRId, ConseRId1),
	(model_checking
	->	ensure_handled_time(HT),
		assert_debug(cmp_ge(HT,Thi))
	;	true
	),
	schedule_fire(ConseRId1, T3, T4).


check_isolated_fire_restl1([LitData|ToDoAnte], Tlo, Thi, ConseRId,PV,Delay) :-
	check_isolated_fire_rest1(Tlo, Thi, LitData, ToDoAnte, ConseRId,PV,
				  Delay).

chknotandyes(AnteHolds, Atom, PN, Id1, IdTerm1, AnteDone, Where) :-
	chknotandyes(AnteHolds, Atom, PN, Id1,IdTerm1,AnteDone, Where, _OLits).

chknotandyes(AnteHolds, Atom, PN, Id1, IdTerm1, AnteDone, Where, OLits) :-
	(	reverse(AnteDone, AnteDone1),
		append(AnteDone1, [ds_lh(lit(Atom,PN),Id1,IdTerm1)|AnteHolds],
		       OLits),
		chk_not_invf(OLits)
	->	true
	;	impl_error('order in update_retrace1~w', [Where])
	).
lt.

local_impl_error(Format, Args) :-
	(lt
	->	impl_error(Format, Args)
	;	warning(Format, Args)
	).
lassert_debug(P) :-
	(lt
	->	assert_debug(P)
	;	true
	).



/* TOO RISKY BUT....
   : We postpone all rm_gc_wait_vars activities till the end of
     update_activity_times.
   Do it optional
   */
:- dynamic dyn_rm_gc_wait_vars/8.

postpone_rm_gc :- fail.
postponed_rm_gc :-
	(postpone_rm_gc
	->	(retract(dyn_rm_gc_wait_vars(Atom,PN,Id1,IdTerm1,AnteHolds,
					    AnteHoldsDone,ToDoAnteL,
					     Removed)),
			rm_gc_wait_vars1(Atom,PN,Id1,IdTerm1,AnteHolds,AnteHoldsDone,
					 ToDoAnteL,Removed),
			fail
		;	true
		)
	;	true
	).


rm_gc_wait_vars_th(Atom,PN,Id1, IdTerm1, AnteHolds,AHDone,ToDoAnte, Id,
		   IdTerm,Removed) :-
	(Removed == initial
	->	true
	;	assert_debug(Removed = wait_var(Id,IdTerm,_HT,_TMin,_FV, _FVL,
						_LitData,_ToDoAnte,
						_AnteHolds,_THolds,
						_ConseRId,_PV,
						_Delay)),
		retractall(dyn_handled_wait_var_instance(Id,IdTerm,_FV2,_))
	),
	rm_gc_wait_vars(Atom,PN,Id1,IdTerm1,AnteHolds,AHDone,ToDoAnte,
		Removed).


/*
  lit(Atom,PN) is part of previously true LitHolds, but is not true long enough.
  We need to remove the excluded instance entry in its predecessor.
  We also need to remove the whole entries of all literals following, i.e. in
  AnteHolds.
  Id,FV,FVL is probably part of LitData current wait_var entry.
  First remove Atom,PN from wait_var

  AnteHolds: Instantiated literals after the current one that are true in a previous
  range but that have not been checked w.r.t. truthness up to THandled:
        We should remove all pending activities depending on them.
  AnteHoldsDone is  instantiated literals before the current one: They have been
        dealt with, i.e. they are true up to THandeled at least.
  ToDoAnteL is list of (uninstantiated) literals that are waiting to be handled.

  So: AnteHoldsDone, lit(Atom, PN), AnteHolds, ToDoAnteL represent the whole
  antecedent of the rule.
  Id1 is the Id of the wait_var entry where lit(Atom, PN) is present as an
  FV entry in FVL.


  AnteHoldsDoneInv,lit(Atom,PN),AnteHolds,ToDoAnteL <-> AnteHolds1,LitData,ToDoAnte
  AnteHoldsDoneInv == AnteHolds1
  AnteHolds,ToDoAnteL <-> ToDoAnte

  So lit(Atom, PN) should be present in wait_var Id1 as an FV in FVL

  Now we must invalidate this entry in wait_var + all dependant wait activities,
  i.e. wait_fired, wait_true and other wait_var entries.

  First remove entry from wait_var.
  Then:
  If AnteHolds, ToDoAnteL == [] (equivalent: ToDoAnte == [])
  we need to remove the wait_fired or wait_true entry corresponding to
  AnteHoldsDoneInv,lit(Atom,PN)

  Otherwise we need to remove all wait_var entries corresponding to:
        ToDoAnte with LitData instantiated to lit(Atom,PN)
  So remove the wait_var entries that have AnteHoldsN = [ds_lh(Atom,PN,Id1)|AnteHolds1]
  with all their dependents. That wait_var entry has atom_to_term(IdN, k(Id1,Atom),[])
  */
rm_gc_wait_vars(Atom,PN,Id1,IdTerm1,AnteHolds,AnteHoldsDone,ToDoAnteL,
		Removed) :-
	(postpone_rm_gc
	->	assertz(dyn_rm_gc_wait_vars(Atom,PN,Id1,IdTerm1,AnteHolds,
					    AnteHoldsDone,ToDoAnteL,
		Removed))
	;	rm_gc_wait_vars1(Atom,PN,Id1,IdTerm1,AnteHolds,AnteHoldsDone,
				 ToDoAnteL,Removed)
	).



rm_gc_wait_vars1(Atom,PN,Id1,IdTerm1,AnteHolds,AnteHoldsDone,ToDoAnteL,
		Removed):-
	(do_log(rgc)
	->	flag(rgc,RGC,RGC+1),
		alog(rgc,'~w:rm_gc_wait_vars~n',[RGC])
	;	true
	),
	chknotandyes(AnteHolds, Atom, PN,Id1,IdTerm1,AnteHoldsDone,
		     rm_gc_wait_vars,_OLits),
	(rm_wait_var(T, HT1,TMin,FV,FVL,LitData,ToDoAnte,AnteHolds1,
				      THolds,ConseRId,PV,Delay,Id1,IdTerm1)
	->	%reverse(AnteHoldsDone, AnteHoldsDoneRev),
		assert_debug(AnteHoldsDone == AnteHolds1,aap1),
		assert_debug(\+ \+append(_X, ToDoAnteL, ToDoAnte)),
		LitData = ds_litd(Atom1, PN1, _PreOps, _PostOps, _PostConds),
		copy_term((Atom1, FV), (Atom2,FV2)),
		assert_debug(ground(FVL)),
		(	Atom = Atom2,
			select(FV2, FVL, FVL1)
		->	set_wait_var(T, HT1, TMin,FV, FVL1, LitData,ToDoAnte,
				     AnteHolds1,THolds, ConseRId,PV,Delay,Id1,
				     IdTerm1),
		        mark_handled(T, Id1, IdTerm1, FV2, Atom2, PN1, Removed)
		;	local_impl_error('Missing entry in wait_var:~w id:~w',
					 [FVL,Id1,IdTerm1])
		)
	;	local_impl_error('missing wait_var',[])
	),
	% NOW instantiate todo part with current Atom
	LitData = ds_litd(Atom, _PN, PreOps, PostOps, PostConds),
				% specialise ToDoAnte
	assert_debug(ground(PreOps)), % should have been done before
	run_ops(PostOps),
	(run_ops(PostConds)
	->	true
	;	impl_error('Post Ops mismatch')
	),
	rm_gc_lit_data_deps_new(Atom, PN, Id1, IdTerm1, AnteHoldsDone,ToDoAnte,
				Removed),!.
rm_gc_wait_vars1(Atom,PN,Id1, IdTerm1, AnteHolds, AnteHoldsDone, ToDoAnteL,
		Removed) :-
	fatal_fail(rm_gc_wait_vars1(Atom,PN,Id1, IdTerm1, AnteHolds,
				   AnteHoldsDone,ToDoAnteL, Removed)).

mark_handled(T, Id1, IdTerm1, FV2, Atom2, PN2, Removed) :-
	functor(Removed, F, _),
	(F == wait_var
	->	true
	;	memberchk(F, [wait_fired, wait_true])
	->	(   do_check_handled,
		    ensure_handled_time(HT),
		    cmp_lt(T,HT)
		->	assertz(dyn_handled_wait_var_instance(Id1,IdTerm1,FV2,PN2)),
			(	is_local
			->	test_mark_handled(T, Id1, IdTerm1, FV2, Atom2,
						  PN2,Removed)

			;	true
			)
		;	true
		)
	;	impl_error('Wrong functor',[])
	).
test_mark_handled(_T, Id1, IdTerm1, FV2, Atom2, PN2, Removed) :-
	flag(dbb, DBB, DBB + 1),
	format('DBB:~w     ~q~n~q:~q~nRemoved:~q~n',[DBB,IdTerm1, FV2, Atom2,
						     Removed]),
	assert_debug(ground(Id1)),
	((recwait
	 ->	 recorded(Id1, dyn_wait_var(Id1,IdTerm1,HT1,TMin,FV,FVL,LitData,ToDoAnte,
				AnteHolds1,
				THolds,ConseRId,PV,Delay,HTN))
	 ;	 dyn_wait_var(Id1,IdTerm1,HT1,TMin,FV,FVL,LitData,ToDoAnte,
				AnteHolds1,
				THolds,ConseRId,PV,Delay,HTN)
	 )
	->	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
		assert_debug(PN == PN2),
		assert_debug(ground(PreOps)),
		findall(ds_at(Atom, FV, AtomTrace),
		       find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, FVL),
		       Traces),
		(	memberchk(ds_at(Atom2, _FV2, _AtomTracea), Traces)
		->	true
		;	\+ ( \+ (Atom2 = Atom)),
			PN = neg,
			cwa(Atom2)
		->	true
		;	ensure_bu_atom(Atom2,Id1,IdTerm1,_FV2_)
		)
	;	impl_error(test_mark_handled)
	).

% Remove anything depending on success lit(Atom, PN),Id1 from wait list
% We have
% WHOLE antecedentAnteHoldsDoneInv, lit(Atom, PN), ToDoAnte is whole antecedent
% If ToDoAnte == [] remove wait_fired or wait_true
rm_gc_lit_data_deps_new(Atom, PN, Id1, IdTerm1, AnteHolds,[],Removed) :-
	!,
	AnteHolds1 = [ds_lh(lit(Atom,PN), Id1, IdTerm1)|AnteHolds],
	rm_gc_wait_entry(AnteHolds1,Removed).
rm_gc_lit_data_deps_new(Atom, PN, Id1, IdTerm1, AnteHoldsDone,ToDoAnte,
			Removed) :-
	IdTerm2 = k(IdTerm1,Atom),
	my_term_to_atom(IdTerm2,Id2),
	rm_gc_lit_data_deps_new1(Id2,IdTerm2,
				 [ds_lh(lit(Atom,PN),Id1,IdTerm1)|
				 AnteHoldsDone],ToDoAnte,Removed).
% rm_gc_lit_data_deps_new1(Id2,AnteHoldsInv,ToDoAnte,Removed)
% inv(AnteHoldsInv), ToDoAnte is whole antecedent
% Id2 is Id following last element
% of inv(AnteHoldsInv)
rm_gc_lit_data_deps_new1(Id2,IdTerm2,AnteHoldsInv,ToDoAnte,Removed) :-
	chk_inv(AnteHoldsInv),
	(rm_wait_var(_T, _HT1,_TMin,FV,FVL,LitData1,ToDoAnte1,
		     AnteHolds,
		     _THolds,_ConseRId,_PV,_Delay,Id2,IdTerm2)
	->	retractall(dyn_handled_wait_var_instance(Id2,IdTerm2,_FV2,_))
	;	Removed = wait_var(Id2,IdTerm2,_HT1,_TMin,FV,FVL,LitData1,
				   ToDoAnte1,AnteHolds,_THolds,_ConseRId,_PV,
				   _Delay)
	->	true
	;	impl_error('Missing wait_var ID:~w',[Id2])
	),
				% wait_var entry has preop done?
	(	true
	->	ToDoAnte = [LitData|_],
		LitData = ds_litd(_Atom, _PN, PreOps, _PostOps, _PostConds),
		(	PreOps == []
		->	true
		;	assert_debug(\+ ground(PreOps), 'preops called?'),
			run_ops(PreOps)
		)
	;	true
	),
	assert_debug(AnteHoldsInv == AnteHolds),
	rm_all_gc_wait_var_deps_new(LitData1, FV, FVL, Id2, IdTerm2,
				    AnteHoldsInv,ToDoAnte1,Removed).
rm_all_gc_wait_var_deps_new(LitData, FV, FVL, Id2, IdTerm2,AnteHoldsInv,
			    ToDoAnte,Removed) :-
	LitData = ds_litd(Atom, PN, _PreOps, PostOps, PostConds),
	(	member(FV, FVL),
		run_ops(PostOps),
		(	run_ops(PostConds)
		->	true
		;	impl_error('Post Ops mismatch')
		),
		rm_gc_lit_data_deps_new(Atom, PN, Id2, IdTerm2,AnteHoldsInv,
					ToDoAnte,Removed),
		fail
	;	true
	).

rm_gc_wait_entry(AnteHolds1,Removed) :-
	chk_inv(AnteHolds1),
	(	(Activity = wait_fired(_THolds, _THandled, AnteHolds1,
				       _ConseRId1, _Delay)
		;	Activity = wait_true(_TMin, _THolds, AnteHolds1,
					     _ConseRId1, _Delay)
		),
		(get_wait1(_HT, Activity, Ref)
		->	erase(Ref)
		;	Activity = Removed
		->	true
		)
	->	true
	;	impl_error('Sloppy gc, missing wait_fired-wait_true',[])
	).




/* We know last atom failed after Thi, so only isolated firing */
check_fire_isolated(TIn, Thi, ToDoAnte, ConseRId,Delay) :-
	efgh0(Delay, _E, _F, G, _H),
	(cmp_ge(Thi - TIn, G)
	->	isolated_fire_all(ToDoAnte, TIn, Thi, ConseRId, Delay)
	;	true
	).
isolated_fire_all([], Tlo, Thi,ConseRId,Delay) :-
	efgh0(Delay, _E, _F, G, H),
	(cmp_ge(Thi - Tlo, G)
	->	lambda_delay(Delay, D),
		T3 is Tlo + G + D,
		T4 is Thi + D + H,
		tr_consep(ConseRId, ConseRId1),
		(model_checking
		->	ensure_handled_time(HT),
			assert_debug(cmp_ge(HT, Thi))
		;	true
		),
		schedule_fire(ConseRId1, T3, T4)
	;	true
	).

isolated_fire_all([LitData|ToDoAnte], Tlo, Thi, ConseRId,Delay) :-
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	run_ops(PreOps),
	default_value(Atom, DefVal),
	pntf(PN, TF),
	efgh0(Delay, _E, _F, G, _H),
	(	find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds),
		matching_range(AtomTrace, maxinf, Tlo, Thi, TF, DefVal, G,
			       Tlo1,Thi1),
		isolated_fire_all(ToDoAnte, Tlo1, Thi1, ConseRId,Delay),
		fail
	;	cwa(Atom),
		PN = neg,
		ensure_setup_time(Tsetup),
		ensure_handled_time(HT),
		(	cmp_ge(Tlo, Tsetup),
			cmp_le(Thi, HT)
		->	true
		;	impl_error('Unexpected isolated_fire_all')
		),
		(ground(Atom)
		->	(find_atom_trace(Atom, _)
			->	true
			;	run_ops(PostOps),run_ops(PostConds),
				isolated_fire_all(ToDoAnte, Tlo, Thi,
						  ConseRId,Delay)
			->	true
			;	true
			)
		;	run_ops(PostOps),run_ops(PostConds),
			\+ find_atom_trace(Atom, _),
			isolated_fire_all(ToDoAnte, Tlo, Thi,
						  ConseRId,Delay),
			fail
		;	true
		)
	).
/*
isolated_fire_all_old([LitData|ToDoAnte], Tlo, Thi, ConseRId,Delay) :-
	LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	run_ops(PreOps),
	default_value(Atom, DefVal),
	pntf(PN, TF),
	efgh0(Delay, _E, _F, G, _H),
	(	find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds),
		matching_range(AtomTrace, maxinf, Tlo, Thi, TF, DefVal, G,
			       Tlo1,Thi1),
		isolated_fire_all(ToDoAnte, Tlo1, Thi1, ConseRId,Delay),
		fail
	;	cwa(Atom),
		PN = neg,
		(ground(Atom)
		->	true
		;	impl_error(['UNIMPLEMENTED:',
				    'cannot handle cwa ',
				    'on atom containing vars:~w'],Atom)
		),
		ensure_setup_time(Tsetup),
		ensure_handled_time(HT),
		(	cmp_ge(Tlo, Tsetup),
			cmp_le(Thi, HT)
		->	true
		;	impl_error('Unexpected isolated_fire_all')
		),
		(find_atom_trace(Atom, _AtomTrace)
		->	true
		;	run_ops(PostOps),run_ops(PostConds),
			isolated_fire_all(ToDoAnte, Tlo, Thi, ConseRId,Delay)
		)
	).
*/
% :- dynamic dyn_warn_bug1/0.

matching_range([], _LastLo, Tlo, Thi, TF, CWAVal, G, Tlo1, Thi1) :-
	CWAVal = TF,
	missing_range1(Tlo, Thi,CWAVal, range(Tlo1, Thi1,FUB)),
	cmp_ge(Thi1 - Tlo1, G),
	assert_debug(FUB == TF).

/*
  Ranges are ordered descending time.
  LastLo is previous lower bound

  ------------------|---------
                  LastLo

  */
matching_range([range(Tlo2,Thi2,TFU2)|AtomTrace], LastLo, Tlo, Thi, TF, CWAVal,
	       G, Tlo1, Thi1) :-
	assert_debug(cmp_ge(LastLo, Thi2)),
/*	(cmp_gt(Tlo, LastLo)
	->	true
	;	(dyn_warn_bug1
		->	true
		;	warning('Solved bug, rule may not have fired previously'),
			assertz(dyn_warn_bug1)
		)
	),
	*/
	(cmp_ge(Tlo2, Thi) % no overlap yet -> back in time
	->	matching_range(AtomTrace, Tlo2,Tlo,Thi,TF, CWAVal,G,Tlo1,Thi1)
	;cmp_ge(Tlo, Thi2)
	->	assert_debug(cmp_le(Tlo, LastLo)),
		CWAVal = TF,
		min_new(LastLo, Thi, Thi3),
		assert_debug(TFU2 \== TF),
		max_new(Tlo2, Tlo, Tlo3),
		(	Thi1 = Thi3,
			Tlo1 = Tlo3
		;	cmp_ge(Tlo2,Tlo + G),
			warning('Fishy code'),
			matching_range(AtomTrace, Tlo2, Tlo, Thi, TF, CWAVal,
				       G, Tlo1, Thi1)
		)
	;TFU2 == TF
	->	min_new(Thi2, Thi, Thi3),
		max_new(Tlo2, Tlo, Tlo3),
		(	Thi1 = Thi3,
			Tlo1 = Tlo3
		;	cmp_ge(Tlo2,Tlo + G),
			matching_range(AtomTrace, Tlo2, Tlo, Thi, TF, CWAVal,
				       G, Tlo1, Thi1)
		)
	;	matching_range(AtomTrace, Tlo2, Tlo, Thi, TF, CWAVal,
			       G, Tlo1, Thi1)
	).


no_overlap_default_match :-
	impl_error('Something unimplemented, email lourens@cs.vu.nl').





/* There must be some range starting at Tlo till THi1
   with some value. We know there is no trace value in the range
   */
missing_range1(Tlo, Thi,DefFU, range(Tlo, Thi1,FUB)) :-
	ensure_handled_time(HT),
	ensure_setup_time(TS),
	(cmp_ge(Tlo, HT)
	->	FUB = blank,
		Thi1 = Thi
	;cmp_ge(Tlo, TS)
	->	min_new(Thi, HT, Thi1),
		FUB = DefFU
	;	min_new(Thi, TS, Thi1),
		FUB = unknown
	).

add_default_cwa(Atom, PN, TMin, PostOps, PostConds, FV, FVLIn, FVLOut,
		ToDoAnte, AnteHolds,THolds,ConseRId,PV,Delay, Id,IdTerm,
		Removed) :-
	(	PN = neg,
		cwa(Atom)
	->	findall(FV, instantiate_op(Atom, PN, TMin,
					  PostOps, PostConds, FV, FVLIn,
					  ToDoAnte, AnteHolds,
					  THolds,ConseRId,PV,Delay, Id,
					  IdTerm, Removed),
		      FVLoc1),
		(   FVLoc1 \= []
		->  find_set(FVLoc1, FVLLoc),
		    append(FVLIn, FVLLoc, FVLOut),
		    assert_debug(uchecklist(var, FV), 'uninst vars')
		;   FVLOut = FVLIn
		)
	;	FVLOut = FVLIn
	), !.
add_default_cwa(Atom, PN, TMin, PostOps, PostConds, FV, FVLIn, FVLOut,
		ToDoAnte, AnteHolds,THolds,ConseRId,PV,Delay, Id,IdTerm,
		Removed) :-
	format('CWA failed~n'),
	trace,
	add_default_cwa(Atom, PN, TMin, PostOps, PostConds, FV, FVLIn, FVLOut,
		ToDoAnte, AnteHolds,THolds,ConseRId,PV,Delay, Id,IdTerm,
		Removed).
instantiate_op(_Atom, _PN, _TMin, _PostOps, _PostConds, _FV, FVLIn,
	       _ToDoAnte, _AnteHolds,_THolds,_ConseRId,_PV,_Delay, _Id,
	       _IdTerm, _Removed) :-
	FVLIn == [[]],
	!,
	fail.


instantiate_op(Atom, PN, TMin, PostOps, PostConds, FV, FVLIn,
	       ToDoAnte, AnteHolds,THolds,ConseRId,PV,Delay, Id, IdTerm,
	       Removed) :-
	ensure_handled_time(HT),
	assert_debug(cmp_ge(THolds, HT)),
	cmp_gt(HT, TMin),
	copy_term(Removed, Removed1),
	run_ops(PostOps),
	run_ops(PostConds),
	\+ memberchk(FV, FVLIn),
	\+ find_atom_trace(Atom, _AtomTrace),
	(fail
	->	THi = THolds
	;	THi = HT
	),
	assert_debug(cmp_lt(TMin, THi)),
	(retract(dyn_handled_wait_var_instance(Id,IdTerm,FV,_))
	->	true
	;	setup_lt_normed(ToDoAnte,
			[ds_lh(lit(Atom,PN),Id,IdTerm)|
			AnteHolds], TMin, THi,
			ConseRId, PV, Delay, Removed1)
	).



/*
var_inst_ds_or_new_ds_o2([], [], []).
var_inst_ds_or_new_ds_o2([ds_or(Atom, FV, O2)|Rs], [FV|FVL],
			 [ds_o2(Atom,O2)|ROs]) :-
	var_inst_ds_or_new_ds_o2(Rs, FVL, ROs).
*/
setup_lt_notground1default(FV, FVL, LitData, ToDoAnte, AnteHolds,
			   TMin, THolds,ConseRId, PV, Delay, Id, IdTerm) :-
	chk_inv(AnteHolds),
	setup_lt_wait_var(FV, FVL, LitData, ToDoAnte, AnteHolds,
			  TMin, THolds, ConseRId, PV, Delay, Id, IdTerm).








overlapping_ground_range_po(Atom, PN, FV, FVL, TMin, PostOps, PostConds, O2) :-
	find_min_range(Atom, PN, FV, FVL, TMin, O2),
	select_po_o2(O2),
	run_ops(PostOps),
	run_ops(PostConds).

select_po_o2(true(_,THi,_)) :-
	ensure_handled_time(HT),
	assert_debug(cmp_ge(THi, HT)).




find_min_range_ground(Atom, PN, TMin, O2) :-
	assert_debug(ground(Atom)),
	(find_min_range(Atom, PN, TMin, O2)
	->	true
	;	overlapping_range_rest1([], Atom, TMin, maxinf, Range, [],
					Ranges),
		Range = range(Tlo, Thi, _),
		assert_debug(cmp_le(TMin,Thi)),
		assert_debug(cmp_lt(Tlo,TMin)),
		default_value(Atom, CWAVal),
		tr_o1_o2(Range,Ranges,PN,Atom,CWAVal,TMin, O2)
	).





:- dynamic dyn_wait1/2.

rm_wait_var(T,HT1,TMin,FV,FVL,LitData,ToDoAnte,AnteHolds1, THolds,ConseRId,
	    PV,Delay,Id, IdTerm) :-
	assert_debug(ground(IdTerm)),
	(do_log(rmv)
	->	flag(rmv, I, I + 1),
		alog(rmv, 'rm_wait_var ~w  ~w ', [I,Id])
	;	true
	),
	(retract_wait1(T,
		       wait_var(Id, IdTerm, HT1,TMin,FV,FVL,LitData,ToDoAnte,
				AnteHolds1,
				THolds,ConseRId,PV,Delay))
	->	true
	;	fail
	),
	alog(rmv, '(~w)~n', [FVL]).


set_wait_var(T, HT, TMin,FV, FVL, LitData,ToDoAnte,
	     AnteHolds,THolds, ConseRId,PV,Delay,Id, IdTerm) :-
	(do_log(addv)
	->	flag(addv, I, I + 1),
		alog(addv, 'set_wait_var ~w  ~w (~w)~n', [I,Id,FVL])
	;	true
	),
	assert_debug(uchecklist(var, FV)),
	assert_debug(ground(AnteHolds)),
	chk_inv(AnteHolds),
	set_wait1_var(T, Id,IdTerm,HT,TMin,FV, FVL, LitData,ToDoAnte,AnteHolds,
		      THolds, ConseRId,PV,Delay).
setup_lt_wait_var(FV, FVL,LitData,ToDoAnte,AnteHolds,TMin,THolds,ConseRId,
		  PV,Delay,Id,IdTerm):-
	/*
	  Need to know time to hang atom on. Should be >= handled_time
	  Simplicity: put it on handled time,
	  Otherwise: what time would first candidate possibly change from
	  unknown to blank?
	  */
	ensure_handled_time(HT),
	set_wait_var(HT, HT, TMin,FV, FVL, LitData,ToDoAnte,AnteHolds,THolds,
		     ConseRId,PV,Delay,Id,IdTerm).


setup_lt_wait_fired(THolds, THandled, AnteHolds, ConseRId, Delay, Removed) :-
	chk_inv(AnteHolds),
	(is_local
	->      chk_wait_var_has_ref(AnteHolds,Removed)
	;       true
	),
	set_wait1(THolds,wait_fired(THolds, THandled, AnteHolds, ConseRId,
				    Delay)).

chk_removed(Removed) :-
	(	is_local,
		Removed = wait_var(_Id,_IdTerm,_HT1,_TMin,FV, _FVL, _LitData,
				   _ToDoAnte,
				   _AnteHolds1,_THolds, _ConseRId,_PV,_Delay)
	->	(uchecklist(var, FV)
		->	true
		;	assert_debug(fverror==aap)
		)
	;	true
	).

chk_wait_var_has_ref(AnteHolds,Removed) :-
	uchecklist(chk_wait_var_has_ref_entry(Removed), AnteHolds).
chk_wait_var_has_ref_entry(Removed,ds_lh(lit(Atom1,PN1),Id, IdTerm)) :-
	flag(pppp, PP, PP + 1),
	format('pppp:~w:~q   ~q~n', [PP,Atom1,IdTerm]),
	assert_debug(ground(Atom1)),
	assert_debug(ground(IdTerm)),
	(((recwait
	  ->	  recorded(Id, dyn_wait_var(Id,IdTerm,HT1,TMin,FV,FVL,LitData,ToDoAnte,
		       AnteHolds1,THolds,ConseRId,PV,Delay,HTN))
	  ;	  dyn_wait_var(Id,IdTerm,HT1,TMin,FV,FVL,LitData,ToDoAnte,
		       AnteHolds1,THolds,ConseRId,PV,Delay,HTN)
	  )
	 ;
	  Removed = wait_var(Id,IdTerm,HT1,TMin,FV, FVL, LitData,ToDoAnte,
			     AnteHolds1,THolds, ConseRId,PV,Delay)
	 )
	->  LitData = ds_litd(Atom, PN, PreOps, PostOps, PostConds),
	    assert_debug(\+ \+ Atom1 = Atom),
	    assert_debug(PN1 == PN),
	    assert_debug(uchecklist(var, FV)),
	    copy_term((Atom,FV),(Atom2,FV2)),
	    Atom2 = Atom1,
	    assert_debug(ground(PreOps)),
	    findall(ds_at(Atom, FV, AtomTrace),
		   find_atom_trace_op(Atom, AtomTrace, PostOps, PostConds,FV, FVL),
		   Traces),
	    (memberchk(ds_at(Atom2, FV2, _AtomTracea), Traces)
	    ->     true
	    ;memberchk(FV2, FVL)
	    ->     true
	    ;	    PN1 == neg,
		    cwa(Atom2)
	    ->	    true
	    ;	    ensure_bu_atom(Atom2,_Id1,_IdTerm1,FV2)
	    ),
	    format('Not missing~n')
	;Removed == initial
	->  format('Missing wait_var initial?~n')
	;   format('Missing wait_var~n')
	).
% We know AnteHolds between TMin and THolds, there are no other literals in
% antecedent. Rule could not fire as yet.
setup_lt_wait_true(TMin, THolds, AnteHolds, ConseRId, Delay) :-
	chk_inv(AnteHolds),
	set_wait1(THolds, wait_true(TMin, THolds, AnteHolds, ConseRId,Delay)).

run_ops(Ops) :-
	(do_log(run_ops)
	->	flag(run_ops, L, L + 1),
		alog(run_ops, '~w ~w', [L, Ops])
	;	true
	),
	run_ops1(Ops),
	(ground(Ops)
	->	true
	;	warning('Unstable code please send spec to lourens')
	).

run_ops1(List) :-
	reverse(List, List1),
	run_ops2(List1).

run_ops2([]).
run_ops2([Op|Ops]) :-
	catch_fatal_halt(calla(Op),runops_exeception),
	run_ops2(Ops).
calla(A = B) :-
	!,A = B.
calla(A \= B) :-
	!,
	(ground(A \= B)
	->	A \= B
	;	warning('Unstable code, warn lourens'),
		A \= B
	).
calla(check_sort_value(Sort, Kind, Elem)) :-
	ground(Kind),
	memberchk(Kind, [sortdef, real, integer]),
	!,
	(ground(Sort)
	->	true
	;	warning('Unstable code, warn lourens')
	),
	check_sort_value(Sort, Kind, Elem).
calla(X) :-
	(is_local,fail
	->	warning('Missed entry:~w', [X]),
		local_trace(rr)
	;	true
	),
	call(X).


/* We found a range matching atom and
   TIn >= Tlo
   and TIn <= Thi
   We need to find the result at time TIN:
   Interval starting at TIn if any:
   true
   blank or
   fail
   */
tr_o1_o2(range(_Tlo, Thi, _TFUB),Ranges1,PN,Atom,CWAVal, TIn, O) :-
	cmp_eq(Thi,TIn),
	!,
	(	Ranges1 = [R2|Ranges]
	->	R2 = range(Tlo1, _Thi1, _TFUB1)
	;	ensure_handled_time(HT),
		ensure_setup_time(TS),
		(cmp_ge(Thi, HT)
		->	R2 = range(Thi, maxinf, blank)
		;cmp_ge(Thi, TS)
		->	R2 = range(Thi, HT, CWAVal)
		;	R2 = range(Thi, HT, unknown)
		),
		Ranges = [],
		R2 = range(Tlo1, _, _)
	),
	% Gaat mis als R2 (ver) voorbij TIn zit.
	(	cmp_eq(Tlo1, TIn)
	->	tr_o1_o2(R2, Ranges, PN, Atom,CWAVal, TIn, O)
	;	ensure_handled_time(THandled),
		cmp_gt(THandled,Thi)
	->	% Tlo1 is begin following next nonblank interval
		% Thi is end of found range, we wish to start at Thi
		% Tnxt is min(THandled, Tlo1)
		Tnxt is min(THandled, Tlo1),
		tr_o1_o2(range(Thi, Tnxt, CWAVal), Ranges1, PN,Atom,CWAVal, TIn, O)
	;	tr_o1_o2(range(Thi, Tlo1, blank), Ranges1, PN,Atom,CWAVal, TIn, O)
	).


tr_o1_o2(range(Tlo, Thi, TFUB), Ranges, PN, Atom, CWAVal, TIn, O) :-
	assert_debug(cmp_ge(TIn,Tlo), tr_o1_o2a),
	assert_debug((cmp_lt(TIn, Thi);Ranges == [])),
	(TFUB == blank
	->	O = blank
	;pntf(PN, TFUB)
	->	propagate_fail(Ranges, Atom, CWAVal, Thi, TFUB, TEnd, Next),
		(cmp_eq(TEnd, Thi)
		->	(Next = blank
			->	Cont = blank
			;	impl_error('aap(~w,~w)', [Next,TFUB])
			)
		;	Cont = fail(TEnd, Next)
		),
		O = true(Tlo, Thi, Cont)
	;	pntf(PN, TF),
		propagate_fail(Ranges, Atom, CWAVal,Thi, TF, TEnd, Next),
		O = fail(TEnd, Next)
	).




/*

  and(pxor(F1,F2),F3) => pxor(and(F1,F3), and(F2, F3))

  and(pxor(P1, F1, F2), pxor(P2, F3, F4)) => pxor(P1, and(F1,pxor(P2, F3, F4)),and(F2, pxor(P2, F3, F4))
  => pxor(P1, pxor(P2, and(F1, F3), and(F1, F4)), pxor(P2, and(F2, F3), and(F2, F4))) =>
  pxor(P1,
  */




code_form(pxor(_P, _LHS, _RHS), _PN, DIn, DIn) :-
	fatal_error('Probabilistic exclusive or ony in positive context allowed'),!.
code_form(and(LHS, RHS), neg, DIn, DOut) :-
	code_form(or(not(LHS),not(RHS)), pos, DIn, DOut),
	!.
code_form(not(Form), PN, DIn, DOut) :-
	pnnp(PN,NP),
	code_form(Form, NP, DIn, DOut),
	!.
code_form(or(LHS, RHS), neg, DIn, DOut) :-
	code_form(and(not(LHS),not(RHS)), pos, DIn, DOut),
	!.

code_form(and(LHS, RHS), pos, DIn, DOut) :-
	code_form(LHS, pos, DIn, D1),
	code_form(RHS, pos, D1, DOut),
	!.
code_form(or(LHS,RHS), pos, DIn, DIn) :-
	fatal_error('Disjunction not defined(use pxor) (in subformula translated into ~w',
		    [or(LHS,RHS)]),!.
code_form(Term, PN, DIn, DOut) :-
	iscmpoploc(Term, NumPos, _OPos, NumNeg, _ONeg, LHS, RHS),
	DIn = ds_d([L|LIn], VIn, PVIn),
	D1In = ds_ta(VIn, PVIn, [], [], []),
	tr_arg_prolog1(LHS, PVIn, LHS1, InstL, D1In, D11),
	tr_arg_prolog1(RHS, PVIn, RHS1, InstR, D11, D1Out),
	D1Out = ds_ta(VOut, PVOut, PreOpsOut, PostOpsOut, PostCondsOut),
	(PN == pos
	->	NumFn = NumPos
	;	NumFn = NumNeg
	),
	functor(NumFn, F, A),
	assert_debug(A == 2),
	Code =.. [F, LHS1, RHS1],
	(ground(Code)
	->	warning('Cannot deal with constant conditions yet:~w', [Code])
	;	true
	),
	(	InstL == inst,
		InstR == inst
	->	true
	;	error('Can only handle comparison of instantiated terms:~w', [Term]),
		fail
	),
	L = ds_litd(true, pos, PreOpsOut, PostOpsOut, [Code|PostCondsOut]),
	DOut = ds_d(LIn, VOut, PVOut),!.
code_form(Term,_PN, _DIn, _DOut) :-
	iscmpoploc(Term, _NumCall),
	!,
	fatal_error('Coding comparison sub-formula ~w failed', [Term]).


code_form(Atom, PN, DIn, DOut) :-
	\+ reserved(Atom),
	code_atom(Atom, PN, DIn, DOut),
	!.
code_form(Form, PN, DIn, DIn) :-
	functor(Form, F, A),
	fatal_error('Cannot handle antecedent function ~w ~w', [F/A,PN]),
	!.

code_atom(Atom, PN, DIn, DOut) :-
%	DIn = ds_d(LIn, VIn, PVIn),
	DIn = ds_d([L|LIn], VIn, PVIn),
	D1In = ds_ta(VIn, PVIn, [], [], []),
	tr_arg_prolog1(Atom, PVIn, Atom1, _Inst, D1In, D1Out),
	D1Out = ds_ta(VOut, PVOut, PreOpsOut, PostOpsOut, PostCondsOut),
	L = ds_litd(Atom1, PN, PreOpsOut, PostOpsOut, PostCondsOut),
	DOut = ds_d(LIn, VOut, PVOut),!.
code_atom(Atom, PN, DIn, DOut) :-
	fatal_fail(code_atom(Atom, PN, DIn, DOut)).







propagate_fail(Ranges, Atom, CWAVal,TFailed, TF, TEnd, Next) :-
	propagate_fail(Ranges, Atom, CWAVal,TFailed, TF, TEnd, Next, _RangesLeft).

propagate_fail([], _Atom, DefaultVal, TFailed, TF, TEnd, Next, []) :-
	ensure_handled_time(THandled),
	(cmp_gt(THandled, TFailed)
	->	(	DefaultVal == TF
		->	TEnd = THandled,
			Next = blank
%			impl_error('cwa should have been performed past TH')
		;	TEnd = THandled,
			Next = blank
		)
	;	Next = blank,
		TEnd = TFailed
	).
propagate_fail([range(TNL, TNH, TFUN)|Ranges], Atom, DefaultVal, TFailed, TF, TEnd,
	       Next,RangesLeft) :-
	ensure_handled_time(THandled),
	(TNL > TFailed
	->	(cmp_ge(THandled, TNL)
		->	(DefaultVal == TF
			->	impl_error('cwa should have been performed past TH')
			;	(TFUN == TF
				->	TEnd = TNL,
					Next = true,
					RangesLeft = [range(TNL, TNH, TFUN)|Ranges]
				;	propagate_fail(Ranges, Atom, DefaultVal, TNH, TF,
						       TEnd, Next,
						       RangesLeft)
				)
			)
		;cmp_gt(THandled, TFailed)
		->	(DefaultVal == TF
			->	TEnd = THandled,
				Next = blank
				%impl_error('cwa should have been performed past TH')
			;	Next = blank,
				TEnd = THandled
			)
		;	TEnd = TFailed,
			Next = blank
		)
	;TNL < TFailed
	->	impl_error('interval ordering')
	;	TFUN == TF
	->	TEnd = TNL,
		Next = true
	;	propagate_fail(Ranges, Atom, DefaultVal, TNH, TF, TEnd, Next, RangesLeft)
	).








bi_sort_element_cond(between(I1, I2),op(NumOp,_OPos,_NumNeg,_ONeg),I,Value) :-
	!,
	(	number(I)
	->	true
	;	fatal_error('Not a number in condition')
	),
	between(I1, I2, Value),
	functor(NumOp, F, 2),
	Op =.. [F, Value, I],
	call(Op).
bi_sort_element_cond(between(I1, I2),none,Const,Value) :-
	!,
	assert_debug(Const == true),
	between(I1, I2, Value).

bi_sort_element_cond(X, _Cmp, _Y, _Value) :-
	!,
        memberchk(X, [real, integer]),
	fatal_error('Cannot start instantiating infinite sort ~w', [X]).


bi_sort_element_cond(X, Cmp, Y, Value) :-
	fatal_error('Unimplemented:~w', [bi_sort_element_cond(X, Cmp, Y, Value)]).


instantiate_var_cond(Sort, Op, ConditionTerm, Kind, Value) :-
	bi_sort(Sort, Kind),
	!,
	bi_sort_element_cond(Sort, Op, ConditionTerm, Value).
instantiate_var_cond(Sort, none, ConditionTerm, Kind, Value) :-
	!,
	assert_debug(ConditionTerm == true),
	sort_kind(Sort, Kind),
	sort_element(Sort, Value).

instantiate_var_cond(Sort, Op, ConditionTerm, Kind, Value) :-
	sort_kind(Sort, userdefined_comp),
	!,
	spec:sort_element(Sort, Kind, Op, ConditionTerm, Value).
instantiate_var_cond(Sort, Op, ConditionTerm, Kind, Value) :-
	sort_kind(Sort, Kind),
	memberchk(Kind, [integer, real, number]),
	!,
	Op = op(NumPos, _OPos, _NumNeg, _ONeg),
	NumPos =.. [Op1, _, _],
	OPos1 =.. [Op1,Value, ConditionTerm],
	sort_element(Sort, Value),
	call(OPos1).
instantiate_var_cond(Sort, Op, ConditionTerm, Kind, Value) :-
	Op = op(_NumPos, OPos, _NumNeg, _ONeg),
	OPos =.. [Op1, _, _],
	OPos1 =.. [Op1,Value, ConditionTerm],
	sort_kind(Sort, Kind),
	!,
	sort_element(Sort, Value),
	call(OPos1).
/*
sort_element(Sort, unspecified, op(_NumPos, OPos, _NumNeg, _ONeg),
	     ConditionTerm, Value) :-
	!,
	sort_element(Sort, Value),
	OPos =.. [Op, _, _],
	OPos1 =.. [Op,Value, ConditionTerm],
	call(OPos1).
*/

/*
  Used for testing whether Ante Atom has value, so sub argument
  X + 3  should be translated to Y, X is Y - 3

  Expecting L-R evaluation and grounding, so earlier Args would be insted.

  We should know what variables are instantiated before encoding
  an atom. Then, we have two sets of operations:
  PreOps and PostOps:
  PreOps perform initial evaluations.
  PostOps perform postops
  PostConditions define tests.

  So, we use PVStart: all those variables are instantiated.

  If we are dealing with a var arg, we may need to add a
  post condition, first probably always add one.
  */
tr_arg_prolog1(Arg, _PVS, _Atom1, _Inst, DIn, DIn) :-
	\+ ground(Arg),
	!,
	fatal_error('Term expected to be ground, got ~w', [Arg]).

tr_arg_prolog1(Arg, PVS, Arg1, Inst, DIn, DOut) :-
	atom(Arg),
	DIn = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VOut, PVOut, PreOpsIn, PostOpsIn, PostCondsOut),
	(var_pl_from_var_list(PVS, Arg, Sort, Kind, Arg1)
	->	!,PVOut = PVIn,
		VOut = VIn,
		Inst = inst,
		Cond = true
	;	var_pl_from_var_list(PVIn, Arg, Sort, Kind, Arg1)
	->	!,PVOut = PVIn,
		VOut = VIn,
		Inst = next,
		Cond = true
	;var_from_var_list(VIn, Arg, Sort, VOut)
	->	!,ensure_sort_kind(Sort, Kind),
		var_pl_to_var_list(Arg, Sort, Kind, PVIn, PVOut, Arg1),
		Inst = var,
		cond_from_sort(Sort, Kind, Arg1, Cond)
	;	fail
	),
	(Cond  == true
	->	PostCondsOut = PostCondsIn
	;	PostCondsOut = [Cond|PostCondsIn]
	).
tr_arg_prolog1(Arg, PVS, Arg1, Inst, DIn, DOut) :-
	spec_constant(Arg, Arg2),
	test_recursive_constant(Arg, Arg1),
	!,
	(tr_arg_prolog1(Arg2, PVS, Arg1, Inst, DIn, DOut)
	->	free_recursive_constant(Arg)
	;	free_recursive_constant(Arg),
		fail
	).
tr_arg_prolog1(Arg, _PVS, Arg, inst, DIn, DIn) :-
	atomic(Arg),
	!.

tr_arg_prolog1(Arg, PVS, Arg1, Inst,  DIn, DOut) :-
	functor(Arg, F, A),
	number_ar_op(F, A),
	!,
	Arg =.. [F|Args],
	tr_args_prolog1(Args, PVS,Args1, Insts, DIn, D1),
	D1 = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VIn, PVIn, PreOpsOut, PostOpsOut, PostCondsOut),
	Expr =.. [F|Args1],
	(uchecklist(=(inst),Insts)
	->	(ground(Expr)
		->	Arg1 is Expr,
			PreOpsOut = PreOpsIn
		;	PreOpsOut = [Arg1 is Expr|PreOpsIn]
		),
		PostOpsOut = PostOpsIn,
		PostCondsOut = PostCondsIn,
		Inst = inst
	;combine_insts_list(Insts, next)
	->	Inst = var,
		PreOpsOut = PreOpsIn,
		PostOpsOut = PostOpsIn,
		PostCondsOut = [Arg1 =:= Expr|PostCondsIn]
	;invert_op(F, A, Args1, Insts, Arg1, Inst, D1, DOut)
	->	true
	;	impl_error('Cannot invert numeric exprs yet')
	).
tr_arg_prolog1(Arg, PVS, Arg1, Inst, DIn, DOut) :-
	Arg =.. [F|Args],
	tr_args_prolog1(Args, PVS, Args1, Insts, DIn, DOut),
	Arg1 =.. [F|Args1],
	inst_from_insts(Insts, Inst).

cond_from_sort(Sort, Kind, Val, check_sort_value(Sort, Kind, Val)).

tr_args_prolog1([], _PVS, [], [], DIn, DIn).

tr_args_prolog1([Arg|Args],PVS, [Arg1|Args1], [I|Insts], DIn, DOut) :-
	tr_arg_prolog1(Arg, PVS, Arg1, I, DIn, D1),
	tr_args_prolog1(Args, PVS, Args1, Insts, D1, DOut).
inst_from_insts([], inst).
inst_from_insts([F|R], Inst) :-
	inst_from_insts(R, Inst1),
	combine_insts_list(F, Inst1, Inst).
/* Prehandled:
   inst*
   {inst|next}*

   Could generalise: one var, rest next => postcondition
   Multiple vars? also in postcondition, but then make sure that
   one occurs somewhere else...
   */
invert_op(+, 2, [Arg1,Arg2], [inst,var], Res, var, DIn, DOut) :-
	!,DIn = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VIn, PVIn, PreOpsOut, PostOpsOut, PostCondsOut),
	PostOpsOut = [Arg2 is Res - Arg1|PostOpsIn],
	PostCondsOut = PostCondsIn,
	PreOpsOut = PreOpsIn.
invert_op(+, 2, [Arg1,Arg2], [next,var], Res, var, DIn, DOut) :-
	!,DIn = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VIn, PVIn, PreOpsOut, PostOpsOut, PostCondsOut),
	PostCondsOut = [Res =:= Arg1 + Arg2|PostCondsIn],
	PostOpsOut = PostOpsIn,
	PreOpsOut = PreOpsIn.
invert_op(-, 2, [Arg1,Arg2], [inst,var], Res, var, DIn, DOut) :-
	!,DIn = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VIn, PVIn, PreOpsOut, PostOpsOut, PostCondsOut),
	PostOpsOut = [Arg2 is Res + Arg1|PostOpsIn],
	PostCondsOut = PostCondsIn,
	PreOpsOut = PreOpsIn.
invert_op(-, 2, [Arg1,Arg2], [next,var], Res, var, DIn, DOut) :-
	!,DIn = ds_ta(VIn, PVIn, PreOpsIn, PostOpsIn, PostCondsIn),
	DOut = ds_ta(VIn, PVIn, PreOpsOut, PostOpsOut, PostCondsOut),
	PostCondsOut = [Res =:= Arg1 - Arg2|PostCondsIn],
	PostOpsOut = PostOpsIn,
	PreOpsOut = PreOpsIn.

/*
  inst: all subterms are inst
  next: all subterms are either inst or next
  var:  term = var
  mixed: var with some o
  */
combine_insts_list(inst, inst, inst) :-
	!.
combine_insts_list(next, inst, next) :-
	!.
combine_insts_list(inst, next, next) :-
	!.
combine_insts_list(next, next, next) :-
	!.
combine_insts_list(_, _, mixed) :-
	!.
combine_insts_list([], inst) :-
	!.
combine_insts_list([Inst], Inst) :-
	!.
combine_insts_list([Inst1|Insts], Inst) :-
	combine_insts_list(Insts, Inst2),
	combine_insts_list(Inst1, Inst2, Inst).

/*
  If Res = not_ground, some quantor range variable cannot be instantiated
  and then we add a variable to Vars. We know we may not use this
  apart from is_true checks etc.
  */
instantiate_ranges([], VarsIn, VarsIn, InstVarsIn, InstVarsIn, ground).

instantiate_ranges([Range|Ranges], VarsIn, VarsOut, InstVarsIn, InstVarsOut,
		   Res) :-
	instantiate_range(Range, VarsIn, Vars1, InstVarsIn, InstVars1, Res1),
	instantiate_ranges(Ranges, Vars1, VarsOut, InstVars1,InstVarsOut,Res2),
	combine_substitute_inst_res(Res1, Res2, Res).

instantiate_range(Range, VIn, VOut, InstVarsIn, InstVarsOut, Res) :-
	range_var_condition(Range, VarName, Sort, Op, ConditionTerm),
	substitute_inst_vars(Sort, InstVarsIn, VIn, Sort1, Res1),
	(Res1 == error
	->	Res = error
	;	Res1 == ground,
		substitute_inst_vars(ConditionTerm,InstVarsIn, VIn, CT1, Res2),
		(Res2 == error
		->	Res = error
		;Res2 == ground
		->	instantiate_var_cond(Sort1, Op, CT1, Kind, Value),
			var_inst_to_var_list(VarName, Value, Sort1, Kind, InstVarsIn,
				     InstVarsOut),
			Res = ground
		;	Res = not_ground,
			var_to_var_list(VarName, Sort1, VIn, VOut)
		)
	).



/* result should be or(T1,T2, T3, ..)
   Ti should be exists(.., exists(.., Tpi)
   Tpi should be forall(..,
   First do not allow quantors, make conjunctive normal form
   */
tr_disj(not(T1), not(T2)) :-
	tr_disj(T1, T2).
tr_disj(not(not(T1)), T1).
tr_disj(not(and(T1, T2)), or(not(T1), not(T2))).
tr_disj(not(or(T1, T2)), and(not(T1),not(T2))).
tr_disj(and(T1, T2), and(T1n, T2)) :-
	tr_disj(T1, T1n).
tr_disj(and(T1, T2), and(T1, T2n)) :-
	tr_disj(T2, T2n).
tr_disj(and(and(T1, T2), T3), and(T1, and(T2, T3))).

tr_disj(and(T1, or(T2, T3)), or(and(T1, T2), and(T1, T3))).
tr_disj(and(or(T1, T2),T3), or(and(T1, T3), and(T2, T3))).
tr_disj(or(T1, T2), or(T1n, T2)) :-
	tr_disj(T1, T1n).
tr_disj(or(T1, T2), or(T1, T2n)) :-
	tr_disj(T2, T2n).
tr_disj(or(or(T1,T2), T3), or(T1, or(T2, T3))).

make_disj_normal(T1, _TN) :-
	functor(T1, F, _A),
	memberchk(F, [exists, forall]),
	!,
	fatal_error('No quantors allowed in make_disj_normal').

make_disj_normal(T1, TN) :-
	tr_disj(T1, T2),
	!,
	make_disj_normal(T2, TN).
make_disj_normal(TN, TN).

tr_dis_normal(or(T1, T2), [T1n|T2n]) :-
	!,tr_conj_lits(T1, T1n),
	tr_dis_normal(T2, T2n).
tr_dis_normal(T1, [T1n]) :-
	tr_conj_lits(T1, T1n),!.
tr_dis_normal(T1, _) :-
	fatal_fail(tr_dis_normal(T1, _)).


tr_conj_lits(and(T1, T2), [T1|T1n]) :-
	!,
	test_lit(T1),
	tr_conj_lits(T2, T1n).
tr_conj_lits(T1, [T1]) :-
	test_lit(T1).

test_lit(T1) :-
	(	T1 = not(A)
	->	true
	;	A = T1
	),
	(reserved0(A)
	->	impl_error('failed to normalise term')
	;	true
	).

test_dis_normal :-
	T1 = and(or(a,not(b)), or(or(p,q), r)),
	make_disj_normal(T1, TA),
	tr_dis_normal(TA, TB),
	write(TB),nl,
	repeat,
	format('Enter term:'),
	read(Term),
	(Term == end_of_file
	->	!
	;	make_disj_normal(Term, T0),
		tr_dis_normal(T0, T2),
		format('~n~w -> ~w~n', [Term, T2]),
		fail
	).


var_pl_from_var_list([pv(Arga, Sorta,Kinda,Arg1a)|PVIn],Arg,Sort,Kind,Arg1) :-
	(Arg = Arga
	->	Sorta = Sort,
		Kinda = Kind,
		Arg1a = Arg1
	;	var_pl_from_var_list(PVIn, Arg,Sort,Kind,Arg1)
	).


var_pl_to_var_list(Arg, Sort, Kind, PVIn,[pv(Arg, Sort, Kind,Arg1)|PVIn],Arg1).




is_false(true, _, _) :-
	!,
	fail.
is_false(false, _, _) :-
	!.
is_false(and(T1, T2), Vars, Status) :-
	!,
	(is_false(T1, Vars, Status)
	->	true
	;	is_false(T2, Vars, Status)
	).
is_false(or(T1, T2), Vars, Status) :-
	!,
	is_false(T1, Vars, Status),
	(var(Status)
	->	is_false(T2, Vars, Status)
	;	true
	).

is_false(Exists, Vars, Status) :-
	quantor(Exists, Ranges, ExForAll, Term),
	is_falseq(ExForAll, Ranges, Term, Vars, Status).



is_falseq(exists, Ranges, Term, Vars, Status) :-
	!,vars_from_ranges(Ranges, Vars, NewVars),
	(\+ instantiate_ranges(Ranges,Vars,_Vars1,[],_InstVars, _Res)
	->	true
	;is_true(Term, NewVars, Status)
	->	(var(Status)
		->	fail
		;	true
		)
	;instantiate_exists(N)
	->	flag(inst, Old, 0),
		(	flag(inst, Now, Now + 1),
			Now =< N,
			instantiate_ranges(Ranges,Vars,Vars1,[],InstVars, Res),
			(Res == error
			->	Status = error
			;	substitute_inst_vars(Term, InstVars, Vars1, Term1, Res2),
				(	Res2 == error
				->	Status = error
				;	is_false(Term1, Vars1, Status)
				)
			)
		->	flag(inst, _, Old)
		;	flag(inst, _, Old),
			fail
		)
	;	fail
	).
is_falseq(forall, Ranges, Term, Vars, Status) :-
	is_trueq(exists, Ranges, not(Term), Vars, Status).
forallexists(exists, forall).
forallexists(forall, exists).


% Try 5 instantiations for is_true, is_false per check
instantiate_exists(5).

/* Even if no instantiation has taken place,
   if possible, test emptiness.
   Even if sorts contain variables, we could test whether all of them are
   empty?
   */


is_true(Term, Vars, _Status) :-
	fail_correct_formula(Term, Vars).
is_true(true, _, _) :-
	!.
is_true(false, _,_) :-
	!,
	fail.
is_true(not(Term), Vars, Status) :-
	!,
	is_false(Term, Vars, Status).

is_true(and(T1, T2), Vars, Status) :-
	!,
	is_true(T1, Vars, Status),
	(nonvar(Status)
	->	true
	;	is_true(T2, Vars,Status)
	).
is_true(or(T1, T2), Vars, Status) :-
	!,
	(is_true(T1, Vars, Status)
	->	true
	;	is_true(T2, Vars, Status)
	).
is_true(Exists, Vars, Status) :-
	quantor(Exists, Ranges, ExForAll, Term),
	is_trueq(ExForAll, Ranges, Term, Vars, Status).

is_trueq(exists, Ranges, Term, Vars, Status) :-
	!,vars_from_ranges(Ranges, Vars, NewVars),
	(is_true(Term, NewVars,Status)
	->	true
	;instantiate_exists(N)
	->	flag(inst, Old, 0),
		(	flag(inst, Now, Now + 1),
			Now =< N,
			instantiate_ranges(Ranges, Vars, Vars1,
					   [], InstVars, Res),
			(Res == error
			->	Status = error,
				flag(inst, _, Old)
			;	substitute_inst_vars(Term, InstVars, Vars1, Term1, Res2),
				(Res2 == error
				->	Status = error
				;	is_true(Term1, Vars1,Status)
				)
			)
		->	flag(inst, _, Old)
		;	flag(inst, _, Old),
			fail
		)
	;	fail
	).



is_trueq(forall, Ranges, Term, Vars, Status) :-
	is_falseq(exists, Ranges, not(Term), Vars, Status).
instantiate_q :-
	true.
not_normalised_p(P,Status) :-
	(   P >= 0.0,
	    P =< 1.0
	->  fail
	;   Status = error,
	    error('Probabilty value ~w should be 0 <= P <= 1',[P])
	).
simplify_pxor(P,T1,T2, Vars, TermOut, Status) :-
	simplify_term(T1, Vars, Term1, Status),
	(var(Status)
	->      simplify_term(T2, Vars, Term2, Status),
	        (var(Status)
	        ->   (Term1 == Term2
		     ->    TermOut = Term1
		     ;     memberchk(Term1, [true, false])
		     ->    pre_eval_num(1.0 - P,P1),
		           Term3 = pxor(P1, Term2, Term1),
		           simplify_term(Term3, Vars, TermOut, Status)
		     ;     TermOut = pxor(P, Term1, Term2)
		     )
	        ;    true
	        )
	;      true
	).
simplify_term(Term, Vars, TermOut, Status) :-
	enable_pxor,
	tr_pxor_all(Term, Term1),
	Term1 \= Term,
	Term1 = pxor(_,_,_),
	!,
	simplify_term(Term1, Vars, TermOut, Status).
simplify_term(pxor(P,T1,T2), Vars, TermOut, Status) :-
	!,
	(	number(P)
	->    (not_normalised_p(P, Status)
	      ->    true
	      ;     (P =:= 0.0
		    ->     simplify_term(T2, Vars, TermOut, Status)
		    ;P =:= 1.0
		    ->     simplify_term(T1, Vars, TermOut, Status)
		    ;      simplify_pxor(P, T1, T2, Vars, TermOut, Status)
		    )
	      )
	;     simplify_pxor(P, T1, T2, Vars, TermOut, Status)
	).

simplify_term(true, _Vars, true,_) :-
	!.
simplify_term(false, _Vars, true,_) :-
	!.
simplify_term(Term, Vars, true,Status) :-
	is_true(Term, Vars,Status),
	!.
simplify_term(Term, Vars, false,Status) :-
	is_false(Term, Vars, Status),
	!.
simplify_term(not(Term), Vars, not(Term1),Status) :-
	simplify_term(Term, Vars, Term1,Status),
	(nonvar(Status)
	->	true
	;is_true(Term1, Vars,Status)
	->	TermR = false
	;is_false(Term1, Vars,Status)
	->	TermR = true
	;	TermR = not(Term1)
	),!.
simplify_term(and(T1, T2), Vars, Res,Status) :-
	simplify_term(T1, Vars, T1S,Status),
	(nonvar(Status)
	->	true
	;	simplify_term(T2, Vars, T2S,Status),
		(	T1S == true
		->	Res = T2S
		;	T2S == true
		->	Res = T1S
		;	Res = and(T1S, T2S)
		)
	),!.
simplify_term(or(T1, T2), Vars, Res, Status) :-
	simplify_term(T1, Vars, T1S, Status),
	(nonvar(Status)
	->	true
	;	simplify_term(T2, Vars, T2S, Status),
		(	T1S == false
		->	Res = T2S
		;	T2S == true
		->	Res = T1S
		;	Res = or(T1S, T2S)
		)
	),!.
simplify_term(exists(Range, Term), Vars, exists(Range, Term1),Status) :-
	add_range_vars(Range, Vars, Vars1),
	simplify_term(Term, Vars1, Term1, Status),!.
simplify_term(forall(Range, Term), Vars, forall(Range, Term1),Status) :-
	add_range_vars(Range, Vars, Vars1),
	simplify_term(Term, Vars1, Term1,Status),!.

simplify_term(Term, _Vars, Term, error) :-
	reserved(Term),
	!,
	(functor(Term, pxor, _A),
		\+ enable_pxor
	->	fatal_error('Unhandled pxor term ~w:Probably pxor not enabled, set option -pxor', [Term])
	;	fatal_error('Missed reserved term in simplify_term:~w', [Term])
	).


simplify_term(Term, _Vars, Term, _).



fail_correct_formula(Term, _Vars) :-
	\+ ground(Term),
	impl_error('is_true on nonground term').
fail_correct_formula(Term, Vars) :-
	spec_constant(Term, Value),
	test_recursive_constant(Term, Value),
	!,
	(fail_correct_formula(Value, Vars)
	->	free_recursive_constant(Term)
	;	free_recursive_constant(Term),
		fail
	).

add_range_vars(Range, Vars, Vars1) :-
	impl_error('Unimplemented : ~w', [add_range_vars(Range, Vars, Vars1)]).



% subst_inst_vars_r(Term, LVars, GVars, Term1, Res)
% Substitute variables occurringing Term from LVars; If they occur in
% GVars, reflect that in result, i.e.
% Res - not_ground(VarName, Sort, GVars1)
% difference with substitute_inst_vars: The "_r" version
% recursively also subst_insts the Sort argument of Res until
% an expression is found containing no GVar
subst_inst_vars_r(Term, LVars, GVars, Term1, Res) :-
	substitute_inst_vars(Term, LVars, GVars, Term1, Res1),
	(Res1 = not_ground(V, Sort, GVars1)
	->	subst_inst_vars_r(Sort, LVars, GVars, Sort1, Res2),
		(Res2 = not_ground(_V2, _Sort2, _GVars2)
		->	Res = Res2
		;	Res2 == error
		->	Res = error
		;	assert_debug(Res2 == ground),
			Res = not_ground(V, Sort1, GVars1)
		)
	;	Res = Res1
	).

% substitute_inst_vars(TermIn, InstVars, Vars, TermOut, Res)
%
% Substitute (target language) variables of TermIn occurring in InstVars
% As soon as a variable from Vars occurs, Res becomes
% not_ground(Term, Sort1, Vars1) (first such occurrence)
substitute_inst_vars(Term, _InstVars, _Vars1, _Term1, error) :-
	\+ ground(Term),
	!,
	error('Term expected to be ground, got ~w', [Term]),
	fail.


substitute_inst_vars(Term:Sort, InstVars, Vars, Term1, Res) :-
	atom(Term),
	!,
	(var_inst_from_var_list(InstVars, Term, Sort1, _Kind, Term1)
	->	Res2 = ground
	;var_from_var_list(Vars, Term, Sort1, Vars1)
	->	Term1 = Term,
		Res2 = not_ground(Term, Sort1, Vars1)
	;	error('Isolated variable ~w', [Term:Sort]),
		Res2 = error
	),
	(Res2 == error
	->	Res = Res2
	;	(	Sort == Sort1,
			Res = Res2
		;	substitute_inst_vars(Sort,InstVars, Vars, Sort2, Res3),
			(Res3==error
			->	Res = Res3
			;	Sort2 == Sort1
			->	Res = Res2
			;	error('Variable type mismatch : ~w - ~w',
				      [Term:Sort1,Term:Sort2]),
				Res = error
			)
		)
	).





substitute_inst_vars(Term, InstVars, Vars, Term1, Res) :-
	atom(Term),
	(var_inst_from_var_list(InstVars, Term, _Sort, _Kind, Term1)
	->	!,Res = ground
	;var_from_var_list(Vars, Term, Sort, Vars1)
	->	!,Term1 = Term,
		Res = not_ground(Term, Sort, Vars1)
	;	fail
	).
substitute_inst_vars(Term, InstVars, Vars, Term1, Res) :-
	spec_constant(Term, Term2),
	!,
	test_recursive_constante(Term, Term2, Status),
	(var(Status)
	->	substitute_inst_vars(Term2, InstVars, Vars, Term1, Res),
		free_recursive_constant(Term)
	;	Res = error
	).


substitute_inst_vars(Term, _InstVars, _Vars, Term, ground) :-
	atomic(Term),
	!.
substitute_inst_vars(Term, InstVars, Vars, Term1, Res) :-
	functor(Term, F, A),
	number_ar_op(F, A),
	!,
	Term =.. [F|Args],
	substitute_inst_vars_list(Args, InstVars, Vars, Args1, Res),
	(Res == error
	->	true
	;	Res == ground,
		ground(Args1)
	->	Op =.. [F|Args1],
		Term1 is Op
	;	Term1 =.. [F|Args1]
	).
substitute_inst_vars(Term, InstVars, Vars, Term1, Res) :-
	Term =.. [F|Args],
	substitute_inst_vars_list(Args, InstVars, Vars, Args1, Res),
	(Res == error
	->	true
	;	Term1 =.. [F|Args1]
	).
substitute_inst_vars_list([], _InstVars, _Vars, [], ground).
substitute_inst_vars_list([Arg|Args], InstVars, Vars, [Arg1|Args1], Res) :-
	substitute_inst_vars(Arg, InstVars, Vars, Arg1, Res1),
	(Res1 == error
	->	Res = error
	;	substitute_inst_vars_list(Args, InstVars, Vars, Args1, Res2),
		combine_substitute_inst_res(Res1, Res2, Res)
	).
combine_substitute_inst_res(error, _Res2, error) :-
	!.
combine_substitute_inst_res(_, error, error) :-
	!.
combine_substitute_inst_res(not_ground(P,Q,R), _, not_ground(P,Q,R)) :-
	!.
combine_substitute_inst_res(_, not_ground(P,Q,R), not_ground(P,Q,R)) :-
	!.
combine_substitute_inst_res(ground, ground, ground) :-
	!.
combine_substitute_inst_res(Res1, Res2, Res) :-
	fatal_fail(combine_substitute_inst_res(Res1, Res2, Res)).



/*
  COMPLICATED: do it in two stages:
  1)Leave forall encoded in result, but determine instantiations
  2)Instantiate
  It could be that in second phase we see that a newly instantiated sort
  is infinite

  All variables except global variables should be replaced by prolog var
  that is part of some code.
  */
expandq(Ante, Conse, Vars, Ante2, Conse2, Vars1) :-
	init_ig(Vars, IGIn),
	expandq1(IGIn, [], Ante, Conse, Ante1, Conse1, IGOut),
	instantiate_ig(IGOut, Ante1, Conse1, Ante2, Conse2, Vars1).

tr_simple(Form, Form1) :-
	functor(Form, and, N),
	N > 2,
	!,
	Form =.. [and|Args],
	list_and(Args, Form1).

expandq1(IGIn, LV, Ante, Conse, Ante1, Conse1, IGOut) :-
	expandq11(Ante, LV, IGIn, Ante1, IG1),
	(IG1 == error
	->	IGOut = error
	;ig_added(IG1, IG2, LV1)
	->	expandq1(IG2, LV1, Ante, Conse, Ante1, Conse1, IGOut)
	;	expandq11(Conse, LV, IG1, Conse1, IG3),
		(IG3 == error
		->	IGOut = error
		;ig_added(IG3, IG4, LV1)
		->	expandq1(IG4, LV, Ante, Conse, Ante1, Conse1, IGOut)
		;	IGOut = IG3
		)
	).


expandq11(not(Form), LV, IGIn, not(Form1), IGOut) :-
	!,expandq11(Form, LV, IGIn, Form1, IGOut).

expandq11(and(F11, F12), LV, IGIn, and(F21, F22), IGOut) :-
	!,expandq11(F11, LV, IGIn, F21, IG1),
	(IG1 == error
	->	IGOut = error
	;ig_added1(IG1, IGOut)
	->	true
	;	expandq11(F12, LV, IG1, F22, IGOut)
	).
expandq11(forall(Ranges, Term), LV, IGIn, forall(Codes, Term1), IGOut) :-
	!,expandqranges(Ranges, LV, IGIn, [], LV1, IG1, Codes),
	assert_debug((nonvar(Codes);ig_added1(IG1, _))),
	(IG1 == error
	->	IGOut = error
	;ig_added1(IG1, IGOut)
	->	true
	;	expandq11(Term, LV1, IG1, Term1, IGOut)
	).
expandq11(Form, LV, IGIn, Form1, IGOut) :-
	tr_simple(Form, Form2),
	!,
	expandq11(Form2, LV, IGIn, Form1, IGOut).
expandq11(pxor(L), LV, IGIn, pxor(L1), IGOut) :-
	!,
	expandq1pxorl(L, LV, IGIn, IGOut,L1).

expandq11(pxor(P, F1, F2), LV, IGIn, pxor(P1, F21, F22), IGOut) :-
	!,
	gvars(IGIn, GVars),
	substitute_inst_vars(P, LV, GVars, P1, Res),
	(Res == error
	->	IGOut = error
	;	expandq11(F1, LV, IGIn, F21, IG1),
	        (IG1 == error
		->	IGOut = error
		;ig_added1(IG1, IGOut)
		->	true
		;	expandq11(F2, LV, IG1, F22, IGOut)
		)
	),!.
expandq11(F, _LV, _IGIn, error, error) :-
	reserved(F),
	!,
	error('Term contains reserved expression ~w', [F]).

expandq11(F, LV, IGIn, F1, IGOut) :-
	gvars(IGIn, GVars),
	substitute_inst_vars(F, LV, GVars, F1, Res),
	(Res == error
	->	IGOut = error
	;	IGOut = IGIn
	),!.
expandq11(F, _LV, _IGIn, _F1, _IGOut) :-
	fatal_error('Cannot expand term ~w', [F]).

expandq1pxorl([], _LV, IGIn, IGIn, []).
expandq1pxorl([pxe(P, F)|R], LV, IGIn, IGOut, [pxe(P1,F1)|R1]) :-
	gvars(IGIn, GVars),
	substitute_inst_vars(P, LV, GVars, P1, Res),
	(Res == error
	->	IGOut = error
	;	expandq11(F, LV, IGIn, F1, IG1),
	        (IG1 == error
		->	IGOut = error
		;ig_added1(IG1, IGOut)
		->	true
		;	expandq1pxorl(R, LV, IG1, IGOut, R1)
		)
	),!.





% expandqranges: Two purposes:
% 1)For complete removal of internal forall formulae
% 2)For outer leadsto (interval) variables
% expandqranges(Ranges, LVIn, IGIn, CodeIn, LV, IGOut, CodeOut)

/*
  GVars : variables that remain uninstantiated
  LV: Local variables, that will be instantiated
  */

expandqranges([], LVIn, IGIn, CIn, LVIn, IGIn, CIn).

expandqranges([Range|Ranges], LVIn,IGIn, CodeIn, LVOut, IGOut,CodeOut) :-
	expandqrange(Range, LVIn, IGIn, CodeIn, LV1, IG1, Code1),
	(ig_added1(IG1, IGOut)
	->	true
	;	expandqranges(Ranges,LV1,IG1,Code1, LVOut, IGOut,CodeOut)
	).
% if not expandvar then if CTerm not true then instantiate
expandqrange(Range, LVIn, IGIn, CodeIn, LVOut, IGOut, CodeOut) :-
	range_var_condition(Range, VarName, Sort, Op, CTerm),
	gvars(IGIn, GVars),
	substitute_inst_vars(CTerm, LVIn, GVars, CTerm1, GRes),
	(	GRes = error
	->	IGOut = error
	;	GRes = ground
	->	expand_range_sort(Sort, LVIn, IGIn, Sort1, IG1),
		(	ig_added1(IG1, IGOut)
		->	true
		;	var_inst_to_var_list(VarName, Value,Sort1,
					     undef, LVIn,LVOut),
			CodeOut = [ds_rc(Op, Sort1,Value, CTerm1)| CodeIn],
			IGOut = IG1
		)
	;	GRes = not_ground(VarName3, Sort3, Vars3)
	->	IGOut = ig_added(IGIn, VarName3, Sort3, Vars3)
	;	impl_error('Res?')
	).



instantiate_ivar(v(_VarName, Value, Sort, Kind)) :-
	(Kind == undefined
	->	instantiate_element(Sort, _Kind1, Value)
	;	instantiate_element(Sort, Kind, Value)
	).

instantiate_ig(ds_ig(Vars1, IVars), Ante2, Conse2, Ante3, Conse3, Vars1) :-
	uchecklist(instantiate_ivar,IVars),
	expandq2(Ante2, Ante3),
	expandq2(Conse2, Conse3).

expandq2(not(F), not(F1)) :-
	!,expandq2(F, F1).
expandq2(and(F1,F2), and(G1, G2)) :-
	!,
	expandq2(F1, G1),
	expandq2(F2, G2).
expandq2(forall(Codes, F), R) :-
	!,
	assert_debug(nonvar(Codes)),
	findall(F1, inst_rangeq2(Codes, F, F1), Fs),
	list_and(Fs, R).

expandq2(F, F1) :-
	tr_simple(F, F2),
	!,
	expandq2(F2, F1).
expandq2(F, F).

inst_rangeq2(Codes, F, F1) :-
	reverse(Codes, Codes1),
	uchecklist(inst_code, Codes1),
	expandq2(F, F1).
inst_code(ds_rc(Op, Sort, Value,  CTerm1)) :-
	instantiate_var_cond(Sort, Op, CTerm1, _Kind, Value).

ig_added1(IGIn, IGIn) :-
	assert_debug(nonvar(IGIn)),
	(IGIn = ig_added(_, _VarName, _Sort, _Vars1)
	;IGIn = error
	).

ig_added(IGIn, IGOut, LV) :-
	assert_debug(nonvar(IGIn)),
	(IGIn = ig_added(ds_ig(Vars,IVars), VarName, Sort, Vars1)
	->	substitute_inst_vars(Sort, IVars, Vars, Sort1, Res1),
		(Res1 = error
		->	IGOut = error
		;Res1 = ground
		->	var_inst_to_var_list(VarName, _Value, Sort1, undefined,
					     IVars, VOut),
			IGOut = ds_ig(Vars1, VOut),
			LV = VOut
		;Res1 = not_ground(VarName2, Sort2, Vars2)
		->	ig_added(ig_added(ds_ig(Vars2, VOut),VarName2, Sort2, Vars2),IGOut,LV)
		;	impl_error('Res?')
		)
	;	IGIn = error
	->	IGOut = error
	;	fail
	).

gvars(ds_ig(GVars,_IVars), GVars) :-
	!.
gvars(P, Q) :-
	fatal_fail(gvars(P, Q)).


init_ig(Vars, ds_ig(Vars, [])).


/*
  expand_range_sort: any variable should be forced onto GLV
  LVIn should contain all possible variables
  */
expand_range_sort(Sort, LVIn, IGIn, Sort1, IGOut) :-
	gvars(IGIn, GVars),
	substitute_inst_vars(Sort, LVIn, GVars, Sort1, Res),
	(Res = error
	->	IGOut = error
	;Res = ground
	->	IGOut = IGIn
	;Res = not_ground(Term, Sort, Vars1)
	->	IGOut = ig_added(IGIn, Term, Sort, Vars1)
	;	Res = ground
	->	IGOut = IGIn
	;	impl_error('expand_range_sort')
	).





pr_traces(Where) :-
	format('TRACE at ~w~n', [Where]),
	(setof(AtomKey-AtomTrace,
	       Atoma^atom_trace(AtomKey, Atoma, AtomTrace),
	       Traces)
	->	pr_tracesl(Traces)
	;	format('   NO ATOMS TRACED~n')
	).
pr_tracesl(Traces) :-
	max_atoms_length(Traces, 0, L1),
	L is L1 + 2,
	format('ATOM~t~*|     TRACE  ~n', [L]),
	pr_tracesl1(Traces, L).
pr_tracesl1([], _L) :-
	nl.
pr_tracesl1([AtomKey-Trace|Traces], L) :-
	pr_trace(AtomKey, Trace, L),
	pr_tracesl1(Traces, L).
pr_trace(AtomKey, Trace, L) :-
	format('~w~t~*|', [AtomKey, L]),
	reverse(Trace,TraceRev),
	pr_trace_cont(TraceRev).
pr_trace_cont([]) :-
	nl.
pr_trace_cont([range(Tlo, Thi, TFU)|TraceRev]) :-
	format('~w - ~w : ~w  ', [Tlo, Thi, TFU]),
	pr_trace_cont(TraceRev).



max_atoms_length([], L, L).
max_atoms_length([AtomKey-_|R], L, L1) :-
	max_atoms_length(R, L, L2),
	max_atom_length(AtomKey, L2, L1).

max_atom_length(AtomKey, L2, L1) :-
	atom_codes(AtomKey, L),
	length(L, N),
	L1 is max(L2, N).

pr_waits(Where) :-
	format('PENDING ACTIVITIES:~w~n',[Where]),
	(get_wait1(_,_,_)
	->	(	get_wait1(TN, Activity,_),
			pr_wait(Activity, TN),
			fail
		;	true
		)
	;	format('     NO ACTIVITIES~n')
	).
pr_wait(wait_var(_Id,IdTerm,HT,_TMin,FV, FVL, LitData,
			      ToDoAnte,
			      _AnteHolds,THolds, ConseRId,_PV,_Delay), HT) :-
	numbervars((FV, LitData, ToDoAnte, ConseRId),0,_),
	LitData = ds_litd(Atom, PN, _PreOps, _PostOps, _PostConds),
	rule_from_id_term(IdTerm, R, Atoms),
	format('   WAITVAR:(th:~w)L:~w,~w vars:~w excluded:~w at ~w~n      ~w:~w~n',
	       [THolds, Atom,PN,FV, FVL, HT, R, Atoms]),
	!.


pr_wait(Activity, TN) :-
	format('    ACTIVITY ~w:~w~n', [TN,Activity]).





log_entry(mayorstats, 'General information', ht,on).
log_entry(ht, 'Time at each step', ht,on).
log_entry(time, 'timing of step', time(_),off).
log_entry(set, 'set intervals', 'SET',off).
log_entry(traces, 'trace output each step', pr_traces,off).
log_entry(pr_waits, 'pending activities', pr_waits,off).
log_entry(readterm, 'input terms as they are handled', readterm,off).
log_entry(leadsto, 'leadsto rule setup', leadsto,off).
log_entry(leadstoinst, 'leadsto rule instantiations', 'DOnontrleads',off).
log_entry(uatid, 'DEBUGGING:update_activity id', db(uatid), off).

/*
log_entry(cb).
log_entry(o1_o2).
log_entry(multifire).
log_entry(setwait).
log_entry(rmv).
log_entry(db(uat)).
log_entry(fdho).
log_entry(rgc).
log_entry(rmv).
log_entry(addv).
log_entry(run_ops).
*/




