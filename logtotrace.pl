:- module(logtotrace,[]).

:- use_module(util).
:- use_module(logutil).
:- use_module(ltversion).
:- multifile user:version/2.


/*local_option(log2trace, single('-onlyoutput',set_option(onlyoutput)),
	     'Only consider output atoms after activation of each element',[]) :-
*/     
savealone:-
	my_autoload_all,
	qsave_program(log2trace, [goal=run, autoload=true, stand_alone=true]),
	halt.
savealone_nsa:-
	my_autoload_all,
	qsave_program(log2trace, [goal=run, autoload=true]),
	halt.

run :-
	catch(run1, E, rep(E)).

run1 :-
	setup_logging(_PgmName, [util]),
	setupmainnew('Log To Trace', log2trace,
		     [os(util,debugging),
		      os(logtotrace, log2trace)],  File1),
	(File1 == []
	->	(get(@finder, file, @on, @default, @default, File)
		->	true
		;	fatal_error('user exit')
		)
	;	File = File1
	),
	open(File, read, _, [alias(log)]),
	flag(lines1, Old, 0),
	repeat,
	read(log, Term),
	(Term == end_of_file
	->	!,
		flag(lines1, New, Old),
		format('Read ~w terms from log ~w~n', [New, File]),
		closeoff
	;	catch(handle_entry(Term), E, rep(E)),
		flag(lines1, L1, L1 + 1)
	->	fail
	;	!, fatal_error('HandleTerm failed')
	).
rep(E) :-
	told,
	tell(user),
	format('ERROR:~w~n', [E]),
	trace,
	fatal_error('Exception',[]).

:- dynamic dyn_facts/5.

use_fact(_Comp, IO, _Pref, _LocTime, _GlobTime) :-
	(IO == output
	->	true
	;	\+ get_option(onlyoutput)
	).

handle_entry(facts(Comp, IO, Pref, LocTime, GlobTime)) :-
	!,
	retractall(dyn_facts(_, _, _, _, _)),
	assertz(dyn_facts(Comp, IO, Pref, LocTime, GlobTime)),
	(memberchk(Pref, ['INPUT', 'OUTPUT'])
	->	true
	;	fatal_error('Sorry, cannot handle differential log(yet)')
	),
	flag(glob_time, _, GlobTime).



:- dynamic dyn_fact/4, dyn_fact/2.

handle_entry(fact(Atom, TFU)) :-
	!,
	(dyn_facts(Comp, IO, Pref, LocTime, GlobTime)
	->	true
	;	fatal_error('No component header')
	),
	(use_fact(Comp, IO, Pref, LocTime, GlobTime)
	->	(dyn_fact(Comp, Atom)
		->	true
		;	assertz(dyn_fact(Comp, Atom))
		),
		(	dyn_fact(Comp, Atom, TFUOld,_)
		->	(TFU == TFUOld
			->	true
			;	asserta(dyn_fact(Comp, Atom, TFU,GlobTime))
			)
		;	asserta(dyn_fact(Comp, Atom, TFU,GlobTime))
		)
	).



handle_entry(Term) :-
	format('Unimplemented:~w~n', [Term]),
	trace.

closeoff :-
	(setof(ca(Comp,Atom), dyn_fact(Comp, Atom), CAs)
	->	true
	;	fatal_error('No atoms in log')
	),
	!,
	tell('trace.tr'),
	flag(glob_time, GT, GT),
	portray_clause(times(0,GT,GT)),
	closeoff1,
	told,
	format('Wrote trace file ~w~n', ['trace.tr']),
	finalhalt(0).


closeoff1 :-
	(	retract(dyn_fact(Comp, Atom)),
		closeoff1(ca(Comp,Atom)),
		fail
	;	true
	).


closeoff1(ca(Comp,Atom)) :-
	setof(GlobTime-TFU, dyn_fact(Comp, Atom, TFU, GlobTime),TTFU),
	construct_trace_entries(0, unknown, TTFU, Trace),
	atom_key(Atom, AtomKey),
	portray_clause(atom_trace(AtomKey, Atom, Trace)).

/*
construct_trace_entries(UptoLastTime, UptoTFU, List[Time-TFU], TraceResult
  */
	
construct_trace_entries(LastT, LastTFU, [T-TFU|Times], Trace) :-
	construct_trace_entries1(Times, T, TFU, Trace).
construct_trace_entries1([T1-TFU1|Times], T, TFU, [range(T,T1, TFU)|Ranges]):-
	construct_trace_entries1(Times, T1, TFU1, Ranges).
construct_trace_entries1([], T, TFU, [range(T,GlobTime, TFU)]) :-
	flag(glob_time, GlobTime, GlobTime).

	
