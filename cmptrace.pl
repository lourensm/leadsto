:- module(cmptrace,
	  [
	      cmptraces/3
	  ]).
:- use_module(algo).

/** <module> cmptrace Comparing traces
 * compare traces.
 *
 * Developed for testing changes in the implementation.
 * This has been developed much later than other parts of the software,
 * so the organisation may be different.
 *
 * Notably the loading of traces as present in algo.pl such as
 * algo:load_results(File, Traces).
 *
 * We also have not explored comparing separately loaded traces.
 *
 * Prepare the functionality here to automated testing that is robust to
 * errors encountered.
 *
 *
 *
 */

:- dynamic dyn_trace_loaded/2.

user:cmptraces :-
	cmptraces('test/heartn.tr', LoadedFile, Result),
	format('Loaded sim:~w, Result:~w~n', [LoadedFile, Result]).

/**
 * cmptraces(+TraceFile, -LoadedSimFile, -Result) is det
 *
 * compare the loaded algo trace with TraceFile.
 *
 * LoadedSimFile is the leadsto sim file currently loaded in algo.
 *
 * Result is the result of the comparison.
 * If Result == [] then the traces are identical.
 */

cmptraces(TraceFile, LoadedFile, Result) :-
	LoadModule = loadedtrace,
	loadtrace(TraceFile, LoadModule),
	flag(algoatoms, _, 0),
	flag(algoonlyatoms, _, 0),
	flag(mismatchatoms, _, 0),
	flag(ltatoms, _, 0),
	sim_status(LoadedFile, Status),
	(   Status == done
	->  Mismatch = []
	;   Mismatch = [sim-statusnotdone]
	),
	(   times(TSetup, THandled, ET),
	    LoadModule:times(TSetup2, THandled2, EndTime2),
	    THandled = THandled2,
	    TSetup = TSetup2,
	    ET = EndTime2
	->  format('times match~n'),
	    Mismatch1 = Mismatch
	;   format('Times mismatch~n'),
	    Mismatch1 = [times-mismatch|Mismatch]
	),
	flag(algoatoms, _, 0),
	flag(algoonlyatoms, _, 0),
	flag(mismatchatoms, _, 0),
	flag(ltatoms, _, 0),
	flag(ltonlyatoms, _, 0),
	(   algo:filled_atom_trace(AtomKey, Atom, AtomTrace),
	    flag(algoatoms, AA, AA + 1),
	    (	LoadModule:atom_trace(AtomKey, Atom, AtomTrace)
	    ->	true
	    ;	LoadModule:atom_trace(AtomKey, Atom, AtomTrace1)
	    ->	format('Atom Trace varying results:~w~n', [Atom]),
		format('algo:~w~nltrace:~w~n', [AtomTrace, AtomTrace1]),
		flag(mismatchatoms, MA, MA + 1)
	    ;	format('Atom Trace only new:~w~n', [Atom]),
		flag(algoonlyatoms, AOA, AOA + 1)
	    ),
	    fail
	;   true
	),
	(   LoadModule:atom_trace(AtomKey, Atom, AtomTrace),
	    flag(ltatoms, LA, LA + 1),
	    (algo:filled_atom_trace(AtomKey, Atom, AtomTrace1)
	    ->	(AtomTrace == AtomTrace1
		->  true
		;   true
		)
	    ;	format('Atom Trace not new:~w~n', [Atom]),
		flag(ltonly, LO, LO + 1)
	    ),
	    fail
	;   true
	),
	flag(algoatoms, AA, AA),
	flag(ltatoms, LA, LA),
	flag(mismatchatoms, MA, MA),
	flag(algoonlyatoms, AOA, AOA),
	flag(ltonly, LO, LO),
	format('~w algoatoms, ~w ltatoms, ~w mismatch traces, ~w algoonly~n',
	       [AA, LA, MA, AOA]),
	(   AA =:= LA, AOA = 0, LO = 0
	->  (MA = 0
	    ->	Mismatch2 = Mismatch1
	    ;	Mismatch2 = [traces-mismatchranges(MA)|Mismatch1]
	    )
	;   Mismatch2 = [traces-mismatchmulti(AOA, LO, MA)|Mismatch1]
	),
	Result = Mismatch2.


unloadtraces(Module) :-
	(   dyn_trace_loaded(Module, File)
	->  unload_file(File),
	    retractall(dyn_trace_loaded(Module, File))
	;   true
	).
loadtrace(File, Module) :-
	unloadtraces(Module),
	Module:load_files([File], []),
	assertz(dyn_trace_loaded(Module, File)).
