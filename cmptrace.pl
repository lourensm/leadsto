:- module(cmptrace,
	  [
	      cmptraces/3
	  ]).
:- use_module(algo).
:- use_module(util).
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

user:tstcmp :-
	reset_error_log,
	reset_algo([]),
	runspec('spec1/error1.lt', results),
	error_log_term(ErrorTerm),
	format('~w~n', [ErrorTerm]).

get_lt_name(LTFile, Name) :-
	file_base_name(LTFile, BaseName),
	(   atom_concat(Name, '.lt', BaseName)
	->  true
	;   fatal_error('Expected filename with lt extension, got ~w', [LTFile])
	).

user:tstrun :-
	shell('rm -rf resulttmp'),
	rundir('spec1', resulttmp, Result),
	format('Result~w~n', [Result]).

user:cmpdir:-
	cmprundir(spec1, resulttmp).

/**
 * rundir(+LTDir, +ResultDir) is det
 *
 * ResultDir is created or must be empty.
 * All lt specifications will run and result stored in resultDir
 *
 */
rundir(LTDir, ResultDir, Result) :-
	set_fatal_throw,
	reset_algo([]),
	make_directory_path(ResultDir),
	(   member(F, ['/*.tr', '/*.txt']),
	    atom_concat(ResultDir, F, E1),
	    expand_file_name(E1, L1),
	    L1 \= []
	->  error('result directory not empty:~w', [L1]),
	    Result = error('resultdir not empty')
	;   atom_concat(LTDir, '/*.lt', LT1),
	    expand_file_name(LT1, LTs),
	    (	member(LT, LTs),
		format('****************~nRunning ~w~n', [LT]),
		runspec(LT, ResultDir),
		reset_algo([]),
		fail
	    ;	true
	    ),
	    Result = []
	).

cmprunspeclist([], _, []).
cmprunspeclist([LT|LTs], ResultDir, CmpResult) :-
	format('****************~nRunning ~w~n', [LT]),
	cmprunspec(LT, ResultDir, CmpResult1, CmpResult),
	reset_algo([]),
	cmprunspeclist(LTs, ResultDir, CmpResult1).

/**
 * cmprundir(+LTDir, +ResultDir) is det
 * run all leadsto specs in LTDir and compare results with
 * earlier results in ResultDir
 */

cmprundir(LTDir, ResultDir) :-
	set_fatal_throw,
	reset_algo([]),
	atom_concat(LTDir, '/*.lt', LT1),
	expand_file_name(LT1, LTs),
	cmprunspeclist(LTs, ResultDir, CmpResult),
	(   CmpResult == []
	->  format('Directories ~w and ~w compare OK~n', [LTDir, ResultDir])
	;   format('MISMATCH:Directories ~w and ~w comparison:~n~w~n', [LTDir, ResultDir, CmpResult])
	).

cmprunspec(LTFile, ResultDir, MatchIn, MatchOut) :-
	get_lt_name(LTFile, LTName),
	(   exists_file('trace.tr')
	->  delete_file('trace.tr')
	;   true
	),
	reset_error_log,
	catch((
	       load_simulation(LTFile),
	       runspecdo([])
	      ),
	      finalhalt(E),  dofinal(E)),
	error_log_term(ErrorTerm),
	(   var(E)
	->  ErrorTerm1 = ErrorTerm
	;   ErrorTerm1 = [finalhalt(E)|ErrorTerm]
	),
	(   exists_file('trace.tr')
	->  concat_atom([ResultDir, '/', LTName, '.tr'], TraceName),
	    cmptraces(TraceName, LTFile, CmpTraceResult),
	    Term1 = ErrorTerm1
	;   Term1 = [error('no trace generated')|ErrorTerm1],
	    TraceName = [],
	    CmpTraceResult = []
	),
	concat_atom([ResultDir, '/', LTName, '.txt'], ResName),
	open(ResName, read, S),
	LtResultNew = ltresult(LTName, TraceName, Term1),
	read(S, LtResult),
	(   CmpTraceResult == [],
	    LtResult = LtResultNew
	->  format('MATCH~n', []),
	    MatchOut = MatchIn
	;   format('Mismatch:LTResult:~w - ~w   CmpTrace:~w~n', [LtResultNew, LtResult, CmpTraceResult]),
	    MatchOut = [LTName-l(LtResultNew, LtResult)-CmpTraceResult|MatchIn]
	).

/**
 * runspec(+LTFile, +ResultDir) is det.
 *
 * runs leadsto specification LTFile, stores Result Trace and result
 * into ResultDir directory as two sparate files.
 *
 *
 *
 */
runspec(LTFile, ResultDir) :-
	get_lt_name(LTFile, LTName),
	(   exists_file('trace.tr')
	->  delete_file('trace.tr')
	;   true
	),
	reset_error_log,
	catch((
	       load_simulation(LTFile),
	       runspecdo([])
	      ),
	      finalhalt(E),  dofinal(E)),
	error_log_term(ErrorTerm),
	(   var(E)
	->  ErrorTerm1 = ErrorTerm
	;   ErrorTerm1 = [finalhalt(E)|ErrorTerm]
	),
	(   exists_file('trace.tr')
	->  concat_atom([ResultDir, '/', LTName, '.tr'], TraceName),
	    rename_file('trace.tr', TraceName),
	    Term1 = ErrorTerm1
	;   Term1 = [error('no trace generated')|ErrorTerm1],
	    TraceName = []
	),
	concat_atom([ResultDir, '/', LTName, '.txt'], ResName),
	open(ResName, write, S),
	portray_clause(S, ltresult(LTName, TraceName, Term1)),
	close(S),
	format('~w~n', [Term1]).

dofinal(E) :-
	format('finalhalt(~w)~n', [E]).

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
	    (	predicate_property(LoadModule:atom_trace(_,_,_), interpreted)
	    ->	(	LoadModule:atom_trace(AtomKey, Atom, AtomTrace)
		->  true
		;   LoadModule:atom_trace(AtomKey, Atom, AtomTrace1)
		->  format('Atom Trace varying results:~w~n', [Atom]),
		    format('algo:~w~nltrace:~w~n', [AtomTrace, AtomTrace1]),
			flag(mismatchatoms, MA, MA + 1)
		;   fail
		)
	    ->	true
	    ;	format('Atom Trace only new:~w~n', [Atom]),
		flag(algoonlyatoms, AOA, AOA + 1)
	    ),
	    fail
	;   true
	),
	(   predicate_property(LoadModule:atom_trace(_,_,_), interpreted),
	    LoadModule:atom_trace(AtomKey, Atom, AtomTrace),
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
