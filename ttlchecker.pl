:- module(ttlchecker,
	  [
	   ttl_check_property/3,
	   checker_trace_management/2,
	   invalidate_holds/0,
	   reload_holds/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(util).
:- use_module(formframe).
:- use_module(formload).

:- use_module(nodes).
:- use_module(library(lists)).
:- use_module(transsat).
:- use_module(satsimpleverbose).
:- use_module(satsimplequiet).
:- use_module(satgenut).
:- use_module(transnew).
:- use_module(transnew2).
:- use_module(transutil).

:- use_module(logutil).
:- use_module(trace_manager).

:- use_module(pareto).





local_option(debugging,single('-g', setdebugging), 'Debugging mode:g',
	     [help_sort(back(10))]).
local_option(debugging,single('-pc', setprof(cumulative)),
	     'Debugging:Profiling(cumulative)',
	     [help_sort(back(10))]).
local_option(debugging,single('-pp', setprof(plain)),
	     'Debugging:Profiling(plain)',
	     [help_sort(back(10))]).
local_option(ttlalgo, single('-nocholds',set_option(nocholds)),
	     'Disable optimizing storage/retrieval holds facts(probably ignored)',[]) :-
             transold.

local_option(ttlalgo, single('-cholds',set_option(cholds)),
	     'Optimize storage/retrieval holds facts(default on)(probably ignored)',[]):-
             transold.
       
local_option(ttlalgo, single('-prcompl',set_option(prcompl)),
	     'Optimize storage/retrieval holds facts',[]) :-
             transold.

local_option(ttlalgo, single('-compile_only',set_option(compile_only)),
	     'Do not check:compile only',[]) :-
             true.
             %transold.

local_option(ttlalgo, single('-nogui',set_option(nogui)),
	     'Do not show gui, quit after handling command line actions',[]).

local_option(ttlalgo, arg('-saveholds', Arg,set_option(saveholds(Arg)),'FILE'),
	     'Save "compacted" holds facts in file FILE',[]).

local_option(ttlalgo, single('-savecompacted',set_option(savecompactedtraces)),
	     'Create a .tr file of the "compacted" traces',[]).
local_option(ttlalgo, arg('-checkv', Arg,set_option(ttlcheck(Arg, @on)),
			  'PROPERTY'),
	     'Check property PROPERTY verbose',[]).
local_option(ttlalgo, arg('-checkq', Arg,set_option(ttlcheck(Arg, @off)),'PROPERTY'),
	     'Check property PROPERTY quiet',[]).
local_option(ttlalgo, arg('-checknew', Arg,set_option(ttlcheck(Arg, new)),'PROPERTY'),
	     'Check property PROPERTY experimental',[]).
local_option(ttlalgo, arg('-batch_all',Arg,
			  set_option(batch(all,Arg)),'LABEL'),
	     'Combine with -checkq:print test(LABEL,false/true) in batch.log',
[]).
local_option(ttlalgo, arg('-batch_true',Arg,
			  set_option(batch(true,Arg)),'LABEL'),
	     'Combine with -checkq:print LABEL in batch.log if true',[]).
local_option(ttlalgo, arg('-batch_false',Arg,
			  set_option(batch(false,Arg)),'LABEL'),
	     'Combine with -checkq:print LABEL in batch.log if false',[]).
config_cholds :-
	assert_debug(transold),
	(get_option(nocholds)
	->	set_config(form_config:options/cholds, false)
	;	true
	),
	(get_option(cholds)
	->	set_config(form_config:options/cholds, true)
	;	true
	),
	(	get_config(form_config:options/cholds, CH),
		CH == true
	->	set_option(cholds)
	;	reset_option(cholds)
	),
	listen(set_config(form_config:options/cholds, CHN),
		       set_bool_option(cholds, CHN)).


config_checker :-
	(	transold
	->	config_cholds
	;	true
	),
	(get_option(saveholds(SaveFile))
	->	set_config(form_config:options/saveholds, SaveFile)
	;	true
	),
	(get_config(form_config:options/saveholds, SH1)
	->	(SH1 == ''
		->	true
		;	update_option(saveholds(_), saveholds(SH1))
		)
	;	true
	),
	listen(set_config(form_config:options/saveholds, SH3),
	       update_option(saveholds(_),saveholds(SH3))
	      ),
	(get_config(form_config:options/add_trace_file, TrF)
	->	(TrF == ''
		->	true
		;	add_trace_file(TrF)
		)
	;	true
	),
	(getprof(Prof1)
	->	assert_debug(memberchk(Prof1, [plain, cumulative, off])),
		set_config(form_config:options/profiling, Prof1)
	;	true
	),
	(get_config(form_config:options/profiling, Prof2)
	->	setprof(Prof2)
	;	true
	),
	listen(set_config(form_config:options/profiling, Prof3),
	       setprof(Prof3)
	      ).


savealonettlchecker(P):-
	my_autoload_all,
	qsave_program(P, [goal=ttlchecker, autoload=true, stand_alone=true]),
	halt.
savealonettlchecker_nsa(P):-
	my_autoload_all,
	qsave_program(P, [goal=ttlchecker, autoload=true]),
	halt.

ttlchecker :-
	catch_fatal_halt(ttlchecker1, ttlchecker_exception).

ttlchecker1 :-
	setup_logging(ttlchecker, [ttlchecker,util,formframe]),
	setupmainnew('TTL CHECKER', ttlchecker,
		     [os(util,[wd,constant,debugging,log]),
		      os(pareto, [pareto]),
		      os(formframe, ftree),
		      os(trace_manager, [trace_management]),
		      os(ttlchecker, [ttlalgo,debugging])], File),
	setup_formula_types(checker,true), % after reading options -allform
	new(P, tree_frame(checker,@on)),
	add_tree_frame_modified_hook(Frame, ModifiedData,
				     ttlchecker:update_compilation(Frame,
								   ModifiedData)),
	(	get_option(nogui)
	->	true
	;	send(P, open)
	),
	(File == []
	->	true
	;	send(P, load_file, File)
	),
	update_wd,
	send(@display, synchronise),
	add_command_line_traces_checker(P),
	(get_option(ttlcheck(Property, Verbose))
	->	(	get(P, find_property, Property, Node)
		->	(	Verbose == new
			->	(send(Node, check_new2)
				->	Succeeded = true
				;	Succeeded = false
				)
			;	(send(Node, check_property, Verbose)
				->	Succeeded = true
				;	Succeeded = false
				)
			)
		;	send(P, report, error, 'No property named %s to check',
			     Property)
		)
	;	true
	),
	(get_option(batch(What, Label))
	->	(What = all
		->	out_batch(test(Label, Succeeded))
		;	Succeeded == What
		->	out_batch(Label)
		;	true
		),
		user_forced_halt
	;	true
	),
	(get_option(nogui)
	->	user_forced_halt
	;	handle_pareto_options
	).
out_batch(Term) :-
	append('batch.log'),
	write(Term),
	nl,
	told.




:- dynamic dyn_valid_spec/1.

%
%  We would like to have loaded the most recent version of
%  the tree into spec:
%
update_compilation(Frame, _ModifiedData) :-
	get(Frame, spec_tree, ST),
	(retract(dyn_valid_spec(RT1))
	->	get(ST, root, RT),
		assert_debug(RT == RT1)
	;	true
	),
	(dyn_valid_spec(_)
	->	impl_error('Multiple specifications loaded')
	;	true
	).


ensure_tree_loaded(Node) :-
	get(Node, frame, F),
	send(F, reporta, ttlload, progress, 'Loading tree...'),
	get(Node?tree, root, ST),
	(dyn_valid_spec(ST1)
	->	assert_debug(ST == ST1)
	;	clear_spec,
		reset_sorts,
		(send(ST?sons, for_all, message(@prolog, store_node,
					 @arg1))
		->	update_sorts,
			(	retract(dyn_trace_sortdef_generated(Elements))
			->	ensure_trace_sort(Elements)
			;	true
			),
			load_cmd_constants,
			assertz(dyn_valid_spec(ST))
		;	send(F, reporta, ttlload, done, 'ERROR: see background window'),
			fail
		)    
	),
	send(F, reporta, ttlload, done, 'DONE').




ttlterm(denotes(_,_)).
ttlterm(sortdef(_,_)).
ttlterm(constant(_,_)).

:- dynamic spec:sort_element/2.
:- dynamic spec:denotes/2.
:- dynamic spec:sortdef/2.
:- dynamic spec:constant/2.

clear_spec :-
	(	ttlterm(Term),
		spec:retractall(Term),
		fail
	;	true
	).


store_node(Node) :-
	get(Node, tr_term, Term),
	(ttlterm(Term)
	->	(	Term = denotes(Header, external(Body))
		->	(	same_denotes_header(Header)
			->	fail
			;	spec:assertz(denotes(Header, external(Header, Body)))
			)
		;Term = denotes(Header, Body)
		->	(	same_denotes_header(Header)
			->	fail
			;	spec:assertz(Term)
			)
		;	spec:assertz(Term)
		)
	;Term = [specification_element]
	->	warning('Ignoring [specification_element]')
	;	error('Ignoring unrecognised ttl code:~w', Term),
		fail
	).
same_denotes_header(Header) :-
	functor(Header, F, A),
	functor(Header1, F, A),
	spec:denotes(Header1, _),
	Header =.. Args,
	Header1 =.. Args1,
	(Header == Header1
	->	error('Ignoring duplicate property definition for ~w', [Header])
	;	maplist(matching_header_arg, Args, Args1),
		error('Overlapping property definitions ~w  - ~w(ignoring second!)', [Header, Header1])
	).
matching_header_arg(Arg1, Arg2) :-
	(Arg1 = _ : _
	->	true
	;	Arg2 = _ : _
	->	true
	;	functor(Arg1, F, A),
		functor(Arg2, F, A),
		Arg1 =.. [_|Args1],
		Arg2 =.. [_|Args2],
		maplist(matching_header_arg, Args1, Args2)
	).

substitute_properties(Node, Test) :-
	ttl_get_property_internals(Node, Test, Header, Formula),
	(Test == @on
	->	true
	;	ensure_tree_loaded(Node),
		get(Node, frame, F),
		ensure_holds_loaded(F),
		tr_form_den_const(Formula, F1),
		term_to_formula_node(F1, FormulaNode, Node),
		new(PN, property_def_node(@off)),
		send(PN, fill_header, substituted(Header)),
		send(PN, son, FormulaNode),
		send(Node, add_below_above, @on, PN, expand_below_above)
	).

ttl_check_property2(Node, Verbose, Test) :-
	ttl_check_property(Node, Verbose, Test, new).
ttl_check_property(Node, Verbose, Test) :-
	ttl_check_property(Node, Verbose, Test, old).

ttl_check_property(Node, Verbose, Test, OldNew) :-
	ttl_get_property_internals(Node, Test, Header, Formula),
	(atomic(Header)
	->	true
	;	(contains_var(Header)
		->	(Test == @on
			->	fail
			;	send(Node, report, error,
				     'Can only check properties with 0arguments'),
				fail
			)
		;	true
		)
	),
	(Test == @on
	->	true
	;	ttl_check_form(Node, Header, Formula, Verbose, OldNew)
	).

ttl_get_property_internals(Node, Test, Header, Formula) :-
	assert_debug(send(Node, instance_of, property_def_node)),
	(send(Node, contains_generic)
	->	(Test == @on
		->	fail
		;	send(Node, report, error,
			     'Invalid property'),
			fail
		)
	;	true
	),
	get(Node, tr_term, Test, Term), % may fail if Test == @on
	(Term = denotes(Header, Formula)
	->	(Test == @on
		->	true
		;	ensure_saved(Node)
		)
	;	impl_error('Unrecognised property code:~w', [Term]),
		fail
	).
ensure_saved(Node) :-
	(	get(Node, frame, Frame),
		send(Frame, instance_of, tree_frame),
		get(Frame, is_modified, @on)
	->	(new(D, dialog('Save before checking')),
			send(D, append,
			     label(l,'Please save specification before checking\nChecking may exit program')),
			send(D, append,
			     new(B,button(save_then_check,
					  if(message(Frame, save),
					     message(D, return, ok),
					     message(D, return, @nil))))),
			send(new(BC, button(cancel,
					    message(D, return, @nil))), right, B),
			send(button(force, message(D, return, force)),
			     right, BC),
			get(D, confirm, Answer),
			send(D, destroy),
			Answer \== @nil
		->	true
		;	send(Node, report, warning, 'check cancelled'),
			fail
		)
	;	true
	).


closesaveholds :-
	(	get_option(saveholds(SaveFile))
	->	telling(P),
		(retract(dyn_saveholds_opened(F1))
		->	assert_debug(SaveFile == F1)
		;	pr_error('Missing savefile entry')
		),
		tell_error(SaveFile),
		told,
		tell_error(P)
	;	true
	).


log_entry(ttlload, 'Loading ttl help files', ttlload, on).
log_entry(ttlcheckstart, 'Start checking property', ttlcheck, on).
log_entry(ttlcomp, 'Compiling property', ttlcomp, on).
log_entry(ttlcheck, 'Checking property', ttlcheckdo, on).
log_entry(saveholds, 'saveholds', saveholds, on).
log_entry(compiled_clause, 'Print compiled prolog clause',compiled_prolog, on).
log_entry(cmd_constants, 'Command line constants used in checked formula',
	  cmd_constants,on).

ttl_check_form(Node, Header, _Formula, Verbose, How) :-
	get(Node, frame, F),
	pl_pce(Header, HP),
	send(F, reporta, ttlcheck, progress, 'Checking %s...', HP),
	ensure_tree_loaded(Node),
	ensure_holds_loaded(F),
	send(F, reporta, ttlcomp, progress, 'Compiling query...'),
	ttl_check_form_new(Header, Verbose, F, How).
	% ttl_check_form_old(Formula, Verbose, F)
ttl_check_form_new(Formula, Verbose, F, old) :-
	!,
	ttl_check_form_new(Formula, Verbose, F).
ttl_check_form_new(Formula, Verbose, F, new) :-
	codePrologNew2(Formula, Code1, Constants).

ttl_check_form_new(Formula, Verbose, F) :-
	send(F, reporta, ttlcomp, inform, 'Compiling formula...'),
	setup_verbose(Verbose),
	(codePrologNew(Formula, Code1, Constants)
	->	send(F, reporta, ttlcomp, done, 'DONE'),
		(do_log(cmd_constants)
		->	(member(constant(N,V), Constants),
				cmd_constant(N, V),
				alog(cmd_constants,
				     'Used CMD constant(~w,~w)~n',
				     [N,V]),
				fail
			;	true
			)
		;	true
		)
	;	send(F, report, error,
		     'Compilation failed, see background window for error message'),
		cleanup_verbose_advanced,
		fail
	),
	(holds:holds(_,_,_)
	->	flag(do_trans_new2, Old, 2),
		(	reload_holds
		->	flag(do_trans_new2, _, Old)
		;	impl_error('reload_holds failed'),
			flag(do_trans_new2, _, Old),
			invalidate_holds,
			cleanup_verbose_advanced,
			fail
		),
	 send(F, reporta, ttlcomp, inform,
	      'Recompiling formula...'),
	 (	codePrologNew(Formula, Code, _Constants2)
	 ->	send(F, reporta, ttlcomp, done, 'DONE')
	 ;	send(F, report, error,
		     'Compilation failed, see background window for error message'),
		invalidate_holds,
		cleanup_verbose_advanced,
		fail
	 )
	;	Code = Code1
	),
	(do_log(compiled_prolog)
	->	nl,nl,
		protocol('compiled.txt'),
		portray_clause((test :- Code)),
		noprotocol,
		send(F, report, inform, 'Wrote compiled formula to file compiled.txt')
	;	true
	),
        (   get_option(compile_only)
	->  send(F, report, inform, 'No check because of option compile_only')
	;	send(F, reporta, ttlchecka, progress,
		     'Checking formula(see background window for result)'),
	    doprof,
	    (	call(Code)
	    ->	invalidate_holds,
		endprof,
		%sat_format('Formula ~w satisfied~n', [Formula]),
		pl_pce(Formula, FP),
		send(F, report, inform, 'Formula %s satisfied',FP),
	        showprof,
		cleanup_verbose_advanced
	    ;	invalidate_holds,
		endprof,
		%sat_format('Formula ~w not satisfied~n',[Formula]),
		pl_pce(Formula, FP),
		send(F, report, inform, 'Formula %s not satisfied',FP),
		showprof,
		cleanup_verbose_advanced,
		fail
	    )
	).

	


ttl_check_form_old(Formula, Verbose, F) :-
	resettrans,
	(transTermVarsProlog(Formula, [], Term1, _Code)
	->	send(F, report, done, 'DONE')
	;	send(F, report, error,
		     'Compilation failed, see background window for error message'),
		fail
	),
	nl,nl,
	(get_option(compile_only)
	->	true
	;	(get_option(cholds)
		->	adaptcholds
		;	true
		),
		send(F, report, progress, 'Checking formula(see background window for result)'),
		(	formframe:reset_interrupted,
			(	Verbose \== @off
			->	sats_verbose(Term1)
			;	doprof,
				(sats_quiet(Term1)
				->	endprof
				;	endprof, fail
				)
			)
		->	sat_format('Formula ~w satisfied~n', [Formula]),
			pl_pce(Formula, FP),
			send(F, report, inform, 'Formula %s satisfied',FP)
		;	sat_format('Formula ~w not satisfied~n',[Formula]),
			pl_pce(Formula, FP),
			send(F, report, inform, 'Formula %s not satisfied',FP),
			fail
		)
	).




add_command_line_traces_checker(Frame) :-
	add_command_line_traces(Frame, checker_trace_manager(Frame)).

checker_trace_management(Frame, Show) :-
	trace_management(Frame,Show,checker_trace_manager(Frame),_M).



:- pce_begin_class(checker_trace_manager, trace_manager).


invalidate_info(_This) :->
	invalidate_holds.


reload_holds(This) :->
	invalidate_holds,
	send(This, report, progress, '(Re)loading traces'),
	(retract(dyn_trace_sortdef_generated(E))
	->	retract(spec:sortdef('TRACE', E1)),
		update_sort('TRACE'),
		assert_debug(E1 == E)
	;	true
	),
	reset_holds,
	(send(This?list_browser?dict?members, empty)
	->	send(This, report, warning, 'No traces loaded')
	;	true
	),
	send(This?list_browser?dict, for_all, message(@arg1,load_atom_traces)),
	setup_holds,
	send(This, report, progress, 'loaded traces'),
	all_traces(This, Traces),
	ensure_trace_sort(Traces),
	assertz(dyn_holds_up_to_date).
:- dynamic dyn_holds_up_to_date/0.

ensure_loaded_holds(This) :->
	(dyn_holds_up_to_date
	->	true
	;	send(This, reload_holds)
	).
invalidate_holds :-
	retractall(dyn_holds_up_to_date),
	(retract(dyn_trace_sortdef_generated(Elements))
	->	retractall(spec:sortdef('TRACE', Elements)),
		update_sort('TRACE')
	;	true
	),
	(dyn_invalidate_client(Call),
		call(Call),
		fail
	;	true
	).
:- dynamic dyn_invalidate_client/1.

user:add_invalidate_client(Call) :-
	assertz(dyn_invalidate_client(Call)).

reload_holds :-
	unique_trace_manager(M),
	send(M, reload_holds).

ensure_holds_loaded(F) :-
	send(F, reporta, ttlload, progress, 'Loading traces...'),
	(unique_trace_manager(M)
	->	send(M, ensure_loaded_holds)
	;	warning('No traces loaded'),
		reset_holds,
		assert_holds(last_time(-1)),
		ensure_trace_sort([]),
		assertz(dyn_holds_up_to_date)
	),
	send(F, reporta, ttlload, done, 'DONE').
reset_holds :-
	%retractall(holds:atom_trace(_,_,_,_)),
	retractall(holds:holds(_,_,_)),
	retractall(holds:last_time(_)),
	%retractall(holds:times(_,_,_,_)),
	retractall(holds:interval(_,_,_)).

printintervals :-
	holds:interval(_, _, _),
	!,
	format('INTERVALS:~n'),
	format('Nr    ~|   BeginTime~|   EndTime~n'),
	(	holds:interval(I, BegVal, EndVal),
		format('~w~t~w~t~w~n', [I, BegVal, EndVal]),
		fail
	;	true
	).
printintervals :-
	format('No intervals defined (no traces loaded?)~n').

:- dynamic dyn_trace_sortdef_generated/1.

ensure_trace_sort(L) :-
	(	spec:sortdef('TRACE', _Elements)
	->	warning('Builtin sortdef(TRACE, ~w) overruled by your definition', [L])
	;	assertz(spec:sortdef('TRACE', L)),
		update_sort('TRACE'),
		assertz(dyn_trace_sortdef_generated(L))
	).



save_compacted_traces(This) :->
	send(This, ensure_loaded_holds),
	send(This, report, progress, 'Saving compacted traces...'),
	save_compacted_traces,
	send(This, report, done, 'DONE').
	
:- pce_end_class.












:- dynamic dyn_time_step/1.


save_compacted_traces :-
	(setof(TN, trace_name(TN),TraceNames)
	->	(member(TN1, TraceNames),
			save_compacted_trace(TN1),
			fail
		;	true
		)
	;	true
	).

setup_holds :-
	setup_holds1,
	(get_option(savecompactedtraces)
	->	save_compacted_traces
	;	true
	).


valid_holds_term(_TraceName, _Atom) :-
	flag(do_trans_new2, 0, 0),
	!.
valid_holds_term(TraceName, Atom) :-
	atom_state(Atom, TraceName, State, _Time, Atom1),
	Holds = holds(State, Atom1, _TF),
	coded_holds(_Holds1, _PreBoundVars1, Holds, _PreBoundVars,
				_BindingVars, _CHolds).
some_trace(TraceName) :-
	holds:times(TraceName,_TSetup, _THandled, _ET).


setup_holds1 :-
	holds:retractall(holds(_,_,_)),
	holds:retractall(last_time(_)),
	retractall(dyn_time_step(_)),
	(	setof(TraceName1, some_trace(TraceName1), TraceNames),
		member(TraceName, TraceNames),
		(holds:times(TraceName,TSetup, _THandled, ET)
		->	true
		;	fatal_error('Trace contains no time info',
				    [TraceName])
		),
		holds:atom_trace(TraceName, Key, Atom, Ranges),
		valid_holds_term(TraceName, Atom),
		member(range(T1, T2, _V), Ranges),
		(T = T1;T = T2),
		cmp_ge(T,TSetup),
		cmp_le(T, ET),
		ensure_time_step(T),
		fail
	;	true
	),
	(	holds:times(_TraceName,TSetup, _THandled, ET),
		(T = TSetup;T = ET),
		ensure_time_step(T),
		fail
	;	true
	),
	(is_local
	->	(dyn_time_step(T),
			\+ number(T)
		->	local_trace(watnu)
		;	true
		)
	;	true
	),
	setof(T, dyn_time_step(T), Ts1),
	get_time_steps([], Ts),
%	setof(T, retract(dyn_time_step(T)), Ts),
	(	Ts == Ts1
	->	true
	;	warning('Solved bug, result could have changed')
	),
	assign_times(Ts, 0, TCodes),
	(	holds:times(TraceName,TSetup, _THandled, ET),
		holds:atom_trace(TraceName, Key, Atom, Ranges),
		valid_holds_term(TraceName, Atom),
		set_holds_facts(TCodes,TraceName, Atom, Ranges,TSetup,ET),
		fail
	;	true
	),
	closesaveholds.

get_time_steps(TIn, TOut) :-
	(	retract(dyn_time_step(T)),
		get_time_steps(TIn,Ts1),
		insert_time(Ts1, T, TOut)
	;	TOut = TIn
	).
insert_time([], T, [T]).
insert_time([T1|TIn], T2, TOut) :-
	(T1 < T2
	->	insert_time(TIn, T2, TR),
		TOut = [T1|TR]
	;	T1 =:= T2
	->	TOut = [T1|TIn]
	;	TOut = [T2,T1|TIn]
	).

		       
trace_name(TN) :-
	holds:atom_trace(TN, _Key, _Atom,_Ranges).

holds_element(Atom, TF, Time, TraceName) :-
	holds:holds(State, Atom1, TF),
	(State = state(TraceName, Time, IOI)
	->	isolate_ioi(Atom, IOI, Atom1)
	;State = state(TraceName, Time)
	->	Atom = Atom1
	;	fail
	).


save_compacted_trace(TN1) :-
	setof(Atom-Facts,
	      setof(f(TS, TF), holds_element(Atom, TF, TS, TN1), Facts),
	      AF),
	(holds:last_time(LT)
	->	true
	;	impl_error('Missing last_time')
	),
	concat_atom([generated_compacted_trace_,TN1,'.tr'], TrF),
	tell_error(TrF),
	portray_clause(content(type(compactedtrace))),
	LT1 is LT + 1,
	portray_clause(times(0, LT1, LT1)),
	(	member(Atom-Facts,AF),
		compacted_atom_trace(Facts, Atom, AT),
		portray_clause(AT),
		fail
	;	true
	),
	told,
	format('Wrote compacted trace to ~w~n', [TrF]).

compacted_atom_trace(Facts, Atom, atom_trace(AtomKey, Atom, TrTrace)) :-
	atom_key(Atom, AtomKey),
	compacted_ranges(Facts, [], TrTrace).

compacted_ranges([], TIn, TIn).
compacted_ranges([f(TS, TF)|Facts], TrIn, TrOut) :-
	join_same_tf(Facts, TF, TS, Facts1, TL),
	TL1 is TL + 1,
	TR = range(TS, TL1, TF),
	compacted_ranges(Facts1, [TR|TrIn], TrOut).


join_same_tf([f(TS, TF)|Facts], TF, _TSold, Facts1, TL) :-
	!,
	join_same_tf(Facts, TF, TS, Facts1, TL).
join_same_tf(Facts, _TF, TS, Facts, TS).






atom_state(Atom, TraceName, State, Time, Atom1) :-
	(isolate_ioi(Atom, IOI, Atom1)
	->	State = state(TraceName, Time, IOI)
	;	State = state(TraceName, Time),
		Atom1 = Atom
	).

set_holds_facts(TCodes, TraceName, Atom, Ranges, TS, TE) :-
	atom_state(Atom, TraceName, State, Time, Atom1),
	set_holds_facts_ranges(Ranges, TCodes, Time, Atom1, State, TS, TE).

set_holds_facts_ranges([], _TCodes, _Time, _Atom1, _State, _TS, _TE).
set_holds_facts_ranges([range(T1,T2, TFU)|Ranges], TCodes,Time, Atom1,State,
		       TS, TE):-
	set_holds_facts_range(T1,T2, TFU, TCodes,Time, Atom1,State,TS,TE),
	set_holds_facts_ranges(Ranges, TCodes,Time, Atom1,State,TS,TE).

set_holds_facts_range(_T1,_T2, unknown,_TCodes,_Time,_Atom1,_State,_TS,_TE) :-
	!.
set_holds_facts_range(T1,T2, TF, TCodes,Time, Atom1,State,TS,TE) :-
	(cmp_le(T2,TS)
	->	true
	;cmp_ge(T1,TE)
	->	true
	;	max_new(T1, TS, T11),
		min_new(T2, TE, T22),
		get_time_start(TCodes, T11, TCodes1, I),
		set_holds_facts_part(I, TCodes1, T22, TF, Time, Atom1,State)
	).
get_time_start([t(T, I)|TCodes], T1, TCodes1, I1) :-
	(T < T1
	->	get_time_start(TCodes, T1, TCodes1, I1)
	;T = T1
	->	TCodes1 = TCodes,
		I1 = I
	;T =:= T1
	->	TCodes1 = TCodes,
		I1 = I
	;	impl_error('Missing time from coded times:~w(~w)', [T1,
								    T/TCodes])
	).
set_holds_facts_part(I, TCodes1, T2, TF, Time, Atom1,State) :-
	set_holds1(I, TF, Time, Atom1,State),
	(	TCodes1 = [t(Ta, Ia)|TCodes2],
		Ta < T2
	->	set_holds_facts_part(Ia, TCodes2, T2, TF, Time, Atom1,State)
	;	true
	).
set_holds1(I, TF, Time, Atom1,State) :-
	\+ \+ (	I = Time,
		      assert_holds(holds(State, Atom1, TF))
	      ).

assign_times([], _, []) :-
	assert_holds(last_time(-1)).

assign_times([T|Ts], I, [t(T, I)|TCodes]) :-
	(Ts == []
	->	I1 is I - 1,
		assert_holds(last_time(I1)),
		TCodes = []
	;	(Ts = [T1|_]
		->	assert_holds(interval(I, T, T1))
		;	true
		),
		I1 is I + 1,
		assign_times(Ts, I1, TCodes)
	).

ensure_time_step(T) :-
	(dyn_time_step(T)
	->	true
	;	assertz(dyn_time_step(T))
	).

:- dynamic dyn_saveholds_opened/1.

assert_holds(Term) :-
	assertz(holds:Term),
	(get_option(saveholds(SaveFile))
	->	(dyn_saveholds_opened(F1)
		->	assert_debug(F1 == SaveFile)
		;	alog(saveholds,'Opened saveholds file ~w~n',
			     [SaveFile]),
			assertz(dyn_saveholds_opened(SaveFile))
		),
		telling(P),
		(tell_error(SaveFile)
		->	(dyn_saveholds_opened(F1)
			->	assert_debug(F1 == SaveFile)
			;	alog(saveholds, 'Opened saveholds file ~w~n',
				     [SaveFile]),
				assertz(dyn_saveholds_opened(SaveFile))
			),
			portray_clause(Term)
		;	warning('option saveholds(~w) ignored', [SaveFile]),
			reset_option(saveholds(SaveFile))
		),
		tell_error(P)
	;	true
	).










