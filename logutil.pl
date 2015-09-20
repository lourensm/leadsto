:- module(logutil,
	  [
	   reset_logging/1,
	   set_logging/1,
	   setup_logging/2,
	   do_log/1,
	   alog/3,
	   alog/2
	   ]).

:- use_module(util).
:- use_module(library(pce)).
:- use_module(library(lists)).

% Define flags for logging, define loging io
% define command line options and gui options

% log_entry(E, Descr, Local,OnOff)
%
% setup_logging(AppicationName, ModuleList)

:- dynamic dyn_logging/2.

setup_logging(AppicationName, ModuleList) :-
	(dyn_logging(_AppicationName1, _ModuleList1)
	->	pr_error('Multiple logging setup')
	;	assertz(dyn_logging(AppicationName, ModuleList)),
		(dyn_log(_)
		->	pr_error('Logging already setup')
		;	true
		),
		log_entry(E, Descr, Local,OnOff),
		(	memberchk(OnOff, [on, off])
		->	(OnOff == on
			->	log_set(E, @on)
			;	true
			)
		;	pr_error('Logentry last argument not oin {on,off}:~w',
				 [log_entry(E, Descr, Local,OnOff)])
		),
		fail
	;	true
	).

def_log_entry(E, Descr, Local,OnOff) :-
	log_entry(E, Descr, Local,OnOff1),
	(	dyn_log(_)
	->	(do_log(Local)
		->	OnOff = on
		;	OnOff = off
		)
	;	memberchk(OnOff1, [on, off])
	->	OnOff = OnOff1
	;	pr_error('Logentry last argument not oin {on,off}:~w',
			 [log_entry(E, Descr, Local,OnOff)])
	).

log_entry(E, Descr, Local) :-
	log_entry(E, Descr, Local, _OnOff).

log_entry(E, Descr, Local, OnOff) :-
	dyn_logging(_AppicationName, ModuleList),
	member(Module, ModuleList),
	predicate_property(Module:log_entry(_, _, _, _), interpreted),
	Module:log_entry(E, Descr, Local, OnOff).

:- pce_global(@log_settings, mk_log_settings).
mk_log_settings(D) :-
	(dyn_logging(_AppicationName, _ModuleList)
	->	true
	;	pr_error('No logging setup done')
	),
	new(D, dialog('Logging settings')),
	send(D, append, new(M, menu(logging, toggle))),
	send(M, layout, vertical),
	send(M, message, message(@prolog, log_set, @arg1, @arg2)),
	(	def_log_entry(E, Descr, _Local,OnOff),
		send(M, append, new(MI,menu_item(E, @default, Descr))),
		send(MI, selected, @OnOff),
		fail
	;	true
	),
	send(D, append, button(dismiss, message(D, show, @off))).
do_log(X) :-
	dyn_log(X).
:- dynamic dyn_log/1.

reset_logging(Opt) :-
	log_entry(Opt, _Doc, A),
	!,
	(dyn_log(A)
	->	retractall(dyn_log(A))
	;	true
	),
	(dyn_log(_)
	->	true
	;	assertz(dyn_log(nothing))
	),
	reguilogging.

set_logging(X) :-
	assert_debug(ground(X)),
	fail.

set_logging(all) :-
	!,
	retractall(dyn_log(_)),
	assertz(dyn_log(_)),
	reguilogging.

set_logging(nothing) :-
	retractall(dyn_log(_)),
	assertz(dyn_log(nothing)),
	reguilogging.
set_logging(Opt) :-
	log_entry(Opt, _Doc, A),
	!,
	(	dyn_log(X),
		var(X)
	->	retractall(dyn_log(X))
	;	dyn_log(nothing)
	->	retractall(dyn_log(nothing))
	;	true
	),
	assertz(dyn_log(A)),
	reguilogging.

set_logging(help) :-
	!,
	format(user_error,'leadsto|ttlchecker|ltbare -log OPTION ...~n', []),
	format(user_error,'    all   : everything~n',[]),
	format(user_error,'    nothing : minimise logging~n',[]),
        format(user_error,'leadsto -nolog OPTION ...~n', []),
	(	log_entry(Opt, Doc, _),
		format(user_error,'    ~w : ~w~n', [Opt, Doc]),
		fail
	;	true
	),
	finalhalt(0).
set_logging(Opt) :-
	error('Ignoring unrecognised -log Option ~w', [Opt]).




%:- debug(logutil).
alog(What, Format) :-
	(do_log(What)
	->	log(Format)
	;	true
	).
alog(What, Format, Args) :-
	(do_log(What)
	->	log(Format, Args)
	;   debugging(logutil)
	->  debug(logutil, Format, Args)
	;	true
	).




log_set(A1, A2) :-
	(log_entry(A1, _Doc, _A)
	->	true
	;	warning('Unrecognised logging option ~w', [A1])
	),
	(A2 == @on
	->	set_logging(A1)
	;A2 == @off
	->	reset_logging(A1)
	;	pr_error('Problem handling logging option ~w', log_set(A1, A2))
	).
reguilogging :-
	(object(@log_settings)
	->	doreguilogging(@log_settings)
	;	true
	).
doreguilogging(D) :-
	get(D, member, logging, M),
	(	log_entry(E, _Descr, Local),
		get(M, member, E, MI),
		(	do_log(Local)
		->	send(MI, selected, @on)
		;	send(MI, selected, @off)
		),
		fail
	;	true
	).








