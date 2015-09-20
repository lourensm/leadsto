:- module(formats, [fmessage/3,ferror/3]).
:- use_module(library(pce)).
:- use_module(library(lists)).



% Next error is always used to check whether error base is loaded
% please leave it
error(dummy_error,     error,   '%N:dummy error').
error(displaytrace_no_file,     warning,
      'Ignoring option -displaytrace:No file specified').

ferror(gui_option_syntax, '-gui ~w: argument must be (quoted) prolog term',1).
ferror(tell, 'Could not write to file ~w',1).

ferror(initial_load_spec,
       'Could not load simulation specification "~w"', 1).
ferror(load_trace(load, error),
       'Could not load trace file "~w"', 1).
ferror(ltbare_exception,
       'Unexpected error in call ~w', 1).
ferror(runops_exception,
       'Unexpected error in call ~w', 1).
ferror(leadsto_exception,
       'Unexpected error in call ~w', 1).
ferror(lteditor_exception,
       'Unexpected error in call ~w', 1).
ferror(checkereditor_exception,
       'Unexpected error in call ~w', 1).
ferror(ttlchecker_exception,
       'Unexpected error in call ~w', 1).
ferror(not_a_variable,
       'Expected a variable Var:Sort, got ~w', 1).
ferror(prolog_var,
       'Got a prolog variable, did you forget to quote uppercase element:~w',
       1).
ferror(num_op_args_non_num,
       'Argument(s) of numeric operator not number(s):~w',1).
ferror(var_sort_mismatch,
       'Variable mismatch for ~w', 1).
ferror(dupl_var_def,'Duplicate variable ~w',1).
ferror(undefined_sort, 'Undefined sort "~w"', 1).
ferror(instantiating_infinite_sort,
       'Cannot start instantiating infinite sort ~w', 1).
ferror(between_argument_not_int1,
       'First rgument of ~w should be integer', 1).
ferror(between_argument_not_int2,
       'Second rgument of ~w should be integer', 1).


/*
error(writing_ps,      progress, '%N:Writing picture to `\%s''...').
error(writing_done,    done,     '%N:done').
*/
fmessage(load_trace(load, start), 'Loading trace file "~w"...', 1).
fmessage(load_trace(load, end), 'Loading trace file "~w" done.', 1).

load_formats :-
	get(@errors, member, dummy_error, _),
	!.

load_formats :-
	(	( /*      predicate_property(error(_,_,_,_), interpreted),
			error(Id, Kind, Format, FeedBack)
		;*/	predicate_property(error(_,_,_), interpreted),
			error(Id, Kind, Format),
			FeedBack = @default
		),
		new(_, error(Id, Format, Kind, FeedBack)),
		fail
	;	true
	).

:- initialization load_formats.
