
:- module(form_config, []).
:- use_module(library(pce)).
:- use_module(library(pce_config)).

:- use_module(nodes).
:- use_module(util).

%:- use_module(ttlchecker).


:- require([ absolute_file_name/3
	   , broadcast_request/1
	   , file_directory_name/2
	   ]).






config_info(App, File) :-
	get_domain(Domain),
	checking_enabled(TF),
	config_info(Domain, TF, App, File).
config_info(checker, true, 'TtlChecker', 'TtlChecker').
config_info(checker, false, 'TtlEditor', 'TtlEditor').
config_info(leadsto, false, 'LeadsToEditor', 'LeadsToEditor').

config(Path, Attributes) :-
	broadcast_request(config(form_config:Path, Attributes)).
config(config/file,
       [ default(File)
       ]) :-
	config_info(_App, File).

config(config/application,
       [ default(App)
       ]) :-
	config_info(App, _File).


config(options/cholds,
       [type(bool),
	 comment('Perform indexing on holds facts'),
	 default(true)
	]) :-
	current_module(ttlchecker),
	transold,
	checking_enabled(true).

config(options/profiling,
       [type({plain,cumulative,off}),
	 comment('Give performance profile'),
	 default(off)
	]) :-
	checking_enabled(true).
config(options/saveholds,
       [ type(name),
	 comment(['Save compacted interval trace used in each check in file(empty string then no save)']),
	 default('')]) :-
	checking_enabled(true).

config(options/add_trace_file,
       [ type(name),
	 comment(['Add this trace file at startup to traces']),
	 default('')]) :-
	checking_enabled(true).

config(print/printer,
       [ type(name),
	 comment(['Name of the default printer.  May be $VARIABLE', 'Fill in empty string if no printer']),
	 default(DefPrinter)
       ]) :-
	\+ has_printer_class,
	(   get(@pce, environment_variable, 'PRINTER', _DefPrinter)
	->  DefPrinter = '$PRINTER'
	;   DefPrinter = ''
	).
config(print/print_command,
       [ type(name),
	 comment(['Command to send PostScript file to printer.\n',
		  '%p refers to the printer, %f to the (temporary) ',
		  'PostScript file\n', 'Fill in empty string if no printer']),
	 default('lpr -P%p %f')
       ]) :-
	\+ has_printer_class.
config(print/print_command_no_p,
       [ type(name),
	 comment(['Command to send PostScript file to printer if no printer defined.\n',
		  '%f refers to the (temporary) PostScript file']),
	 default('lpr %f')
       ]) :-
	\+ has_printer_class.
config(debugging/local,
       [ type(bool),
	 comment('debugging mode "local"'),
	 default(false)
       ]).

config(debugging/g,
       [ type(bool),
	 comment('debugging mode -g'),
	 default(false)
       ]).
/*
config(tree/node/color,
       [ type(colour),
	 comment('Node background colour'),
	 default(white)
       ]).
config(tree/node/font,
       [ type(font),
	 comment('Node font'),
	 default(font(helvetica, roman, 12))
       ]).
*/
config(tree/menu_items, [type(name), comment('Maximum number of menu entries vertical.(1000:one column)'),
			 default(1000)]).
config(tree/background, [type(colour), comment('Picture background'),
			 default(white)]).
config(tree/level_gap, [type(name), comment('Distance between a node and its parents or sons'),
			 default(50)]).
config(tree/link_gap, [type(name), comment('Distance between image and line.(does nothing?)'),
			 default(2)]).
config(tree/neighbour_gap, [type(name), comment('Distance between two nodes that are at the same level.'),
			 default(0)]).
config(tree/link_pen, [type(name), comment('Line width.'),
			 default(1)]).
config(tree/link_colour, [type(colour), comment('Line width.'),
			 default(black)]).
config(tree/node/Prop, Config) :-
	node_prop_options(Prop, Config).

config(history/geometry/main_window,
       [ type(geometry),
	 comment('(X-)geometry specification of main window'),
	 editable(false),
	 default('600x600')
       ]).





:- register_config(config).
















