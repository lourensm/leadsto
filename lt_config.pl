
:- module(lt_config, []).
:- use_module(library(pce)).
:- use_module(library(pce_config)).


:- use_module(util).



:- require([ absolute_file_name/3
	   , broadcast_request/1
	   , file_directory_name/2
	   ]).






config_info(App, File) :-
	App = 'LeadsTo',
	File = App.

config(Path, Attributes) :-
	broadcast_request(config(lt_config:Path, Attributes)).
config(config/file,
       [ default(File)
       ]) :-
	config_info(_App, File).

config(config/application,
       [ default(App)
       ]) :-
	config_info(App, _File).


config(options/aap,
       [type(bool),
	 comment('Perform indexing on holds facts'),
	 default(true)
	]) :-
	true.
/*
	current_module(ttlchecker),
	ttlchecker:transold,
	checking_enabled(true).
*/
config(options/profiling,
       [type({plain,cumulative,off}),
	 comment('Perform indexing on holds facts'),
	 default(off)
	]) :-
	true.
config(display/diagram/height,
       [type(name),
	comment('Number of pixels per diagram'),
	default(200)]).
config(display/diagram/true_pen,
       [type(name),
	comment('Width of true lines'),
	default(4)]).
config(display/diagram/false_pen,
       [type(name),
	comment('Width of false lines'),
	default(4)]).
config(display/diagram/unknown_pen,
       [type(name),
	comment('Width of unknown lines'),
	default(1)]).
config(graph/background,
       [type(colour), comment('Graph picture background'),
			 default(navajowhite)]).
config(graph/half_time_fraction,
       [type(name), comment('Communication half_time as fraction of total trace time'),
			 default(0.02)]).
config(graph/link/unit_width,
       [type(name), comment('Width of communication connection right after an initial communication takes place'), default(4)]).
config(graph/link/min_length,
       [type(name), comment('Minimum ideal length of a connection when there is a maximal communication along the link'),default(10)]).
config(graph/link/min_length_strength,
       [type(name), comment('The communication strength at and above which min_distance ideal length will hold'),default(5)]).
config(graph/link/max_length,
       [type(name), comment('Ideal length of a connection when strength would be 0, length is linear in strength'),default(30)]).
config(graph/node/diameter,
       [type(name),comment('Diameter of a role node(number > 0)'),
	default(30)]).
config(graph/node/communication_factor,
       [type(name),comment('Diameter increment fraction for one communication strenth unit'),
	default(0.33)]).
config(graph/node/fill_colour,
       [type(colour),comment('Fill colour of role node'),
	default(red)]).
config(graph/node/pen_colour,
       [type(colour),comment('Line colour of role node'),
	default(blue)]).
config(graph/node/pen,
       [type(name),comment('Border pen width of role node'),
	default(1)]).
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

/*
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
*/
config(history/geometry/main_window,
       [ type(geometry),
	 comment('(X-)geometry specification of main window'),
	 editable(false),
	 default('1022x316')
       ]).

config(history/geometry/graph_window,
       [ type(geometry),
	 comment('(X-)geometry specification of graph window'),
	 editable(false),
	 default('1024x768')
       ]).



:- register_config(config).
















