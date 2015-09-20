:- module(psprinting,[print_canvas/1,
		      postscript_print/4,
		      print_postscript/2,
		      postscript_file/2,
		      mk_postscript_popup/1,
		      setup_win_printer/3
		     ]).
:- use_module(library(pce)).
:- use_module(util).
:- use_module(library(lists)).
:- use_module(library(pce_shell)).

:- multifile user:version/2.


user:version(psprinting,1).



:- pce_begin_class(postscript_printing, object).

variable(report_to, object, both).
variable(ps_object, object, both).

:- pce_global(@ps_printing, new(postscript_printing)).


do_object(This, Operation:name, Object:object, Receiver:object) :->
	send(This, report_to, Receiver),
	(send(Object, instance_of, picture)
	->	Object1 = Object
	;	get(Object, frame, F),
		get(F, member, ps_picture, Object1)
	->	true
	;	Object1 = Object
	),
	send(This, ps_object, Object1),
	send(This, Operation).

do(This, Operation:name, Receiver:object) :->
	send(This, do_object, Operation, Receiver, Receiver).


default_psfile(_This, Name:[name]) :<-
				% Bug in windows version
       (       is_win32,
	       current_prolog_flag(version, V),
	       V =< 50209
       ->      Name = @default
       ;       Name = 'scratch.eps'
       ).



postscript1(This) :->
	"Write PostScript to default file"::
	get(This, default_psfile, File),
	send(This, generate_postscript, File).


postscript_as(This) :->
	"Write PostScript to file"::
	get(This, default_psfile, DefFile),
	(current_prolog_flag(version, V),
		V >= 50108
	->	DefFile1 = DefFile
	;	DefFile1 = @default
	),
	get(@finder, file, @off, '.eps', @default, DefFile1, FileName),
	send(This, generate_postscript, FileName).


postscript_print(This, PsFile, What, Device) :-
	send(PsFile, open, write),
	send(This, report, status, 'Writing %s to "%s"...', What, PsFile?name),
	(catch(send(PsFile, append, Device?postscript), E,
	       prerror(This,E))
	 ->	send(PsFile, append, 'showpage'),
		send(PsFile, newline),
		send(PsFile, close),
		send(This, report, inform, 'Wrote %s to `%s''...',
		    What, PsFile?base_name)
	;	send(This, report, error,
			'Could not write %s to `%s''',What,
			PsFile?base_name),
		fail
	).

generate_postscript(This, PsFile:file) :->
	"Write PostScript to named file"::
	get(This, ps_object, Device),
	postscript_print(This, PsFile, picture, Device).


try_win_print(This) :->
	get(This, ps_object, Canvas),
	print_canvas(Canvas).

setup_win_printer(Canvas, Device, Prt) :-
	Job = scratch,
	new(Prt, win_printer(Job)),
	send(Prt, setup, Canvas),
	send(Prt, open),
	get(Device, bounding_box, area(X, Y, W, H)),
	get(@display, dots_per_inch, size(DX, DY)),
	InchW is W/DX,
	InchH is H/DY,
	get(Prt, size, size(PW0, PH0)),
	get(Prt, dots_per_inch, size(RX, RY)),
	MarX is RX,			% default 1 inch margins
	MarY is RY,
	PrInchW is (PW0-MarX*2)/RX,
	PrInchH is (PH0-MarY*2)/RY,
	send(Prt, map_mode, isotropic),
	(   InchW < PrInchW,
	    InchH < PrInchH		% it fits on the page
	->  OX is MarX + ((PrInchW-InchW)/2)*RX,
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(OX, MarY, RX, RY))
	;   Aspect is min(PrInchW/InchW, PrInchH/InchH),
	    ARX is integer(Aspect*RX),
	    ARY is integer(Aspect*RY),
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(MarX, MarY, ARX, ARY))
	),!.
setup_win_printer(Canvas, _Device, _Prt) :-
	send(Canvas, report, error, 'Could not connect to printer'),
	fail.


print_canvas(Canvas) :-
	get(@pce, convert, win_printer, class, _), !,
	setup_win_printer(Canvas, Canvas, Prt),
	%get(Canvas, default_file, Job),
	send(Prt, draw_in, Canvas?graphicals),
	send(Prt, close),
	free(Prt).

prerror(This,E) :-
	term_to_atom(E, E1),
	send(This, error, caught_exeception, E1),
	fail.

%    format('Caught exception ~w~n', [E]),
%    fail.

print_postscript(This, Picture) :-
	config_mod(Mod),
	print_postscript(Mod, This, Picture).

print_postscript(Module, This, Picture) :-
	new(PsFile, file),
	send(PsFile, open, write),
	send(PsFile, append, Picture?postscript),
	send(PsFile, append, 'showpage'),
	send(PsFile, newline),
	send(PsFile, close),
	get(PsFile, absolute_path, File),
	(	(default_printer(Module, Printer),
			Printer \== ''
		->	(	get_config(Module:print/print_command,
					   CmdTempl)
			->	true
			;	CmdTempl = 'lpr -P%p %f'
			),
			print_cmd(CmdTempl, Printer, File, Cmd)
		;	(	get_config(Module:print/print_command_no_p,
					   CmdTempl)
			->	true
			;	CmdTempl = 'lpr %f'
			),
			print_cmd(CmdTempl, File, Cmd),
			Printer = ''
		)
	->	(shell(Cmd)
		->	(Printer = ''
			->	send(This, report, status,
			      'Sent to printer')
			;       send(This, report, status,
				     'Sent to printer "%s"', Printer)
			),
			send(PsFile, remove),
			send(PsFile, done)
		;	send(This, report, error,
			     'Printing failed(%s)', Cmd),
			send(PsFile, remove),
			send(PsFile, done),
			fail
		)
	;	send(This, report, error,
			     'Printing failed(missing printer configuration)'),
		send(PsFile, remove),
		send(PsFile, done),
		fail
	).
% Copy/paste from draw.pl
print_cmd(Template, Printer, File, Cmd) :-
	new(S, string('%s', Template)),
	substitute(S, '%p', Printer),
	substitute(S, '%f', File),
	get(S, value, Cmd),
	free(S).
print_cmd(Template, File, Cmd) :-
	new(S, string('%s', Template)),
	substitute(S, '%f', File),
	get(S, value, Cmd),
	free(S).
substitute(S, F, T) :-
	new(R, regex(F)),
	send(R, for_all, S,
	     message(@arg1, replace, @arg2, T)),
	free(R).

postscript_file(File, Picture) :-
	print_postscript_file0(File, Picture),
	send(@pce, format,
		     'Saved picture in file `%s''\n', File).

print_postscript_file0(File, Picture) :-
	new(PsFile, file(File)),
	send(PsFile, open, write),
	send(PsFile, append, Picture?postscript),
	send(PsFile, append, 'showpage'),
	send(PsFile, newline),
	send(PsFile, close).

print_postscript(This) :->
	"Send to default printer"::
	get(This, ps_object, Picture),
	print_postscript(This, Picture).


default_printer(Module, Printer) :-
	get_config(Module:print/printer, Printer0),
	Printer0 \== @default, !,
	(   get(Printer0, scan, '$%[a-zA-Z0-9_]', vector(VarName)),
	    get(@pce, environment_variable, VarName, Printer)
	->  true
	;   Printer = Printer0
	),!.
default_printer(_, Printer) :-
	get(@pce, environment_variable, 'PRINTER', Printer),
	!.






:- pce_global(@postscript_popup, mk_postscript_popup).

do_ps(A,B):-
	send(@ps_printer, do, A, B).



mk_postscript_popup(Popup) :-
	new(Popup, popup(printing)),
	send(Popup, message, message(@ps_printing, do,
				     @arg1, @event?receiver)),
%	send(Popup, message, message(@prolog, do_ps, @arg1, @event?receiver)),
	(    is_win32,
             get(@pce, convert, win_printer, class, _)
        ->     send_list(Popup, append, [
                        menu_item(try_win_print,@default,'Print...'),
				  menu_item(postscript_as,@default,
					    'Postscript As...'),
				  menu_item(print_postscript,@default,
					    'Unsupported Postscript to printer')
				 ]
		 )
        ;	send_list(Popup, append, [menu_item(postscript1,@default,
					    'Postscript'),
				  menu_item(postscript_as,@default,
					    'Postscript As...'),
				  menu_item(print_postscript,@default,
					    'Postscript to printer')
				 ]
		 )
        ).

:- pce_end_class.


:- use_module(library(pce_helper)).

:- pce_global(@ps_picture_popup, mk_ps_picture_popup).

mk_ps_picture_popup(Popup) :-
	mk_postscript_popup(Popup),
	send(Popup, append, menu_item(advanced,
					  message(@prolog, open_ps_picture,
						  @event?receiver?frame),
				      'Advanced...')).

open_ps_picture(Frame) :-
	send(?(Frame?members,find,message(@arg1, instance_of,ps_picture)),
	     open_advanced).


:- pce_begin_class(advanced_dialog, dialog).

variable(ps_picture, ps_picture, both).

variable(pagesize_menu, menu, both).
variable(where_menu, menu, both).

variable(formfactor_ti, text_item, both).
variable(file_ti, text_item, both).

pagesize_set(This) :->
	get(This?pagesize_menu, selection, S),
	(S == a4
	->	send(This?formfactor_ti, selection, 1.41),
		send(This?formfactor_ti, editable, @off)
	;S == single_page
	->	send(This?formfactor_ti, editable, @off)
	;	send(This?formfactor_ti, editable, @on)
	).
where_set(This) :->
	get(This?where_menu, selection, S),
	(no_file_where(S)
	->	send(This?file_ti, editable, @off)
	;	send(This?file_ti, editable, @on)
	).

initialise(This, ST:ps_picture) :->
	send(This, send_super, initialise, 'Advanced Printing'),
	send(This, ps_picture, ST),
	new(M, menu('PageSize:', choice)),
	send_list(M, append, [a4,single_page,specified]),
	send(This, pagesize_menu, M),
	send(M, message, message(This, pagesize_set)),
	send(This, append, M),
	send(new(TI, text_item('Specified Formfactor:', 1.41)), below, M),
	send(This, formfactor_ti, TI),
	send(new(M1, menu('Whereto:', choice)), below, TI),
	send(M1, message, message(This, where_set)),
	send(This, where_menu, M1),
	(get(@pce, convert, win_metafile, class, _)
	->	L = [wmf_files]
	;	L = []
	),
	(get(@pce, convert, win_printer, class, _)
	->	L0 = [win_printer|L]
	;	L0 = L
	),
	(is_win32
	->	L1 = L0
	;	L1 = [postscript_printer|L0]
	),
	send_list(M1, append,
		  [screen,postscript_files|L1]),
	send(new(TI2, text_item('FilePrefix:', scratch)), below, M1),
	send(This, file_ti, TI2),
	send(new(B, button(activate)), below, TI2),
	send(new(Rep, label(reporter)), below, B),
	send(new(CB,button(dismiss, message(This, show, @off))), below, Rep),
	send(This, resize_message, message(CB, center_x, This?center_x)),
	send(This, pagesize_set),
	send(This, where_set).
no_file_where(Option) :-
	member(Option, [screen, postscript_printer, win_printer]).


activate(This) :->
	get(This?pagesize_menu, selection, S),
	get(This?where_menu, selection, Where),
	(S == specified
	->	get(This?formfactor_ti, selection, Factor),
		(number(Factor),
			Factor > 0
		->	true
		;	send(This, report, error, 'Specified Formfactor should be number > 0'),
			fail
		)
	;	S == single_page
	->	Factor = default
	;	Factor = a4
	),
	(no_file_where(Where)
	->	W1 = Where
	;	get(This?file_ti, selection, File),
		W1 =.. [Where,File]
	),
	Opts = paging(Factor, W1),
	send(This?ps_picture, paging, Opts),
	send(This?ps_picture, setup_display),
	send(This?ps_picture, activate_print),
	send(This?ps_picture, close_pages).




:- pce_end_class.

:- pce_begin_class(ps_picture, picture).

clear(This, How:[{destroy,free, erase}]) :->
	send(This, send_super, clear, How).

variable(paging,  prolog := unset, both).
paging_print_to_screen(default).
paging_print_to_screen(paging(_Size,Destination)) :-
	memberchk(Destination, [default,screen]).

paging_size_info(default, default).
paging_size_info(paging(Size,_Destination), Size).


setup_pages(This) :->
        (send(This, print_to_screen)
	->	send(This, clear)
	;	send(This, reset_pages)
	).

print_to_screen(This) :->
	get(This, paging, P),
	paging_print_to_screen(P).

setup_display(This) :->
	(send(This, print_to_screen)
	->	send(This, no_dev)
	;	send(This, clear_dev)
	).

max_integer(Max) :-
	(   current_prolog_flag(max_integer, Max)
	->  true
	;   Max = 10000000000
	).
page_length_pixels(This, Pixels:int) :<-
        get(This, paging, P1),
	paging_size_info(P1,P),
	(P == default
	->      max_integer(Pixels)
	;	number(P) % 1.41 -> A4
	->	get(This, max_x, MX),
		Pixels is MX*abs(P)
	;	P == a4
	->	get(This, max_x, MX),
		Pixels is MX*sqrt(2)
	;	error('Paging option ~w not handled', [P]),
		max_integer(Pixels)
	).

close_pages(This) :->
	(page_destination(This, win_printer)
	->	get(This, win_printer, Prt),
		get(Prt, job, J),
		send(Prt, close),
		send(This, win_printer, @nil),
		free(Prt),
		send(This?advanced_dialog, report, inform,
		     'Printout done (Job %s)', J)
	;	true
	).
out_page_file(This) :->
	get(This, page_file_name, F),
	new(MF, win_metafile),
	send(MF, draw_in, This?display_chain),
	send(MF, save, F),
	send(This, next_page).
next_page(This) :->
	send(This, page_no, This?page_no + 1),
	send(This, clear_dev).

no_dev(This) :->
	(	get(This, print_dev,Dev),
		Dev \== @nil
	->	send(Dev, free),
		send(This, print_dev, @nil)
	;	true
	).
page_file(This, FilePrefix:char_array, Ext:char_array, FileName:name) :<-
        new(FileName, string('%s%s%s', FilePrefix, This?page_no, Ext)).

page_destination(This, Dest) :-
	get(This, paging, P),
	P = paging(_Size,Dest).
print_page(This) :->
	(page_destination(This, Destination)
	->	(	Destination = postscript_files(FilePrefix)
		->	get(This, page_file, FilePrefix, '.ps', PsFile),
			get(This, print_dev, Device),
			new(F, file(PsFile)),
			postscript_print(This, F, postscript_page, Device)
		;	Destination = wmf_files(FilePrefix)
		->	get(This, page_file, FilePrefix, '.wmf', PsFile),
			(	new(MF, win_metafile),
				send(MF, draw_in, This?print_dev?graphicals),
				send(MF, save,  PsFile)
			->	send(This, report, inform,
				     'Wrote %s to `%s''...',
				     wmf_page, PsFile)
			;	send(This, report, error,
				     'Could not write %s to `%s''...',
				     wmf_page, PsFile),
				fail
			)
		;	Destination == win_printer
		->	send(This?win_printer, draw_in, This?print_dev?graphicals)
		;	fail
		)
	->	send(This, next_page)
	;	get(This, paging, P),
		error('Unsupported printing option ~w',[P]),
		fail
	).

clear_dev(This) :->
	send(This, no_dev),
	send(This, print_dev, new(device)).
display(This, G:graphical, Pos:[point]) :->
	(	get(This, print_dev, DC),
		DC \= @nil
	->	send(DC, display, G, Pos)
	;	send(This, send_super, display, G, Pos)
	).

reset_pages(This) :->
	send(This, page_no, 1),
	send(This, clear_dev),
	(page_destination(This, win_printer)
	->	setup_win_printer(This, This, Prt),
		send(This, win_printer, Prt)
	;	true
	).

variable(print_dev, device*, both).
variable(page_no, int := 0, both).
variable(win_printer, win_printer*, both).


initialise(This, Label:[name]) :->
	send(This, send_super, initialise, Label),
	send(This, popup, @ps_picture_popup).
variable(advanced_dialog, advanced_dialog*, both).

open_advanced(This) :->
	(get(This, advanced_dialog, AD),
		AD \= @nil
	->	true
	;	send(This, advanced_dialog, new(AD,advanced_dialog(This)))
	),
	send(AD, expose).

activate_print(This) :->
	send(This, report, error, 'Advanced printing not supported yet').


:- pce_end_class.






