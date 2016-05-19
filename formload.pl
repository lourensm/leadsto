:- module(formload,
	  [
	   load_spec/2,
	   save_spec/2,
	   lteditorspecloaded/1,
	   term_to_formula_node/3,
	   enable_comment/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
%:- use_module(pce_boot(pce_operator)).
:- use_module(util).
:- use_module(nodes).


:- dynamic dyn_content/1.


load_spec(File, This) :-
	get(This?spec_tree, root, Root),
	send(Root, delete_all_sons),
	load_spec_node(File, Root, This).

catch_read_error(ET) :-
	print_message(error, ET),
	warning('Ignoring entry'),
	fail.

ttt :-
	set_prolog_flag(iso, true),
	open('tsto.lt', read, _, [alias(spec)]),
	push_form_ops,
	read_term(spec, Term,[subterm_positions(TP),
				term_position(TP1),variable_names(Vars),
			     comments(Comments)]),
	pop_operators.
load_spec_node(File, Root, This) :-
	retractall(dyn_content(_)),
	open(File, read, _, [alias(spec)]),
	(enable_comment
	->	open(File, read, _,[alias(ac)]),
		flag(last_end, _, 0)
	;	true
	),
	push_form_ops,
	reset_warned,
	ReadEnv = [file(File), this(This)],
	(repeat,
		send(@display, synchronise),

		read_with_comment(Root,ReadEnv, Term, Vars, LineNo, Status),
		(Status == end_of_file
		->	!
		;Status == error
		->	!,fail
		;	handle_read(Term, Vars, LineNo, File, Root, This),
			fail
		)
	->	pop_operators,
		close(spec),
		(enable_comment
		->	close(ac)
		;	true
		)
	;	pop_operators,
		close(spec),
		(enable_comment
		->	close(ac)
		;	true
		),
		fail
	).

%  old comment handling when read_term(..[comments(..)]) wasnt avalable
enable_comment :-
	current_prolog_flag(version, V),
	V =< 50210.
enable_comment_new :-
	current_prolog_flag(version, V),
	V > 50300.


%  TP1 start-position of Term
read_term1(Stream, Term, ExtraOptions, Start, End, TP1) :-
	stream_property(Stream, file_name(F)),
	stream_property(Stream, position('$stream_position'(CharPos,LineNo,LinePos))),
	catch(
	read_term(Stream, Term,[subterm_positions(TP),
				term_position(TP1)|ExtraOptions]),
	      E, recover_read_term_error(E,F,CharPos,LineNo,LinePos)),
	TP1 = '$stream_position'(SP,_,_),
	get_start_end(TP, Start, End),
	(SP == Start
	->	true
	;	trace
	).
get_start_end(Start - End, Start, End).
get_start_end(string_position(Start, End), Start, End).
get_start_end(brace_term_position(Start, End,_), Start, End).
get_start_end(list_position(Start, End, _,_), Start, End).
get_start_end(term_position(Start, End, _, _, _), Start, End).
recover_read_term_error(E,F,_CharPos,LineNo,LinePos) :-
	format(user_output, 'While reading FILE ~w, Line ~w, Position:~w:~n',
	       [F,LineNo, LinePos]),
	print_message(error, E),
	fail.


%
%
%     Probably, the comment that gets handled is the comment before
read_with_comment(Root,ReadEnv,Term, Vars, LineNo, Status) :-
	enable_comment_new,
	!,
	read_term(spec, Term,[subterm_positions(TP),
				term_position(TP1),variable_names(Vars),
			     comments(Comments)]),
	stream_position_data(char_count, TP1, LineNo),
	get_start_end(TP, StartTerm, EndTerm),
	handle_comments_new(Comments, Root, ReadEnv, StartTerm, EndTerm),
	(   Term == end_of_file
	->  Status = end_of_file
	;   Status = ok
	).


read_with_comment(Root,_ReadEnv,Term, Vars, LineNo, Status) :-
	enable_comment,
	!,
	flag(last_end, End, End),
	set_stream_position(ac, '$stream_position'(End,0,0)),
	(	read_term1(spec, Term, [variable_names(Vars)], Start1,End1,Pos),
		ensure_pos(Pos, LineNo)
	->	% Make comment stream point to end of previous term
		% i.e. skip until . of previous term, probably already almost there
		(	End \== 0,
			repeat,
			get_byte(ac,B0),
			(	B0 == 46 % full_stop
			->	!,fail
			;	B0 == -1
			->	!,error('Missing end of term')
			;	fail
			)
		->	error('Missing dot in term'),
			Status = error
		;	new(S, string),
			repeat,
			get(S, size, SL),
			stream_property(ac,position('$stream_position'(CP3,_,_))),
			(	CP3 >= Start1
			->	!,
				handle_comment(Root,S),
				(	Term == end_of_file
				->	Status = end_of_file
				;	Status = ok,
					flag(last_end, _, End1)
				)
			;	get_byte(ac,B1),
				(	B1 == -1
				->	format('EFF~n'),
					Status = end_of_file,
					handle_comment(Root,S),
					!
				;	memberchk(B1, [13,10]),
					SL == 0
				->	fail
				;	send(S, insert_character, B1),
					fail
				)
			)
		;	Status = error
		)
	;	Status = error
	).



read_with_comment(_Root,_ReadEnv, Term, Vars, LineNo, Status) :-
	(catch(read_term(spec, Term, [variable_names(Vars),
				     term_position(Pos)]),
	      ET, catch_read_error(ET))
	->	(Term==end_of_file
		->	Status = end_of_file
		;	Status = ok
		),
	        ensure_pos(Pos, LineNo)
	;	Status = error
	).
rm_comment_delimiters(Comment, CommentBare) :-
	atom_concat('/*', C1, Comment),
	atom_concat(CommentBare, '*/', C1).

handle_comments_new([], _Root, _ReadEnv, _StartTerm, _EndTerm).
handle_comments_new([StartPos-Comment|Comments], Root, ReadEnv, StartTerm, EndTerm) :-
	stream_position_data(char_count, StartPos, CommentStart),
	stream_position_data(line_count, StartPos, CommentLine),
%	stream_position_data(line_position, StartPos, CommentLinePos),
	(   CommentStart < StartTerm
	->  (rm_comment_delimiters(Comment, CommentBare)
	    ->	add_comment_node(Root, CommentBare)
	    ;	atom_concat('%', _, Comment)
	    ->	true
	    ;	option(file(File), ReadEnv),
		option(this(This), ReadEnv),
	        error('File:~w Near Line:~w  Comment unexpected:~n~w~n',[File, CommentLine,Comment]),
	        send(This, report, error, 'line %s:Comment unexpected: %s',  CommentLine,
		 Comment)
	    )
	;   CommentStart >= EndTerm
	->  option(file(File), ReadEnv),
	    option(this(This), ReadEnv),
	    error('File:~w Near Line:~w  Trailing comment unexpected:~n~w~n',
		      [File, CommentLine,Comment]),
	    send(This, report, error, 'line %s:Trailing comment unexpected: %s',  CommentLine,
		 Comment)
	;   option(file(File), ReadEnv),
	    option(this(This), ReadEnv),
	    error('File:~w Near Line:~w  Comment in LEADSTO element unexpected(ignored):~n~w~n',
		      [File, CommentLine,Comment]),
	    send(This, report, warning, 'line %s:Comment in element ignored: %s',  CommentLine,
		 Comment)
	),
	handle_comments_new(Comments, Root, ReadEnv, StartTerm, EndTerm).


handle_comment(Root,S) :-
	get(S, size, SZ),
	(SZ == 0
	->	true
	;	%send(@pce, format, 'COMMENT:%sEND\n', S),
		extract_comments(Root,S)
	).

extract_comments(Root,String) :-
	extract_comments(Root,String, 0).
:- pce_global(@comment_wrapper_open, new(regex('/\\*'))).
:- pce_global(@comment_wrapper_close, new(regex('\\*/'))).
:- pce_global(@comment_wrapper_nl, new(regex('\n'))).

extract_comments(Root,String, Start) :-
	(multiline_regex_search(@comment_wrapper_open, Start, String,_S,P)
	->	(	multiline_regex_search(@comment_wrapper_close,P, String, ES, EE)
		->	get(String, sub, P, ES, Sub),
			add_comment_node(Root, Sub)
		;	get(String, sub, P, Sub),
			add_comment_node(Root, Sub),
			fail
		),
		extract_comments(Root,String, EE)
	;	true
	).
add_comment_node(Root, Sub) :-
	new(C, comment_node(Sub)),
	send(Root, son, C).

multiline_regex_search(Regex, String, FoundIndex) :-
	multiline_regex_search(Regex, String, FoundIndex, _End).

multiline_regex_search(Regex, String, FoundIndex, End) :-
	multiline_regex_search(Regex, 0, String, FoundIndex, End).
multiline_regex_search(Regex, StartIndex, String, FoundIndex, End) :-
	(get(Regex, search, String, StartIndex, FoundIndex)
	->	get(Regex, register_end, 0, End)
	;	get(@comment_wrapper_nl, search, String, StartIndex, I),
		multiline_regex_search(Regex, I, String, FoundIndex)
	).




extra_domain(extra(_Vars, _Pos, _File, This), Domain) :-
	!,
	get(This, domain, Domain).
extra_domain(This, Domain) :-
	(object(This)
	->	true
	;	impl_error('Not object extra_domain:~w', [This])
	),
	(get(This, frame, F)
	->	get(F, domain, Domain)
	;	get_domain(Domain)
	).
extra_report(extra(_Vars, _Pos, _File, This), Error, Format, Args) :-
	!,
	extra_error1(This, Error, Format, Args).
extra_report(This, Error, Format, Args) :-
	extra_error1(This, Error, Format, Args).
extra_error1(This, Error, Format, Args) :-
	object_from_extra(This, This1),
	Code =.. [send, This1, report, Error, Format|Args],
	call(Code).
object_from_extra(extra(_Vars, _Pos, _File, This), Object) :-
	!,object_from_extra1(This, Object).
object_from_extra(This, Object) :-
	object_from_extra1(This, Object).
object_from_extra1(This, Object) :-
	(object(This)
	->	Object = This
	;	Object = @pce,
		impl_error('Not object extra_error:~w',[This])
	).

ensure_ground_term(Term, Extra) :-
	(ground(Term)
	->	true
	;	pl_pce(Term, ATerm),
		extra_report(Extra, error,
		     'line %s:Expected ground term, got %s', [ATerm])
	),
	!.
ensure_ground_term(Term, Node) :-
	(ground(Term)
	->	true
	;	pl_pce(Term, ATerm),
		send(Node, report, error,
		     'line %s:Expected ground term, got %s', ATerm)
	).

ensure_pos(Pos, LineNo) :-
	(Pos = '$stream_position'(_CharIndex, LineNo, _LinePos)
	->	true
	;	impl_error('unrecognised stream_pos')
	).


handle_read(Term, Vars, LineNo, File, Root, This) :-
	handle_read1(Term, Root, extra(Vars, LineNo, File, This)).


:- dynamic dyn_warned/2.

ignore_lt_chk(Extra, Term) :-
	functor(Term, F, A),
	(	dyn_warned(F, A)
	->	true
	;	extra_report(Extra, warning, 'Ignoring leadsto entries %s/%s in checker specification', [F, A]),
		assertz(dyn_warned(F, A))
	).

leadsto_domain(Extra) :-
	extra_domain(Extra, leadsto).

constant_term_info(start_time(ST), ST, start_time_node).
constant_term_info(end_time(ST), ST, end_time_node).
constant_term_info(global_lambda(ST), ST, global_lambda_node).
%constant_term_info(cwa(ST), ST, cwa_node).

other_term(display(_,_), leadsto).
other_term(display_number_range(_,_,_,_), leadsto).

% Term starting with upper case, always error?
% Later allow meta things here, variable substitution?
handle_read1(Term, _Root, Extra) :-
	var(Term),
	!,
	Extra = extra(Vars, Line, File, This),
	pl_pce(Term, Vars, ATerm),
	error('File:~w Near Line:~w  Term not recognised:~n~w~n',
		      [File, Line,ATerm]),
	send(This, report, error, 'line %s:Unhandled term %s',  Line, ATerm).

% PLPCE:in principle, a prolog term, although Uppercase atoms are allowed.
% start_time(PLPCE)
% end_time(PLPCE)
% global_lambda(PLPCE)
handle_read1(Term, Root, Extra) :-
	constant_term_info(Term, Value, ConstantClass),
	!,
	(leadsto_domain(Extra)
	->	ensure_ground_term(Term, Extra),
		get(Root, addend, ConstantClass, Son),
		pl_pce(Value, ValueA),
		send(Son, change_value, ValueA)
	;	ignore_lt_chk(Extra, Term)
	).

% qterm(cwa(X))        cwa_node
% qterm(external(X))   external_node
% qterm(X) ...         other_node
handle_read1(qterm(QTerm), Root, _Extra) :-
	!,
	var_containing_constant(Class, X, QTerm),
	!,send(Root, son, new(CN, Class)),
	send(CN, change_value, X).


% display(_,_)
% display_number_range(_,_,_,_)
handle_read1(Term, Root, Extra) :-
	extra_domain(Extra, Domain),
	other_term(Term, Domain),
	!,
	Extra = extra(Vars, _Pos, _File, _This),
	new(N, other_node),
	send(Root, son, N),
	pl_pce(Term, Vars, T1),
	send(N, change_value, T1).

/*handle_read1(cwa(Term), Root, Extra) :-
	!,
*/
% periodic([Var1,Var2,..], Range, P, Formula)
handle_read1(periodic(Vars, Range, P, Formula), Root, Extra) :-
	is_list(Vars),
	!,
	(leadsto_domain(Extra)
	->	Term = periodic(Vars, Range, P, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval(Vars, Range, Formula, Root, Son, Extra),
		get(Son, add_period, PN),
		pl_pce(P, P1),
		send(PN, change_value, P1)
	;	ignore_lt_chk(Extra, periodic(Vars, Range, P, Formula))
	).
% periodic(ST, ET, P, Formula)
handle_read1(periodic(ST, ET, P, Formula), Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = periodic(ST, ET, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval([], range(ST, ET), Formula, Root, Son, Extra),
		get(Son, add_period, PN),
		pl_pce(P, P1),
		send(PN, change_value, P1)
	;	ignore_lt_chk(Extra, periodic(ST, ET, P, Formula))
	).
% periodic(Vars, ST, ET, P, Formula)
handle_read1(periodic(Vars, ST, ET, P, Formula), Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = periodic(ST, ET, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval(Vars, range(ST, ET), Formula, Root, Son, Extra),
		get(Son, add_period, PN),
		pl_pce(P, P1),
		send(PN, change_value, P1)
	;	ignore_lt_chk(Extra, periodic(ST, ET, P, Formula))
	).
% interval(Vars, Range, Formula)
handle_read1(interval(Vars, Range, Formula), Root, Extra) :-
	is_list(Vars),
	!,
	(leadsto_domain(Extra)
	->	Term = interval(Vars, Range, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval(Vars, Range, Formula, Root, _Son, Extra)
	;	ignore_lt_chk(Extra, interval(Vars, Range, Formula))
	).
% interval(Vars, ST, ET, Formula)
handle_read1(interval(Vars, ST, ET, Formula), Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = interval(Vars, ST, ET, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval(Vars, range(ST, ET), Formula, Root, _Son, Extra)
	;	ignore_lt_chk(Extra, interval(Vars, ST, ET, Formula))
	).
% interval(ST, ET, Formula)
handle_read1(interval(ST, ET, Formula), Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = interval(ST, ET, Formula),
		ensure_ground_term(Term, Extra),
		handle_interval([], range(ST, ET), Formula, Root, _Son, Extra)
	;	ignore_lt_chk(Extra, interval(ST, ET, Formula))
	).


% leadsto(AnteFormula, ConseFormula, Delay)
handle_read1(leadsto(AnteFormula, ConseFormula, Delay),
	    Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = leadsto(AnteFormula, ConseFormula, Delay),
		ensure_ground_term(Term,Extra),
		handle_leadsto1(Root, AnteFormula, ConseFormula, Delay, Extra, _Son)
	;	ignore_lt_chk(Extra, leadsto(AnteFormula, ConseFormula, Delay))
	).
% leadsto(Vars, AnteFormula, ConseFormula, Delay)
handle_read1(leadsto(Vars, AnteFormula, ConseFormula, Delay),
	    Root, Extra) :-
	!,
	(leadsto_domain(Extra)
	->	Term = leadsto(AnteFormula, ConseFormula, Delay),
		ensure_ground_term(Term,Extra),
		handle_leadsto1(Root, AnteFormula, ConseFormula, Delay, Extra, Son),
		handle_vars(Vars, Son, Extra)
	;	ignore_lt_chk(Extra, leadsto(Vars, AnteFormula, ConseFormula, Delay))
	).


handle_read1(specification(_), _Root, _Extra) :-
	!.
handle_read1(content(C), _Root, _Extra) :-
	!,
	assertz(dyn_content(C)).

handle_read1(denotes(Header, Formula), Root, Extra):-
	!,
	(leadsto_domain(Extra)
	->	ignore_lt_chk(Extra, denotes(Header, Formula))
	;	term_to_formula_node(Formula, FormulaNode, Extra),
		new(PN, property_def_node(@off)),
		send(PN, fill_header, Header),
		send(PN, son, FormulaNode),
		send(Root, son, PN)
	).

handle_read1((sort_element(SortName, Term):- member(Term2, List)),
	     Root, Extra) :-
	!,
	(Term == Term2
	->	true
	;	extra_report(Extra, error,
			     'Cannot translate sort specification ~w(contact lourens@cs.vu.nl)',[]),
		fail
	),
	test_sort_def(SortName, List, Extra),
	pl_pce(SortName, SortName1),
	ensure_sort_son(Root, SortName1, SNode),
	add_sort_contents(SNode, List).

handle_read1(constant(Name, Value), Root, _Extra) :-
	!,
	check_constant(Name, Value),
	send(Root, son, new(N, constant_def_node)),
	send(N, fill_header, Name),
	send(N, fill_value, Value).

handle_read1(sortdef(SortName, Objs), Root, Extra) :-
	!,
	test_sort_def(SortName, Objs, Extra),
	send(Root, son, new(SN, sort_node)),
	pl_pce(SortName, SortName1),
	send(SN, change_gui_prop, sort_name, SortName1),
	add_sort_contents(SN, Objs).



handle_read1(Node, Root, Extra) :-
	read_simple_constant_node(Node, Root, Extra),
	!.
handle_read1([specification_element], Root, Extra) :-
	!,
	extra_domain(Extra, Domain),
	send(Root, son, new(generic_node(Domain))).


handle_read1(Term, _, extra(Vars, Line, File, This)) :-
	functor(Term, F, A),
	(dyn_warned(F, A)
	->	true
	;	pl_pce(Term, Vars, ATerm),
		error('File:~w Near Line:~w  Term not recognised:~n~w~n',
		      [File, Line,Term]),
		send(This, report, warning,
		     'line %s:Unhandled term %s (only first occurrence flagged)', Line, ATerm),
		assertz(dyn_warned(F, A))
	),
	!.




:- dynamic dyn_warned/2.

reset_warned :-
	retractall(dyn_warned(_,_)).


% sortdef utils
ensure_sort_son(Root, SortName, SNode) :-
	(get(Root?sons, find, and(
				 message(@arg1, instance_of, sort_node),
				 message(@arg1?sort_name, equal, SortName)
				), SNode)
	->	true
	;	send(Root, son, new(SNode, sort_node)),
		send(SNode, change_gui_prop, sort_name, SortName)
	).

sort_extensions :-
	is_local.

test_sort_def(SortName, Objs, Extra) :-
	(sort_extensions
	->	true
	;	ground(SortName),
		is_list(Objs),
		ground(Objs),
		no_vars(sortdef(SortName, Objs))
	->	true
	;	extra_report(Extra, error,
			     'Can only handle simple sorts containing no variables',[]),
		fail
	).

add_sort_contents(SNode, Objs) :-
	forall(member(Object, Objs),
	       (       send(SNode, son, new(ON, object_node)),
		       (       Object = 'SUBSORT'(O1)
		       ->      send(ON, is_sub_sort, @on)
		       ;       O1 = Object
		       ),
		       pl_pce(O1, PO),
		       send(ON, change_gui_prop, object_name, PO)
	       )
	      ).






handle_vars([], _Son, _Extra).
handle_vars([Var|Vars], Son, Extra) :-
	handle_var(Var, Son, Extra),
	handle_vars(Vars, Son, Extra).
handle_var(Var, Son, _) :-
	get(Son, add_var_end, VN),
	send(VN, fill_term, Var).




handle_interval(Vars, Range, Formula, Root, Son, Extra) :-
	range_to_range_node(Range, RangeNode, Extra),
	term_to_formula_node(Formula, FormulaNode, Extra),
	get(Root, addend, interval_node, Son),
	send(Son, delete_all_sons),
	send(Son, son, RangeNode),
	send(Son, son, FormulaNode),
	handle_vars(Vars, Son, Extra).


handle_leadsto1(Root, AnteFormula, ConseFormula, Delay, Extra, Son) :-
	term_to_formula_node(AnteFormula, AnteFormulaNode, Extra),
	term_to_formula_node(ConseFormula, ConseFormulaNode, Extra),
	delay_to_delay_node(Delay, DelayNode, Extra),
	get(Root, addend, leadsto_node, Son),
	send(Son, delete_all_sons),
	send(Son, son, AnteFormulaNode),
	send(Son, son, ConseFormulaNode),
	send(Son, son, DelayNode).


range_to_range_node(range(ST, ET), RangeNode, Extra) :-
	!,
	Term = range(ST, ET),
	ensure_ground_term(Term,Extra),
	new(RangeNode, range_node),
	(	ST == [time],
		ET == [time]
	->	true
	;	pl_pce(range(ST, ET), PceRange),
		send(RangeNode, change_range, PceRange)
	).

range_to_range_node(Range, RangeNode, Extra) :-
	!,
	ensure_ground_term(Range,Extra),
	new(RangeNode, range_node),
	pl_pce(Range, RangeA),
	send(RangeNode, change_range, RangeA).
delay_to_delay_node(Delay, DelayNode, _Extra) :-
	!,
	pl_pce(Delay, DelayA),
	new(DelayNode, delay_node),
	send(DelayNode, change_gui_prop, delay, DelayA).


and_or_list(PlFormula, AndOr, List) :-
	PlFormula =.. [AndOr|List],
	memberchk(AndOr, [and,or]).

simple_lt_var_toplevel(cwa(C), C, cwa_node).
simple_lt_var_toplevel(model(M), M, model_node).
read_simple_constant_node(Node, Root, Extra) :-
	simple_lt_var_toplevel(Node, Data, Class),
	!,
	(Extra = extra(Vars, _Pos, _File, _This)
	->	true
	;	impl_error('Unexpected handle_read1_cwa'),
		fail
	),
	(leadsto_domain(Extra)
	->	send(Root, son, new(CN, Class)),
		pl_pce(Data, Vars, P),
		send(CN, change_value, P)
	;	ignore_lt_chk(Extra, Node)
	).

term_to_formula_node(PlFormula, Node, Extra) :-
	var(PlFormula),
	!,
	(Extra = extra(Vars, Line, File, This)
	->	true
	;	impl_error('Unexpected Extra'),
		fail
	),
	pl_pce(PlFormula, Vars, P),
	error('File:~w Near Line:~w  Term not recognised:~n~w~n(replaced by false)',
		      [File, Line,P]),
	send(This, report, error, 'Term %s not recognised', P),
	new(Node, formula_simple_node(false)).


term_to_formula_node(PlFormula, Node, _Extra) :-
	memberchk(PlFormula, [true, false]),
	!,
	new(Node, formula_simple_node(PlFormula)).
term_to_formula_node(external(Term), Node, Extra) :-
	!,
	(Extra = extra(Vars, _Pos, _File, _This)
	->	true
	;	impl_error('Unexpected handle_read1_cwa'),
		fail
	),
	new(Node, external_node),
	pl_pce(Term, Vars, P),
	send(Node, change_value, P).



term_to_formula_node(PlFormula, Node, Extra) :-
	and_or_list(PlFormula, AndOr, List),
	!,
	ensure_ground_term(PlFormula,Extra),
	new(Node, formula_simple_node(AndOr, @off)),
	forall(member(PTerm, List),
	       (
		       term_to_formula_node(PTerm, SNode, Extra),
		       send(Node, son, SNode)
	       )
	      ).
term_to_formula_node(not(PlFormula), Node, Extra) :-
	!,
	ensure_ground_term(PlFormula,Extra),
	term_to_formula_node(PlFormula, Node1, Extra),
	new(Node, formula_simple_node(not, @off)),
	send(Node, son, Node1).
term_to_formula_node(implies(PlFormula1,PlFormula2), Node, Extra) :-
	!,
	Term = implies(PlFormula1,PlFormula2),
	ensure_ground_term(Term,Extra),
	term_to_formula_node(PlFormula1, Node1, Extra),
	term_to_formula_node(PlFormula2, Node2, Extra),
	new(Node, formula_simple_node(implies, @off)),
	send(Node, son, Node1),
	send(Node, son, Node2).
term_to_formula_node(Quantifier, Node, Extra) :-
	quantor(Quantifier, Vars, Name, Formula),
	!,
	ensure_ground_term(Quantifier,Extra),
	term_to_formula_node(Formula, FormulaNode, Extra),
	new(Node, formula_simple_node(Name, @off)),
	uchecklist(handle_qvar(Node),Vars),
	send(Node, son, FormulaNode).
term_to_formula_node([Keyword], Node, Extra) :-
	atom(Keyword),
	!,
	(Keyword == formula
	->	new(Node, formula_simple_node)
	;Keyword == atom
	->	new(Node, atom_node)
	;	term_to_atom([Keyword], LK),
		extra_report(Extra, error,
			     'Unhandled generic formula %s', [LK]),
		fail
	).
term_to_formula_node(pxor(PList), Node, Extra) :-
	(PList = [_|_]
	->	true
	;	extra_report(Extra, error,
		     'pxor entry expects >= 1 subnodes',
		     []),
		fail
	),
	new(Node, pxor_node(@off)),
	(	member(E, PList),
		term_to_pxe(E, N1, Extra),
		send(Node, son, N1),
		fail
	;	true
	),!.


term_to_formula_node(itef(Cond, Vars, Form), Node, Extra) :-
	extra_domain(Extra, checker),
	!,
	term_to_formula_node(Cond, CondNode, Extra),
	term_to_formula_node(Form, LastNode, Extra),
	new(C, chain),
	uchecklist(add_cond_var(Extra, C), Vars),
	new(Node, itef_node(CondNode, LastNode, C)).

term_to_formula_node(PlFormula, Node, Extra) :-
	functor(PlFormula, holds, _),
	extra_domain(Extra, checker),
	!,
	new(Node, holds_node),
	send(Node, fill_term, PlFormula).
term_to_formula_node(PlFormula, Node, Extra) :-
	functor(PlFormula, '|=', _),
	extra_domain(Extra, checker),
	!,
	new(Node, bareq_node),
	send(Node, fill_term, PlFormula).
term_to_formula_node(PlFormula, Node, _Extra) :-
	(	PlFormula = '[op]'(_LHS, _RHS)
	->	true
	;	iscmpoploc(PlFormula, _Call1)
	),
	!,
	new(Node, condition_node),
	send(Node, fill_term, PlFormula).

term_to_formula_node(PlFormula, Node,  Extra) :-
	\+ reserved(PlFormula),
	\+ is_list(PlFormula),
	ensure_ground_term(PlFormula,Extra),
	(extra_domain(Extra, leadsto)
	->	new(Node, atom_node),
		pl_pce(PlFormula, Name),
		send(Node, change_term_name, Name)
	;	new(Node, property_node),
		pl_pce(PlFormula, Name),
		send(Node, change_term_name, Name)
	),!.



term_to_formula_node(PlFormula, _Node, Extra) :-
	term_to_atom(PlFormula, F1),
	extra_report(Extra, error,
		     'Input %s not recognised as a valid Formula',
		     [F1]),
	fail.
add_cond_var(_Extra, Chain, Var) :-
	send(Chain, append, new(N, conditional_var_node)),
	send(N, fill_term, Var).


term_to_pxe(pxe(Prob, Form), Node, Extra) :-
	(Prob == @nil
	->	P = @nil
	;	pl_pce(Prob, P)
	),
	object_from_extra(Extra, Editor),
	test_probability(Editor, P),
	term_to_formula_node(Form, Node1, Extra),
	new(Node, pxor_formula_node(P, Node1, @on)).

handle_qvar(Node, Var) :-
	send(Node, son, new(VN,var_node)),
	(send(VN, fill_term, Var)
	->	true
	;	impl_error('~w failed', [handle_qvar(Node, Var)]),
		fail
	).


lteditorspecloaded(File) :-
	dyn_content(type(save_lt_editor(File))).

set_content(P) :-
	portray_clause(content(P)),
	assertz(dyn_content(P)).

save_spec(File, This) :-
	retractall(dyn_content(_)),
	version_details(_, App),
	tell(File),
	set_content(type(save_lt_editor(File))),
	set_content(generator(App)),
	get(new(date)?string, value, Current),
	set_content(run([date(Current)])),
	save_nodes(This),
	told.

save_nodes(This) :-
	retractall(dyn_err_done(_)),
	get(This?spec_tree, root, Root),
	send(Root?sons, for_some, message(@prolog, save_node,
					 @arg1, This)).


save_node(Node, This) :-
	get(Node?class, name, ClassName),
	save_node_special(ClassName, Node, This),!.


save_node(Node, _This) :-
	get(Node, tr_term, Term),
	portray_clause(Term).



%var_containing_constant(model_node, bind).



save_node_special(comment_node, Node, _This) :-
	!,send(Node, save_current_stream).

save_node_special(Class, Node, _This) :-
	var_containing_constant(Class, V, Term),
	!,
	get(Node?constant0, value, V),
	%get(Node, tr_term_bindings, tb(Term,Bindings)),
	writeq(qterm(Term)),
	/*
	;	sformat(S, '~q', [V]),
		name(S, [39|R]),
		append(R1, [39], R),
		append([39, 99, 119, 97, 40|R1], [41, 39], R2),
		name(Res, R2)
	->	writeq(qterm(Res))
	;	name(V, L),
		append([39, 99, 119, 97, 40|L], [41, 39], R2),
		name(Res, R2),
		writeq(qterm(Res))
	),
	%print_var_names(Term, Bindings),
	  */
	format('.~n').














