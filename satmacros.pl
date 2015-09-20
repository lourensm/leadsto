user:goal_expansion(wr_element(Txt, Id, X, Sort, VarName), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(	20 =< P
	->	Res = format('~p:~p NEXT    ~p:~p = ~w~n',
			     [Id, Txt, VarName, Sort,X])
	;	Res = []
	).
user:goal_expansion(wr_sat(Txt, Id, NOT), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(	20 =< P
	->	Res = format('~p:~p ~p SATISFIED~n',[Id, Txt, NOT])
	;	Res = []
	).
user:goal_expansion(wr_not_sat(Txt, Id), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(	20 =< P
	->	Res = format('~p:~p NOT SATISFIED~n',[Id, Txt])
	;	Res = []
	).
user:goal_expansion(wr_sat(Txt, Id), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(	20 =< P
	->	Res = format('~p:~p SATISFIED~n',[Id, Txt])
	;	Res = []
	).
user:goal_expansion(inform(I, Format, Args), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(	I =< P
	->	Res = format(Format, Args)
	;	Res = true
	).
user:goal_expansion(inform(I, Format), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(I =< P
	->	Res = format(Format)
	;	Res = true
	).
user:goal_expansion(iflocal(Call), Res) :-
	(	predicate_property(user:io_level(_,_), interpreted),
		context_module(M),
		user:io_level(M, P),
		number(P)
	->	true
	;	P = 30
	),
	(12 =< P
	->	Res = (is_local
		      ->      ignore(call(Call))
		      ;	      true
		      )
	;	Res = true
	).
	
/*
  higher level less important info
  level 6 : test2/test3
  level 7  : all models
  level 10 : printout model satisfied
  level 20 : printout
  level 50 : everything
  level 2  : error
  level 5  : warning
  */
/*
inform(I, Format, Args) :-
	inform_level(J),
	(I =< J
	->	format(Format, Args)
	;	true
	).
inform(I, Format) :-
	inform(I, Format, []).
*/
