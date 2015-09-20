:- module(ltversion, [
		      wrversion/0
		     ]).
user:app_version_n(leadsto_software,1,29).


user:app_version(Tag, Version, Number) :-
    user:app_version_n(Tag, May, Min),
    Number is 100*May + Min,
    sformat(Version, '~w.~w', [May, Min]).



save_version(File) :-
    app_version_n(leadsto_software, May, Min),
    tell(File),
    format('!define VERSION ~w_~w~n', [May, Min]),
    get_time(X), convert_time(X, Date),
    format('Name "Leadsto & TTL Checking Software ~w.~w   (~s)"~n',
	       [May, Min, Date]),
    current_prolog_flag(home, P), prolog_to_os_filename(P,Q),
    format('!define PLLIB "~w\\bin"~n', [Q]),
    told.

wrversion :-
	app_version_n(leadsto_software, May, Min),
	format('~w_~w~n', [May,Min]).
