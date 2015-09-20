:- module(satsimplequiet,
	  [
	   sats_quiet/1
	  ]).

:- use_module(util).
:- use_module(satgenut).
:- use_module(library(lists)).
:- multifile user:io_level/2.

user:io_level(satsimplequiet, 10).


sats_quiet(Formula) :-
	sats(Formula).
