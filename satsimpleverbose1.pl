:- module(satsimpleverbose,
	  [
	   sats_verbose/1
	  ]).

:- use_module(util).
:- use_module(satgenut).
:- use_module(library(lists)).
:- multifile user:io_level/2.

user:io_level(satsimpleverbose, 20).


sats_verbose(Formula) :-
	sats(Formula).
