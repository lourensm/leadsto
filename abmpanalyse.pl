:- module(abmpanalyse).

:- dynamic dyn_log_file/1.

:- use_module(util).
:- use_module(abmptrace).
:- use_module(abmputil).
:- use_module(tocdat).



savealone(Name):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(Name, [goal=run, autoload=true, stand_alone=true]),
	halt.
savealone_nsa(Name):-
	spec:require([member/2,memberchk/2]),
	my_autoload_all,
	qsave_program(Name, [goal=run, autoload=true]),
	halt.

mformat(Format) :-
	mformat(Format, []).

mformat(Format, Args) :-
	flag(dynlog, P, P + 1),
	format(Format, Args),
	(dyn_log_file(Log)
	->	true
	;	assert_debug(P = 0),
		open(aaplog, write, Log, [close_on_abort(true)]),
		assertz(dyn_log_file(Log))
	),
	format(Log, Format, Args).



close_log :-
	(dyn_log_file(Log)
	->	close(Log)
	;	true
	).


:- dynamic participant/1.
local_option(datsources, arg('-participant', Arg, set_option(abmptr(participant(Arg))),'Participant'),
	     'Participant directory(must be simple subdirectory):Participant/abmp contains dat files (multiple participants allowed)',[]).

local_option(datsources, arg('-participants', Arg, set_option(abmptr(participants(Arg))),'Participants'),
	     'Participants directories(may contain wildcards(*?)(but then must be quoted))):Participanti/abmp contains dat files',[]).

local_option(datsources, arg('-participantfile', Arg, set_option(abmptr(participantfile(Arg))),'ParticipantFile'),
	     'ParticipantFile contains participant(Directory) prolog facts',[]).
local_option(datsources, arg('-datfile', Arg, set_option(abmptr(datfile(Arg))),'DATFILE'),
	     'A datfile',[]).
run :-
	PgmName = abmptr,
	Header = 'AnalyseAbmp',
	setupmainnew(Header, PgmName, [os(util,[wd,constant,log,debugging]),
				       os(abmpanalyse,[datsources])], File),
	analyseargdats(File),
	createtraces,
	close_log,
	finalhalt(0),!.
run :-
	fatal_error('Something went wrong with analysis of participants').


/* HOBBY */




analyseargdats(File) :-
	File == [],
	\+ get_option(abmptr(datfile(_DatFile))),
	\+ get_option(abmptr(participant(_Participant))),
	\+ get_option(abmptr(participants(_Participant))),
	\+ get_option(abmptr(participantfile(_ParticipantFile))),
	!,
	%new(D, dialog('DATA source(s)')),
	(get(@finder, file, @on, '.dat', @default, File1)
	->	local_inform('File:~w~n', [File1]),
		analysedatinfo(_, File1)
	;	fatal_error('user exit')
	).

analyseargdats(File) :-
	(File == []
	->	true
	;	analysedatinfo(_, File)
	),
	(	get_option(abmptr(datfile(DatFile))),
		analysedatinfo(_, DatFile),
		fail
	;	true
	),
	(	get_option(abmptr(participant(Participant))),
		analyseparticipant(Participant),
		fail
	;	true
	),
	(	get_option(abmptr(participants(Participants))),
		expand_file_name(Participants, List),
		member(Participant, List),
		analyseparticipant(Participant),
		fail
	;	true
	),
	(	get_option(abmptr(participantfile(ParticipantFile))),
		(catch(open(ParticipantFile,read,Stream), E,
		      (print_message(error, E),
			      fail
		      ))
		->	repeat,
			(catch(read(Stream,Term), E1,
			      (	      print_message(error, E1),
				      fail
			      ))
			->	(Term = participant(Participant)
				->	analyseparticipant(Participant),
					fail
				;Term == end_of_file
				->	!
				;	!,
					error('expected participant/1 facts got ~w',
					      [Term])
				)
			;	!,
				error('ignoring rest of file ~w', [ParticipantFile])
			)
		;	error('Could not open participantfile ~w',
			      [ParticipantFile])
		),
		fail
	;	true
	).

analyseparticipants :-
	ensure_loaded('vu10data.pl'),
	(	participant(Participant),
		analyseparticipant(Participant),
		fail
	;	true
	),
	createtraces.
createtraces :-
	join_linked_negos,
	(setof(I-O, dyn_socket_negos(I, O), NL)
	->	true
	;	NL = []
	),
	(onepass
	->	true
	;	open('bids.pl',write, _, [alias(bids)])
	),
	make_mm_traces(NL),
	length(NL, L),
	mformat('PARTNERS:~n',[]),
	(	member(I1-O1, NL),
		mformat('    ~w - ~w~n', [I1,O1]),
		fail
	;	true
	),
	mformat('~w PAIRS FOUND~n', [L]),
	make_single_traces,
	(onepass
	->	construct_traces
	;	close(bids)
	),!.
createtraces :-
	error('Something went wrong with analysis of participants').

make_single_traces :-
	%open('abids.pl',write, _, [alias(bids)]),
	/* not the mm or any socket role */
	(	dyn_id(Id, Participant, DatFile, Num),
		make_single_trace(Id, Participant, DatFile, Num),
		fail
	;	true%close(bids)
	).
make_single_trace(Id, Participant, DatFile, Num) :-
	(	dyn_dat_term(Id, socket_role(_BS, _R))
	->	mformat('~w contains socket info, no isolated trace generated~n',
			[DatFile]),
		fail
	;	true
	),
	Trace = single(Participant, Num),
	buysel_auto_man(Id, AMBuyer, AMSeller),
	dyn_deal_info(Id, Res),
	role_bid_info(Id, buyer, Object-OwnBI),
	role_bid_info(Id, seller, Object-OtherBI),
	%flag(debacc, _, 1),
	inform_flag(debacc, 'debacc:~w~n', [d(Id, Participant, DatFile, Num)]),
	all_accessoire_info(Id, DSAllAcc),
	rest_seller_ut_info(Id, FInfo),
	save_bid_fact(Trace, negotiation(idbuyerseller(Id,AMBuyer, AMSeller))),
	save_file_info(Trace, Id),
	save_bid_fact(Trace, object(Object)),
	set_bids(OwnBI, Trace, Id, buyer),
	set_bids(OtherBI, Trace, Id, seller),
	save_bid_fact(Trace, DSAllAcc),
	save_bid_fact(Trace, FInfo),
	save_single_deal_info(Trace, Res),
	!.
make_single_trace(Id, _Participant, DatFile, _Num) :-
	(dyn_dat_term(Id, socket_role(_BS, _R))
	->	fail
	;	warning('No trace generated for file ~w', [DatFile]),
		fail
	).

save_single_deal_info(Trace, Res) :-
	cmp_save_deal_info(Trace, Res, Res).


	
make_mm_traces(NL) :-
	%open('bids.pl',write, _, [alias(bids)]),
	(	member(I1-O1, NL),
		make_trace(I1, O1),
		fail
	;	true
	).
				%close(bids).
trace_name_from_participants(I, O, p(I, O)).
:- dynamic user:dyn_fact/2.


save_bid_fact(Trace, Fact) :-
	onepass,
	!,
	assertz(user:dyn_fact(Trace, Fact)).

save_bid_fact(Trace, Fact) :-
	format(bids, 'fact(~q, ~q).~n', [Trace, Fact]).
save_file_info(Trace, Id) :-
	(dyn_id(Id, Participant, DatFile, Num)
	->	Info1 = [file(DatFile),participant(Participant),test(Num)],
		(dyn_dat_term(Id, run(Run))
		->	(memberchk(user=User, Run)
			->	Info2 = [user=User|Info1]
			;	Info2 = Info1
			),
			(memberchk(time=Time, Run)
			->	Info3 = [time=Time|Info2]
			;	Info3 = Info2
			)
		;	Info3 = Info1
		),
		save_bid_fact(Trace, extra_info(Info3))
	;	error('Missing file info')
	).

make_trace(I, O) :-
	trace_name_from_participants(I, O, Trace),
	role_bid_info(I, buyer, Object-OwnBI),
	role_bid_info(O, seller, Object-OtherBI),
	save_bid_fact(Trace, negotiation(socket(I, O))),
	save_file_info(Trace, I),
	save_file_info(Trace, O),
	save_bid_fact(Trace, object(Object)),
	set_bids(OwnBI, Trace, I, buyer),
	set_bids(OtherBI, Trace, I, seller),
	(dyn_deal_info(I, ResI)
	->	true
	;	error('Missing deal info'),
		trace
	),
	(dyn_deal_info(O, ResO)
	->	true
	;	error('Missing deal info'),
		trace
	),
	cmp_save_deal_info(Trace, ResI, ResO),
	all_accessoire_info(I, DSAllAcc),
	save_bid_fact(Trace, DSAllAcc),
	rest_seller_ut_info(I, FInfo),
	save_bid_fact(Trace, FInfo).


	
cmp_save_deal_info(Trace, ResI, ResO) :-
	(ResI == ResO
	->	(ResI = accepted_others_bid(Role, Res1)
		->	save_bid_fact(Trace, deal_first(Role, others_last_bid_ok)),
			(Res1 == confirmed
			->	Second = confirmed
			;Res1 == rejected
			->	Second = rejected
			;	Second = unconfirmed
			),
			otherrole(Role, OtherRole),
			save_bid_fact(Trace, deal_second(OtherRole, Second))
		;ResI = no_deal(Role)
		->	save_bid_fact(Trace, deal_first(Role, no_deal))
		;	save_bid_fact(Trace, deal_unknown)
		)
	;	error('Deal mismatch'),
		fail
	).

set_bids([],_,_,_).
set_bids([Bid|Bids], Trace, Id, Role) :-
	set_bid(Bid, Trace, Id, Role),
	set_bids(Bids, Trace, Id, Role).
set_bid(Round-Vals, Trace, Id, Role) :-
	config_from_vals(Vals, Config),
	save_bid_fact(Trace, bid(Role, Round, Config)),
	role_info(Role, _, EPRole, VisRep),
	(dyn_dat_term(Id, saved_fact(EPRole, my_utility_of_my_bid_in_round(OwnUt, Round)))
	->	save_bid_fact(Trace, bid_utility(Round, Role, Role, OwnUt))
	;
		dyn_dat_term(Id,
			     vis_fact(VisRep, Round, utility(own, utility), OwnUt))
	->	save_bid_fact(Trace, bid_utility(Round, Role, Role, OwnUt))
	;	error('~w:Missing bid utility info ~w',[Trace,bid_utility(Round, Role, Role,Trace)]),
		local_trace(aap)
	),
	otherrole(Role, OtherRole),
	flag(dB, Db, Db + 1),
	(	dyn_dat_term(Id,
			     saved_fact(EPRole,
					my_utility_of_opponents_bid_in_round(OthersUt, Round)))
	->	save_bid_fact(Trace, bid_utility(Round, OtherRole, Role,
						 OthersUt))
	;	dyn_dat_term(Id,
			     vis_fact(VisRep, Round, utility(other, utility),
				      OthersUt))
	->	save_bid_fact(Trace, bid_utility(Round, OtherRole, Role,
						 OthersUt))
	;	warning('Missing bid utility info ~w(~w)', [bid_utility(Round, Role, OtherRole),Db])
	).

config_from_vals(Vals, Config) :-
	(	bagof(TAttr, table_acc(TAttr), TAttrs),
		config_vals_from_vals([price|TAttrs], Vals, ValsLeft, ConfigList),
		ValsLeft == [],
		Config =.. [bid|ConfigList]
	->	true
	;	error('Wrong configuration'),
		fail
	).
config_vals_from_vals([], Vals, Vals, []).
config_vals_from_vals([AttrName|Attrs], Vals, ValsLeft, [CVal|CVals]) :-
	(select(AttrName-CVal, Vals, Vals1)
	->	config_vals_from_vals(Attrs, Vals1, ValsLeft, CVals)
	;	error('Missing attribute ~w', [AttrName]),
		fail
	).



part_prefix('/abmp/abmp').

analyseparticipant(Participant) :-
	mformat('~nPARTICIPANT ~w~n', [Participant]),
	part_prefix(PartPref),
	concat_atom([Participant,PartPref,'*.dat'],FS),
	expand_file_name(FS, DatFiles),
	uchecklist(analysedatinfo(Participant), DatFiles).
id_from_dat_file(Participant, DatFile, Id, Num) :-
	var(Participant),
	!,
	(atom_concat(Id, '.dat', DatFile)
	->	Participant = Id,
		Num = 1
	;	error('Cannot generate id from file ~w', [DatFile]),
		fail
	).


		
id_from_dat_file(Participant, DatFile, Id, Num) :-
	(	part_prefix(PartPref),
		atom_concat(Participant,PartPref, Prefix),
		atom_concat(Prefix, DF, DatFile),
		atom_concat(Pr0, '.dat', DF),
		atom_chars(Pr0, AC),
		number_chars(Num, AC),
		concat_atom([Participant, 'DAT', Num], Id)
	->	true
	;	Participant = isolated,
		atom_concat(Id, '.dat', DatFile)
	->	Num = 1
	;	error('Cannot generate id from file ~w', [DatFile]),
		fail
	),
	(dyn_id(Id, _Participant1, DatFile1, _Num1)
	->	error('Duplicate id for files ~w and ~w',
		      [DatFile, DatFile1]),
		fail
	;	true
	).

	
	

		
:- dynamic dyn_id/4.

analysedatinfo(Participant, DatFile) :-
	(id_from_dat_file(Participant, DatFile, Id, Num)
	->	reset_info(Id),
		assertz(dyn_id(Id, Participant, DatFile, Num)),
		analysedat(DatFile, Id)
	;	true
	).



analysedat(DatFile, Id) :-
	mformat('~nDATFILE ~w~n', [DatFile]),
	open(DatFile, read, _, [alias(datfile)]),
	analysedatcont(Id, DatFile),
	close(datfile).
analysedatcont(Id, DatFile) :-
	repeat,
	(catch(read(datfile, Term),Error, recover_read(Error,Id))
	->	(Term == end_of_file
		->	!,analyse_after_read(Id, DatFile)
		;	analysedatterm(Term, Id),
			fail
		)
	;	!
	).
analyse_after_read(Id, DatFile) :-
	(check_required(Id)
	->	(analyse_use(Id)
		->	mformat('DATFILE ~w OK~n',[DatFile])
		;	mformat('File ~w ignored~n',[DatFile]),
			reset_info(Id)
		)
	;	mformat('File ~w ignored~n', [DatFile]),
		reset_info(Id)
	).

recover_read(Error,Id) :-
	mformat('Encountered ERROR:~w~n', [Error]),
	mformat('Ignoring file~n'),
	reset_info(Id),
	fail.


check_required(Id) :-
	(	require_term(Term),
		(dyn_require_term(Id, Term)
		->	true
		;	mformat('Missing required term ~w~n',[Term]),
			!,
			fail
		),
		fail
	;	true
	).

otherrole(buyer,seller).
otherrole(seller,buyer).

join_linked_negos :-
	(	Role = buyer,
		dyn_dat_term(Id,socket_role(Role,s(OtherRole,
						   abmp(File,Date)))),
		(otherrole(Role, OtherRole)
		->	true
		;	mformat('ERROR:role mismatch~n'),
			fail
		),
		\+ dyn_handled_dat_term(Id,socket_role(Role,s(OtherRole,
						   abmp(File,Date)))),
		ignore(find_linked_nego(Id, Role, File, Date)),
		assertz(dyn_handled_dat_term(Id,socket_role(Role,s(OtherRole,
						   abmp(File,Date))))),
		fail
	;	true
	).

:- ensure_loaded('~/wrk/abmp/database.pl').

rest_seller_ut_info(Id, fin_info_seller(FRF, MFM, BCost, TAttrFEDs)) :-
	set_object(Id, Object),
	atom_codes(Object, Object1),
	dyn_require_term(Id, initial_factor(human_seller_interface, V, financial_rationality_factor(FRF, V))),
	dyn_require_term(Id, initial_factor(human_seller_interface, _G989, max_financial_margin(MFM, _G989))),
	data_element(car_db_aj, attribute_has_value(Object1, acquisition_price, BCost), pos),
	setof(TAttr, table_acc(TAttr),TAttrs),
	setof(AttrVal, attr_val(AttrVal), AttrVals),
	maplist(get_feds(AttrVals), TAttrs, TAttrFEDs),!.

get_feds(AttrVals, TAttr, TAttr = AttrValPrices) :-
	maplist(get_fed(TAttr), AttrVals, AttrValPrices).
get_fed(TAttr, AttrVal, AttrVal - AttrValPrice) :-
	atom_codes(AttrVal, AttrValQ),
	data_element(accessoire_price_db_aj,
		     attribute_value_price(TAttr,
					   AttrValQ, AttrValPrice),pos).

inform_flag(Flag, Format, Args) :-
	(	flag(Flag, P, P),
		P > 0
	->	format(Format, Args),
		trace
	;	true
	).
inform_flag(Flag, Format) :-
	inform_flag(Flag, Format, []).

all_accessoire_info(Id, ds_all_acc(HBI,HSI)) :-
	(accessoire_info(Id, human_buyer_interface, HBI)
	->	inform_flag(debacc, 'humanbuyerok~n')
	;	inform_flag(debacc, 'humanbuyermissing~n'),
		fail
	),
	accessoire_info(Id, human_seller_interface, HSI).
pr_acc_info(HBI,HSI) :-
	Tab = '    ',concat_atom([Tab, '    '], Tab1),
	mformat('~wHBI:~n',[Tab]),
	HBI = ds_hi_acc(PriceMax,PriceSlope,PriceImp, AttrData),
	mformat('~wPriceMax:  ~w  ', [Tab1,PriceMax]),
	mformat('PriceSlope:~w   ', [PriceSlope]),
	mformat('Price Imp:  ~w~n', [PriceImp]),
	uchecklist(pr_tbl_attr_data(Tab1), AttrData),
	mformat('~wHSI:~n',[Tab]),
	HSI = ds_hi_acc(PriceMax1,PriceSlope1,PriceImp1, AttrData1),
	(uchecklist(\=(undefined),[PriceMax1,PriceSlope1,PriceImp1])
	->	impl_error('Unaccounted HSI Price info')
	;	uchecklist(pr_tbl_attr_data(Tab1), AttrData1)
	).

pr_tbl_attr_data(Tab1, ds_attr(AttrName, Evals, AttrImp)) :-
	mformat('~w~w Imp: ~w   ', [Tab1, AttrName, AttrImp]),
	mformat('Table:', []),
	(member(ds_eval(AttrVal, Eval), Evals),
		mformat('(~w : ~w)  ', [AttrVal, Eval]),
		fail
	;	mformat('~n', [])
	).

acc_attr_imp_element(Id, HI, AttrName, AttrImp1) :-
	(dyn_dat_term(Id, att_imp(HI, accessoire, AttrName, AttrImp1))
	->	true
	;	dyn_dat_term(Id, saved_fact(ws(HI, accessoire), to_be_communicated_to(attribute_importance(AttrName,  AttrImp1,_RId1), pos, _Rep)))
	->	true
	;	error('Missing attribute_importance'),
		fail
	).

% ds_attr(Name, AttrEvals, AttrImp)
accessoire_info(Id, HI, ds_hi_acc(PriceMax,PriceSlope,PriceImp, AttrData)) :-
	setof(AttrName, table_acc(AttrName),AttrNames),
	setof(AttrVal, attr_val(AttrVal), AttrVals),
	(maplist(get_accessoire_attr_info(Id, HI, AttrVals),AttrNames,AttrData)
	->	inform_flag(debacc, gotaccattr)
	;	inform_flag(debacc, missedaccattr),
		fail
	),
	(HI == human_buyer_interface
	->	(acc_attr_imp_element(Id, HI, price, PriceImp)
		->	true
		;	inform_flag(debacc, 'Missing human_acc for ~w~n',[Id]),
			fail
		),
		(dyn_dat_term(Id, saved_fact(ws(human_buyer_interface, accessoire), to_be_communicated_to(eval_description_for(function_type, linear(PriceMax, PriceSlope), price, _RId), pos, buyer_representative)))
		->	true
		;	dyn_dat_term(Id, linear_def(HI, accessoire, price,
						    PriceMax, PriceSlope))
		->	true
		;	inform_flag(debacc, 'missing linear def'),
			fail
		)
	;	PriceMax = undefined,
		PriceSlope = undefined,
		PriceImp = undefined
	).


get_accessoire_attr_info(Id, HI, AttrVals, AttrName,
			 ds_attr(AttrName, Evals, AttrImp)) :-
	maplist(get_attr_eval(Id, HI, AttrName), AttrVals, Evals),
	acc_attr_imp_element(Id, HI, AttrName, AttrImp1),
	(AttrName == price
	->	AttrImp = AttrImp1
	;	AttrImp is AttrImp1/20
	).


get_attr_eval(Id, HI, AttrName, AttrVal, ds_eval(AttrVal, Eval)) :-
	(dyn_dat_term(Id, table(HI, accessoire, AttrName, AttrVal, Eval))
	->	true
	;	dyn_dat_term(Id, saved_fact(ws(HI, accessoire), to_be_communicated_to(eval_description_for(table, entry('STR'(AttrVal), Eval), AttrName, _RId), pos, _Rep)))
	->	true
	;	error('Missing table entry'),
		fail
	).




num_from_sock_file(File, Num) :-
	(	atom_concat('h:/abmp/abmp', Dat, File),
		atom_concat(NumAt, '.dat', Dat),
		atom_codes(NumAt, Codes),
		number_codes(Num, Codes)
	->	true
	;	sub_atom(File, _Before, _Len, After, 'abmp/abmp'),
		Len1 is After - 4,
		sub_atom(File, _, Len1, 4, NumAt),
		atom_codes(NumAt, Codes),
		number_codes(Num, Codes)
	->	true
	;	mformat('WARNING:cannot analyse  role file ~w~n',
			[File]),
		fail
	).

linked_other_candidate(Id, Role, File, OtherId) :-
	num_from_sock_file(File, Num),
	dyn_id(OtherId, _OtherParticipant, _OtherDatFile, Num),
	match_socket(Id, Role, OtherId),
	match_config(Id, OtherId),
	match_bidding(Id, OtherId).
match_config(_Id, _OtherId).
match_bidding(_Id, _OtherId).
match_socket(Id, Role, OtherId) :-
	otherrole(Role, OtherRole),
	dyn_dat_term(OtherId,socket_role(OtherRole,s(Role1,
						   abmp(File,_Date)))),
	(Role1 == Role
	->	true
	;	mformat('IMPL ERROR:mismatch role2~n'),
		halt(1)
	),
	num_from_sock_file(File, Num),
	dyn_id(Id, _Participant, _DatFile, Num).


buysel_auto_man(Id, AMBuyer, AMSeller) :-
	ensure_req(Id,saved_fact(external_planning__human_buyer_interfaces,
				      bidding(AMBuyer))),
	ensure_req(Id,saved_fact(external_planning__human_seller_interfaces,
					 bidding(AMSeller))).

pr_dat_d(Id, Tab) :-
	(dyn_id(Id, _Participant, File, _Num)
	->	true
	;	mformat('ERROR:missing dyn_id~n')
	),
	mformat('~wFILE:~w  Id:~w~n',[Tab,File,Id]),
	(buysel_auto_man(Id, AMBuyer, AMSeller)
	->	mformat('~w BIDDING buyer:~w seller:~w~n', [Tab,AMBuyer,AMSeller])
	;	true
	),
	(dyn_deal_info(Id, Res)
	->	mformat('~w Deal:~w~n', [Tab,Res])
	;	mformat('~w ERROR:deal not stored~n',[Tab])
	).

role_bid_info(Id, Role, Object-Bids) :-
	id_object(Id, Object),
	(setof(Object-Bids1,
	      setof(Round-Vals,
		    setof(Attr-Value, bid_attr(Id, Object, Round, Attr, Value, Role), Vals), Bids1), OBids),
		OBids = [Object-Bids|R]
	->	assert_debug(R == [])
	;	Bids = []
	).



:- dynamic dyn_bid_how/3, dyn_object/2.
set_object(Id, Object) :-
	dyn_object(Id, Object),!.
set_object(Id, Object) :-
	ground(Object),
	!,
	assertz(dyn_object(Id, Object)).
set_object(Id, Object) :-
	dyn_dat_term(Id, saved_fact(ws(human_buyer_interface, selected_proposal), to_be_communicated_to(selected_proposal('STR'(Object), 'STR'('Auto Jansen'), _RId), pos, broker))),
	!,
	assertz(dyn_object(Id, Object)).
set_object(Id, Object) :-
	dyn_dat_term(Id, saved_bid(Object, _Attr, _Value, _Round, _Rep1,
					   _RId, _EPRole)),
	!,
	assertz(dyn_object(Id, Object)).
set_object(Id, Object) :-
	dyn_dat_term(Id, initial_selected_object(Object)),
	!,
	warning('Selected object inferred from initial selecte object'),
	assertz(dyn_object(Id, Object)).


set_object(Id, some_object) :-
	error('Missing object for ~w',[Id]),
	assertz(dyn_object(Id, some_object)).
set_bidding(Id, Role) :-
	dyn_bid_how(Id, Role, _),
	!.
set_bidding(Id, Role) :-
	role_info(Role, Rep, EPRole, VisRep),
	(dyn_dat_term(Id, saved_bid(Object, Attr, Value, Round, Rep1,
					   _RId, EPRole)),
		Rep = Rep1
	->	assertz(dyn_bid_how(Id, Role, saved_bid_self))
	;	otherrole(Role, OtherRole),
		role_info(OtherRole, _, EPOtherRole),
		dyn_dat_term(Id, saved_bid(Object, Attr, Value, Round, other,
					   _RId, EPOtherRole))
	->	assertz(dyn_bid_how(Id, Role, saved_bid_other))
	;dyn_dat_term(Id, vis_fact(VisRep, Round, Attr, Value))
	->	assertz(dyn_bid_how(Id, Role, vis_fact)),
		Object = some_object
	;	assertz(dyn_bid_how(Id, Role, nothing)),
		warning('No bids for role ~w', [Role])
	).



bid_attr(Id, Object, Round, Attr, Value, Role) :-
	set_bidding(Id, Role),
	(dyn_bid_how(Id, Role, How)
	->	true
	;	impl_error(missing_hoew)
	),
	role_info(Role, Rep, EPRole, VisRep),
	(How == saved_bid_self
	->	dyn_dat_term(Id, saved_bid(Object1, Attr, Value, Round, Rep1,
					   _RId, EPRole)),
		Rep = Rep1
	;How == saved_bid_other
	->	otherrole(Role, OtherRole),
		role_info(OtherRole, _, EPOtherRole),
		dyn_dat_term(Id, saved_bid(Object1, Attr, Value, Round, other,
					   _RId, EPOtherRole))
	;How == vis_fact
	->	(table_acc(Attr);Attr=price),
		dyn_dat_term(Id, vis_fact(VisRep, Round, Attr, Value))
	;	assert_debug(How == nothing),
		fail
	),
	assert_debug(Object = Object1).





/*
  First find all matching Ids by analysing File
  Then look at configuration info
  Then look at bidding info
  */
find_linked_nego(Id, Role, File, _Date) :-
	mformat('~nLinking socket buyer dat file:~n', []),
	pr_dat_d(Id, '   '),
	(dyn_deal_info(Id, Res)
	->	mformat('  Deal:~w~n', [Res])
	;	mformat('ERROR:deal not stored~n')
	),
	(all_accessoire_info(Id, ds_all_acc(HBI,HSI))
	->	pr_acc_info(HBI,HSI)
	;	mformat('ERROR:~w:Missing accessoire pref info~n',
				[Id]),
		fail
	),
	assert_debug(Role == buyer),
	role_bid_info(Id, Role, Object-OwnBI),
	otherrole(Role, OtherRole),
	role_bid_info(Id, OtherRole, Object1-OtherBI),
	assert_debug(Object==Object1),
	(setof(OtherId, linked_other_candidate(Id, Role, File, OtherId),
	      OtherIds)
	->	OtherIds = [_OtherId1|R],
		length(OwnBI, N1),
		length(OtherBI, N2),
		FoundDist is N1 + N2 + 1,
		(filter_matching_bids(OtherIds, FoundDist, Object, OwnBI, OtherBI,
				     [], OtherIdsMinBidEqs)
		->	(	OtherIdsMinBidEqs == []
			->	mformat('WARNING:~w:No partner found~n',[Id])
			;	OtherIdsMinBidEqs = [OtherId1-NewOwnBI-NewOtherBI]
			->	test_partner_info(Id, OtherId1, Role, OtherRole),
				assert_debug(NewOwnBI == OwnBI),
				assert_debug(NewOtherBI == OtherBI)
			;	mformat('ERROR:~w:Multiple partner candidates :~w~n',
					[Id,OtherIds])
			)
		;	R == []
		->	warning('Lost only partner candidate')
		;	warning('Lost all partner candidates')
		)
	;	mformat('WARNING:~w:No partner found~n',[Id])
	).

filter_matching_bids([], _MinDiff, _Object, _OwnBI, _OtherBI, Fd, Fd).

filter_matching_bids([OtherId|OtherIds], MinDiff, Object, OwnBI, OtherBI, MatchingIn, Matching) :-
	(	other_bids_distance(OtherId, Object, OwnBI, OtherBI, NextDiff, NewOwnBI, NewOtherBI),
		(	NextDiff < MinDiff
		->	filter_matching_bids(OtherIds, NextDiff, Object, OwnBI, OtherBI, [OtherId-NewOwnBI-NewOtherBI],
					     Matching)
		;	NextDiff =:= MinDiff
		->	filter_matching_bids(OtherIds, MinDiff, Object, OwnBI, OtherBI,
				     [OtherId-NewOwnBI-NewOtherBI|MatchingIn], Matching)
		;	fail
		)
	->	true
	;	filter_matching_bids(OtherIds, MinDiff, Object, OwnBI, OtherBI,MatchingIn, Matching)
	).
other_bids_distance(OtherId, Object, OwnBI, OtherBI, NextDiff, NewOwnBI, NewOtherBI) :-
	role_bid_info(OtherId, seller, Object1-OtherBI1),
	role_bid_info(OtherId, buyer, Object2-OwnBI1),
	assert_debug(Object1==Object2),
	(Object == Object1
	->	(    OwnBI == OwnBI1,
			OtherBI1 == OtherBI1
		->	NewOwnBI = OwnBI,
			NewOtherBI = OtherBI,
			NextDiff = 0
		;	warning('Only allow identical bidding'),
			fail
		)
	;	warning('Didnt expect different cars'),
		fail
	).

:- dynamic dyn_socket_negos/2.

test_partner_info(Id,OtherId1, Role, OtherRole) :-
	(dyn_dat_term(OtherId1,socket_role(OtherRole,s(Role,
						       abmp(OFile,ODate))))
	->	assertz(dyn_handled_dat_term(OtherId1,
					     socket_role(OtherRole,
							 s(Role,
							   abmp(OFile,
								ODate)
							  )))),
		assertz(dyn_socket_negos(Id, OtherId1)),
		mformat('   NEGO PARTNER:~n'),
		pr_dat_d(OtherId1, '       ')
	;	mformat('ERROR: partner ~w not found~n',
			[OtherId1])
	).


require_term(initial_factor(human_seller_interface, Var1, concession_factor(0.9, Var1))).
require_term(initial_factor(human_seller_interface, Var1, configuration_tolerance(0.9,Var1))).
require_term(initial_factor(human_seller_interface, V, min_financial_margin(0.1, V))).
require_term(initial_factor(human_seller_interface, _G989, max_financial_margin(0.3, _G989))).
require_term(initial_factor(human_seller_interface, V, financial_rationality_factor(0.5, V))).



require_term(script_comp(configure_pa_cm_ajr)).
require_term(script_comp(configure_pa_cm_br)).
require_term(script_comp(initialization_by_tester)).
require_term(script_comp(initialization_mai)).

require_term(script_fact(configure_pa_cm_ajr, 1, allowed_utility_gap(0.02), true)).
require_term(script_fact(configure_pa_cm_ajr, 1, minimal_utility_shift(0.1), true)).
require_term(script_fact(configure_pa_cm_ajr, 1, allowed_price_gap(200), true)).

require_term(script_fact(configure_pa_cm_br, 1, allowed_utility_gap(0.02), true)).
require_term(script_fact(configure_pa_cm_br, 1, minimal_utility_shift(0.006), true)).
require_term(script_fact(configure_pa_cm_br, 1, allowed_price_gap(250), true)).
require_term(script_fact(initialization_by_tester, 1, default_speed(0.5), true)).
require_term(script_fact(initialization_by_tester, 1, default_impatience_factor(4), true)).
require_term(script_fact(initialization_by_tester, 1, default_configuration_tolerance(0.5),
			 true)).
require_term(script_fact(initialization_by_tester, 1, default_concession_factor(0.3), true)).
require_term(script_fact(initialization_mai, 1, default_speed(0.4), true)).
require_term(script_fact(initialization_mai, 1, default_impatience_factor(4), true)).
require_term(script_fact(initialization_mai, 1, default_discard_factor(0.7), true)).


require_term(bidding(buyer, _Kind)).
require_term(bidding(seller, _Kind)).




require_term(init_bi([tgm([]), no_leap_button, no_slope(human_buyer_interface)])).

require_term(saved_fact(external_planning__human_buyer_interfaces, bidding(_AM))).

require_term(saved_fact(external_planning__human_seller_interfaces, bidding(_AM))).

attr_val(good).
attr_val('fairly good').
attr_val(standard).
attr_val(meager).
attr_val(none).



table_initial_attr(brand_and_type).
table_initial_attr(color).
table_initial_attr(number_of_doors).
table_initial_attr(fuel).
table_initial_attr(price_class).
linear_initial_attr(age).
linear_initial_attr(price_class).
linear_initial_attr(mileage).


attr_value(brand_and_type, volvo).
attr_value(brand_and_type, mazda).
attr_value(brand_and_type, peugeot).
attr_value(brand_and_type, mercedes).
attr_value(color, Value) :-
	member(Value, [red,green,blue,yellow]).
attr_value(number_of_doors, Value) :-
	between(2,5, Value).
attr_value(fuel, Value) :-
	member(Value, [benzine, lpg, gasoline]).


ignore_term(text(_,_)).

store_term(hidenash).
store_term(nashsize(500)).
store_term(nonash).

store_term(hidevisualisation).
store_term(table(human_seller_interface, accessoire, Attr, Val, _)) :-
	table_acc(Attr),
	attr_val(Val).
store_term(table(human_buyer_interface, accessoire, Attr, Kind, _)) :-
	    table_acc(Attr),
	    attr_val(Kind).
store_term(att_imp(human_seller_interface, accessoire, Attr, _Val)) :-
	table_acc(Attr).
store_term(att_imp(human_buyer_interface, accessoire, Attr, _)) :-
	(table_acc(Attr);Attr=price).

store_term(saved_fact(ws(human_seller_interface, accessoire),
			to_be_communicated_to(financial_rationality_factor(0.5,
									   request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))).
store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(max_financial_margin(0.3, request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))).
store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(min_financial_margin(0.1, request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))).
store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(configuration_tolerance(0.9, request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))).
store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(concession_factor(0.9, request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))).
store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(attribute_importance(Attr, _, _RId), pos, representative_of('STR'('Auto Jansen'))))) :-
	table_acc(Attr).
store_term(saved_fact(ws(human_buyer_interface, accessoire), to_be_communicated_to(attribute_importance(Attr, _, _RId), pos, buyer_representative))) :-
	(table_acc(Attr);Attr = price).


store_term(saved_fact(ws(human_seller_interface, accessoire), to_be_communicated_to(eval_description_for(table, entry('STR'(Val), _), Attr, request_id('STR'(maarten), 'STR'(r1))), pos, representative_of('STR'('Auto Jansen'))))) :-
	attr_val(Val),
	table_acc(Attr).
store_term(saved_fact(ws(human_buyer_interface, accessoire), to_be_communicated_to(eval_description_for(table, entry('STR'(Val), _), Attr, _RId), pos, buyer_representative))) :-
	attr_val(Val),
	table_acc(Attr).

store_term(saved_fact(ws(human_buyer_interface, accessoire), to_be_communicated_to(eval_description_for(function_type, linear(_Ext, _Slope), price, _RId), pos, buyer_representative))
).



store_term(saved_fact(ws(human_buyer_interface, initial), to_be_communicated_to(attribute_importance(Attr, _, _RId), pos, broker))) :-
	(table_initial_attr(Attr)
	;linear_initial_attr(Attr)
	).
store_term(saved_fact(ws(human_buyer_interface, initial), to_be_communicated_to(eval_description_for(function_type, linear(_Ext, _Sl), Attr, _RId), pos, broker))) :-
	linear_initial_attr(Attr).
store_term(saved_fact(ws(human_buyer_interface, initial),
		      to_be_communicated_to(eval_description_for(table, entry(V, _), Attr, _RId), pos, broker))) :-
	attr_value(Attr, Value),
	tr_value(Value, V).



store_term(saved_fact(ws(human_buyer_interface, selected_proposal), to_be_communicated_to(selected_proposal('STR'(_Obj), 'STR'('Auto Jansen'), _RId), pos, broker))).








store_term(table(human_buyer_interface, initial, Attr, Value, _)) :-
	attr_value(Attr, Value).


store_term(att_imp(human_buyer_interface, initial, Attr, _)) :-
	table_initial_attr(Attr).
store_term(att_imp(human_buyer_interface, initial, Attr, _)) :-
	linear_initial_attr(Attr).


store_term(linear_def(human_buyer_interface, initial, Attr, _Ext, _Slope)) :-
	linear_initial_attr(Attr).

store_term(linear_def(human_buyer_interface, accessoire, price, _Ext, _Slope)).





store_term(saved_fact(_, my_utility_of_my_bid_in_round(_, _))).
store_term(saved_fact(_, my_utility_of_opponents_bid_in_round(_, _))).
store_term(saved_fact(_, no_deal(self, _RId, 'STR'('No reason given')))).
store_term(saved_fact(_, others_last_bid_ok(_RId))).
store_term(saved_fact(_EPHXI, deal_confirmed(self, _RId))).



store_term(initial_selected_object(_Object)).
store_term(bi_leap(human_buyer_interface)).
store_term(bi_leap(human_seller_interface)).
store_term(socket_role(seller, _)).
store_term(socket_role(buyer, _)).
store_term(saved_bid(_Object, _Attr, _Value, _Round, buyer_representative,
		     _RId,
		     external_planning__human_buyer_interfaces)).
store_term(saved_bid(_Object, _Attr, _Value, _Round,
		     representative_of('$STR'('Auto Jansen')),
		     _RId,
		     external_planning__human_seller_interfaces)).
store_term(saved_bid(_Object, _Attr, _Value, _Round,
		     other,
		     _RId,
		     external_planning__human_seller_interfaces)).
store_term(saved_bid(_Object, _Attr, _Value, _Round,
		     other,
		     _RId,
		     external_planning__human_buyer_interfaces)).
store_term(vis_fact(_Rep, _Round, _What, _Val)).
store_term(answer(HI, Action, TrueFalse)) :-
	(	ground(HI),
		ground(Action),
		ground(TrueFalse),
		hi(HI),
		memberchk(Action, [permission_to_close_the_deal, deal_confirmed_other_side, other_side_agrees])
	->	true
	;	error('Unexpected answer ~w', [answer(HI, Action, TrueFalse)]),
		trace,
		fail
	).
store_term(run(_)).

ensure_role_info(A,B,C,D,E) :-
	(role_info(A,B,C,D,E)
	->	true
	;	impl_error('Missing ~w', [role(A,B,C,D)])
	).

role_info(buyer, buyer_representative, external_planning__human_buyer_interfaces,
	  buyer_representative, human_buyer_interface).
role_info(seller, representative_of('$STR'('Auto Jansen')), external_planning__human_seller_interfaces, representative_of('STR'('Auto Jansen')),human_seller_interface).

role_info(A, B, C, D) :-
	role_info(A,B,C,D,_E).
role_info(A, B, C) :-
	role_info(A,B,C,_).





tr_value(Value, V) :-
	(number(Value)
	->	V = Value
	;	V = 'STR'(Value)
	).



:- dynamic dyn_dat_term/2, dyn_unhandled_term/1, dyn_require_term/2,
	dyn_handled_dat_term/2.

reset_info(Id) :-
	(var(Id)
	->	mformat('IMPL ERROR:var id reset~n'),
		trace,
		halt(1)
	;	true
	),
	retractall(dyn_id(Id, _Participant, _DatFile, _Num)),
	retractall(dyn_require_term(Id, _Term1)),
	retractall(dyn_dat_term(Id, _Term2)),
	retractall(dyn_handled_dat_term(Id, _)),
	retractall(dyn_socket_negos(Id, _OtherId)),
	retractall(dyn_socket_negos(_, Id)).



ensure_req(Id,Term) :-
	(dyn_require_term(Id, Term)
	->	true
	;	mformat('Missing term ~w~n',[Term]),
		fail
	).

id_object(Id, Object) :-
	set_object(Id, Object).
/*
	dyn_dat_term(Id, initial_selected_object(Object));
	dyn_dat_term(Id, saved_fact(ws(human_buyer_interface, selected_proposal), to_be_communicated_to(selected_proposal('STR'(Object), 'STR'('Auto Jansen'), _RId), pos, broker))).
*/
analyse_use(Id) :-
	(	id_object(Id, Object)
	->	ensure_req(Id,saved_fact(external_planning__human_buyer_interfaces,
				      bidding(AMBuyer))),
		ensure_req(Id,saved_fact(external_planning__human_seller_interfaces,
				      bidding(AMSeller))),
		mformat('BIDDING buyer:~w seller:~w~n', [AMBuyer,AMSeller]),
		(dyn_dat_term(Id, socket_role(buyer, s(Seller,abmp(File,Date))))
		->	(Seller == seller
			->	true
			;	mformat('ERROR: expected ~w == seller~n', [Seller]),
				fail
			),
			(AMBuyer == manual
			->	true
			;	mformat('ERROR: expected ~w == manual~n', [AMBuyer]),
				fail
			),
			mformat('  SOCKET buyer  seller:~w   ~w~n', [File,Date])
		;	true
		),
		(dyn_dat_term(Id, socket_role(seller, s(Buyer,abmp(File2,Date2))))
		->	(Buyer == buyer
			->	true
			;	mformat('ERROR: expected ~w == ~w~n', [Buyer]),
				fail
			),
			(AMSeller == manual
			->	true
			;	mformat('ERROR: expected ~w == manual~n', [AMSeller]),
				fail
			),
			mformat('  SOCKET seller  buyer:~w   ~w~n', [File2,Date2])
		;	true
		),
		analyse_deal(Id, Object)
	;	mformat('Missing selected object, probably nego not started~n'),
		fail
	).
other_interfaces(external_planning__human_buyer_interfaces,
		 external_planning__human_seller_interfaces).
other_interfaces(external_planning__human_seller_interfaces,
		 external_planning__human_buyer_interfaces).

:- dynamic dyn_deal_info/2.

analyse_deal(Id, _Object) :-
	(	dyn_dat_term(Id, saved_fact(EP__human_X_interfaces,
					    others_last_bid_ok(RId)))
	->	mformat('BIDDING OTHERSLASTBIDOK:~w~n',[EP__human_X_interfaces]),
		ensure_role_info(Role, _, EP__human_X_interfaces, _, _),
		Res = accepted_others_bid(Role, Res1),
		other_role_response(Role, Id, Res1)
	;	dyn_dat_term(Id, saved_fact(EP__human_X_interfaces,
					    no_deal(self, RId,_REason)))
	->	mformat('BIDDING NO DEAL:~w~n',[EP__human_X_interfaces]),
		ensure_role_info(Role, _, EP__human_X_interfaces, _, _),
		Res = no_deal(Role)
	;	dyn_dat_term(Id, answer(HI, permission_to_close_the_deal, TF))
	->	ensure_role_info(Role, _, _, _,HI),
		(TF == true
		->	Res = accepted_others_bid(Role, Res1),
			other_role_response(Role, Id, Res1)
		;	Res = no_deal(Role)
		)
	;	mformat('BIDDING RESULT UNKNOWN~n'),
		Res = unknown
	),
	assertz(dyn_deal_info(Id, Res)).
other_role_response(Role, Id, Res1) :-
	ensure_role_info(Role, _, EP__human_X_interfaces, _, _),
	other_interfaces(EP__human_X_interfaces,EP__human_X0_interfaces),
	(dyn_dat_term(Id, saved_fact(EP__human_X0_interfaces,
					     deal_confirmed(self, _)))
	->	mformat('  BIDDING other deal_confirmed~n'),
		Res1 = confirmed
	;	dyn_dat_term(Id, saved_fact(EP__human_X0_interfaces,
						    no_deal(self, _, _Re)))
	->	mformat('  BIDDING deal_rejected~n'),
			Res1 = rejected
	;	otherrole(Role, OtherRole),
		ensure_role_info(OtherRole, _, _, _,HI),
		dyn_dat_term(Id, answer(HI, Answer, TF)),
		(Answer == other_side_agrees
		->	(TF == true
			->	Res1 = confirmed
			;	Res1 = rejected
			)
		;	mformat('  BIDDING deal in limbo~n'),
			Res1 = limbo
		)
	;	mformat('  BIDDING deal in limbo~n'),
			Res1 = limbo
	).

			
/*
  Result: answer(HI, Action, TrueFalse)
  Action: permission_to_close_the_deal
  Action: deal_confirmed_other_side
  Action: other_side_agrees
  answer(P) in demos.pl
  save_bi(answer(HI, Action, TF)) in
  do_hbi_modal(Action, HI, Interface, Term) :-
	saved_answer(Action, HI, TF),

  saved_answer(Action, HI, TF) :-
	retract(dyn_action_answer(Action, HI, TF)).
  user:save_answer(Action, HI, TF) :-

  permission_to_close_the_deal:
  new(AB, button(accept, 
				message(@prolog, save_answer, permission_to_close_the_deal, HI, true)
  send(new(button(no, 
				message(@prolog, save_answer, permission_to_close_the_deal, HI, false)

  Other:
deal_fact(deal_confirmed(opponent,RId), RId, deal_confirmed_other_side).
deal_fact(others_agreement_my_last_bid(RId), RId, other_side_agrees).
do_hi_no_to_be_provided(HI, Term) :-
	deal_fact(What, _RId, Action),
	Fact = communicated_by(What, pos, _Other),
	con_get_fact(Fact, true),
	!,
	(\+ auto_bidding(HI)
	->	con_termination(Term)
	;	do_yes_no_dialog(HI, Action),
		Term = non_modal
	).
  
  */
analysedatterm(Term, Id) :-
	require_term(Term),
	assertz(dyn_require_term(Id, Term)),
	!.

analysedatterm(Term, _Id) :-
	ignore_term(Term),
	!.
analysedatterm(Term, Id) :-
	store_term(Term),
	!,
	assertz(dyn_dat_term(Id, Term)).

analysedatterm(Term, _Id) :-
	functor(Term, F, A),
	functor(Term1, F, A),
	(dyn_unhandled_term(Term1)
	->	true
	;	mformat('Unhandled term ~w~n', [Term]),
		assertz(dyn_unhandled_term(Term1))
	).

/*
analysedatterm(Term, Id) :-
	
	assertz(dyn_dat_term(Id, Term)).
*/
%	mformat('Dat ~w:Unhandled term:~w~n', [Id, Term]).


