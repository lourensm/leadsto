:- module(abmptrace,[
	  construct_traces/0
	 ]).
:- use_module(util).
:- use_module(abmputil).

%:- ensure_loaded('bids.pl').

/*
TODO:

- herbereken utilities en vergelijk resultaat!
*/
/*
go :-
	ensure_loaded('bids.pl'),
	setof(TraceName1, Fact^fact(TraceName1, Fact), TraceNames),
	(	member(TraceName, TraceNames),
		go(TraceName),
		fail
	;	true
	),
	halt.
*/
construct_traces :-
	(setof(TraceName1, Fact^fact(TraceName1, Fact), TraceNames)
	->	true
	;	warning('No candidate traces found'),
		TraceNames = []
	),
	(	member(TraceName, TraceNames),
		go(TraceName),
		fail
	;	true
	).

fact(Trace, Fact) :-
	dyn_fact(Trace, Fact).

bmf(auto,a).
bmf(manual,m).
bmf(socket,s).

file_from_trace_name(p(L,_R), TraceF) :-
	abmp:dyn_id(L, Participant, DatFile, Num),
	concat_atom([L,ss, '.tr'], TraceF).
file_from_trace_name(single(Dir,Num), TraceF) :-
	Trace = single(Dir, Num),
	(fact(Trace, negotiation(idbuyerseller(Id,AMBuyer, AMSeller)))
	->	bmf(AMBuyer, BAT),
		bmf(AMSeller, SAT)
	;	impl_error('Missing negotiation fact')
	),
	concat_atom([Dir,'DAT',Num,BAT,SAT, '.tr'], TraceF).
save_fact(Fact) :-
	format(trace, '~q.~n', [Fact]).

save_trace(Atom, Ranges) :-
	atom_key(Atom, Key),
	save_fact(atom_trace(Key, Atom, Ranges)).


go(TraceName) :-
	file_from_trace_name(TraceName, File),
	format('Writing trace file ~w~n', [File]),
	open(File, write, _, [alias(trace)]),
	save_fact(content(type(generatedtrace('bids.pl')))),
	(	member(Info, [object(_),negotiation(_),extra_info(_)]),
		fact(TraceName, Info),
		save_trace(Info, [range(mininf,maxinf, true)]),
		fail
	;	true
	),
	save_util_info(TraceName),
	bids(TraceName, buyer, BidsBuyer, BL),
	save_bids(BidsBuyer, TraceName, buyer),
	bids(TraceName, seller, BidsSeller, SL),
	save_bids(BidsSeller, TraceName, seller),
	(	fact(TraceName, deal_first(Role1, Res))
	->	(Role1 == buyer
		->	LR is BL + 1
		;	LR is SL + 1
		),
		round_range(Role1, LR, DealRange),
		save_trace(deal_first(Role1, Res), [DealRange]),
		(fact(TraceName, deal_second(Role2, Res2))
		->	(Role2 == buyer
			->	LR2 is BL + 1
			;	LR2 is SL + 1
			),
			round_range(Role2, LR2, DealRange2),
			save_trace(deal_second(Role2, Res2), [DealRange2])
		;	true
		)
	;	true
	),
	LRAll is max(BL, SL) + 2,
	round_start(seller, LRAll, LT),
	save_fact(times(0, maxinf, LT)),
	format('Finished trace ~w~n', [File]),
	close(trace),!.
go(TraceName) :-
	error('Failed to generate correct trace ~w', [TraceName]),
	fail.



save_bids([], _TraceName, _).
save_bids([Round-Bid|Bids], TraceName, Role) :-
	save_bid(TraceName, Role, Round, Bid),
	save_bids(Bids, TraceName, Role).
save_bid(TraceName, Role, Round, Bid) :-
	round_range(Role, Round, Range),
	save_trace(bid(Role, Round, Bid), [Range]),
	save_bid_utils(TraceName, Role, Round, Bid),!.
save_bid(TraceName, Role, Round, Bid) :-
	(is_local
	->	local_trace(save_bid_error),
		save_bid(TraceName, Role, Round, Bid)
	;	true
	),
	impl_error(save_bid).

	

bid_util(Bid, UtilityRole, Utility) :-
	bagof(TAttr, table_acc(TAttr), TAttrs),
	Attrs = [price|TAttrs],
	Bid =.. [bid|BidVals],
	length(Attrs, N),
	assert_debug(length(BidVals, N)),
	sum_bid_utils(Attrs, BidVals, UtilityRole, Utility).
sum_bid_utils([], [], _UtilityRole, 0).
sum_bid_utils([Attr|Attrs], [BidVal|BidVals], UtilityRole, U) :-
	sum_bid_utils(Attrs, BidVals, UtilityRole, U1),
	bid_util(Attr, BidVal, UtilityRole, U2),
	U is U1 + U2.
bid_util(price, Price, buyer, U) :-
	!,
	utility_info(buyer_price_utility_info(PriceMax,BUMax,BUFact,
					      PriceBUZero)),
	(Price < PriceMax
	->	U = BUMax
	;Price > PriceBUZero
	->	U = 0.0
	;	U is BUFact*(Price - PriceBUZero)
	).
bid_util(price, Price, seller, U) :-
	!,
	utility_info(seller_price_utility_info(BCost, SFactor)),
	U is SFactor*(Price - BCost).
bid_util(Attr, BidVal, UtilityRole, U) :-
	nonvar(BidVal),
	BidVal = 'STR'(BidVal1),
	bid_util(Attr, BidVal1, UtilityRole, U).

bid_util(Attr, BidVal, UtilityRole, U) :-
	utility_info(attr_utility(UtilityRole, Attr, BidVal, U)).

pr_if_diff(TraceName,Role,Header, T1, T2) :-
	(abs(T1 - T2) < 0.01
	->	true
	;	format('Trace:~w:~w''s bid ~w:Unexpected difference: ~w - ~w~n',
		       [TraceName,Role,Header, T1, T2])
	).

save_bid_utils(TraceName, Role, Round, Bid) :-
	round_range(Role, Round, Range),
	bid_util(Bid, buyer, BUtility1),
	(fact(TraceName, bid_utility(Round, Role, buyer, BUt))
	->	save_trace(bid_utility(Round, Role, buyer, BUt), [Range]),
		pr_if_diff(TraceName,Role,'Buyer Utilities', BUt, BUtility1)
	;	warning('Buyer utility from calculated value~n'),
		save_trace(bid_utility(Round, Role, buyer, BUtility1), [Range])
	),
	(fail,Role == seller
	->	trace
	;	true
	),
	bid_util(Bid, seller, SUtility1),
	(	fact(TraceName, bid_utility(Round, Role, seller, SUt))
	->	save_trace(bid_utility(Round, Role, seller, SUt), [Range]),
		pr_if_diff(TraceName,Role,'Seller Utilities', SUt, SUtility1)
	;	warning('Seller utility from calculated value(~w,~w,~w)~n',
			[Role, Round,SUtility1]),
		save_trace(bid_utility(Round, Role, seller, SUtility1),
			   [Range])
	).


	
round_range(Role, Round, range(Start, End, true)) :-
	round_start(Role, Round, Start),
	End is Start + 1.

round_start(buyer, Round, Start) :-
	Start is 2*Round.
round_start(seller, Round, Start) :-
	Start is 2*Round + 1.
/* TIMING POSSIBLY:
   1 : setup
   2 : buyer   so buyer bids : 2*BuyerRound
   3 : seller  so seller bids: 2*SellerRound + 1 
   n: deal_first
   n + 1: deal_second
   LastTime >= 2*LastBuyerRound
   LastTime >= 2*LastSellerRound
   
   */
bids(TraceName, Role, Bids, LastRound) :-
	(	setof(Round-Bid, fact(TraceName, bid(Role, Round, Bid)), Bids)
	->	last_round(Bids, LastRound)
	;	Bids = [],
		LastRound = 0
	).

last_round([Round-_Bid], Round) :-
	!.
last_round([_|RB], Round) :-
	last_round(RB, Round).

:- dynamic utility_info/1.
reset_utility_data :-
	retractall(utility_info(_)).

save_utility_data(Data) :-
	asserta(utility_info(Data)),
	save_trace(Data, [range(1, maxinf, true)]).

save_util_info(TraceName) :-
	reset_utility_data,
	fact(TraceName, ds_all_acc(ds_hi_acc(PriceMax, PriceSlope, PriceImp,
					     AttrDataB),
				   ds_hi_acc(undefined, undefined, undefined,
					     AttrDataS))),
	PriceBUZero is PriceMax - 1.0/PriceSlope,
	get_sum_imp_acc(AttrDataB, SIB1),
	SIFB is SIB1 + PriceImp,
	(PriceImp =:= 0.0
	->	zero_correction_aibu(BUMax)
        ;       BUMax is PriceImp/SIFB
        ),
	(	SIFB =:= 0
	->	BUFact = 0
	;	BUFact is PriceSlope*PriceImp/SIFB
	),
	save_utility_data(buyer_price_utility_info(PriceMax, BUMax, BUFact, PriceBUZero)),
	% Buyer Price Utility = (Price < PriceMax ? BUMax:Price > PriceBUZero?0: BUFact*(Price - PriceBUZero)
	fact(TraceName, fin_info_seller(FRF, MFM, BCost, TAttrFEDs)),
	N is MFM* BCost,
	SFactor is FRF/N,
	save_utility_data(seller_price_utility_info(BCost, SFactor)),
	% Seller Price Utility = SFactor*(Price - BCost)  Klopt dit??
	save_buyer_attr_utility_info(AttrDataB, SIFB),
	get_sum_imp_acc(AttrDataS, SIFS),
	save_seller_attr_utility_info(AttrDataS, FRF,MFM,BCost,SIFS,TAttrFEDs).
save_buyer_attr_utility_info([],_).

save_buyer_attr_utility_info([ds_attr(Attr, Evals,AttrImp)|AttrDataB], SIFB) :-
	(	member(ds_eval(AttrVal, LUt), Evals),
		save_buyer_attr_utility_info_val(Attr,AttrImp,SIFB,AttrVal,
						 LUt),
		fail
	;	true
	),
	save_buyer_attr_utility_info(AttrDataB, SIFB).

save_buyer_attr_utility_info_val(Attr, AttrImp, SIFB, AttrVal, LUt) :-
	Ut is LUt*AttrImp/SIFB,
	save_utility_data(attr_utility(buyer, Attr, AttrVal, Ut)).
save_seller_attr_utility_info([], _FRF, _MFM, _BCost, _SIFS,_TAttrFEDs).

save_seller_attr_utility_info([ds_attr(Attr, Evals, AttrImp)|AttrDataS], FRF,
			      MFM, BCost, SIFS,TAttrFEDs) :-
	memberchk(Attr=Vals, TAttrFEDs),
	save_seller_attr_utility_attr(Attr, Evals, AttrImp, Vals, FRF, MFM,
				      BCost, SIFS),
	save_seller_attr_utility_info(AttrDataS, FRF, MFM, BCost, SIFS,
			      TAttrFEDs).
save_seller_attr_utility_attr(Attr, Evals, AttrImp, Vals, FRF, MFM, BCost,
			      SIFS) :-
	(	member(ds_eval(AttrVal, LUt), Evals),
		memberchk(AttrVal-Price, Vals),
		save_seller_attr_utility_attr1(Attr, AttrVal, LUt, Price,
					       AttrImp, FRF, MFM, BCost,SIFS),
		fail
	;	true
	),!.
save_seller_attr_utility_attr(Attr, Evals, AttrImp, Vals, FRF, MFM, BCost,
			      SIFS) :-
	error('~w failed', [save_seller_attr_utility_attr(Attr, Evals, AttrImp, Vals, FRF, MFM, BCost,
			      SIFS)]),
	trace.

save_seller_attr_utility_attr1(Attr, AttrVal, LUt, Price, AttrImp, FRF, MFM,
			       BCost,SIFS) :-
	N is MFM * BCost,
	EUt is LUt * AttrImp/SIFS,
	FUt is - Price/N,
	Ut is FRF*FUt + (1.0 - FRF)*EUt,
	save_utility_data(attr_utility(seller, Attr, AttrVal, Ut)).
get_sum_imp_acc([], 0).
get_sum_imp_acc([ds_attr(_Attr, _DSEvals, AttrImp)|AttrDataB], SIF) :-
	get_sum_imp_acc(AttrDataB, SIF1),
	SIF is SIF1 + AttrImp.

	
zero_correction_aibu(0.001).

/*
  Price Utility Buyer:
  (Price > PriceBUZero
  ->     BU = 0.0
  ;      Price < CutOffB
  ->     (AIB =:= 0
	->	zero_correction_aibu(BU)
        ;       BU is AIB/SIFB
        )
  ;	BU is SlopeB* AIB/SIFB*(Price - PriceBUZero)
  )
PriceBUZero is CutOffB - 1.0/SlopeB,
RESULT
  (Price > PriceBUZero
  ->     BU = 0.0
  ;      Price < CutOffB
  ->     BU = BUMax
  ;      BU is BUFact*(Price - PriceBUZero)
  )
*/

/*
Utility evaluation
  check_puc_role(RId, buyer,Comp) :-
          ech_eval_descr_non_price(RId, buyer, Comp),
          ech_linear_price_buyer(CutOffB, SlopeB, RId,Comp),
          ech_attr_imp(price,buyer,AIB,RId,Comp),
          check_sum_imp(buyer, RId, Comp, SIFB),
          BUMax is AIB/SIFB,
          (BUMax =:= 0
	  ->	warn(['attribute importance price zero, changed into very small value']),
		zero_correction_aibu(BUMax1)
	  ;	BUMax1 = BUMax
	  )

Buyer Attr:
  ech_ev_descr_for_table_buyer(Attrib,PValue,PE,RId,Comp),
  ech_sif(buyer, RId, Comp, SIF),
  ech_attr_imp(Attrib,buyer,AIB,RId, Comp),
  non_zero_check(SIF, 'SIF'),
  Ut is PE * AIB/SIF,

eval_description_for(function_type,
		linear(CutOffB, SlopeB), price,
		RId),buyer,
	     ech_linear_price_buyer(_CutOffB1, _SlopeB1, RId,Comp)

price_utility(buyer, RId, Comp, Price, BU) :-
	ech_linear_price_buyer(CutOffB, SlopeB, RId, Comp),
	non_zero_check(SlopeB, 'SlopeB'),
	PriceBUZero is CutOffB - 1.0/SlopeB,
	(Price > PriceBUZero
	->	BU = 0.0
	;	ech_attr_imp(price,buyer,AIB,RId,Comp),
		ech_sif(buyer, RId, Comp, SIFB),
		non_zero_check(SIFB, 'SIFB'),
		(	Price < CutOffB
		->	(AIB =:= 0
			->	zero_correction_aibu(BU)
			;	BU is AIB/SIFB
			)
		;	BU is SlopeB* AIB/SIFB*(Price - PriceBUZero)
		)
	).
check_puc_role(RId, seller,Comp) :-
	ech_eval_descr_non_price(RId, seller, Comp),
	ech_obj_nego_seller(OBJECT_ID,RId,Comp),
	ech_basic_cost_of(OBJECT_ID, BC,Comp),
	ech_fin_rat_fact_seller(FRF,RId,Comp),
	ech_mfm(MFM, RId,Comp),
	check_sum_imp(seller, RId, Comp, _SIFS)

SELLER Attr:
  ech_eev_descr_for_table_seller(Attrib,PValue,PE,RId,Comp),
  ech_attr_imp(Attrib,seller,AIB,RId,Comp),
  ech_sif(seller, RId, Comp, SIF),
  ech_fin_ev_descr_for_table_seller(Attrib,PValue,EFin,RId,Comp),
  ech_obj_nego_seller(OBJECT_ID,RId,Comp),
  ech_basic_cost_of(OBJECT_ID, BCost, Comp),
  ech_fin_rat_fact_seller(FRF,RId, Comp),
  ech_mfm(MFM, RId,Comp),
  N is MFM * BCost,
  non_zero_check(SIF, 'SIF'),
  EUt is PE * AIB/SIF,
  non_zero_check(N, 'MFM * BCost'),
  FUt is - EFin/N,
  Ut is FRF*FUt + (1.0 - FRF)*EUt

if 	observation_result(attribute_has_value(
               	O: OBJECT_ID, acquisition_price, V: integers),pos)
                  and	VR: reals = V: integers
               then	new_price_info(basic_cost_of(VR: reals, O: OBJECT_ID), 
               		pos);
if	attribute_value_price(A: NEGOTIANT_ATTRIBUTE, 
               	V: VALUE, I: reals)
                  and	negotiation_session_id(RID: REQUEST_ID)
               then	financial_eval_description_for(table, entry(V: VALUE, 
               		I: reals), A: NEGOTIANT_ATTRIBUTE, 
               		RID: REQUEST_ID);
ANWB internal? or
database.pl
auto_jansen_db : alternative specification database

SELLER gets observations from database.
pl with flag to_be_observed(car_db)

financial_eval_description_for(table,entry(VALUE, EFin),
			NEGO_ATTR, RId)
	ech_fin_ev_descr_for_table_seller(NEGO_ATTR,PVALUE,PEFin,
						    RId,Comp)
	
price_utility(seller, RId, Comp, Price, Ut) :-
	ech_fin_rat_fact_seller(FRF,RId,Comp),
	ech_mfm(MFM, RId,Comp),
	ech_obj_nego_seller(OBJECT_ID,RId,Comp),
	ech_basic_cost_of(OBJECT_ID, BCost,Comp),
	N is MFM * BCost,
	non_zero_check(N, 'MFM * BCost'),
	Ut is FRF*(Price - BCost)/N.
*/