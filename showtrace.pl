:- module(showtrace, []).
:- use_module(util).
:- use_module(algo).
:- use_module(library(lists)).

:- multifile version/2.
:- discontiguous version/2.
:- use_module(library(pce)).
:- use_module(psprinting).

:- multifile user:version/2.
user:version(showtrace, 8).

:- pce_begin_class(show_trace, ps_picture).
class_variable(atom_font, font, font(helvetica, roman, 18)).
class_variable(range_font, font, font(helvetica, roman, 10)).
class_variable(header_font, font, font(helvetica, roman, 18)).
class_variable(time_font, font, font(helvetica, roman, 10)).
class_variable(y_per_cont_trace, real, 200).


variable(traces, prolog, both).
variable(start_time, real, both).
variable(end_time, real, both).
variable(left_offset, real, both).
variable(x0t, real, both).
variable(alpha_t, real, both).
variable(max_x, int, both).
variable(base_t, real, both).
variable(last_t, real, both).
variable(step_t, real, both).
variable(sub_step_t, real, both).
variable(trace_height, int, both).
variable(cont_lines, bool, both).
variable(adjust_height, bool := @on, both).

variable(display_start_time, real, both).
variable(display_end_time, real, both).

% Do we want to have the time axis on each entry?
% Need start_time, end_time argument + display_start_time, display_end_time
% split_postscript(This, FilePrefix:char_array, HdivW:[real]) :->

% Probably we should have global values for print ranges
% Probably give calls Picture arguments

time_coord(This, Time:real, X:int) :<-
        get(This, alpha_t, Alpha),
	get(This, x0t, X0),
	X is round(X0 + Alpha*Time).

class_variable(true_pen, int, 4).
class_variable(false_pen, int, 4).
class_variable(unknown_pen, int, 1).

class_config_var(display/diagram/height, y_per_cont_trace).
class_config_var(display/diagram/true_pen, true_pen).
class_config_var(display/diagram/false_pen, false_pen).
class_config_var(display/diagram/unknown_pen, unknown_pen).

initialise(This) :->
	send(This, send_super, initialise, 'Trace Result'),
%	send(This, popup, @postscript_popup),
	send(This, popup, @ps_picture_popup),
	send(This, done_message, message(@prolog, user_forced_halt)),
	send(This, left_offset, 100),
	def_width(MX),
	send(This, max_x, MX),
	send(This, width, MX),
	send(This, adjust_height, @on),
	(	class_config_var(Path, _ClassVara),
		listen(This, set_config(lt_config:Path, VT2),
		       send(This, config, Path, VT2)),
		fail
	;	true
	),
	(	class_config_var(Path, _ClassVarb),
		get_config(lt_config:Path, VT3),
		send(This, config, Path, VT3),
		fail
	;	true
	).



config(This, Attr1:prolog, Value) :->
	(class_config_var(Attr1, ClassVar)
	->	send(?(This?class, class_variable, ClassVar), value,
		     Value)
	;	send(This, report, error, 'Unhandled config')
	).
y_per_cont_trace(This, Y:real) :<-
        get(This, class_variable_value(y_per_cont_trace), Y).

atom_tick(5). % extra mark in middle
atom_space(3). % extra space front of tick
ytext_gap(3).
max_height(700).

set_if(This, What:name, Arg:any) :->
	(Arg == @default
	->	true
	;	send(This, What, Arg)
	).

activate_print(This) :->
	send(This, display_traces1, @default, @default, @default).
display_traces(This,Traces1:[prolog],StartTime:[real],EndTime:[real],
	       Paging:[prolog]) :->
	(Paging == @default
	->	P = default
	;	P = Paging
	),
	send(This, paging, P),
	send(This, setup_display),
	send(This, display_traces1, Traces1, StartTime, EndTime),
	send(This, close_pages).

view(_This, View:prolog) :<-
        (get_option(view(View))
	->	true
	;	View = standard
	).

filter_traces(This, TracesIn, TracesOut) :-
	get(This, view, View),
	((algo:display(View,show_atoms(_));algo:display(View, no_show_atoms(_)))
	->	!,filter_trace(TracesIn, View, TracesOut)
	;	TracesOut = TracesIn
	).
filter_trace(TIn, View, TOut) :-
	filter_trace_in(TIn, View, TOut).

filter_trace_in([], _View, []).
filter_trace_in([AT|Traces], View, TracesOut) :-
	!,filter_trace_in(AT, View, Traces, TracesOut).
filter_trace_in(A-T, View, Traces, [A-T|TracesOut]) :-
	(algo:display(View,show_atoms(_))
	->	algo:display(View,show_atoms(A))
	;	true
	),
	\+ algo:display(View, no_show_atoms(A)),
	!,
	filter_trace_in(Traces, View, TracesOut).
filter_trace_in(_, View, Traces, TracesOut) :-
	filter_trace_in(Traces, View, TracesOut).
display_traces1(This,Traces1:[prolog],StartTime:[real],EndTime:[real]) :->
	send(This, set_if, start_time, StartTime),
	send(This, set_if, end_time, EndTime),
	send(This, set_if, display_start_time, StartTime),
	send(This, set_if, display_end_time, EndTime),
%	local_trace(traces),
	(Traces1 == @default
	->	get(This, traces, Traces)
	;	filter_traces(This, Traces1, Traces2),
		send(This, traces, Traces2),
		Traces = Traces2
	),
	assert_debug(ground(Traces)),
	partition_displays(Traces, AtomTraces, ContTraces),
	(	Traces1 == @default,
		StartTime == @default,
		EndTime == @default
	->	get(This, left_offset, LeftOffset),
		get(This, traces, Traces)
	;	get(This, class_variable_value(atom_font), AtomFont),
		max_atom_name_length(AtomTraces, AtomFont, AtomLength),
		atom_tick(AtomTick),
		atom_space(AtomSpace),
		LeftOffset is max(100, AtomLength + AtomTick + AtomSpace),
		send(This, left_offset, LeftOffset),
		get(AtomFont, ascent, Asc),
		get(AtomFont, descent, Desc),
		ytext_gap(Gap),
		MinHeight = 10,
		(	Asc + Desc + 2*Gap >= MinHeight
		->	Height1 is Asc + Desc + 2*Gap
		;	Height1 = MinHeight
		),
		Height is 2*round(Height1/2),
		send(This, trace_height, Height),
		send(This, recalc_t) % after left_offset
	),
	get(This, page_length_pixels, PLP),
	send(This, setup_pages),
	fill_traces(AtomTraces, This, YOffs, PLP, PLP, _PLPOut),
	(send(This, print_to_screen);ContTraces == []
	->	true
	;	send(This, report, warning, 'Printing only prints simple traces')
	),
	fill_cont_traces(ContTraces, LeftOffset, This, YOffs, YOut),
	(	get(This, adjust_height, @on),
		send(This, print_to_screen)
	->	max_height(H1),
		H is min(H1, YOut),
		send(This, height, H),
		send(This, ver_stretch, 100)
	;	true
	).

variable(rulers, hash_table := new(hash_table), both).
ensure_ruler(This, Kind:name, Ruler:ruler) :<-
        (get(This?rulers, member, Kind, Ruler)
	->	get(Ruler, device, D),
		(D == This
		->	send(Ruler, displayed, @on)
		;	send(This, display, Ruler)
		)
	;	send(This?rulers, append, Kind, new(Ruler,ruler(This, Kind))),
		send(This, display, Ruler)
	).
rm_rulers(This) :->
	send(This?rulers, for_all, message(@arg2, displayed, @off)).

set_ruler(This, Kind:name, T:prolog) :->
	(number(T)
	->	get(This, time_coord, T, X),
		get(This, ensure_ruler, Kind, Ruler),
		send(Ruler, set_x, X)
	;	get(This, ensure_ruler, Kind, Ruler),
		send(Ruler, displayed, @off)
	).






min_pts_def(100).
min_pts(Pts) :-
	(	get_option(ltshow(rangepixels(Pts))),
		(	integer(Pts),
			Pts >= 20
		->	true
		;	error('Ignoring option -tracerangepixels, argument illegal:~w',
			      [Pts]),
			fail
		)
	;
		min_pts_def(Pts)
	).
def_width(Pts) :-
	(	get_option(ltshow(picturepixels(Pts))),
		(	integer(Pts),
			Pts >= 200
		->	true
		;	error('Ignoring option -tracepicturepixels, argument illegal:~w',
			      [Pts]),
			fail
		)
	;
		def_width_def(Pts)
	).
def_width_def(1000).


/* Calculate time scale constants and time tick things */
recalc_t(This) :->
	get(This, class_variable_value(time_font), TimeFont),
	new(T1, text('200.0',@default, TimeFont)),
	get(T1, width, W),
	W1 is 2*W,
	get(This, left_offset, LeftOffset),
	%get(This, max_x, MaxX1),
	min_pts(MP),
	def_width(MaxX1),
	send(This, max_x, MaxX1),
	MaxX is max(MaxX1, max(MP + LeftOffset, W1*2 + LeftOffset)),
	(MaxX \= MaxX1
	->	send(This, max_x, MaxX),
		format('Made internal window size larger(~w) to accomodate atomname length~n', [MaxX])
	;	true
	),
	Fits is (MaxX - LeftOffset)/W1,
	(Fits < 2
	->
		impl_error('screen too small ~w: ~w ~w ~w',
			   [This,MaxX,LeftOffset,W1])
	;	true
	),
	get(This, display_start_time, ST),
	get(This, end_time, ET),
	scale_constants(ST, ET, Fits, Base, Step, Last),
	B1 is Base - Step/2,
	L1 is Last + Step/2,
	get_scale(B1, L1, LeftOffset, MaxX, Y0, Alpha),
	SN is floor(abs(Alpha*Step)/10),
	sub_step_size(Step, SN, SubStep),
	send(This, x0t, Y0),
	send(This, alpha_t, Alpha),
	send(This, base_t, Base),
	send(This, last_t, Last),
	send(This, step_t, Step),
	send(This, sub_step_t, SubStep).
print_times1(This, YL:int, AtY:int) :->
	get(This?x0t, value, X0T),
	get(This?alpha_t, value, AlphaT),
	get(This?base_t, value, BaseT),
	get(This?step_t, value, StepT),
	get(This?last_t, value, LastT),
	get(This?sub_step_t, value, SubStepT),
	get(This, class_variable_value(time_font), TimeFont),
	forall(betweenf(BaseT, LastT, SubStepT, V1),
	       (
		       X is X0T + AlphaT*V1,
		       send(This, display, line(X, AtY, X, AtY + 2))
	       )
	      ),
	forall(betweenf(BaseT, LastT, StepT, V),
	       (
		       X is X0T + AlphaT*V,
		       send(This, display, line(X, YL, X, AtY + 5)),
		       new(T, text(V, @default, TimeFont)),
		       send(This, display, T, point(X - T?width/2, AtY + 6))
	       )
	      ),
	Xs1 is X0T + AlphaT*BaseT,
	Xs2 is X0T + AlphaT*LastT,
	send(This, display, line(Xs1, AtY, Xs2, AtY)).
:- dynamic dyn_th/1.

print_times(This, AtY, YB) :-
	get(This?x0t, value, X0T),
	get(This?alpha_t, value, AlphaT),
	get(This?base_t, value, BaseT),
	get(This?step_t, value, StepT),
	get(This?last_t, value, LastT),
	get(This?sub_step_t, value, SubStepT),
	get(This, class_variable_value(time_font), TimeFont),
	forall(betweenf(BaseT, LastT, SubStepT, V1),
	       (
		       X is X0T + AlphaT*V1,
		       send(This, display, line(X, AtY, X, AtY - 2))
	       )
	      ),
	assert_debug(\+	dyn_th(_)),
	forall(betweenf(BaseT, LastT, StepT, V),
	       (
		       X is X0T + AlphaT*V,
		       send(This, display, line(X, AtY, X, AtY - 5)),
		       new(T, text(V, @default, TimeFont)),
		       send(This, display, T, point(X - T?width/2, AtY + 2)),
		       (dyn_th(_)
		       ->      true
		       ;       get(T?area, bottom_side, YB),

			       assertz(dyn_th(YB))
		       )
	       )
	      ),
	(retract(dyn_th(YB))
	->	true
	;	true
	).





:- pce_end_class.

marker_config_source(current, fv_config,sim_tool/marker/current).
marker_config_source(next, fv_config,sim_tool/marker/next).
marker_prop(colour).
marker_prop(width).

:- pce_begin_class(ruler, line).

set_x(This, X:integer) :->
	send(This, start_x, X),
	send(This, end_x, X).

initialise(This, ST:show_trace, Kind:name) :->
	get(ST, bounding_box, Area),
	send(This, send_super, initialise, 0, Area?y, 0, Area?y + Area?height),
	(	marker_config_source(Kind, Spec, Tree),
		marker_prop(Elem),
		get_config(Spec:Tree/Elem,Bg),
		send(This, update_settings, Elem, Bg),
		fail
	;	true
	),
	(	marker_config_source(Kind, Spec, Tree),
		marker_prop(Elem),
		listen(This, set_config(Spec:Tree/Elem, Bg2),
		       send(This, update_settings, Elem, Bg2)),
		fail
	;	true
	).
update_settings(This, What:name, Value:any) :->
	(What == colour
	->	send(This, colour, Value)
	;What == width
	->	send(This, pen, Value)
	;	send(This, report, error, 'Unrecognised property %s for marker',
		     What)
	).


:- pce_end_class.


fill_cont_traces([], _LeftOffset, _This, YOffs, YOffs).
fill_cont_traces([Trace|ContTraces], LeftOffset, This, YIn, YOut) :-
	fill_cont_trace(Trace, LeftOffset, This, YIn, Y1),
	fill_cont_traces(ContTraces, LeftOffset, This, Y1, YOut).
fill_cont_trace(Head-Var-Elements, LeftOffset,This, YIn, YOut) :-
	get_ranges(Elements, Head, maxinf, mininf, Min, Max),
	get(This, class_variable_value(range_font), RangeFont),
	get(This, class_variable_value(header_font), HeaderFont),
	new(HText, text(Head, @default, HeaderFont)),
	new(VarText, text(Var, @default, HeaderFont)),
	get(HText, height, HTH),
	new(T, text(0.3, @default, RangeFont)),
	get(T, height, RH),
	get(This, class_variable_value(time_font), TimeFont),
	new(T2, text(0, @default, TimeFont)),
	get(T2, height, TH),
	% Require at least space for 3 values with half space
	% in between so total of 4
	MinYWidth is HTH + TH + 4*RH,
	get(This, y_per_cont_trace, YMax),
	ReservedWidth is max(MinYWidth, YMax),
	RangeMaxY is YIn + HTH,
	RangeMinY is  YIn + ReservedWidth - TH,
	get_scales(Min, Max, RangeMinY, RangeMaxY, RH,
		   Base, Step, Last, Y0, Alpha, SubStep),
	get(This, max_x, XMax),
	send(This, display, HText, point(XMax/2, YIn)),
	send(This, display, line(0, RangeMaxY, XMax, RangeMaxY)),
	send(This, display, line(LeftOffset,RangeMaxY,LeftOffset, RangeMinY)),
	send(This, display, line(0, RangeMinY, XMax, RangeMinY)),
	draw_cont_vals(This, Base, Step, Last, LeftOffset, SubStep, Y0,
		       Alpha, RangeX),
	send(This, display, VarText, point(RangeX/2-VarText?width/2,
					   RangeMaxY+ VarText?width/2)),
	print_times(This, RangeMinY, YB),
	(var(YB)
	->	YB = RangeMinY
	;	true
	),
	YOut is max(YIn + ReservedWidth,YB+1),
	get(This, alpha_t, AlphaT),
	get(This, x0t, X0T),
	length(Elements, L),
	fill_trace_vals(Elements, L, This, X0T, AlphaT, Y0, Alpha).


fill_trace_vals([], _L, _This, _X0T, _AlphaT,  _Y0, _Alpha).

fill_trace_vals([Val-Atom-Ranges|Elements], L, This, X0T, AlphaT, Y0, AlphaY) :-
	YVal is Y0 + AlphaY*Val,
	default_value(Atom, DVal),
	get(This, start_time, ST),
	ensure_setup_time(SetT),
	assert_debug(cmp_ge(ST,SetT)),
	get(This, display_end_time, DET),
	fill_trace_ranges(Ranges, DET, DVal, YVal, This, X0T, AlphaT, L),
	fill_trace_vals(Elements, L, This, X0T, AlphaT, Y0, AlphaY).

no_line_if_l(5).

draw_if_l_tfu_ok(This, N, FUB12, Color1,Pen1,Texture1) :-
	assert_debug(integer(N)),
	no_line_if_l(LMax),
	tfu_color(This, FUB12, Color1,Pen1,Texture1),
	(	memberchk(FUB12, [true, false])
	;	N =< LMax
	).


fill_trace_ranges([], TDone, DVal, Y, This, X0T, AlphaT, N) :-
	!,get(This, display_start_time, ST),
	(cmp_gt(TDone, ST)
	->	missing_range1(ST, TDone, DVal, range(T1, T2,FUB12)),
		(draw_if_l_tfu_ok(This, N, FUB12, Color,Pen,Texture1)
		->	Xlo1 is X0T + T1*AlphaT,
			Xhi1 is X0T + T2*AlphaT,
			send(This, display, new(L1, line(Xlo1 , Y, Xhi1, Y))),
			send(L1, colour, Color),
			send(L1, pen, Pen),
			send(L1, texture, Texture1)
		;	true
		)
	;	true
	).


fill_trace_ranges([range(Tlo, _Thi, _TFU)|Ranges],TDone,DVal,Y,This, X0T,
		  AlphaT, N) :-
	cmp_ge(Tlo, TDone),
	!,
	fill_trace_ranges(Ranges, TDone,DVal,Y,This, X0T,AlphaT, N).

fill_trace_ranges([range(Tlo, Thi, TFU)|Ranges],TDone,DVal,Y,This, X0T,
		  AlphaT, N) :-
	get(This, display_start_time, Tstart),
	assert_debug(cmp_gt(TDone, Tlo),a3),
	(cmp_le(TDone, Tstart)
	->	true
	;cmp_lt(Thi, TDone)
	->	max_new(Thi, Tstart, Tlo11),
		missing_range1(Tlo11,TDone, DVal, range(T1, T2,FUB12)),
		(draw_if_l_tfu_ok(This, N, FUB12, Color1,Pen1,Texture1)
		->	Xlo1 is X0T + T1*AlphaT,
			Xhi1 is X0T + T2*AlphaT,
			send(This, display, new(L1, line(Xlo1 , Y, Xhi1, Y))),
			send(L1, colour, Color1),
			send(L1, pen, Pen1),
			send(L1, texture, Texture1)
		;	true
		)
	;	true
	),
	max_new(Tlo, Tstart, Tlo1),
	min_new(Thi, TDone, Thi1),
	(cmp_le(Thi1, Tlo1)
	->	true
	;draw_if_l_tfu_ok(This, N, TFU, Color2,Pen2,Texture2)
	->	Xlo is X0T + Tlo1*AlphaT,
		Xhi is X0T + Thi1*AlphaT,
		send(This, display, new(L, line(Xlo , Y, Xhi, Y))),
		send(L, colour, Color2),
		send(L, pen, Pen2),
		send(L, texture, Texture2)
	;	true
	),
	fill_trace_ranges(Ranges, Tlo, DVal, Y, This, X0T, AlphaT, N).

tfu_fill_pattern(true, @black_image, blue).
tfu_fill_pattern(false, @black_image, light_blue).
tfu_color(This, TFU, Color, W, Dotted) :-
	tfu_color(TFU, Color, _W, Dotted),
	tfu_w(TFU, CV),
	get(This, class_variable_value, CV, W).

tfu_color(true, blue,2, none).
tfu_color(false, light_blue,2, none).
tfu_color(unknown, black,1, dotted).
tfu_w(true, true_pen).
tfu_w(false, false_pen).
tfu_w(unknown, unknown_pen).





/* We skip first and last entries */
draw_cont_vals(This, Base, Step, Last, LeftOffset, SubStep, Y0, Alpha,
	       RangeX) :-
	get(This, class_variable_value(range_font), RangeFont),
	max_range_name_length(Base, Step, Last, RangeFont, Max),
	TickLen = 5,
	RangeX is LeftOffset - Max - TickLen - 2,
	% YCoord is Y0 + Alpha*X
	forall(betweenf(Base, Last, SubStep, V1),
	       (
		       Y is Y0 + Alpha*V1,
		       send(This, display, line(LeftOffset -5, Y, LeftOffset, Y))
	       )
	      ),
	forall(betweenf(Base, Last, Step, V),
	       (
		       Y is Y0 + Alpha*V,
		       new(T, text(V, @default, RangeFont)),
		       send(This, display, T, point(RangeX, Y - T?height/2))
	       )
	      ).


betweenf(Low, _Hi, _Step, Low).
betweenf(Low, Hi, Step, Val) :-
	Low < Hi,
	Low1 is Low + Step,
	Low1 =< Hi,
	betweenf(Low1, Hi, Step, Val).



get_scale(Base, End, YMax, YInc, Y0, Alpha) :-
	Alpha is (YMax - YInc)/(Base - End),
	Y0 is YMax - Alpha*Base.

max_range_name_length(Curr, _Step, Last, _RangeFont, 0) :-
	Curr >= Last,
	!.
max_range_name_length(Curr, Step, Last, RangeFont, Max) :-
	Curr1 is Curr + Step,
	max_range_name_length(Curr1, Step, Last, RangeFont, Max1),
	range_name_length(Curr, RangeFont, Max2),
	Max is max(Max2, Max1).
range_name_length(Val, RangeFont, L) :-
	tr_atom(Val, AtomKey),
	get(RangeFont, width(AtomKey), L).

tr_atom(Atom, AtomName) :-
	pl_pce(Atom, AtomName).


get_scales(Min, Max, RangeMinY, RangeMaxY, FontHeight,
	   Base, Step, Last, Y0, Alpha, SubStep) :-
	Fits is ceiling(abs(RangeMaxY - RangeMinY)/FontHeight),
	Fits2 is floor(2*Fits/3),
	Fits3 is floor(Fits/2),
	(	Fits3 >= 5
	->	Fits1 = Fits3
	;	Fits2 >=3
	->	Fits1 = Fits2
	;	Fits < 2
	->	fail
	;	Fits1 = Fits
	),
	Fits4 is min(Fits1, 10),
	scale_constants(Min, Max, Fits4, Base, Step, Last),
	B1 is Base - Step/2,
	L1 is Last + Step/2,
	get_scale(B1, L1, RangeMinY, RangeMaxY, Y0, Alpha),
	SN is floor(abs(Alpha*Step)/10),
	sub_step_size(Step, SN, SubStep).
sub_step_size(Step, SN, SubStep) :-
	(SN =< 1
	->	SubStep = Step
	;	SubStep1 is Step/SN,
		round125(SubStep1, SubStep)
	).


scale_constants(Min, Max, Fits, Base, Step, Last) :-
	Range0 is (Max - Min)/Fits,
	round125(Range0, Step),
	Base is Step*floor(Min/Step),
	Last is Step*ceiling(Max/Step).


roundoff_range(Min, Max, Base, Step, Count) :-
	(Min >= 0,
		Max >= 0
	->	roundoff_pos(Min, Max, Min1, Max1)
	;	Min =< 0,
		Max =< 0
	->	Min2 is -Min,
		Max2 is -Max,
		roundoff_pos(Min2, Max2, Min3, Max3),
		Min is -Min3,
		Max is -Max3
	;	assert_debug(cmp_gt(Max, Min)),
		roundoff_np(Min, Max, Min1, Max1)
	),
	scale_constants(Min1, Max1, Base, Step, Count).
scale_constants(Min1, Max1, Base, Step, Count) :-
	Range0 is (Max1 - Min1)/10,
	round125(Range0, Step),
	Base is Step*floor(Min1/Step),
	Count is ceiling(Max1/Step)-floor(Min1/Step).

round125(Range1, 0.1) :-
	Range1 =:= 0,
	warning('Empty graph range'),
	!.

round125(Range1, Range2) :-
	A is log10(Range1),
	B is floor(A),
	C is A - B,
	assert_debug(C >= 0),
	assert_debug(C < 1),
	round125n(C, Fact0),
	Range2 is Fact0*(10**B).
round125n(C, Fact) :-
	(C < log10(2)/2
	->	Fact = 1
	;C < (log10(2) + log10(5))/2
	->	Fact = 2
	;C < (log10(5) + 1)/2
	->	Fact = 5
	;	Fact = 10
	).

/*
  Never increase range more than 20% of original range
  If difference between 0 and simple range minimum < 15%
  alway incorporate 0 in range.
  */
roundoff_pos(Min, Max, Min1, Max1) :-
	Min1 is Min - (Max - Min)/10,
	Max1 is Max + (Max - Min)/10.
roundoff_np(Min, Max, Min1, Max1) :-
	roundoff_pos(Min, Max, Min1, Max1).

get_ranges([], _Head, Min, Max, Min, Max).

get_ranges([Val-Atom-_AtomTrace|Elements], Head, OldMin, OldMax, Min, Max) :-
	(algo:display_number_range(Atom, Val, Head, _Var)
	->	true
	;	impl_error('Mismatch display_range')
	),
	min_new(OldMin, Val, Min1),
	max_new(OldMax, Val, Max1),
	get_ranges(Elements, Head, Min1, Max1, Min, Max).

get_cont_traces(Traces, ContTraces) :-
	(setof(Head-Var-Elements,
	       setof(Val-Atom-AtomTrace,
		     display_number_range_elements_l(Atom,Val, Head, Var,
						     AtomTrace, Traces),
		     Elements),
	       ContTraces)
	->	true
	;	ContTraces = []
	).

partition_displays(Traces, AtomTraces, ContTraces) :-
	(get_option(view(View))
	->	true
	;	View = standard
	),
	partition_displays(View, Traces, AtomTraces, ContTraces).
partition_displays(View, Traces, AtomTraces, ContTraces) :-
	algo:display(View, sort_atoms_global),
	!,
	(setof(Atom-Trace, normal_atom_trace_l(View,Atom, Trace, Traces), AtomTraces)
	->	(is_local
		->	assert_debug(ground(AtomTraces)),
			sort(AtomTraces, AtomTraces1),
			(AtomTraces == AtomTraces1
			->	local_trace(same)
			;	local_trace(different)
			)
		;	true
		)
	;	AtomTraces = []
	),
	get_cont_traces(Traces, ContTraces).
partition_displays(View, Traces, AtomTraces, ContTraces) :-
	algo:display(View, sort_atoms_time_global),
	!,
	(	setof(T,
	       Atom^Trace^Traces^normal_atom_trace_lmint(View,Atom, Trace, T, Traces), Times),
		bagof(T-Atom-Trace, (member(T, Times),
					    normal_atom_trace_lmint(View,Atom, Trace, T, Traces)), AtomTraces1)
	->	maplist(rm_firstm, AtomTraces1, AtomTraces)
	;	AtomTraces = []
	),
	get_cont_traces(Traces, ContTraces).
partition_displays(View, Traces, AtomTraces, ContTraces) :-
	algo:display(View, sort_atoms_time_abc_global),
	!,
	(setof(T-Atom-Trace,
	       normal_atom_trace_lmint(View,Atom, Trace, T, Traces), AtomTraces1)
	->	maplist(rm_firstm, AtomTraces1, AtomTraces)
	;	AtomTraces = []
	),
	get_cont_traces(Traces, ContTraces).
partition_displays(View, Traces, AtomTraces, ContTraces) :-
	(bagof(Atom-Trace, normal_atom_trace_l(View,Atom, Trace, Traces),
	       AtomTraces)
	->	true
	;	AtomTraces = []
	),
	get_cont_traces(Traces, ContTraces).
rm_firstm(_-B-C, B-C).


normal_atom_trace_lmint(View, Atoma, AtomTrace, T, Traces) :-
	normal_atom_trace_l(View, Atoma, AtomTrace, Traces),
	mint(AtomTrace, Atoma, T).
mint(AtomTrace, Atoma, T) :-
	default_value(Atoma, CWAVal),
	mint1(AtomTrace, CWAVal, T1),
	trfloat(T1, T).


mint1([], _CWAVal, other).
mint1([range(T1,_,TFU)], CWAVal, T2) :-
	!,
	(TFU \= CWAVal
	->	T2 = T1
	;	T2 = other
	).
mint1([range(T1,_,TFU),range(T2,_, TFU2)|R], CWAVal, T3) :-
	!,
	(T1 =< T2
	->	mint1norm([range(T1,_,TFU),range(T2,_, TFU2)|R], CWAVal, T3),
		local_warning('traces with different ordering'),
		local_trace(mint1)
	;	mint1inv([range(T1,_,TFU),range(T2,_, TFU2)|R], CWAVal, T3)
	).
mint1inv([], _, other).
mint1inv([range(T1,_,TFU)|R], CWAVal, T3) :-
	(TFU = CWAVal
	->	mint1inv(R, CWAVal, T3)
	;	mint1inv(R, CWAVal, T4),
		(T4 = other
		->	T3 = T1
		;	assert_debug(T4 < T1),
			T3 = T4
		)
	).
mint1norm([range(T1,_,TFU)|R], CWAVal, T3) :-
	(TFU = CWAVal
	->	mint1norm(R, CWAVal, T3)
	;	T3 = T1
	).



tminmember(T, Atoma1, AtomTrace1, Traces) :-
	member(Atoma1-AtomTrace1, Traces),
	mint(AtomTrace1, Atoma1, T).

normal_atom_trace_l(View, Atoma, AtomTrace, Traces) :-
	algo:display(View,show_atoms(_)),
	!,
	algo:display(View,show_atoms(Atoma1)),
	(\+ algo:display(View,sort_atoms_global)
	->	(algo:display(View,sort_atoms_time)
		->	setof(T- Atoma1-AtomTrace1,
			      tminmember(T, Atoma1, AtomTrace1, Traces),
			      Sorted),
			member(_T- Atoma-AtomTrace, Sorted)
		;	setof(Atoma1-AtomTrace1, member(Atoma1-AtomTrace1, Traces),
			      Sorted),
			member(Atoma-AtomTrace, Sorted)
		)
	;	Atoma1 = Atoma,
		member(Atoma-AtomTrace, Traces)
	),
	\+ algo:display_number_range(Atoma, _, _, _),
	\+ algo:display(View, no_show_atoms(Atoma)).

normal_atom_trace_l(View, Atoma, AtomTrace, Traces) :-
	algo:display_number_range(_, _, _, _),
	!,
	member(Atoma-AtomTrace, Traces),
	\+ algo:display_number_range(Atoma, _, _, _),
	\+ algo:display(View, algo:no_show_atoms(Atoma)).


normal_atom_trace_l(View, Atoma, AtomTrace, Traces) :-
	member(Atoma-AtomTrace, Traces),
	\+ algo:display(View, algo:no_show_atoms(Atoma)).





display_number_range_elements_l(Atom, Val, Head, Var, AtomTrace, Traces) :-
	once(algo:display_number_range(_, _, _, _)),
	member(Atom-AtomTrace, Traces),
	algo:display_number_range(Atom, Val, Head, Var).



max_atom_name_length([], _AtomFont, 0).

max_atom_name_length([AtomKey-_Data|Traces], AtomFont, Length) :-
	max_atom_name_length(Traces, AtomFont, Length1),
	atom_name_length(AtomKey, AtomFont, L1),
	Length is max(L1, Length1).
atom_name_length(Atom, AtomFont, L) :-
	tr_atom(Atom, AtomKey),
	get(AtomFont, width(AtomKey), L).

add_left_word(This, Atom, YOffs) :-
	ytext_gap(YGap),
	atom_space(AtomSpace),
	atom_tick(AT),
	tr_atom(Atom, Key),
	get(This, class_variable_value(atom_font), AtomFont),
	get(This, left_offset, L),
	new(T,text(Key, @default, AtomFont)),
	get(T, width, W),
	X is L - AtomSpace - AT - W,
	send(This, display, T,
	     point(X, YOffs+YGap)).

print_simple_time(This, PrevOffs, YOffs1, NewOffs) :-
	add_left_word(This, time, YOffs1),
	get(This, trace_height, Height),
	send(This, print_times1, PrevOffs, YOffs1),
	NewOffs is Height + YOffs1.

fill_traces([], _This, 0, _PLP, PPLeft, PPLeft) :-
	!.

fill_traces(Traces, This, YOffs, PLP, PLIn, PLOut) :-
	fill_traces1(Traces,0,This,YOffs1, PLP, 0, PLIn, PL1, LastTimeY),
	print_simple_time(This, LastTimeY, YOffs1, YOffs2),
	(send(This, print_to_screen)
	->	YOffs = YOffs2
	;	send(This, print_page),
		YOffs = 0
	),
	get(This, trace_height, Height),
	PLOut is PL1 - Height.


fill_traces1([],YOffs,_This,YOffs, _PagelenghPixels, PrevStartPageY, PLIn, PLIn,
	     PrevStartPageY).

fill_traces1([Key-Trace|Traces],YOffs,This,YOut, PLP, PrevStartPageY,PLIn,PLOut,LastTimeY) :-
	fill_trace(Key, Trace, YOffs,This),
	get(This, trace_height, Height),
	YOffs1 is YOffs + Height,
	%send(This, display, new(L, line(0, YOffs1, 100, YOffs1))),
	%send(L, colour, red),
	PL1 is PLIn - Height,
	(PL1 >= 2*Height
	->	fill_traces1(Traces,YOffs1,This,YOut,PLP,PrevStartPageY,PL1,
			     PLOut,LastTimeY)
	;	(Traces = []
		->	YOut = YOffs,
			PLOut is PL1,
			LastTimeY = PrevStartPageY
		;	print_simple_time(This, PrevStartPageY, YOffs1,YOffs2),
			(send(This, print_to_screen)
			->	YOffs3 = YOffs2
			;	send(This, print_page),
				YOffs3 = 0
			),
			fill_traces1(Traces,YOffs3,This,YOut, PLP, YOffs3,PLP,
				     PLOut,LastTimeY)
		)
	).



fill_trace(Atom, Trace, YOffs,This) :-
	add_left_word(This, Atom, YOffs),
	get(This, trace_height, Height),
	get(This, left_offset, L),
	send(This, display, line(L,YOffs,L,YOffs+Height)),
	TY is YOffs + Height/2,
	atom_tick(AT),
	send(This, display, line(L,TY,L-AT,TY)),
	get(This, end_time, ET),
	default_value(Atom, CWAVal),
	fill_tfu(Trace, ET, YOffs,This, CWAVal).

fill_tfu([], HandledTime, YOffs,This, DefFU) :-
	get(This, start_time, ST),
	(cmp_le(HandledTime, ST)
	->	true
	;	get_tfub(ST, HandledTime, DefFU, TFUB),
		fill_val(ST, HandledTime, TFUB, YOffs,This)
	).


fill_tfu([range(T1, _T2,_)|Trace], HandledTime, YOffs,This,DefFU) :-
	cmp_ge(T1, HandledTime),
	!,
	fill_tfu(Trace, HandledTime, YOffs,This,DefFU).

fill_tfu([range(_T1, T2,_)|_Trace],_HandledTime, _YOffs,This,_DefFU) :-
	get(This, start_time, ST),
	cmp_le(T2, ST),
	!.
fill_tfu([range(T1, T2, TFU)|Trace], HandledTime, YOffs,This,DefFU) :-
	(cmp_gt(T2,HandledTime)
	->	T2n = HandledTime
	;	get_tfub(T2, HandledTime, DefFU, TFUB),
		fill_val(T2, HandledTime, TFUB, YOffs,This),
		T2n = T2
	),
	get(This, start_time, ST),
	(cmp_le(ST, T1)
	->	T11 = T1
	;	T11 = ST
	),
	fill_val(T11, T2n, TFU, YOffs,This),
	fill_tfu(Trace, T11, YOffs,This,DefFU).

/* range with no entry, what TFUB belongs to it
   is error if special time in middle of range
   */
get_tfub(T1, T2, DefTFU, TFUB) :-
	ensure_handled_time(HT),
	ensure_setup_time(ST),
	(cmp_le(T2, ST)
	->	TFUB = unknown
	;	cmp_lt(T1, ST)
	->	impl_error('mix range')
	;	cmp_le(T2, HT)
	->	TFUB = DefTFU
	;	cmp_lt(T1, HT)
	->	TFUB = mix
	;	TFUB = blank
	).
fill_val(T1, T2, TFUS, YOffs,This) :-
	(TFUS = blank
	->	true
	;	get(This, time_coord, T1, X1),
		get(This, time_coord, T2, X2),
		get(This, trace_height, Height),
		tf_gap(TFGap),
		(memberchk(TFUS, [true, false])
		->	new(B, box(X2 - X1, Height/2 - TFGap)),
			(TFUS == true
			->	Y is YOffs + TFGap
			;	Y is YOffs + Height/2
			),
			send(This, display, B, point(X1,Y)),
			tfu_fill_pattern(TFUS, _Im, Col),
			get(@pce, convert, Col, colour, Col1),
			%send(B, fill_pattern, Im),
			send(B, fill_pattern, Col1),
			send(B, colour, @default)
		;memberchk(TFUS, [unknown,mix])
		->	send(This, display, line(?(This, time_coord, T1), YOffs + Height/2,
					 ?(This, time_coord, T2), YOffs + Height/2))
		;	impl_error('fill_val ~w?', [TFUS])
		)
	).

get_tfu_offset(unknown, This, TFUOffs) :-
	get(This, trace_height, Height),
	TFUOffs is Height/2.
get_tfu_offset(true, _This, TFUOffs) :-
	tf_gap(TFUOffs).
get_tfu_offset(false, This, TFUOffs) :-
	tf_gap(TFGap),
	get(This, trace_height, Height),
	TFUOffs is Height - TFGap.
tf_gap(5).



