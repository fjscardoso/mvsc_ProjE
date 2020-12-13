-module(betterNode).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {phase, id, max, pointer}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	{ok, build, #state{phase = 0, id=self(), max=self()}}.

build({pointer, Pointer}, S=#state{}) ->
	receive 
		start ->
			gen_fsm:send_event(Pointer, {1, S#state.max, S#state.phase, math:pow(2, S#state.phase)}),
			{next_state, active, S#state{pointer=Pointer}}
	end.

active({1, Max, _Phase, _Counter}, S=#state{}) ->
	testMaximum(Max, S#state.id),
	if Max > S#state.max ->
		{next_state, waiting, S#state{max = Max}};
	Max =< S#state.max ->
		gen_fsm:send_event(S#state.pointer, {2, S#state.max}),
		{next_state, passive, S#state{}}
	end.

passive({1, Max, Phase, Counter}, S=#state{}) ->
	testMaximum(Max, S#state.id),
	if Max >= S#state.max andalso Counter >= 1 ->
		if Counter > 1 ->
			gen_fsm:send_event(S#state.pointer, {1, Max, Phase, Counter-1}),
			{next_state, passive, S#state{max = Max}}; %not sure about this state or terminates
		Counter == 1 ->
			gen_fsm:send_event(S#state.pointer, {1, Max, Phase, 0}),
			{next_state, waiting, S#state{max = Max, phase = Phase}}
		end;
	Max < S#state.max orelse Counter < 1 ->
		gen_fsm:send_event(S#state.pointer, {1, Max, Phase, 0}),
		{next_state, passive, S#state{}} %not sure about this state or terminates
	end;
passive({2, Max}, S=#state{}) ->
	testMaximum(Max, S#state.id),
	if Max < S#state.max ->
		{next_state, passive, S#state{}};
	Max >= S#state.max ->
		gen_fsm:send_event(S#state.pointer, {2, Max}),
		{next_state, passive, S#state{}}
	end.


waiting({2, Max}, S=#state{}) ->
	testMaximum(Max, S#state.id),
	if S#state.max == Max ->
		gen_fsm:send_event(S#state.pointer, {1, S#state.max, S#state.phase, math:pow(2, S#state.phase)}),
		{next_state, active, S#state{phase= S#state.phase + 1}};
	S#state.max /= max ->
		{stop, error, S#state{}}
	end;
waiting({1, Max, _Phase, _Counter}, S=#state{}) ->
	testMaximum(Max, S#state.id),
	{next_state, passive, S#state{}}.

testMaximum(Max, Id) ->
	if Max == Id ->
		io:format("Maximum is: ~p~n", [Id]),
		%{normal, active, S#state{}},
		exit(normal);
	Max /= Id ->
		false
	end.
