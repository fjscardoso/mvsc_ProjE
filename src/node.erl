-module(node).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {max, left, pointer}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	{ok, build, #state{max=self()}}.

build({pointer, Pointer}, S=#state{}) ->
	receive 
		start ->
			gen_fsm:send_event(Pointer, {1, S#state.max}),
			{next_state, active, S#state{pointer=Pointer}}
	end.

active({1, Max}, S=#state{}) ->
	if S#state.max /= Max ->
		gen_fsm:send_event(S#state.pointer, {2, Max}),
		{next_state, active, S#state{left = Max}};
	S#state.max == Max ->
		io:format("Finished with maximum ~p~n", [S#state.max]),
		{stop, normal, S#state{}}
	end;
active({2, Max}, S=#state{}) -> 
	if S#state.left > Max andalso S#state.left > S#state.max ->
		gen_fsm:send_event(S#state.pointer, {1, S#state.left}),
		{next_state, active, S#state{max = S#state.left}};
	S#state.left =< Max orelse S#state.left =< S#state.max ->
		{next_state, passive, S#state{}}
	end.

passive({P1, P2}, S=#state{}) ->
	gen_fsm:send_event(S#state.pointer, {P1, P2}),
	{next_state, passive, {S#state{}}}.

