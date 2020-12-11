-module(node).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {max, left, pointer}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	{ok, build, #state{max=self()}}.

build({pointer, Pointer}, S=#state{}) ->
	{next_state, active, S#state{pointer=Pointer}}.

active({start}, S=#state{}) ->
	S#state.pointer ! {1, S#state.max},
	{next_state, active, S#state{}};
active({1, Max}, S=#state{}) ->
	io:format("Received ~p", {1, Max}),
	if S#state.max /= Max ->
		S#state{left = Max},
		S#state.pointer ! {2, Max},
		{next_state, active, S#state{}};
	S#state.max == Max ->
		{stop, finished, S#state{}}
	end;
active({2, Max}, S=#state{}) -> 
	if S#state.left > Max andalso S#state.left > S#state.max ->
		S#state{max = S#state.left},
		{next_state, active, S#state{}};
	S#state.left =< Max orelse S#state.left =< S#state.max ->
		{next_state, passive, S#state{}}
	end.

passive({P1, P2}, S=#state{}) ->
	S#state.pointer ! {P1, P2},
	{next_state, passive, {S#state{}}}.

try1() ->
	gen_fsm:send_event(?MODULE,{1, 5}).

try2() -> 
	gen_fsm:send_event(?MODULE,{2, 5}).

startit() ->
	gen_fsm:send_event(?MODULE,{start}).

add(Pid) ->
	gen_fsm:send_event(?MODULE,{pointer, Pid}).

show() ->
	gen_fsm:send_event(?MODULE,{show}), ok.
