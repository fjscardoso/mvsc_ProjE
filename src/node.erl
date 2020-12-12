-module(node).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {max, left, pointer, queue}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	{ok, build, #state{max=self(), queue=[]}}.

build({pointer, Pointer}, S=#state{}) ->
	{next_state, active, S#state{pointer=Pointer}}.

%starting({start}, S=#state{}) ->
%	gen_fsm:send_event(S#state.pointer, {1, S#state.max}),
%	catchUp(S#state{}), %try to do active and passive processing? idk
%	{next_state, catchUp, S#state{}};
%starting({N, Max}, S=#state{}) ->
%	S#state{queue= S#state.queue ++ [{N, Max}]},
%	{next_state, starting, S#state{}}.


active({1, Max}, S=#state{}) ->
	%io:format("Processing msg: ~p", {1,Max}),
	if S#state.max /= Max ->
		S#state{left = Max},
		gen_fsm:send_event(S#state.pointer, {2, Max}),
		{next_state, active, S#state{}};
	S#state.max == Max ->
		io:format("Finished with maximum ~p", S#state.max),
		{stop, finished, S#state{}}
	end;
active({2, Max}, S=#state{}) -> 
	io:format("Processing msg: ~p", {2,Max}),
	if S#state.left > Max andalso S#state.left > S#state.max ->
		S#state{max = S#state.left},
		{next_state, active, S#state{}};
	S#state.left =< Max orelse S#state.left =< S#state.max ->
		{next_state, passive, S#state{}}
	end.

passive({P1, P2}, S=#state{}) ->
	gen_fsm:send_event(S#state.pointer, {P1, P2}),
	{next_state, passive, {S#state{}}}.

handle_info({V, M}, State, S=#state{}) ->
	gen_fsm:send_event_after(2000, {V, M}),
	{next_state, State, S#state{}}.



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
