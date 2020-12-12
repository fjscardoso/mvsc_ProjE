-module(node_serv).

-behaviour(gen_server).

-compile(export_all).

-record(state, {state, max, left, pointer}).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, #state{max=self(), state=waiting}}.

handle_cast({pointer, Pointer}, S=#state{}) ->
	io:format("Received pointer ~p~n", [Pointer]),
	receive 
		start -> 
			io:format("Received start~n"),
			gen_server:cast(Pointer, {1, S#state.max}),
			{noreply, S#state{pointer=Pointer, state=active}}
	end;
handle_cast({N, Max}, S=#state{}) ->
	io:format("Received message ~p~n", [{N, Max}]),
	if S#state.state == active andalso N == 1 ->
		if S#state.max /= Max ->
			io:format("remain on active state ~p~n", [self()]),
			gen_server:cast(S#state.pointer, {2, Max}),
			{noreply, S#state{left = Max}};
		S#state.max == Max ->
			io:format("Finished with maximum ~p~n", [S#state.max]),
			{stop, normal, S#state{}}
		end;
	S#state.state == active andalso N == 2 ->
	 	if S#state.left > Max andalso S#state.left > S#state.max ->
	 		io:format("~premain on active state~n", [self()]),
	 		gen_server:cast(S#state.pointer, {1, S#state.left}),
			{noreply, S#state{max = S#state.left}};
		S#state.left =< Max orelse S#state.left =< S#state.max ->
			io:format("~pchange to passive state~n", [self()]),
			{noreply, S#state{state=passive}}
		end;
	S#state.state == passive ->
		io:format("~premain on passive state~n", [self()]),
		gen_server:cast(S#state.pointer, {N, Max}),
		{noreply, {S#state{}}}
	end.
 


