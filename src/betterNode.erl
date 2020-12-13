-module(betterNode).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {phase, id, max, pointer}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, build, #state{phase = 0, id = self(), max = self()}}.

%% Wait for the supervisor to signal that the ring is built before
%% starting the algorithm execution.
build({pointer, Pointer}, S = #state{}) ->
    io:format("Building ~p with pointer ~p~n", [self(), Pointer]),
    receive
        start -> b1_initiate_msg(Pointer, S)
    end.

%% send initial msg <1, max, phase, 2^phase> to the neighbor
%% TODO: think of a better name for this function
b1_initiate_msg(Pointer, S = #state{}) ->
    gen_fsm:send_event(Pointer, {m1, S#state.max, S#state.phase, math:pow(2, S#state.phase)}),
    {next_state, active, S#state{pointer = Pointer}}.


%% an active process never receives a type m2 message
active({m1, Max, _Phase, _Counter}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    if Max > S#state.max ->
        {next_state, waiting, S#state{max = Max}};
    Max =< S#state.max ->
        gen_fsm:send_event(S#state.pointer, {m2, S#state.max}), %comentar esta linha para testar
        {next_state, passive, S#state{}}  %trocar de passive para potato para testar, inicial {next_state, passive, S#state{}}
    end.
%%    case Max > S#state.max of
%%        true ->
%%            {next_state, waiting, S#state{max = Max}};
%%        false ->
%%            gen_fsm:send_event(S#state.pointer, {m2, S#state.max}), %comentar esta linha para testar
%%            {next_state, passive, S#state{}}  %trocar de passive para potato para testar, inicial {next_state, passive, S#state{}}
%%    end,



potato({potato}, S = #state{}) ->
    {next_state, active, S#state{}}.

passive({m1, Max, Phase, Counter}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    if Max >= S#state.max andalso Counter >= 1 ->
        if Counter > 1 ->
            gen_fsm:send_event(S#state.pointer, {m1, Max, Phase, Counter - 1}),
            {next_state, passive, S#state{max = Max}}; %not sure about this state or terminates
        Counter == 1 ->
            gen_fsm:send_event(S#state.pointer, {m1, Max, Phase, 0}),
            {next_state, waiting, S#state{max = Max, phase = Phase}}
        end;
    Max < S#state.max orelse Counter < 1 ->
        gen_fsm:send_event(S#state.pointer, {m1, Max, Phase, 0}),
        {next_state, passive, S#state{}} %not sure about this state or terminates
    end;
passive({m2, Max}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    if Max < S#state.max ->
        %% skip this msg
        {next_state, passive, S#state{}};
    Max >= S#state.max ->
        gen_fsm:send_event(S#state.pointer, {m2, Max}),
        {next_state, passive, S#state{}}
    end.


waiting({m1, Max, _Phase, _Counter}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    {next_state, passive, S#state{}};
waiting({m2, Max}, S = #state{max = Max}) ->
    %% if a message of type M2 arrives then Max should be equal to S#state.max
    test_maximum(Max, S#state.id),
    b1_initiate_msg(S#state.pointer, S#state{phase = S#state.phase + 1});
waiting({m2, _Max}, S = #state{}) ->
    % if Max =/= S#state.max then some error has occurred
    {stop, error, S#state{}}.

handle_event(restart, _StateName, _State) ->
    io:format("Restarting waiting for builds ~p~n", [self()]),
    {next_state, build, #state{phase = 0, id = self(), max = self()}}.

test_maximum(Max, Max) ->
    %% if a node received his Id as his predecessor's Max,
    %% then his Id is the global maximum - can terminate the algorithm
    io:format("Maximum is: ~p~n", [Max]),
    %% TODO(?): terminate processes in an orderly fashion - maybe it can just be the order of the list in the supervisor
    exit(shutdown);
test_maximum(_, _) ->
    false.
