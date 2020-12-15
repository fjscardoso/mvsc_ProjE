-module(betterNode).

-behaviour(gen_fsm).

-compile(export_all).

-record(state, {phase, id, max, pointer}).

start_link(Id) ->
    gen_fsm:start_link(?MODULE, [Id], []).

init([Id]) ->
    {ok, build, #state{phase = 0, id = Id, max = Id}}.

%% Wait for the supervisor to signal that the ring is built before
%% starting the algorithm execution.
build({pointer, Pointer}, S = #state{}) ->
    io:format("Building ~p with pointer ~p~n", [self(), Pointer]),
    receive
        start -> b1_initiate_msg(S#state{pointer = Pointer})
    end.


%% send initial msg <1, max, phase, 2^phase> to the neighbor
%% TODO: think of a better name for this function
b1_initiate_msg(S = #state{pointer = Pointer, max = Max, phase = Phase}) ->
    gen_fsm:send_event(Pointer, {one, Max, Phase, math:pow(2, Phase)}),
    {next_state, active, S#state{}}.


active({one, PredMax, _Phase, _Counter}, S = #state{}) ->
    test_maximum(PredMax, S#state.id),
    case PredMax > S#state.max of
        true ->
            {next_state, waiting, S#state{max = PredMax}};
        false ->
            gen_fsm:send_event(S#state.pointer, {two, S#state.max}),
            {next_state, passive, S#state{}}
    end;
active({two, _}, S = #state{}) ->
    %% an active process should never receive a type two message
    {stop, error, S#state{}}.


passive({one, Max, Phase, Counter}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    if Max >= S#state.max andalso Counter >= 1 ->
        if Counter > 1 ->
            gen_fsm:send_event(S#state.pointer, {one, Max, Phase, Counter - 1}),
            {next_state, passive, S#state{max = Max}};
        Counter == 1 ->
            gen_fsm:send_event(S#state.pointer, {one, Max, Phase, 0}),
            {next_state, waiting, S#state{max = Max, phase = Phase}}
        end;
    Max < S#state.max orelse Counter < 1 ->
        gen_fsm:send_event(S#state.pointer, {one, Max, Phase, 0}),
        {next_state, passive, S#state{}}
    end;
passive({two, Max}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    case Max < S#state.max of
        true -> %% just skip this msg
            {next_state, passive, S#state{}};
        false ->
            gen_fsm:send_event(S#state.pointer, {two, Max}),
            {next_state, passive, S#state{}}
    end.


waiting(M1 = {one, Max, _Phase, _Counter}, S = #state{}) ->
    test_maximum(Max, S#state.id),
    %% artificially transition to passive state
    passive(M1, S#state{});
waiting({two, Max}, S = #state{max = Max}) ->
    %% if a message of type two arrives then Max should be equal to S#state.max
    test_maximum(Max, S#state.id),
    b1_initiate_msg(S#state{phase = S#state.phase + 1});
waiting({two, _Max}, S = #state{}) ->
    % if Max =/= S#state.max then some error has occurred
    {stop, error, S#state{}}.

handle_event(restart, _StateName, S = #state{id = Id}) ->
    io:format("Restarting waiting for builds ~p~n", [self()]),
    %% reset local variables
    {next_state, build, S#state{phase = 0, max = Id}};
handle_event(got_max, _StateName, S = #state{}) ->
    %% signal from the supervisor that the leader was found
    %% and it's not this process - just terminate
    {stop, normal, S#state{}}.

terminate(normal, _StateName, _StateData) ->
    io:format("Leader was found - terminating...~n"),
    ok;
terminate(_Reason, _StateName, _StateData) ->
    io:format("Something bad, got to restart"),
    ok.

test_maximum(Max, Max) ->
    %% if a node received his Id as his predecessor's Max,
    %% then his Id is the global maximum - can terminate the algorithm
    io:format("Maximum is: ~p~n", [Max]),
    exit(normal);
test_maximum(_, _) ->
    false.

