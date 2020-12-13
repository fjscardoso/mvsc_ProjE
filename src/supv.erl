-module(supv).
-compile(export_all).

start(M, NumNodes) ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [{M, NumNodes}])),
    Pid.

init({M, NumNodes}) ->
    process_flag(trap_exit, true),
    startNodes(M, NumNodes, []).

startNodes(M, 1, [H | T]) ->
    {_, Pid} = apply(M, start_link, []),
    io:format("Started~p~n", [Pid]),
    List = [H | T] ++ [Pid],
    buildNodes(List, H),
    startProtocol(List),
    receiving(List, M);
startNodes(M, NumNodes, List) ->
    {_, Pid} = apply(M, start_link, []),
    io:format("Started~p~n", [Pid]),
    startNodes(M, NumNodes - 1, List ++ [Pid]).

buildNodes([H], First) ->
    gen_fsm:send_event(H, {pointer, First});
buildNodes([H1, H2 | T], First) ->
    gen_fsm:send_event(H1, {pointer, H2}),
    buildNodes([H2 | T], First).

startProtocol([]) -> ok;
startProtocol([H | T]) ->
    H ! start,
    startProtocol(T).

receiving(List, M) ->
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown); % will kill the child too
        {'EXIT', From, Reason} ->
            io:format("Process ~p exited for reason ~p~n", [From, Reason]),
            RList = lists:delete(From, List),
            sendStopAll(RList),
            {_, Pid} = apply(M, start_link, []),
            io:format("Started~p~n", [Pid]),
            [H | T] = RList ++ [Pid],
            buildNodes([H | T], H),
            receiving([H | T], M)
    end.

sendStopAll([]) -> ok;
sendStopAll([H]) -> gen_fsm:send_all_state_event(H, restart);
sendStopAll([H | T]) ->
    gen_fsm:send_all_state_event(H, restart),
    sendStopAll(T).

