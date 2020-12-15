-module(supv).
-compile(export_all).

start(Mod, NumNodes) ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [{Mod, NumNodes}])),
    Pid.

init({Mod, NumNodes}) ->
    process_flag(trap_exit, true),
    start_nodes(Mod, NumNodes, NumNodes, []).


%% Assign a random id to a process so as to avoid trivial algorithm case
%% in which the process ids follow the order of the ring.
start_node(Mod, NumNodes) ->
    Id = rand:uniform(1000 * NumNodes),
    {_, Pid} = apply(Mod, start_link, [Id]),
    io:format("Started ~p with id ~p~n", [Pid, Id]),
    Pid.


start_nodes(Mod, TotalNumNodes, 0, List = [First | _]) ->
    build_ring(List, First),
    start_algo_execution(Mod, TotalNumNodes, List);
start_nodes(Mod, TotalNumNodes, NumNodes, List) ->
    Pid = start_node(Mod, TotalNumNodes),
    start_nodes(Mod, TotalNumNodes, NumNodes - 1, List ++ [Pid]).

%% Send each node its neighbor on the ring;
%% the last node in the list connects to the first, closing the ring.
build_ring([H], First) ->
    gen_fsm:send_event(H, {pointer, First});
build_ring([H1, H2 | T], First) ->
    gen_fsm:send_event(H1, {pointer, H2}),
    build_ring([H2 | T], First).

%% Signal every node that they can start executing the algorithm,
%% and go on to monitoring them.
start_algo_execution(Mod, TotalNumNodes, Nodes) ->
    SignalStart = fun(Node) -> Node ! start end,
    lists:foreach(SignalStart, Nodes),
    receiving(Mod, TotalNumNodes, Nodes).

%% Monitor child processes
receiving(Mod, TotalNumNodes, List) ->
    receive
        {'EXIT', From, normal} ->
            %% The leader (From) was found
            terminate_children(lists:delete(From, List)),
            exit(normal);
        {'EXIT', From, Reason} ->
            %% If a node dies, the algorithm is restarted.
            io:format("Process ~p exited for reason ~p~n", [From, Reason]),
            NewList = lists:delete(From, List),
            send_restart_all(NewList),
            start_nodes(Mod, TotalNumNodes, 1, NewList)
    end.

%% Send to every alive node an event that makes the node
%% reset its local variables and wait for a new start signal.
send_restart_all(Nodes) ->
    SendRestart = fun(Node) -> gen_fsm:send_all_state_event(Node, restart) end,
    lists:foreach(SendRestart, Nodes).

%% The leader has been found - terminate all processes in
%% an orderly fashion.
terminate_children(Nodes) ->
    SendTerminate = fun(Node) -> gen_fsm:send_all_state_event(Node, gotMax) end,
    lists:foreach(SendTerminate, Nodes).


