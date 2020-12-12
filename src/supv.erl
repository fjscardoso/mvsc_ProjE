-module(supv).
-compile(export_all).

start(NumNodes) ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [NumNodes])),
    Pid.

init(NumNodes) ->
    process_flag(trap_exit, true),
    startNodes(NumNodes, 1, [], []).

startNodes(1, 0, Last, [H|T]) ->
	{_,Pid} = apply(node, start_link, []),
	io:format("Started~p~n", [Pid]),
	gen_fsm:send_event(Last ,{pointer, Pid}),
	gen_fsm:send_event(Pid ,{pointer, H}),
	startProtocol([H|T] ++ [Pid]);
startNodes(NumNodes, 0, Last, List) ->
	{_,Pid} = apply(node, start_link, []),
	io:format("Started~p~n", [Pid]),
	gen_fsm:send_event(Last,{pointer, Pid}),
	startNodes(NumNodes-1, 0, Pid, List++[Pid]);
startNodes(NumNodes, 1, _, List) ->
	{_,Pid} = apply(node, start_link, []),
	io:format("Started~p~n", [Pid]),
	startNodes(NumNodes-1, 0, Pid, List++[Pid]).

startProtocol([]) -> receiving();
startProtocol([H]) -> H ! start;
startProtocol([H|T]) -> 
	H ! start,
	startProtocol(T).

receiving() ->
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown); % will kill the child too
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
            exit(shutdown)
    end.
