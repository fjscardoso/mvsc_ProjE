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
	gen_fsm:send_event(Last ,{pointer, Pid}),
	gen_fsm:send_event(Pid ,{pointer, H}),
	startProtocol([H|T]);
startNodes(NumNodes, 0, Last, List) ->
	{_,Pid} = apply(node, start_link, []),
	gen_fsm:send_event(Last,{pointer, Pid}),
	startNodes(NumNodes-1, 0, Pid, List++[Pid]);
startNodes(NumNodes, 1, _, List) ->
	{_,Pid} = apply(node, start_link, []),
	startNodes(NumNodes-1, 0, Pid, List++[Pid]).

startProtocol([]) -> ok;
startProtocol([H]) -> gen_fsm:send_event(H ,{start});
startProtocol([H|T]) -> 
	io:format("This ~w~n", [H]),
	gen_fsm:send_event(H ,{start}),
	startProtocol(T).
