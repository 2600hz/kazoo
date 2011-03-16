%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(allocator).

-export([start/0, alloc/0, free/1, status/0]).

%% alloc() -> {yes, Resource} | no
%% free(Resource)

start() ->
    register(allocator1, spawn(fun() -> run() end)).

alloc()  -> rpc(alloc).
free(X)  -> rpc({free, X}).
status() -> rpc(status).
    
rpc(Q) ->
    allocator1 ! {self(), Q},
    receive
	{allocator1, Reply} ->
	    Reply
    end.

init() ->
    [1,2,3,4,5,6,7,8,9,10].

run() ->
    process_flag(trap_exit, true),
    loop(init(), []).

loop(Free, Used) ->
    receive
	{From, alloc} ->
	    case Free of
		[] ->
		    From ! {allocator1, no},
		    loop(Free, Used);
		[H|T] ->
		    From ! {allocator1, {yes, H}},
		    link(From),
		    loop(T, [{H,From}|Used])
	    end;
	{From, {dealloc, X}} ->
	    Used1 = delete({X, From}, Used),
	    case count_allocated(From, Used1) of
		0 -> unlink(From);
		_ -> void
	    end,
	    From ! {allocator1, ack},
	    loop([X|Free], Used1);
	{From, status} ->
	    From ! {allocator1, {free, Free, used, Used}},
	    loop(Free, Used);
	{'EXIT', Pid, _Why} ->
	    {Used1, Free1} = dealloc(Pid, Used, [],  Free),
	    loop(Free1, Used1)
    end.
		
delete(H, [H|T])  -> T;
delete(H, [H1|T]) -> [H1|delete(H, T)].

dealloc(Pid, [{H,Pid}|T], Used, Free) -> dealloc(Pid, T, Used, [H|Free]);
dealloc(Pid, [X|T], Used, Free)       -> dealloc(Pid, T, [X|Used], Free);
dealloc(_,   [], Used, Free)          -> {Used, Free}.

count_allocated(Pid, [{_,Pid}|T]) -> 1 + count_allocated(Pid, T);
count_allocated(Pid, [_|T])       -> count_allocated(Pid, T);
count_allocated(_, [])            -> 0.
