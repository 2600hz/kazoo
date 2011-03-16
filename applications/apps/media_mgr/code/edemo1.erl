%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(edemo1).
-export([start/2]).

start(Bool, M) ->
    A = spawn(fun() -> a() end),
    B = spawn(fun() -> b(A, Bool) end),
    C = spawn(fun() -> c(B, M) end),
    sleep(1000),
    status(b, B),
    status(c, C).


a() ->      
    process_flag(trap_exit, true),
    wait(a).

b(A, Bool) ->
    process_flag(trap_exit, Bool),
    link(A),
    wait(b).

c(B, M) ->
    link(B),
    case M of
	{die, Reason} ->
	    exit(Reason);
	{divide, N} ->
	    1/N,
	    wait(c);
	normal ->
	    true
    end.



wait(Prog) ->
    receive
	Any ->
	    io:format("Process ~p received ~p~n",[Prog, Any]),
	    wait(Prog)
    end.



sleep(T) ->
    receive
    after T -> true
    end.

status(Name, Pid) ->	    
    case erlang:is_process_alive(Pid) of
	true ->
	    io:format("process ~p (~p) is alive~n", [Name, Pid]);
	false ->
	    io:format("process ~p (~p) is dead~n", [Name,Pid])
    end.


%% 3> edemo1:start(false, {die, abc}).
%% Process a received {'EXIT',<0.44.0>,abc}
%% process a (<0.43.0>) is alive
%% process b (<0.44.0>) is dead
%% process c (<0.45.0>) is dead
%% ok
%% 4> edemo1:start(false, {die, normal}).
%% process a (<0.47.0>) is alive
%% process b (<0.48.0>) is alive
%% process c (<0.49.0>) is dead
%% ok
%% 5> edemo1:start(false, {divide,0}).

%% =ERROR REPORT==== 8-Dec-2006::11:12:47 ===
%% Error in process <0.53.0> with exit value: {badarith,[{edemo1,c,2}]}

%% Process a received {'EXIT',<0.52.0>,{badarith,[{edemo1,c,2}]}}
%% process a (<0.51.0>) is alive
%% process b (<0.52.0>) is dead
%% process c (<0.53.0>) is dead
%% ok
%% 6> edemo1:start(false, {die,kill}).
%% Process a received {'EXIT',<0.56.0>,killed}   <-- ** changed to killed ***
%% process a (<0.55.0>) is alive
%% process b (<0.56.0>) is dead
%% process c (<0.57.0>) is dead
%% ok


%% 7> edemo1:start(true, {die, abc}).
%% Process b received {'EXIT',<0.61.0>,abc}
%% process a (<0.59.0>) is alive
%% process b (<0.60.0>) is alive
%% process c (<0.61.0>) is dead
%% ok
%% 8> edemo1:start(true, {die, normal}).
%% Process b received {'EXIT',<0.65.0>,normal}
%% process a (<0.63.0>) is alive
%% process b (<0.64.0>) is alive
%% process c (<0.65.0>) is dead
%% ok
%% 9> edemo1:start(true, normal).
%% Process b received {'EXIT',<0.69.0>,normal}
%% process a (<0.67.0>) is alive
%% process b (<0.68.0>) is alive
%% process c (<0.69.0>) is dead
%% ok
%% 10> edemo1:start(true, {die,kill}).
%% Process b received {'EXIT',<0.73.0>,kill}
%% process a (<0.71.0>) is alive
%% process b (<0.72.0>) is alive
%% process c (<0.73.0>) is dead
%% ok
%% 11>

