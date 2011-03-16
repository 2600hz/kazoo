%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(edemo2).
-export([start/2]).

%% test exit(B, Why)

start(Bool, M) ->
    %% Spawn three process A B and C
    A = spawn(fun() -> a() end),
    B = spawn(fun() -> b(A, Bool) end),
    C = spawn(fun() -> c(B, M) end),
    sleep(1000),
    status(a, A),
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
    process_flag(trap_exit, true),
    link(B),
    exit(B, M),
    wait(c).


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
    case process_info(Pid) of
	undefined ->
	    io:format("process ~p (~p) is dead~n", [Name,Pid]);
	_ ->
	    io:format("process ~p (~p) is alive~n", [Name, Pid])
    end.

%% 11> c(edemo2).
%% {ok,edemo2}
%% 12> edemo2:start(false, abc).
%% Process c received {'EXIT',<0.81.0>,abc}
%% Process a received {'EXIT',<0.81.0>,abc}
%% process a (<0.80.0>) is alive
%% process b (<0.81.0>) is dead
%% process c (<0.82.0>) is alive
%% ok
%% 13> edemo2:start(false, normal).
%% process a (<0.84.0>) is alive
%% process b (<0.85.0>) is alive
%% process c (<0.86.0>) is alive
%% ok
%% 14> edemo2:start(false, normal).
%% process a (<0.88.0>) is alive
%% process b (<0.89.0>) is alive
%% process c (<0.90.0>) is alive
%% ok
%% 15> edemo2:start(false, die).
%% Process c received {'EXIT',<0.93.0>,die}
%% Process a received {'EXIT',<0.93.0>,die}
%% process a (<0.92.0>) is alive
%% process b (<0.93.0>) is dead
%% process c (<0.94.0>) is alive
%% ok
%% 16> edemo2:start(false, kill).
%% Process c received {'EXIT',<0.97.0>,killed}
%% Process a received {'EXIT',<0.97.0>,killed}
%% process a (<0.96.0>) is alive
%% process b (<0.97.0>) is dead
%% process c (<0.98.0>) is alive
%% ok
%% 17> edemo2:start(true, abc).
%% Process b received {'EXIT',<0.102.0>,abc}
%% process a (<0.100.0>) is alive
%% process b (<0.101.0>) is alive
%% process c (<0.102.0>) is alive
%% ok
%% 18> edemo2:start(true, normal).
%% Process b received {'EXIT',<0.106.0>,normal}
%% process a (<0.104.0>) is alive
%% process b (<0.105.0>) is alive
%% process c (<0.106.0>) is alive
%% ok
%% 19> edemo2:start(true, kill).
%% Process c received {'EXIT',<0.109.0>,killed}
%% Process a received {'EXIT',<0.109.0>,killed}
%% process a (<0.108.0>) is alive
%% process b (<0.109.0>) is dead
%% process c (<0.110.0>) is alive
%% ok
