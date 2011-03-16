%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(mod_srpc).
-export([start/3, rpc/4]).
-import(lists, [member/2]).
-import(lib_chan_mm, [send/2, close/1]).

start(MM, _Client, Allow) ->
    server_loop(MM, Allow).

server_loop(MM, Allow) ->
    receive
	{chan, MM, {rpc, Mod, Func, Args}} ->
	    case member(Mod, Allow) of
		true ->
		    case (catch apply(Mod, Func, Args)) of
			{'EXIT', Why} ->
			    send(MM, {rpcExit, Why}),
			    close(MM);
			Ret ->
			    send(MM, {rpcOK, Ret}),
			    server_loop(MM, Allow)
		    end;
		false ->
		    send(MM, {exit, {modNotAllowed, Mod}}),
		    close(MM)
	    end;
	{chan, MM, {call, Mod, Func, Args}} ->
	    case member(Mod, Allow) of
		true ->
		    apply(Mod, Func, Args),
		    server_loop(MM, Allow);
		false ->
		    send(MM, {exit, badMod}),
		    close(MM)
	    end;
	{chan_closed, MM} ->
	    %% io:format("server closed connection~n"),
	    true;
	Other ->
	    io:format("unexpected message:~p~n",[Other]),
	    server_loop(MM, Allow)
    end.

rpc(MM, M, F, A) ->
    send(MM, {rpc, M, F, A}),
    receive
	{chan, MM, {rpcOK, R}} ->
	    R;
	{chan, MM, {rpcExit, Why}} ->
	    exit(Why);
	{chan, MM, {exit, Why}} ->
	    exit(Why);
	Other ->
	    io:format("Glurkkkk~p~n",[Other])
    end.
