%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lib_chan_test).

-compile(export_all).

-import(lib_chan, [connect/5, rpc/4, disconnect/1]).

start_server() ->
    lib_chan:start_server(),
    wait().

start_client() ->
    io:format("Running client tests~n"),
    tests(1).

test(1) ->
    {ok, MM} = connect("localhost", 1234, echo, "echo1", {echo,client,arg1}),
    MM ! {send, "hello"},
    receive
	{chan, MM, "hello"} ->
	    io:format("test(1) succeeded~n");
	Other ->
	    io:format("test(1) failed:~p~n", [Other])
    end,
    exit(MM, close);
test(2) ->
    {ok, MM} = connect("localhost", 1234, math, "qwerty", ""),
    MM ! {send, {factorial, 6}},
    receive
	{chan, MM, 720}  ->
	    io:format("test(2) succeeded~n");
	Other ->
	    io:format("test(2) failed:~p~n", [Other])
    end,
    MM ! close;
test(3) ->
    case connect("localhost", 1234, mod_non_exists, "facSecret", "") of
	{error, badService} ->
	    io:format("test(3) succeeded~n");
	Other ->
	    io:format("test(3) failed:~p~n", [Other])
    end;
test(4) ->
    case connect("localhost", 1234, math, "badSecret", "") of
	{error, authFail} ->
	    io:format("test(4) succeeded~n");
	Other ->
	    io:format("test(4) failed:~p~n",[Other])
    end;
test(5) ->
    {ok, MM} = connect("localhost", 1234, srpc, "secret", ""),
    case mod_srpc:rpc(MM, test1, fac, [20]) of
	2432902008176640000 ->
	    io:format("test(5) succeeded~n");
	Other ->
	    io:format("test(5) failed:~p~n",[Other])
    end,
    disconnect(MM);
test(6) ->
    {ok, MM} = connect("localhost", 1234, srpc, "secret", ""),
    case (catch mod_srpc:rpc(MM, test2, fac, [20])) of
	{'EXIT', {modNotAllowed, test2}} ->
	    io:format("test(6) succeeded~n");
	Other ->
	    io:format("test(6) failed:~p~n",[Other])
    end,   
    disconnect(MM);
test(7) ->
    {ok, MM} = connect("localhost", 1234, root, "verySecret", ""),
    case (catch mod_srpc:rpc(MM, any_apply, mfa, [test1, fac, [20]])) of
	2432902008176640000 ->
	    io:format("test(7) succeeded~n");
	Other ->
	    io:format("test(7) failed:~p~n",[Other])
    end;
test(8) ->
    done.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

tests(N) ->
    case test(N) of 
	done -> init:stop();
	_Val -> tests(N+1)
    end.

wait() ->
    receive
	_Any ->
	    wait()
    end.

