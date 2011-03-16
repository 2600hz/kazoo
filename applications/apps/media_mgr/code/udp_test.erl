%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(udp_test).
-export([start_server/0, client/1]).

start_server() ->
    spawn(fun() -> server(4000) end).

%% The server 		  
server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opened socket:~p~n",[Socket]),
    loop(Socket).

loop(Socket) ->
    receive
	{udp, Socket, Host, Port, Bin} = Msg ->
	    io:format("server received:~p~n",[Msg]),
	    N = binary_to_term(Bin),
	    Fac = fac(N),
	    gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
	    loop(Socket)
    end.
    
fac(0) -> 1;
fac(N) -> N * fac(N-1).

%% The client

client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, "localhost", 4000, 
		      term_to_binary(N)),
    Value = receive
		{udp, Socket, _, _, Bin} = Msg ->
		    io:format("client received:~p~n",[Msg]),
		    binary_to_term(Bin)
	    after 2000 ->
		    0
	    end,
    gen_udp:close(Socket),
    Value.


    

