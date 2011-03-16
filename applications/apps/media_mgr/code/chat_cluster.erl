%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_cluster).

-export([start/1, remote/2]).

start(Node) ->
    spawn(fun() -> chatter(Node) end).

chatter(Node) ->
    S = self(),
    Local  = io_widget:start(S),
    io_widget:set_title(Local, "chat to " ++ atom_to_list(Node)),
    Remote = rpc:call(Node, chat, remote, [node(), Local]),
    loop(Local, Remote).

remote(RemoteNode, Remote) ->
    S = self(),
    spawn(fun() -> remote1(S, RemoteNode, Remote) end),
    receive
	Pid -> Pid
    end.

remote1(Parent, RemoteNode, Remote) ->
    Local =  io_widget:start(self()),
    io_widget:set_title(Local, "chat to " ++ atom_to_list(RemoteNode)),
    Parent ! Local,
    loop(Local, Remote).
	    

loop(A, B) ->
    receive
	{_, {data, Str}} ->
	    io_widget:insert(A, [$\n|Str]),
	    io_widget:insert(B, [$\n|Str]),
	    loop(A, B);
	Any ->
	    io:format("~p~n", [Any]),
	    loop(A, B)
    end.

