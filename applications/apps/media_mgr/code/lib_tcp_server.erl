%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
%% Copyright (C) 2002, Joe Armstrong
%% File    : lib_tcp_server.erl
%% Author  : Joe Armstrong (joe@sics.se)
%% Purpose : Keeps track of a number of TCP sessions
%% Time-stamp: <2006-09-18 14:53:15 ejoearm>

-module(lib_tcp_server).

-export([start_server/4, start_client/4]).
-export([stop/1]).
-export([children/1]).

%% -export([start_child/3]).

%% start_raw_server(Port, Fun, Max)
%%   This server accepts up to Max connections on Port
%%   The *first* time a connection is made to Port
%%   Then Fun(Socket) is called. 
%%   Thereafter messages to the socket result in messsages to the handler.

%% tcp_is typically used as follows:

%% To setup a lister

%% start_agent(Port) ->    
%%     S = self(),
%%     process_flag(trap_exit, true),
%%     tcp_server:start_raw_server(Port, 
%% 				fun(Socket) -> input_handler(Socket, S) end, 
%% 				15,
%%                              0)
%%     loop().

%% The loop() process is a central controller that all
%% processes can use to synchronize amongst themselfves if necessary
%% It ends up as the variable "Controller" in the input_handler


%% Here is how the server code is used
%%
%% start_server(Port, StartSpec) -> true.
%% stop_server(Port) -> true.
%% StartSpec = {Mod, Func, Args}
%%    Mod must export: Func/2
%%    start will be spawned with one argument C
%%    Here is a template for a typical server
%%
%% start() -> start_server(3030, mod_handle)
%%
%% in mod_handle:
%%
%% start(C) ->
%%     receive
%%        {msg, M} -> ...
%%            C ! {msg, T}
%%            C ! close
%%        closed ->
%%     end

start_client(Host, Port, StartSpec, Trace) ->
    S = self(),
    Pid = spawn_link(fun() -> start_client(Host,Port,S,StartSpec,Trace) end),
    receive
	{Pid, Reply} ->
	    Reply
    end.

start_client(Host, Port, S, StartSpec, Trace) ->
    process_flag(trap_exit, true),
    case gen_tcp:connect(Host, Port,
			 [binary, {active, true}, {packet, 4}]) of
	{ok, Socket} ->
	    {Mod, Func, Args} = StartSpec,
	    S ! {self(), {ok, self()}},
	    Pid = spawn_link(Mod, Func, [self(), Args]),
	    loop(Socket, Pid, Trace);
	Error ->
	    S ! {self(), Error}
    end.

					 
%% Note when start_raw_server retruns it should be ready to
%% Immediately accept connections

start_server(Port, StartSpec, Trace, Max) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Self = self(),
	    Pid = spawn_link(fun() ->
				     cold_start(Self, Port, StartSpec, Trace,
						Max)
			     end),
	    receive
		{Pid, ok} ->
		    register(Name, Pid),
		    {ok, self()};
		{Pid, Error} ->
		    Error
	    end;
	_Pid ->
	    {error, already_started}
    end.

stop(Port) when integer(Port) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    not_started;
	Pid ->
	    exit(Pid, kill),
	    (catch unregister(Name)),
	    stopped
    end.

children(Port) when integer(Port) ->
    port_name(Port) ! {children, self()},
    receive
	{session_server, Reply} -> Reply
    end.

port_name(Port) when integer(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).

cold_start(Master, Port, StartSpec, Trace, Max) ->
    process_flag(trap_exit, true),
    io:format("Starting a port server on ~p...~n",[Port]),
    case gen_tcp:listen(Port, [binary,
			       %% {dontroute, true},
			       {nodelay,true},
			       {packet, 4},
			       {reuseaddr, true}, 
			       {active, true}]) of
	{ok, Listen} ->
	    io:format("Listening on:~p~n",[Listen]),
	    Master ! {self(), ok},
	    New = start_accept(Listen, StartSpec, Trace),
	    %% Now we're ready to run
	    socket_loop(Listen, New, [], StartSpec, Trace, Max);
	Error ->
	    Master ! {self(), Error}
    end.

%% Don't mess with the following code uless you really know what you're 
%% doing (and Thanks to Magnus for heping me get it right)

socket_loop(Listen, New, Active, StartSpec, Trace, Max) ->
    receive
	{istarted, New} ->
	    Active1 = [New|Active],
	    possibly_start_another(false,Listen,Active1,StartSpec,Trace,Max);
	{'EXIT', New, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    possibly_start_another(false,Listen,Active,StartSpec,Trace,Max);
	{'EXIT', Pid, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    Active1 = lists:delete(Pid, Active),
	    possibly_start_another(New,Listen,Active1,StartSpec,Trace,Max);
	{children, From} ->
	    From ! {session_server, Active},
	    socket_loop(Listen,New,Active,StartSpec,Trace,Max);
	Other ->
	    io:format("Here in loop:~p~n",[Other])
    end.

possibly_start_another(New, Listen, Active, StartSpec, Trace, Max) 
  when pid(New) ->
    socket_loop(Listen, New, Active, StartSpec, Trace, Max);
possibly_start_another(false, Listen, Active, StartSpec, Trace, Max) ->
    case length(Active) of
	N when N < Max ->
	    New = start_accept(Listen, StartSpec, Trace),
	    socket_loop(Listen, New, Active, StartSpec, Trace,Max);
	_ ->
	    socket_loop(Listen, false, Active, StartSpec, Trace, Max)
    end.

start_accept(Listen, StartSpec, Trace) ->
    S = self(),
    spawn_link(fun() -> start_child(S, Listen, StartSpec, Trace) end).

start_child(Parent, Listen, StartSpec, Trace) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Parent ! {istarted,self()},		    % tell the controller
	    inet:setopts(Socket, [{packet,4},
				  binary,
				  {nodelay,true},
				  {active, true}]), % before we activate socket
	    %% io:format("running the child:~p StartSpec=~p~n",
	    %%  [Socket, StartSpec]),
	    %% unpack StartSpec
	    {Mod, Start, Args} = StartSpec,
	    Pid = spawn_link(Mod, Start, [self(), Args]),
	    process_flag(trap_exit, true),
	    loop(Socket, Pid, Trace);
	_Other ->
	    exit(oops)
    end.

%% Loop is in active mode :-)

loop(Socket, Pid, Trace) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = bin_with_length_to_term(Bin),
	    trace_msg(Trace, {received, Term}),
	    Pid ! {msg, Term},
	    loop(Socket, Pid, Trace);
	{tcp_closed, Socket} ->
	    trace_msg(Trace, socket_closed),
	    Pid ! closed;
	{msg, Term} ->
	    trace_msg(Trace, {sent, Term}),
	    send_term_with_length_check(Socket, Term),
	    loop(Socket, Pid, Trace);
	close ->
	    trace_msg(Trace, closed_by_application),
	    trace_msg(Trace, server_terminating),
	    gen_tcp:close(Socket);
	{'EXIT', Pid, Why} ->
	    trace_msg(Trace, {application_died_with_exit, Why}),
	    trace_msg(Trace, server_terminating),
	    gen_tcp:close(Socket);
	Other ->
	    trace_msg(Trace, {unexpected_message, Other}),
	    trace_msg(Trace, server_terminating),
	    exit({protocolError, Other})
    end.

trace_msg(no, _) -> true;
trace_msg({yes, Tag}, Msg) ->
    io:format("trace: ~p ~p~n",[Tag, Msg]).

send_term_with_length_check(Socket, Term) ->
    B = term_to_binary(Term),
    Len = size(B),
    B1 = <<Len:32>>,
    gen_tcp:send(Socket, [B1, B]).

bin_with_length_to_term(<<Length:32,Bin/binary>>) ->
    case size(Bin) of
	Length ->
	    %% io:format("packet length ok~n",[]),
	    binary_to_term(Bin);
	Len1 ->
	    io:format("Length error:~p ~p ~n",[Length, Len1]),
	    error1231312423235
    end.



    







