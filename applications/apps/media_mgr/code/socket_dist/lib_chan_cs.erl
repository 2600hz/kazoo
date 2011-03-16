%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lib_chan_cs).
%% cs stands for client_server

-export([start_raw_server/4, start_raw_client/3]).
-export([stop/1]).
-export([children/1]).


%% start_raw_server(Port, Fun, Max, PacketLength)
%%   This server accepts up to Max connections on Port
%%   The *first* time a connection is made to Port
%%   Then Fun(Socket) is called. 
%%   Thereafter messages to the socket result in messages to the handler.
%%   PacketLength is usually 0,1,2 or 4 (see the inet manual page for details).

%% tcp_is typically used as follows:
%% To setup a listener
%%   start_agent(Port) ->    
%%     process_flag(trap_exit, true),
%%     lib_chan_server:start_raw_server(Port, 
%% 		         	       fun(Socket) -> input_handler(Socket) end, 
%% 				       15, 0).

start_raw_client(Host, Port, PacketLength) ->
    gen_tcp:connect(Host, Port,
		    [binary, {active, true}, {packet, PacketLength}]).

%% Note when start_raw_server returns it should be ready to
%% Immediately accept connections

start_raw_server(Port, Fun, Max, PacketLength) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Self = self(),
	    Pid = spawn_link(fun() ->
				 cold_start(Self,Port,Fun,Max,PacketLength)
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


cold_start(Master, Port, Fun, Max, PacketLength) ->
    process_flag(trap_exit, true),
    %% io:format("Starting a port server on ~p...~n",[Port]),
    case gen_tcp:listen(Port, [binary,
			       %% {dontroute, true},
			       {nodelay,true},
			       {packet, PacketLength},
			       {reuseaddr, true}, 
			       {active, true}]) of
	{ok, Listen} ->
	    %% io:format("Listening to:~p~n",[Listen]),
	    Master ! {self(), ok},
	    New = start_accept(Listen, Fun),
	    %% Now we're ready to run
	    socket_loop(Listen, New, [], Fun, Max);
	Error ->
	    Master ! {self(), Error}
    end.


socket_loop(Listen, New, Active, Fun, Max) ->
    receive
	{istarted, New} ->
	    Active1 = [New|Active],
	    possibly_start_another(false,Listen,Active1,Fun,Max);
	{'EXIT', New, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    possibly_start_another(false,Listen,Active,Fun,Max);
	{'EXIT', Pid, _Why} ->
	    %% io:format("Child exit=~p~n",[Why]),
	    Active1 = lists:delete(Pid, Active),
	    possibly_start_another(New,Listen,Active1,Fun,Max);
	{children, From} ->
	    From ! {session_server, Active},
	    socket_loop(Listen,New,Active,Fun,Max);
	_Other ->
	    socket_loop(Listen,New,Active,Fun,Max)
    end.


possibly_start_another(New, Listen, Active, Fun, Max) 
  when pid(New) ->
    socket_loop(Listen, New, Active, Fun, Max);
possibly_start_another(false, Listen, Active, Fun, Max) ->
    case length(Active) of
	N when N < Max ->
	    New = start_accept(Listen, Fun),
	    socket_loop(Listen, New, Active, Fun,Max);
	_ ->
	    socket_loop(Listen, false, Active, Fun, Max)
    end.

start_accept(Listen, Fun) ->
    S = self(),
    spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Parent ! {istarted,self()},		    % tell the controller
	    inet:setopts(Socket, [{packet,4},
				  binary,
				  {nodelay,true},
				  {active, true}]), 
	    %% before we activate socket
	    %% io:format("running the child:~p Fun=~p~n", [Socket, Fun]),
	    process_flag(trap_exit, true),
	    case (catch Fun(Socket)) of
		{'EXIT', normal} ->
		    true;
		{'EXIT', Why} ->
		    io:format("Port process dies with exit:~p~n",[Why]),
		    true;
		_ ->
		    %% not an exit so everything's ok
		    true
	    end
    end.


