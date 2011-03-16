%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
%% Protocol
%%   To the controlling process
%%      {chan, MM, Term}
%%      {chan_closed, MM}
%%   From any process
%%      {send, Term}
%%      close

-module(lib_chan_mm).
%% TCP Middle man
%%   Models the interface to gen_tcp

-export([loop/2, send/2, close/1, controller/2, set_trace/2, trace_with_tag/2]).

send(Pid, Term)       -> Pid ! {send, Term}.
close(Pid)            -> Pid ! close.
controller(Pid, Pid1) -> Pid ! {setController, Pid1}.
set_trace(Pid, X)     -> Pid ! {trace, X}.
    
trace_with_tag(Pid, Tag) ->
    set_trace(Pid, {true, 
		    fun(Msg) -> 
			    io:format("MM:~p ~p~n",[Tag, Msg]) 
		    end}).
    
loop(Socket, Pid) ->
    %% trace_with_tag(self(), trace),
    process_flag(trap_exit, true),
    loop1(Socket, Pid, false).

loop1(Socket, Pid, Trace) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = binary_to_term(Bin),
	    trace_it(Trace,{socketReceived, Term}),
	    Pid ! {chan, self(), Term},
	    loop1(Socket, Pid, Trace);
	{tcp_closed, Socket} ->  
	    trace_it(Trace, socketClosed),
	    Pid ! {chan_closed, self()};
	{'EXIT', Pid, Why} ->
	    trace_it(Trace,{controllingProcessExit, Why}),
	    gen_tcp:close(Socket);
	{setController, Pid1} ->
	    trace_it(Trace, {changedController, Pid}),
	    loop1(Socket, Pid1, Trace);
	{trace, Trace1} ->
	    trace_it(Trace, {setTrace, Trace1}),
	    loop1(Socket, Pid, Trace1);
	close ->
	    trace_it(Trace, closedByClient),
	    gen_tcp:close(Socket);
	{send, Term}  ->
	    trace_it(Trace, {sendingMessage, Term}),
	    gen_tcp:send(Socket, term_to_binary(Term)),
	    loop1(Socket, Pid, Trace);
	UUg ->
	    io:format("lib_chan_mm: protocol error:~p~n",[UUg]),
	    loop1(Socket, Pid, Trace)
    end.
trace_it(false, _)     -> void;
trace_it({true, F}, M) -> F(M). 
