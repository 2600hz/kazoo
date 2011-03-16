%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(tracer_test).

-include_lib("stdlib/include/ms_transform.hrl").
-compile(export_all).

%% www.otp.org/doc/man/ms_transform.html


test1() ->
    dbg:tracer(),
    dbg:tpl(tracer_test,fib,'_',
	    dbg:fun2ms(fun(_) -> return_trace() end)),
    dbg:p(all,[c]),
    tracer_test:fib(4).



test2() ->
    trace_module(tracer_test, fun() -> fib(4) end).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

	

trace_module(Mod, StartFun) ->
    %% We'll spawn a process to do the tracing
    spawn(fun() -> trace_module1(Mod, StartFun) end).

trace_module1(Mod, StartFun) ->
    %% The next line says: trace all function calls and return
    %%                     values in Mod
    erlang:trace_pattern({Mod, '_','_'},  
			 [{'_',[],[{return_trace}]}], 
			 [local]),
    %% spawn a function to do the tracing
    S = self(),
    Pid = spawn(fun() -> do_trace(S, StartFun) end),
    %% setup the trace. Tell the system to start tracing 
    %% the process Pid
    erlang:trace(Pid, true, [call,procs]),
    %% Now tell Pid to start
    Pid ! {self(), start},
    trace_loop().

%% do_trace evaluates StartFun()
%%    when it is told to do so by Parent
do_trace(Parent, StartFun) ->
    receive
	{Parent, start} ->
	    StartFun()
    end.

%% trace_loop displays the function call and return values
trace_loop() ->
    receive
	{trace,_,call, X} ->
	    io:format("Call: ~p~n",[X]),
	    trace_loop();
	{trace,_,return_from, Call, Ret} ->
	    io:format("Return From: ~p => ~p~n",[Call, Ret]),
	    trace_loop();
	Other ->
	    %% we get some other message - print them
	    io:format("Other = ~p~n",[Other]),
	    trace_loop()
    end.


    
