%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lib_auth_cs).

%% authenticated client/server

-compile(export_all).


start_server(Port, MFA, Secret, Trace, Max) ->			    
    lib_tcp_mm:start_server(Port, 
			    {?MODULE, start_auth_server, {MFA, Secret}}, 
			    Trace,
			    Max).


start_auth_client(Host, Port, MFA, Secret, Trace) ->
    lib_tcp_mm:start_client(Host, Port, 
			    {?MODULE, start_auth_client, {MFA, Secret}}, 
			    Trace).


%%______________________________________________________________________
%%

start_auth_server(C, {{Mod,Func,ArgsS}, Secret}) ->
    case Secret of
	none ->
	    %% no authentication necessary
	    C ! {msg, auth},
	    apply(Mod, Func, [C, ArgsS]);
	_ ->
	    Ran =lib_primes:make_random_int(20),
	    C ! {msg, {challenge, Ran}},
	    receive
		{msg, {response, R}} ->
		    Expect = md5:string(Ran ++ Secret),
		    case R of
			Expect ->
			    C ! {msg, auth},
			    apply(Mod, Func, [C, ArgsS]);
			_ ->
			    C ! {msg, eBadResponse},
			    exit(protocol)
		    end;
		_ ->
		    exit(protocol)
	    end
    end.

start_auth_client(C, {{Mod, Func, ArgsC}, Secret}) ->
    io:format("lib_auth_cs start auth client:~p~n",
	      [{{Mod, Func, ArgsC}, Secret}]),
    receive
	{msg, auth} ->
	    apply(Mod, Func, [C, ArgsC]);
	{msg, {challenge, Challenge}} ->
	    case Secret of
		none ->
		    exit(eNoSecret);
		_ ->
		    R = md5:string(Challenge ++ Secret),
		    C ! {msg, {response, R}},
		    receive
			{msg, auth} ->
			    apply(Mod, Func, [C, ArgsC]);
			_ ->
			    exit(protocol)
		    end
	    end;
	_ ->
	    exit(protocol)
    end.



											
    
    
	    
	    

	    
