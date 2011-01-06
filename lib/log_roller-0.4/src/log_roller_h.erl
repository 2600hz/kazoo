%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc An event handler that broadcasts log messages to subscribers.
%% log_roller_h is a gen_event that is registered with the error_logger
%% and receives a copy of any progress, info and error reports.  A list
%% of subscriber nodes is maintained in the gen_event state and all
%% log messages are broadcast to that list of nodes.
-module(log_roller_h).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("log_roller.hrl").
	
%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% @hidden
%%----------------------------------------------------------------------
init(_) ->	
	{ok, []}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler       
%% @hidden                       
%%----------------------------------------------------------------------
handle_event({error, _Gleader, {Pid, Format, Data}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=error, node=get_node(Pid), time=erlang:now(), message=msg(Format, Data)});
	
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=error, node=get_node(Pid), time=erlang:now(), message=Report});
	
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) when is_pid(Pid), is_atom(Type) ->
	commit(State, #log_entry{type=package(Type), node=get_node(Pid), time=erlang:now(), message=Report});
	
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=warning, node=get_node(Pid), time=erlang:now(), message=msg(Format, Data)});
		
handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=warning, node=get_node(Pid), time=erlang:now(), message=Report});
		
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State) when is_pid(Pid), is_atom(Type) ->
	commit(State, #log_entry{type=package(Type), node=get_node(Pid), time=erlang:now(), message=Report});
		
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=info, node=get_node(Pid), time=erlang:now(), message=msg(Format, Data)});
		
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) when is_pid(Pid) ->
	commit(State, #log_entry{type=info, node=get_node(Pid), time=erlang:now(), message=Report});
		
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) when is_pid(Pid), is_atom(Type) ->
	commit(State, #log_entry{type=package(Type), node=get_node(Pid), time=erlang:now(), message=Report});

handle_event(_, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}         
%% @hidden                   
%%----------------------------------------------------------------------	
handle_call(_Request, State) ->
    {ok, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}
%% @hidden
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% @hidden
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    %error_logger:add_report_handler(?MODULE),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
package(info_report) -> info;
package(error_report) -> error;
package(warning_report) -> warning;
package(progress_report) -> progress;
package(supervisor_report) -> supervisor;
package(crash_report) -> error;
package(Atom) when is_atom(Atom) -> Atom;
package(_) -> undefined.

get_node(emulator) -> emulator;
get_node(Other) -> 
	case (catch node(Other)) of
		{'EXIT',_} -> undefined;
		Node -> Node
	end.
	
commit(State, Log) ->
	case pg2:get_members(log_roller_server) of
		{error, _Reason} ->
			{ok, State};
		Pids ->
			[Pid ! {log_roller, self(), Log} || Pid <- Pids],
			{ok, State}
	end.

msg(Format, Args) ->
	case (catch lists:flatten(io_lib:format(Format, Args))) of
	    {'EXIT', _} ->
	        lists:flatten(io_lib:format("Format FAIL: io_lib:format(~p, ~p)", [Format, Args]));
	    Msg -> Msg
	end.