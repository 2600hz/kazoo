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
-module(lrb).
-author('jacob.vorreuter@gmail.com').

-export([fetch_all/1, fetch_all/2, fetch/1, fetch/2]).

-include("log_roller.hrl").

-define(TIMEOUT, infinity).

fetch_all(NameOrContinuation) -> fetch_all(NameOrContinuation, []).

fetch_all(NameOrContinuation, Opts) -> fetch_all(NameOrContinuation, Opts, []).

fetch_all(NameOrContinuation, Opts, Acc) ->
	case fetch(NameOrContinuation, Opts) of
		{undefined, Results} -> 
			lists:append(Acc, Results);
		{Cont, Results} ->
			fetch_all(Cont, Opts, lists:append(Acc, Results))
	end.	
	
fetch(NameOrContinuation) -> fetch(NameOrContinuation, []).
	
%% @spec fetch(Continuation, Opts) -> {continuation(), Results}
%%		 Continuation = atom() | continuation()
%% 		 Opts = [{types, [atom()]} |
%%				 {nodes, [node()]} |
%%				 {grep, string()}]
%%		 Result = list(list(Time::string(), Type::atom(), Node::atom(), Message::any()))
%% @doc fetch a list of log entries for a specific disk_logger
fetch(Name, Opts) when is_atom(Name), is_list(Opts) -> 
	fetch(new_continuation(Name, Opts), Opts);
	
fetch(Continuation, Opts) when is_record(Continuation, continuation), is_list(Opts) -> 
	Timeout = proplists:get_value(timeout, Opts, ?TIMEOUT),
	Parent = self(),
	Pid = spawn(
		fun() ->
			receive start ->
				Parent ! {self(), fetch_by_continuation(Continuation, [], Opts)}
			end
		end),
	Mref = erlang:monitor(process, Pid),
	Pid ! start,
	receive
		{'DOWN', Mref, process, Pid, Reason} ->
			exit(Reason);
		{Pid, Result} ->
			erlang:demonitor(Mref, [flush]),
			Result
	after Timeout ->
		erlang:demonitor(Mref),
		exit(Pid, normal),
		exit(timeout)
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
new_continuation(Name, Opts) ->
	UseCache = proplists:get_value(use_cache, Opts, true),
	lr_read_from_disk:start_continuation(Name, UseCache).
	
fetch_by_continuation(Continuation, Acc, Opts) ->
	case (catch lr_read_from_disk:terms(Continuation)) of
		{ok, Continuation1, Terms} ->
			NumItems = ?GET_CSTATE(Continuation1, num_items),
			Acc1 = filter(Terms, Opts, Acc),
			{?SET_CSTATE(Continuation1, num_items, NumItems+length(Acc1)), Acc1};
		{'EXIT', {error, read_full_cycle}} ->
			{undefined, Acc};
		{'EXIT', Error} ->
			exit(Error)
	end.
	
filter([], _Opts, Acc) -> lists:reverse(Acc);
filter([LogEntry|Tail], Opts, Acc) ->
	case lr_filter:filter(LogEntry, Opts) of
		[] ->
			filter(Tail, Opts, Acc);
		[_] ->
			Term = log_roller_utils:format_log_entry(LogEntry),
			filter(Tail, Opts, [Term|Acc])
	end.