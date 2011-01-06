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
-module(lr_filter).
-author('jacob.vorreuter@gmail.com').

-export([filter/2]).

-include("log_roller.hrl").

%% @spec filter(log_entry() | [log_entry()], [{atom(), any()}]) -> [log_entry()]
filter(LogEntry, Filters) when is_record(LogEntry, log_entry), is_list(Filters) ->
	filter(LogEntry, Filters, true, []);
	
filter(LogEntries, Filters) when is_list(LogEntries), is_list(Filters) ->
	lists:foldl(
		fun(LogEntry, Acc) ->
			filter(LogEntry, Filters, true, Acc)
		end, [], LogEntries).
	
filter(LogEntry, [], true, Acc) -> [LogEntry|Acc];

filter(_, _, false, Acc) -> Acc;

filter(LogEntry, [{nodes, Nodes}|Tail], _, Acc) ->
	case Nodes of
		[] -> 
			filter(LogEntry, Tail, true, Acc);
		_ ->
			filter(LogEntry, Tail, lists:member(LogEntry#log_entry.node, Nodes), Acc)
	end;
	
filter(LogEntry, [{types, Types}|Tail], _, Acc) ->
	case Types of
		[] -> 
			filter(LogEntry, Tail, true, Acc);
		_ ->
			filter(LogEntry, Tail, lists:member(LogEntry#log_entry.type, Types), Acc)
	end;
	
filter(LogEntry, [{grep, Grep}|Tail], _, Acc) ->	
	case Grep of
		undefined ->
			filter(LogEntry, Tail, true, Acc);
		[] -> 
			filter(LogEntry, Tail, true, Acc);
		_ ->
			case re:compile(Grep) of
				{ok, MP} -> 
					Subject = lists:flatten(io_lib:format("~p ~p ~p", [LogEntry#log_entry.type, LogEntry#log_entry.node, LogEntry#log_entry.message])),
					case re:run(Subject, MP) of
						{match, _} ->
							filter(LogEntry, Tail, true, Acc);
						nomatch ->
							filter(LogEntry, Tail, false, Acc)
					end;
				_ -> 
					error_logger:warning_report({?MODULE, bad_regexp, Grep}),
					filter(LogEntry, Tail, false, Acc)
			end
	end;
	
filter(LogEntry, [_|Tail], _, Acc) ->
	filter(LogEntry, Tail, true, Acc).
	