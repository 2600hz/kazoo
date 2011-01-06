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
%% @doc Browse log files collected by a log_roller subscriber node.
%% log roller browser is a gen_server that is started by the log_roller 
%% application when the "-log_roller_type subscriber" argument is given 
%% in the shell.
-module(log_roller_utils).
-author('jacob.vorreuter@gmail.com').
-compile(export_all).

-include("log_roller.hrl").

format_log_entry(LogEntry) ->
    [format_time(LogEntry#log_entry.time), LogEntry#log_entry.type, LogEntry#log_entry.node, LogEntry#log_entry.message].
    
format_time({Mega,Secs,Micro}) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time({Mega,Secs,Micro}),
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~w", [Y,Mo,D,H,Mi,S,Micro])).	
    
get_app_version(AppName) ->
	case code:lib_dir(AppName) of
		{error, bad_name} ->
			exit({bad_name, AppName});
		Dir ->
			case lists:reverse(string:tokens(Dir, "-")) of
				[Vsn|_] -> {AppName, Vsn};
				_ ->
					exit({failed_to_tokenize, Dir})
			end
	end.