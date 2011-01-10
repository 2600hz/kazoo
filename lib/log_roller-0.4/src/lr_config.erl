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
-module(lr_config).
-author('jacob.vorreuter@gmail.com').

-export([get_log_configs/0]).

-include("log_roller.hrl").

get_log_configs() ->
	case application:get_env(log_roller_server, logs) of
		{ok, Logs} when is_list(Logs) ->
			[begin
				#log_config{
					name = Name,
					cache_size = proplists:get_value(cache_size, Props, 10485760),
					maxbytes = proplists:get_value(maxbytes, Props, 10485760),
					maxfiles = proplists:get_value(maxfiles, Props, 10),
					filters = proplists:get_value(filters, Props, []), 
					log_dir = proplists:get_value(log_dir, Props, undefined)
				}
			end || {Name, Props} <- Logs];
		undefined ->
			Props = application:get_all_env(log_roller_server),
			[#log_config{
				name = default,
				cache_size = proplists:get_value(cache_size, Props, 10485760),
				maxbytes = proplists:get_value(maxbytes, Props, 10485760),
				maxfiles = proplists:get_value(maxfiles, Props, 10),
				filters = proplists:get_value(filters, Props, []), 
				log_dir = proplists:get_value(log_dir, Props, undefined)
			}]
	end.