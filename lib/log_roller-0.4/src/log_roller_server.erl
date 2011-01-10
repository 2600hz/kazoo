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
-module(log_roller_server).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([
	start/0, start/2, stop/1, init/1, 
	start_phase/3, build_rel/1, compile_templates/0
]).

-include("log_roller.hrl").
	
%%%
%%% Application API
%%%
start() ->
    application:start(?MODULE).

%% @doc start the application
start(_StartType, _StartArgs) -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
%% @doc stop the application
stop(_) -> 
	ok.
	
%%%
%%% Internal functions
%%%
init(_) ->
	Logs = lr_config:get_log_configs(),
	{ok, {{one_for_one, 10, 10}, [
			{erlang:make_ref(), {lr_write_to_disk, start_link, [LogConfig]}, permanent, 5000, worker, [lr_write_to_disk]}
		 || LogConfig <- Logs] ++ [
		    {lr_hooks, {lr_hooks, start_link, []}, permanent, 5000, worker, [lr_hooks]},
		    {lr_web_server, {lr_web_server, start_link, [[]]}, permanent, 5000, worker, [lr_web_server]},
    		{lr_tail, {lr_tail, start_link, []}, permanent, 5000, worker, [lr_tail]}
		 ]
	}}.
	
start_phase(world, _, _) ->
	net_adm:world(),
	ok;
	
start_phase(pg2, _, _) ->
    pg2:which_groups(),
    ok.
	
build_rel(AppVsn) ->
	Apps = [kernel,stdlib],
	{ok, FD} = file:open("bin/" ++ atom_to_list(?MODULE) ++ ".rel", [write]),
	RelInfo = {release,
	    {atom_to_list(?MODULE), AppVsn},
	    	log_roller_utils:get_app_version(erts),
            [log_roller_utils:get_app_version(AppName) || AppName <- Apps] ++ [
            {mochiweb, "0.01"},
	        {?MODULE, AppVsn}
	    ]
	},
	io:format(FD, "~p.", [RelInfo]),
	file:close(FD),
	systools:make_script("bin/" ++ atom_to_list(?MODULE), [local]),
	ok.

compile_templates() ->
  {ok, Filenames} = file:list_dir("templates"),
  [erltl:compile("templates/" ++ Filename, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars]) 
    || Filename <- Filenames],
  ok.