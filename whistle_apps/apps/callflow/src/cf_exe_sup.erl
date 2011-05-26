%%%============================================================================
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created :     17 May 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(cf_exe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->    
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->    
    CFExeSpec = {cf_exe, {cf_exe, start_link, []},
                 temporary, 1000, worker, [cf_server]},
    StartSpecs = {{simple_one_for_one, 0, 1}, [CFExeSpec]},
    {ok, StartSpecs}.
