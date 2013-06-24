%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', term()}.
start(_StartType, _StartArgs) -> sysconf:start_link().

stop(_State) -> sysconf:stop().
