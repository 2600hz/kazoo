%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_number_manager_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    whistle_number_manager_sup:start_link().

-spec stop(any()) -> 'true'.
stop(_State) ->
    exit(whereis('whistle_number_manager_sup'), 'shutdown').
