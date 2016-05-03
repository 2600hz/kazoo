%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kazoo_config_app).

-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_config_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
