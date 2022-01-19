%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_amqp_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kz_amqp_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
