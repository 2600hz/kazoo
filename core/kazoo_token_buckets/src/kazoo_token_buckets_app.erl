%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_token_buckets_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    kazoo_token_buckets_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
