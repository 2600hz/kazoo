%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_app).

-behaviour(application).

-include("doodle.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-export([start/2, stop/1]).
-export([register_views/0]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    register_views(),
    _ = kapps_maintenance:bind_and_register_views('doodle', 'doodle_app', 'register_views'),
    doodle_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_sms:declare_exchanges(),
    _ = kapi_self:declare_exchanges().

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('doodle').

