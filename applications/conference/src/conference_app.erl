%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @author Karl Anderson <karl@2600hz.org>
%%% @doc
%%% Created : 27 June 2011 by Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(conference_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    conference_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_authn:declare_exchanges(),
    _ = kapi_conference:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    kapi_self:declare_exchanges().
