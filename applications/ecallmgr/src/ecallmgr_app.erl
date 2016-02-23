%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2]).
-export([stop/1]).


%% Application callbacks

%% @public
%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    ecallmgr_sup:start_link().

%% @public
%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_authn:declare_exchanges(),
    _ = wapi_authz:declare_exchanges(),
    _ = wapi_call:declare_exchanges(),
    _ = wapi_conference:declare_exchanges(),
    _ = wapi_dialplan:declare_exchanges(),
    _ = wapi_media:declare_exchanges(),
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_rate:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    _ = wapi_resource:declare_exchanges(),
    _ = wapi_route:declare_exchanges(),
    _ = wapi_sysconf:declare_exchanges(),
    _ = wapi_sms:declare_exchanges(),
    _ = wapi_presence:declare_exchanges(),
    wapi_self:declare_exchanges().
