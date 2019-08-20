%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_app).

-behaviour(application).

-include("ecallmgr.hrl").

-export([start/2
        ,request/1
        ]).
-export([stop/1]).

-export([freeswitch_node_modules/0]).

%% Application callbacks

%% @doc Implement the application start behaviour.
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    _ = node_bindings(),
    _ = event_stream_bind(),
    _ = fetch_handlers_bind(),
    _ = freeswitch_nodesup_bind(),
    ecallmgr_sup:start_link().

-spec request(kz_nodes:request_acc()) -> kz_nodes:request_acc().
request(Acc) ->
    Servers = [{kz_term:to_binary(Server)
               ,node_info(Server, Started)
               }
               || {Server, Started} <- ecallmgr_fs_nodes:connected('true')
              ],
    [{'media_servers', props:filter_undefined(Servers)}
    ,{'channels', ecallmgr_fs_channels:count()}
    ,{'conferences', ecallmgr_fs_conferences:count()}
    ,{'registrations', ecallmgr_registrar:count()}
     | Acc
    ].

-spec node_info(atom(), kz_time:gregorian_seconds()) -> kz_term:api_object().
node_info(Server, Started) ->
    try
        kz_json:from_list(
          [{<<"Startup">>, Started}
          ,{<<"Instance-UUID">>, ecallmgr_fs_node:instance_uuid(Server)}
          ,{<<"Interfaces">>, ecallmgr_fs_node:interfaces(Server)}
          ])
    catch
        _E:_R:_ST -> 'undefined'
    end.



%% @doc Implement the application stop behaviour.
-spec stop(any()) -> any().
stop(_State) ->
    _ = event_stream_unbind(),
    _ = fetch_handlers_unbind(),
    _ = freeswitch_nodesup_unbind(),
    _ = kz_nodes_bindings:unbind('ecallmgr', ?MODULE),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_authn:declare_exchanges(),
    _ = kapi_authz:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_conference:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_media:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_rate:declare_exchanges(),
    _ = kapi_registration:declare_exchanges(),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_sysconf:declare_exchanges(),
    _ = kapi_sms:declare_exchanges(),
    _ = kapi_switch:declare_exchanges(),
    _ = kapi_presence:declare_exchanges(),
    kapi_self:declare_exchanges().

-spec node_bindings() -> 'ok'.
node_bindings() ->
    _ = kz_nodes_bindings:bind('ecallmgr', ?MODULE),
    'ok'.

-spec freeswitch_nodesup_bind() -> 'ok'.
freeswitch_nodesup_bind() ->
    _ = kazoo_bindings:bind(<<"freeswitch.node.modules">>, ?MODULE, 'freeswitch_node_modules'),
    'ok'.

-spec freeswitch_nodesup_unbind() -> 'ok'.
freeswitch_nodesup_unbind() ->
    _ = kazoo_bindings:unbind(<<"freeswitch.node.modules">>, ?MODULE, 'freeswitch_node_modules'),
    'ok'.

-spec freeswitch_node_modules() -> kz_term:ne_binaries().
freeswitch_node_modules() ->
    application:get_env(?APP, 'node_modules', ?NODE_MODULES).

-spec event_stream_bind() -> 'ok'.
event_stream_bind() ->
    _ = [Mod:init() || Mod <- ?EVENTSTREAM_MODS],
    'ok'.

-spec event_stream_unbind() -> 'ok'.
event_stream_unbind() ->
    _ = [kazoo_bindings:flush_mod(Mod) || Mod <- ?EVENTSTREAM_MODS],
    'ok'.

-spec fetch_handlers_bind() -> 'ok'.
fetch_handlers_bind() ->
    _ = [Mod:init() || Mod <- ?FETCH_HANDLERS_MODS],
    'ok'.

-spec fetch_handlers_unbind() -> 'ok'.
fetch_handlers_unbind() ->
    _ = [kazoo_bindings:flush_mod(Mod) || Mod <- ?FETCH_HANDLERS_MODS],
    'ok'.
