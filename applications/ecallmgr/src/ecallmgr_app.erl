%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_app).

-behaviour(application).

-include("ecallmgr.hrl").

-export([start/2
        ,request/1
        ]).
-export([stop/1]).

%% Application callbacks

%% @doc Implement the application start behaviour.
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    _ = node_bindings(),
    _ = event_stream_bind(),
    _ = fetch_handlers_bind(),
    ecallmgr_sup:start_link().

-spec request(kz_nodes:request_acc()) -> kz_nodes:request_acc().
request(Acc) ->
    Servers = [{kz_term:to_binary(Server)
               ,kz_json:from_list(
                  [{<<"Startup">>, Started}
                  ,{<<"Instance-UUID">>, ecallmgr_fs_node:instance_uuid(Server)}
                  ,{<<"Interfaces">>, ecallmgr_fs_node:interfaces(Server)}
                  ])
               }
               || {Server, Started} <- ecallmgr_fs_nodes:connected('true')
              ],
    [{'media_servers', Servers}
    ,{'channels', ecallmgr_fs_channels:count()}
    ,{'conferences', ecallmgr_fs_conferences:count()}
    ,{'registrations', ecallmgr_registrar:count()}
     | Acc
    ].

%% @doc Implement the application stop behaviour.
-spec stop(any()) -> any().
stop(_State) ->
    _ = event_stream_unbind(),
    _ = fetch_handlers_unbind(),
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

-define(EVENTSTREAM_MODS, ['ecallmgr_fs_channel_stream'
                          ,'ecallmgr_fs_event_stream_notify_gproc'
                          ,'ecallmgr_call_event_publisher'
                          ,'ecallmgr_fs_conference_stream'
                          ,'ecallmgr_fs_conference_publish'
                          ,'ecallmgr_fs_presence'
                          ]).

-define(EVENTSTREAM_PUBLISHERS_MODS, ['ecallmgr_call_event_publisher'
                                     ,'ecallmgr_fs_conference_publish'
                                     ]).

-spec event_stream_bind() -> 'ok'.
event_stream_bind() ->
    _ = [Mod:init() || Mod <- ?EVENTSTREAM_MODS],
    'ok'.

-spec event_stream_unbind() -> 'ok'.
event_stream_unbind() ->
    _ = [kazoo_bindings:flush_mod(Mod) || Mod <- ?EVENTSTREAM_MODS],
    'ok'.

-define(FETCH_HANDLERS_MODS, ['ecallmgr_fs_fetch_configuration_acl'
                             ,'ecallmgr_fs_fetch_configuration_conference'
                             ,'ecallmgr_fs_fetch_configuration_kazoo'
                             ,'ecallmgr_fs_fetch_configuration_sofia'
                             ,'ecallmgr_fs_fetch_dialplan'
                             ,'ecallmgr_fs_fetch_channels'
                             ,'ecallmgr_fs_fetch_directory'
                             ]).

-spec fetch_handlers_bind() -> 'ok'.
fetch_handlers_bind() ->
    _ = [Mod:init() || Mod <- ?FETCH_HANDLERS_MODS],
    'ok'.

-spec fetch_handlers_unbind() -> 'ok'.
fetch_handlers_unbind() ->
    _ = [kazoo_bindings:flush_mod(Mod) || Mod <- ?FETCH_HANDLERS_MODS],
    'ok'.
