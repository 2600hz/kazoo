%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_app).

-behaviour(application).

-include("ecallmgr.hrl").

-export([start/2
        ,request/1
        ]).
-export([stop/1]).

-export([freeswitch_node_modules/0]).

%% Application callbacks

%% @public
%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    _ = node_bindings(),
    _ = freeswitch_nodesup_bind(),
    ecallmgr_sup:start_link().

-spec request(kz_nodes:request_acc()) -> kz_nodes:request_acc().
request(Acc) ->
    Servers = [{kz_term:to_binary(Server)
               ,kz_json:from_list_recursive(
                  [{<<"Startup">>, Started}
                  ,{<<"Interface">>, [ecallmgr_fs_node:interface(Server)]}
                  ])
               }
               || {Server, Started} <- ecallmgr_fs_nodes:connected('true')
              ],
    [{'media_servers', Servers}
    ,{'channels', ecallmgr_fs_channels:count()}
    ,{'registrations', ecallmgr_registrar:count()}
     | Acc
    ].

%% @public
%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
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

-spec freeswitch_node_modules() -> ne_binaries().
freeswitch_node_modules() ->
    application:get_env(?APP, 'node_modules', ?NODE_MODULES).
