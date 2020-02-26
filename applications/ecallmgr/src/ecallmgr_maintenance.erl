%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_maintenance).

-export([add_fs_node/1
        ,add_fs_node/2
        ]).
-export([remove_fs_node/1
        ,remove_fs_node/2
        ]).
-export([list_fs_nodes/0]).
-export([get_fs_nodes/0]).

-export([carrier_acls/0
        ,carrier_acls/1
        ,test_carrier_ip/1, test_carrier_ip/2
        ]).
-export([allow_carrier/1
        ,allow_carrier/2
        ,allow_carrier/3
        ]).
-export([deny_carrier/1
        ,deny_carrier/2
        ,deny_carrier/3
        ]).

-export([sbc_acls/0
        ,sbc_acls/1
        ,test_sbc_ip/1, test_sbc_ip/2
        ]).
-export([allow_sbc/1
        ,allow_sbc/2
        ,allow_sbc/3
        ]).
-export([deny_sbc/1
        ,deny_sbc/2
        ,deny_sbc/3
        ]).

-export([remove_acl/1
        ,remove_acl/2
        ]).
-export([acl_summary/0
        ,acl_summary/1
        ]).
-export([reload_acls/0]).
-export([flush_acls/0]).

-export([node_summary/0]).
-export([node_details/0
        ,node_details/1
        ]).
-export([channel_summary/0
        ,channel_summary/1
        ]).
-export([channel_details/0
        ,channel_details/1
        ]).
-export([sync_channels/0
        ,sync_channels/1
        ]).
-export([conference_summary/0
        ,conference_summary/1
        ]).
-export([conference_details/0
        ,conference_details/1
        ]).
-export([sync_conferences/0
        ,sync_conferences/1
        ]).
-export([flush_node_channels/1]).
-export([flush_node_conferences/1]).
-export([flush_registrar/0
        ,flush_registrar/1
        ,flush_registrar/2
        ]).
-export([registrar_summary/0
        ,registrar_summary/1
        ]).
-export([registrar_details/0
        ,registrar_details/1
        ,registrar_details/2
        ]).
-export([registrar_sync/0]).
-export([flush_authn/0
        ,flush_util/0
        ,enable_authz/0, enable_local_resource_authz/0
        ,disable_authz/0, disable_local_resource_authz/0
        ]).

-export([show_channels/0]).
-export([show_calls/0]).
-export([check_sync/2]).

-export([limit_channel_uptime/1, limit_channel_uptime/2
        ,hangup_long_running_channels/0, hangup_long_running_channels/1
        ,hangup/1
        ]).

-include("ecallmgr.hrl").

-type config_fun() :: fun((kapps_config:config_category(), kapps_config:config_key(), any()) ->
                                 {'ok', kz_json:object()} |
                                 kz_datamgr:data_error()
                                     ) |
                      fun((kapps_config:config_category(), kapps_config:config_key(), any(), node()) ->
                                 {'ok', kz_json:object()} |
                                 kz_datamgr:data_error()
                                     ).

-type acl_fun() :: fun((kz_term:ne_binary()) -> kz_json:object()).

-spec add_fs_node(kz_term:text()) -> 'ok'.
add_fs_node(FSNode) ->
    FSNodes = get_fs_nodes(node()),
    add_fs_node(FSNode, FSNodes, fun kapps_config:set/3).

-spec add_fs_node(kz_term:text(), kz_term:text() | boolean()) -> 'ok'.
add_fs_node(FSNode, AsDefault) when not is_boolean(AsDefault) ->
    add_fs_node(FSNode, kz_term:is_true(AsDefault));
add_fs_node(FSNode, 'true') ->
    FSNodes = get_fs_nodes(<<"default">>),
    add_fs_node(FSNode, FSNodes, fun kapps_config:set_default/3);
add_fs_node(FSNode, 'false') ->
    FSNodes = get_fs_nodes(node()),
    add_fs_node(FSNode, FSNodes, fun kapps_config:set_node/4).

-spec remove_fs_node(kz_term:text() | atom()) -> 'ok'.
remove_fs_node(FSNode) ->
    FSNodes = get_fs_nodes(node()),
    remove_fs_node(FSNode, FSNodes, fun kapps_config:set/3).

-spec remove_fs_node(kz_term:text(), kz_term:text() | boolean()) -> 'ok'.
remove_fs_node(FSNode, AsDefault) when not is_boolean(AsDefault) ->
    remove_fs_node(FSNode, kz_term:is_true(AsDefault));
remove_fs_node(FSNode, 'true') ->
    FSNodes = get_fs_nodes(<<"default">>),
    remove_fs_node(FSNode, FSNodes, fun kapps_config:set_default/3);
remove_fs_node(FSNode, 'false') ->
    FSNodes = get_fs_nodes(node()),
    remove_fs_node(FSNode, FSNodes, fun kapps_config:set_node/4).

-spec list_fs_nodes() -> 'no_return'.
list_fs_nodes() ->
    _ = [io:format("~s~n", [Node]) || Node <- ecallmgr_fs_nodes:connected()],
    'no_return'.

-spec get_fs_nodes() -> 'no_return'.
get_fs_nodes() ->
    _ = [io:format("~s~n", [Node]) || Node <- get_fs_nodes(node())],
    'no_return'.

-spec carrier_acls() -> 'no_return'.
carrier_acls() -> carrier_acls('false').

-spec carrier_acls(boolean() | kz_term:text()) -> 'no_return'.
carrier_acls(AsDefault) when not is_boolean(AsDefault) ->
    carrier_acls(kz_term:is_true(AsDefault));
carrier_acls('true') ->
    list_acls(get_acls(<<"default">>), ?FS_CARRIER_ACL_LIST);
carrier_acls('false') ->
    list_acls(get_acls(), ?FS_CARRIER_ACL_LIST).

-spec test_carrier_ip(kz_term:ne_binary()) -> 'ok'.
test_carrier_ip(IP) ->
    Nodes = ecallmgr_fs_nodes:connected(),
    test_carrier_ip(IP, Nodes).

-spec test_carrier_ip(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> 'ok'.
test_carrier_ip(_, []) -> 'no_return';
test_carrier_ip(IP, [Node|Nodes]) ->
    _ = test_ip_against_acl(IP, Node, ?FS_CARRIER_ACL_LIST),
    test_carrier_ip(IP, Nodes);
test_carrier_ip(IP, Node) ->
    test_ip_against_acl(IP, Node, ?FS_CARRIER_ACL_LIST).

-spec allow_carrier(kz_term:ne_binary()) -> 'no_return'.
allow_carrier(Name) -> allow_carrier(Name, Name, 'false').

-spec allow_carrier(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
allow_carrier(Name, IP) -> allow_carrier(Name, IP, 'false').

-spec allow_carrier(kz_term:ne_binary(), kz_term:ne_binary(), boolean() | kz_term:text()) -> 'no_return'.
allow_carrier(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    allow_carrier(Name, IP, kz_term:is_true(AsDefault));
allow_carrier(Name, IP, 'true') ->
    allow_carrier(Name, IP, get_acls(<<"default">>), fun kapps_config:set_default/3);
allow_carrier(Name, IP, 'false') ->
    allow_carrier(Name, IP, get_acls(), fun kapps_config:set_node/4).

-spec allow_carrier(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), config_fun()) ->
          'no_return'.
allow_carrier(Name, IP, ACLs, SetterFun) ->
    modify_acls(Name, IP, ACLs, fun carrier_acl/1, SetterFun).

-spec deny_carrier(kz_term:ne_binary()) -> 'no_return'.
deny_carrier(Name) -> deny_carrier(Name, Name, 'false').

-spec deny_carrier(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
deny_carrier(Name, IP) -> deny_carrier(Name, IP, 'false').

-spec deny_carrier(kz_term:ne_binary(), kz_term:ne_binary(), boolean() | kz_term:text()) -> 'no_return'.
deny_carrier(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    deny_carrier(Name, IP, kz_term:is_true(AsDefault));
deny_carrier(Name, IP, 'true') ->
    deny_carrier(Name, IP, get_acls(<<"default">>), fun kapps_config:set_default/3);
deny_carrier(Name, IP, 'false') ->
    deny_carrier(Name, IP, get_acls(), fun kapps_config:set_node/4).

-spec deny_carrier(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), config_fun()) ->
          'no_return'.
deny_carrier(Name, IP, ACLs, SetterFun) ->
    modify_acls(Name, IP, ACLs, fun(_) -> carrier_acl(IP, <<"deny">>) end, SetterFun).

-spec sbc_acls() -> 'no_return'.
sbc_acls() -> sbc_acls('false').

-spec sbc_acls(boolean() | kz_term:text()) -> 'no_return'.
sbc_acls(AsDefault) when not is_boolean(AsDefault) ->
    sbc_acls(kz_term:is_true(AsDefault));
sbc_acls('true') ->
    list_acls(get_acls(<<"default">>), ?FS_SBC_ACL_LIST);
sbc_acls('false') ->
    list_acls(get_acls(), ?FS_SBC_ACL_LIST).

-spec test_sbc_ip(kz_term:ne_binary()) -> 'ok'.
test_sbc_ip(IP) ->
    Nodes = ecallmgr_fs_nodes:connected(),
    test_sbc_ip(IP, Nodes).

-spec test_sbc_ip(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> 'ok'.
test_sbc_ip(_, []) -> 'no_return';
test_sbc_ip(IP, [Node|Nodes]) ->
    _ = test_ip_against_acl(IP, Node, ?FS_SBC_ACL_LIST),
    test_sbc_ip(IP, Nodes);
test_sbc_ip(IP, Node) ->
    test_ip_against_acl(IP, Node, ?FS_SBC_ACL_LIST).

-spec allow_sbc(kz_term:ne_binary()) -> 'no_return'.
allow_sbc(Name) -> allow_sbc(Name, Name, 'false').

-spec allow_sbc(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
allow_sbc(Name, IP) -> allow_sbc(Name, IP, 'false').

-spec allow_sbc(kz_term:ne_binary(), kz_term:ne_binary(), boolean() | kz_term:text()) -> 'no_return'.
allow_sbc(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    allow_sbc(Name, IP, kz_term:is_true(AsDefault));
allow_sbc(Name, IP, 'true') ->
    allow_sbc(Name, IP, get_acls(<<"default">>), fun kapps_config:set_default/3);
allow_sbc(Name, IP, 'false') ->
    allow_sbc(Name, IP, get_acls(), fun kapps_config:set_node/4).

-spec allow_sbc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), config_fun()) -> 'no_return'.
allow_sbc(Name, IP, ACLs, SetterFun) ->
    modify_acls(Name, IP, ACLs, fun sbc_acl/1, SetterFun).

-spec deny_sbc(kz_term:ne_binary()) -> 'no_return'.
deny_sbc(Name) -> deny_sbc(Name, Name, 'false').

-spec deny_sbc(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
deny_sbc(Name, IP) -> deny_sbc(Name, IP, 'false').

-spec deny_sbc(kz_term:ne_binary(), kz_term:ne_binary(), boolean() | kz_term:text()) -> 'no_return'.
deny_sbc(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    deny_sbc(Name, IP, kz_term:is_true(AsDefault));
deny_sbc(Name, IP, 'true') ->
    deny_sbc(Name, IP, get_acls(<<"default">>), fun kapps_config:set_default/3);
deny_sbc(Name, IP, 'false') ->
    deny_sbc(Name, IP, get_acls(), fun kapps_config:set_node/4).

-spec deny_sbc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), config_fun()) -> 'no_return'.
deny_sbc(Name, IP, ACLs, SetterFun) ->
    modify_acls(Name, IP, ACLs, fun(_) -> sbc_acl(IP, <<"deny">>) end, SetterFun).

-spec acl_summary() -> 'no_return'.
acl_summary() ->
    list_acls(get_acls(), 'undefined').

-spec acl_summary(kz_term:api_binary() | boolean()) -> 'no_return'.
acl_summary(AsDefault) when not is_boolean(AsDefault) ->
    acl_summary(kz_term:is_true(AsDefault));
acl_summary('true') ->
    list_acls(get_acls(<<"default">>), 'undefined');
acl_summary('false') ->
    list_acls(get_acls(), 'undefined').

-spec remove_acl(kz_term:text()) -> 'no_return'.
remove_acl(Name) ->
    remove_acl(kz_term:to_binary(Name)
              ,get_acls()
              ,fun kapps_config:set/3
              ).

-spec remove_acl(kz_term:text(), kz_term:text() | boolean()) -> 'no_return'.
remove_acl(Name, AsDefault) when not is_boolean(AsDefault) ->
    remove_acl(Name, kz_term:is_true(AsDefault));
remove_acl(Name, 'true') ->
    remove_acl(kz_term:to_binary(Name)
              ,get_acls(<<"default">>)
              ,fun kapps_config:set_default/3
              );
remove_acl(Name, 'false') ->
    remove_acl(kz_term:to_binary(Name)
              ,get_acls()
              ,fun kapps_config:set_node/4
              ).

-spec maybe_reload_acls(kz_term:ne_binary(), 'modify' | 'remove', non_neg_integer()) -> 'no_return'.
maybe_reload_acls(_Name, _Action, 0) ->
    io:format("Timeout during updating ACLs, try reload ACLs and flush config later:~n"),
    io:format("Use these commands:~n"),
    io:format("# sup kapps_config flush~n"),
    io:format("# sup -necallmgr ecallmgr_maintenance reload_acls~n"),
    'no_return';
maybe_reload_acls(Name, Action, Tries) ->
    case has_acl(Name, Action, get_acls()) of
        'true' ->
            kapps_config:flush(?APP_NAME, <<"acls">>),
            reload_acls(),
            'no_return';
        'false' ->
            io:format("Trying to reload ACLs for ~B more times~n", [Tries]),
            timer:sleep(500),
            maybe_reload_acls(Name, Action, Tries - 1)
    end.

-spec has_acl(kz_term:ne_binary(), 'modify' | 'remove', kz_json:object()) -> boolean().
has_acl(Name, Action, ACLs) ->
    FilteredACLs = filter_acls(ACLs),
    case kz_json:get_value(Name, FilteredACLs) of
        'undefined' when Action =:= 'modify' -> 'false';
        'undefined' when Action =:= 'remove' -> 'true';
        _ACL when Action =:= 'modify' -> 'true';
        _ACL when Action =:= 'remove' -> 'false'
    end.

-spec reload_acls() -> 'no_return'.
reload_acls() ->
    _ = [begin
             io:format("issued reload ACLs to ~s~n", [Node]),
             lager:info("issued reload ACLs to ~s", [Node]),
             freeswitch:bgapi(Node, 'reloadacl', "")
         end
         || Node <- ecallmgr_fs_nodes:connected()
        ],
    _ = kz_amqp_worker:cast(kz_api:default_headers(?APP_NAME, ?APP_VERSION), fun kapi_trusted:publish_reload/1),
    'no_return'.

-spec test_ip_against_acl(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
test_ip_against_acl(IP, NodeBin, AclList) ->
    Node = kz_term:to_atom(NodeBin, 'true'),
    {'ok', Bool} = freeswitch:api(Node, 'acl', <<IP/binary, " ", AclList/binary>>),
    io:format("IP ~s on node ~s would be ~s~n", [IP, NodeBin, acl_action(Bool)]).

-spec acl_action(kz_term:ne_binary()) -> kz_term:ne_binary().
acl_action(Bool) ->
    case kz_term:is_true(Bool) of
        'true' -> <<"accepted">>;
        'false' -> <<"denied">>
    end.

-spec flush_acls() -> 'ok'.
flush_acls() ->
    kapps_config:flush(?APP_NAME, <<"acls">>).

-spec node_summary() -> 'no_return'.
node_summary() ->
    ecallmgr_fs_nodes:summary(),
    'no_return'.

-spec node_details() -> 'no_return'.
node_details() ->
    ecallmgr_fs_nodes:details(),
    'no_return'.

-spec node_details(kz_term:text()) -> 'no_return'.
node_details(NodeName) ->
    ecallmgr_fs_nodes:details(NodeName),
    'no_return'.

-spec channel_summary() -> 'no_return'.
channel_summary() ->
    ecallmgr_fs_channels:summary(),
    'no_return'.

-spec channel_summary(kz_term:text()) -> 'no_return'.
channel_summary(Node) ->
    ecallmgr_fs_channels:summary(Node),
    'no_return'.

-spec channel_details() -> 'no_return'.
channel_details() ->
    ecallmgr_fs_channels:details(),
    'no_return'.

-spec channel_details(kz_term:text()) -> 'no_return'.
channel_details(UUID) ->
    ecallmgr_fs_channels:details(UUID),
    'no_return'.

-spec sync_channels() -> 'ok'.
sync_channels() ->
    lists:foreach(fun ecallmgr_fs_node:sync_channels/1
                 ,gproc:lookup_pids({'p', 'l', 'fs_node'})).

-spec sync_channels(kz_term:text()) -> 'ok'.
sync_channels(Node) ->
    N = kz_term:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'}),
            ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec conference_summary() -> 'no_return'.
conference_summary() ->
    ecallmgr_fs_conferences:summary(),
    'no_return'.

-spec conference_summary(kz_term:text()) -> 'no_return'.
conference_summary(Node) ->
    ecallmgr_fs_conferences:summary(Node),
    'no_return'.

-spec conference_details() -> 'no_return'.
conference_details() ->
    ecallmgr_fs_conferences:details(),
    'no_return'.

-spec conference_details(kz_term:text()) -> 'no_return'.
conference_details(UUID) ->
    ecallmgr_fs_conferences:details(UUID),
    'no_return'.

-spec sync_conferences() -> 'ok'.
sync_conferences() ->
    lists:foreach(fun ecallmgr_fs_conferences:sync_node/1
                 ,ecallmgr_fs_nodes:connected()).

-spec sync_conferences(kz_term:text()) -> 'ok'.
sync_conferences(Node) ->
    N = kz_term:to_atom(Node, 'true'),
    ecallmgr_fs_conferences:sync_node(N),
    'ok'.

-spec flush_node_channels(string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    ecallmgr_fs_channels:flush_node(Node).

-spec flush_node_conferences(string() | binary() | atom()) -> 'ok'.
flush_node_conferences(Node) ->
    ecallmgr_fs_conferences:flush_node(Node).

-spec flush_registrar() -> 'ok'.
flush_registrar() ->
    ecallmgr_registrar:flush().

-spec flush_registrar(kz_term:text()) -> 'ok'.
flush_registrar(Realm) ->
    ecallmgr_registrar:flush(Realm).

-spec flush_registrar(kz_term:text(), kz_term:text()) -> 'ok'.
flush_registrar(Username, Realm) ->
    ecallmgr_registrar:flush(Username, Realm).

-spec registrar_summary() -> 'no_return'.
registrar_summary() ->
    ecallmgr_registrar:summary(),
    'no_return'.

-spec registrar_summary(kz_term:text()) -> 'no_return'.
registrar_summary(Realm) ->
    ecallmgr_registrar:summary(Realm),
    'no_return'.

-spec registrar_details() -> 'no_return'.
registrar_details() ->
    ecallmgr_registrar:details(),
    'no_return'.

-spec registrar_details(kz_term:text()) -> 'no_return'.
registrar_details(Realm) ->
    ecallmgr_registrar:details(Realm),
    'no_return'.

-spec registrar_details(kz_term:text(), kz_term:text()) -> 'no_return'.
registrar_details(Username, Realm) ->
    ecallmgr_registrar:details(Username, Realm),
    'no_return'.

-spec registrar_sync() -> 'no_return'.
registrar_sync() ->
    ecallmgr_registrar:sync(),
    'no_return'.

-spec flush_authn() -> 'ok'.
flush_authn() ->
    kz_cache:flush_local(?ECALLMGR_AUTH_CACHE).

-spec flush_util() -> 'ok'.
flush_util() ->
    kz_cache:flush_local(?ECALLMGR_UTIL_CACHE).

-spec show_channels() -> 'no_return'.
show_channels() ->
    io:format("This function is depreciated, please use channel_summary or channel_details~n"),
    'no_return'.

-spec show_calls() -> 'no_return'.
show_calls() ->
    io:format("This function is depreciated, please use channel_summary or channel_details~n"),
    'no_return'.

-spec check_sync(kz_term:text(), kz_term:text()) -> 'ok'.
check_sync(Username, Realm) ->
    ecallmgr_fs_notify:notify(kz_term:to_binary(Username)
                             ,kz_term:to_binary(Realm)
                             ,<<"check-sync">>
                             ).

-spec add_fs_node(kz_term:text(), kz_term:ne_binaries(), config_fun()) ->
          'ok' |
          {'error', any()}.
add_fs_node(FSNode, FSNodes, ConfigFun) when not is_binary(FSNode) ->
    add_fs_node(kz_term:to_binary(FSNode), FSNodes, ConfigFun);
add_fs_node(FSNode, FSNodes, ConfigFun) ->
    _ = case lists:member(FSNode, FSNodes) of
            'true' -> 'ok';
            'false' ->
                io:format("adding ~s to ecallmgr system config~n", [FSNode]),
                run_config_fun(ConfigFun, <<"fs_nodes">>, [FSNode | FSNodes])
        end,
    ecallmgr_fs_nodes:add(kz_term:to_atom(FSNode, 'true')).

-spec remove_fs_node(kz_term:text(), kz_term:ne_binaries(), config_fun()) -> 'ok' | {'error', any()}.
remove_fs_node(FSNode, FSNodes, ConfigFun) when not is_binary(FSNode) ->
    remove_fs_node(kz_term:to_binary(FSNode), FSNodes, ConfigFun);
remove_fs_node(FSNode, FSNodes, ConfigFun) ->
    _ = case lists:member(FSNode, FSNodes) of
            'false' -> 'ok';
            'true' ->
                io:format("removing ~s from ecallmgr system config~n", [FSNode]),
                run_config_fun(ConfigFun, <<"fs_nodes">>, lists:delete(FSNode, FSNodes))
        end,
    ecallmgr_fs_nodes:remove(kz_term:to_atom(FSNode, 'true')).

-spec get_fs_nodes(kz_term:ne_binary() | atom()) -> kz_term:ne_binaries().
get_fs_nodes(Node) ->
    case ?FS_NODES(Node) of
        [_|_]=FSNodes -> FSNodes;
        [] -> [];
        _Other ->
            io:format("fs_nodes in ~s was misconfigured(~p), using []~n", [Node, _Other]),
            []
    end.

-spec modify_acls(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), acl_fun(), config_fun()) ->
          'no_return'.
modify_acls(Name, IP0, ACLS, ACLFun, ConfigFun) ->
    case kz_network_utils:resolve(IP0) of
        [] ->
            Identities = [{'cidr', kz_network_utils:is_cidr(IP0)}
                         ,{'ip', kz_network_utils:is_ip(IP0)}
                         ],
            io:format("the supplied address ~s could not be processed.~n", [IP0]),
            [io:format("  is ~s: ~s~n", [Type, Bool]) || {Type, Bool} <- Identities],
            'no_return';
        [IP | _] ->
            ACL = ACLFun(IP),
            io:format("updating ~s ACLs ~s(~s) to ~s traffic~n"
                     ,[kz_json:get_value(<<"network-list-name">>, ACL)
                      ,Name
                      ,kz_json:get_value(<<"cidr">>, ACL)
                      ,kz_json:get_value(<<"type">>, ACL)
                      ]),
            lager:info("updating ~s ACLs ~s(~s) to ~s traffic~n"
                      ,[kz_json:get_value(<<"network-list-name">>, ACL)
                       ,Name
                       ,kz_json:get_value(<<"cidr">>, ACL)
                       ,kz_json:get_value(<<"type">>, ACL)
                       ]),
            _ = run_config_fun(ConfigFun, <<"acls">>, kz_json:set_value(Name, ACL, filter_acls(ACLS))),
            maybe_reload_acls(Name, 'modify', 4)
    end.

-spec run_config_fun(config_fun(), kz_json:key(), kz_json:json_term()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error().
run_config_fun(ConfigFun, Key, Value) when is_function(ConfigFun, 3) ->
    ConfigFun(?APP_NAME, Key, Value);
run_config_fun(ConfigFun, Key, Value) when is_function(ConfigFun, 4) ->
    ConfigFun(?APP_NAME, Key, Value, node()).

-spec remove_acl(kz_term:ne_binary(), kz_json:object(), config_fun()) -> 'no_return'.
remove_acl(Name, ACLs, ConfigFun) ->
    FilteredACLs = filter_acls(ACLs),
    _ = case kz_json:get_value(Name, FilteredACLs) of
            'undefined' -> io:format("no ACL named ~s found~n", [Name]);
            ACL ->
                io:format("removing ~s ACLs ~s(~s) from ecallmgr system config~n"
                         ,[kz_json:get_value(<<"network-list-name">>, ACL)
                          ,Name
                          ,kz_json:get_value(<<"cidr">>, ACL)
                          ]),
                run_config_fun(ConfigFun, <<"acls">>, kz_json:set_value(Name, 'null', FilteredACLs))
        end,
    maybe_reload_acls(Name, 'remove', 4).

-spec list_acls(kz_json:object(), kz_term:api_binary()) -> 'no_return'.
list_acls(ACLs, Network) ->
    ThinBar  = "+--------------------------------+--------------------+---------------+-------+------------------+----------------------------------+\n",
    ThickBar = "+================================+====================+===============+=======+==================+==================================+\n",
    io:put_chars(ThinBar),
    FormatString = "| ~-30s | ~-18s | ~-13s | ~-5s | ~-16s | ~-32s |~n",
    io:format(FormatString, [<<"Name">>, <<"CIDR">>, <<"List">>, <<"Type">>, <<"Authorizing Type">>, <<"ID">>]),
    io:put_chars(ThickBar),
    Props = kz_json:foldl(fun(Name, ACL, Acc) ->
                                  [{kz_json:get_value(<<"network-list-name">>, ACL)
                                   ,kz_json:set_value(<<"name">>, Name, ACL)}
                                   | Acc
                                  ]
                          end, [], ACLs),
    _ = [maybe_print_acl(Network, FormatString, ACL)
         || {_, ACL} <- lists:sort(fun list_acls_sort/2, Props)
        ],
    io:put_chars(ThinBar),
    'no_return'.

list_acls_sort({Network1, _}, {Network2, _}) ->
    Network1 =< Network2.

maybe_print_acl('undefined', FormatString, ACL) ->
    print_acl(FormatString, ACL);
maybe_print_acl(Network, FormatString, ACL) ->
    case kz_json:get_value(<<"network-list-name">>, ACL) =:= Network of
        'true' -> print_acl(FormatString, ACL);
        'false' -> 'ok'
    end.

-spec print_acl(string(), kz_json:object()) -> 'ok'.
print_acl(FormatString, ACL) ->
    io:format(FormatString, [kz_json:get_value(<<"name">>, ACL)
                            ,kz_json:get_value(<<"cidr">>, ACL)
                            ,kz_json:get_value(<<"network-list-name">>, ACL)
                            ,kz_json:get_value(<<"type">>, ACL)
                            ,kz_json:get_value(<<"authorizing_type">>, ACL, <<"system_config">>)
                            ,kz_json:get_first_defined([<<"account_id">>
                                                       ,<<"authorizing_id">>
                                                       ], ACL, <<>>)
                            ]).

-spec get_acls() -> kz_json:object().
get_acls() -> get_acls(node()).

-spec get_acls(atom() | kz_term:ne_binary()) -> kz_json:object().
get_acls(Node) ->
    ecallmgr_fs_acls:get(Node).

-spec filter_acls(kz_json:object()) -> kz_json:object().
filter_acls(ACLs) ->
    kz_json:filter(fun filter_acls_fun/1, ACLs).

-spec filter_acls_fun({kz_json:key(), kz_json:json_term()}) -> boolean().
filter_acls_fun({_Name, ACL}) ->
    kz_json:get_ne_binary_value(<<"authorizing_type">>, ACL) =:= 'undefined'.

-spec carrier_acl(kz_term:ne_binary()) -> kz_json:object().
carrier_acl(IP) -> carrier_acl(IP, <<"allow">>).

-spec carrier_acl(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
carrier_acl(IP, Type) ->
    kz_json:from_list([{<<"type">>, Type}
                      ,{<<"network-list-name">>, ?FS_CARRIER_ACL_LIST}
                      ,{<<"cidr">>, kz_network_utils:to_cidr(IP)}
                      ]).

-spec sbc_acl(kz_term:ne_binary()) -> kz_json:object().
sbc_acl(IP) -> sbc_acl(IP, <<"allow">>).

-spec sbc_acl(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
sbc_acl(IP, Type) ->
    kz_json:from_list([{<<"type">>, Type}
                      ,{<<"network-list-name">>, ?FS_SBC_ACL_LIST}
                      ,{<<"cidr">>, kz_network_utils:to_cidr(IP)}
                      ]).

-spec enable_authz() -> 'ok'.
enable_authz() ->
    _ = kapps_config:set_default(?APP_NAME, <<"authz_enabled">>, 'true'),
    io:format("turned on authz; calls will now require authorization~n").

-spec disable_authz() -> 'ok'.
disable_authz() ->
    _ = kapps_config:set_default(?APP_NAME, <<"authz_enabled">>, 'false'),
    io:format("turned off authz; calls will no longer require authorization~n").

-spec enable_local_resource_authz() -> 'ok'.
enable_local_resource_authz() ->
    _ = kapps_config:set_default(?APP_NAME, <<"authz_local_resources">>, 'true'),
    io:format("turned on authz for local resources; calls to local resources will now require authorization~n").

-spec disable_local_resource_authz() -> 'ok'.
disable_local_resource_authz() ->
    _ = kapps_config:set_default(?APP_NAME, <<"authz_local_resources">>, 'false'),
    io:format("turned off authz for local resources; calls to local resources will no longer require authorization~n").

-spec limit_channel_uptime(kz_term:ne_binary()) -> 'ok'.
limit_channel_uptime(MaxAge) ->
    limit_channel_uptime(MaxAge, 'true').

-spec limit_channel_uptime(kz_term:ne_binary(), kz_term:ne_binary() | boolean()) -> 'ok'.
limit_channel_uptime(MaxAge, AsDefault) ->
    _ = ecallmgr_fs_channels:set_max_channel_uptime(kz_term:to_integer(MaxAge), kz_term:is_true(AsDefault)),
    io:format("updating max channel uptime to ~p (use 0 to disable check)~n", [MaxAge]).

-spec hangup_long_running_channels() -> 'ok'.
hangup_long_running_channels() ->
    MaxAge = ecallmgr_fs_channels:max_channel_uptime(),
    hangup_long_running_channels(MaxAge).

-spec hangup_long_running_channels(kz_term:text() | pos_integer()) -> 'ok'.
hangup_long_running_channels(MaxAge) ->
    io:format("hanging up channels older than ~p seconds~n", [MaxAge]),
    N = ecallmgr_fs_channels:cleanup_old_channels(kz_term:to_integer(MaxAge)),
    io:format("hungup ~p channels~n", [N]).

-spec hangup(kz_term:text()) -> freeswitch:fs_api_ret().
hangup(UUID) ->
    case ecallmgr_fs_channel:fetch(UUID, 'record') of
        {'ok', #channel{node=Node}} ->
            freeswitch:api(Node, 'uuid_kill', UUID);
        _ -> io:format("channel ~s not found~n", [UUID])
    end.
