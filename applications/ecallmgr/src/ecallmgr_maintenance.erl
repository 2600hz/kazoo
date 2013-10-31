%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%-------------------------------------------------------------------
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
        ]).
-export([allow_carrier/2
         ,allow_carrier/3
        ]).
-export([deny_carrier/2
         ,deny_carrier/3
        ]).

-export([sbc_acls/0
         ,sbc_acls/1
        ]).
-export([allow_sbc/2
         ,allow_sbc/3
        ]).
-export([deny_sbc/2
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
-export([authz_summary/0]).
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
-export([flush_authn/0]).
-export([flush_util/0]).

-export([show_channels/0]).
-export([show_calls/0]).

-include("ecallmgr.hrl").

-spec add_fs_node(text()) -> 'ok'.
add_fs_node(FSNode) ->
    FSNodes = get_fs_nodes(node()),
    add_fs_node(FSNode, FSNodes, fun ecallmgr_config:set/2).

-spec add_fs_node(text(), text() | boolean()) -> 'ok'.
add_fs_node(FSNode, AsDefault) when not is_boolean(AsDefault) ->
    add_fs_node(FSNode, wh_util:is_true(AsDefault));
add_fs_node(FSNode, 'true') ->
    FSNodes = get_fs_nodes(<<"default">>),
    add_fs_node(FSNode, FSNodes, fun ecallmgr_config:set_default/2);
add_fs_node(FSNode, 'false') ->
    FSNodes = get_fs_nodes(node()),
    add_fs_node(FSNode, FSNodes, fun ecallmgr_config:set_node/2).

-spec remove_fs_node(text() | atom()) -> 'ok'.
remove_fs_node(FSNode) ->
    FSNodes = get_fs_nodes(node()),
    remove_fs_node(FSNode, FSNodes, fun ecallmgr_config:set/2).

-spec remove_fs_node(text(), text() | boolean()) -> 'ok'.
remove_fs_node(FSNode, AsDefault) when not is_boolean(AsDefault) ->
    remove_fs_node(FSNode, wh_util:is_true(AsDefault));
remove_fs_node(FSNode, 'true') ->
    FSNodes = get_fs_nodes(<<"default">>),
    remove_fs_node(FSNode, FSNodes, fun ecallmgr_config:set_default/2);
remove_fs_node(FSNode, 'false') ->
    FSNodes = get_fs_nodes(node()),
    remove_fs_node(FSNode, FSNodes, fun ecallmgr_config:set_node/2).

-spec list_fs_nodes() -> 'no_return'.
list_fs_nodes() -> 
    [io:format("~s~n", [Node]) || Node <- ecallmgr_fs_nodes:connected()],
    'no_return'.

-spec get_fs_nodes() -> 'no_return'.
get_fs_nodes() -> 
    [io:format("~s~n", [Node]) || Node <- get_fs_nodes(node())],
    'no_return'.

-spec carrier_acls() -> 'ok'.
carrier_acls() -> carrier_acls('false').

-spec carrier_acls(boolean() | text()) -> 'ok'.
carrier_acls(AsDefault) when not is_boolean(AsDefault) ->
    carrier_acls(wh_util:is_true(AsDefault));
carrier_acls('true') ->
    list_acls(get_acls(<<"default">>), <<"trusted">>);
carrier_acls('false') ->
    list_acls(get_acls(), <<"trusted">>).

-spec allow_carrier(ne_binary(), ne_binary()) -> 'no_return'.
allow_carrier(Name, IP) ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun carrier_acl/1
                ,fun ecallmgr_config:set/2).

-spec allow_carrier(ne_binary(), ne_binary(), boolean() | text()) -> 'no_return'.
allow_carrier(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    allow_carrier(Name, IP, wh_util:is_true(AsDefault));
allow_carrier(Name, IP, 'true') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls(<<"default">>)
                ,fun carrier_acl/1
                ,fun ecallmgr_config:set_default/2);
allow_carrier(Name, IP, 'false') ->
    modify_acls(Name
                ,IP
                ,get_acls()
                ,fun carrier_acl/1
                ,fun ecallmgr_config:set_node/2).

-spec deny_carrier(ne_binary(), ne_binary()) -> 'no_return'.
deny_carrier(Name, IP) ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun(_) -> carrier_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set/2).

-spec deny_carrier(ne_binary(), ne_binary(), boolean() | text()) -> 'no_return'.
deny_carrier(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    deny_carrier(Name, IP, wh_util:is_true(AsDefault));
deny_carrier(Name, IP, 'true') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls(<<"default">>)
                ,fun(_) -> carrier_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set_default/2);
deny_carrier(Name, IP, 'false') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun(_) -> carrier_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set_node/2).

-spec sbc_acls() -> 'ok'.
sbc_acls() -> sbc_acls('false').

-spec sbc_acls(boolean() | text()) -> 'ok'.
sbc_acls(AsDefault) when not is_boolean(AsDefault) ->
    sbc_acls(wh_util:is_true(AsDefault));
sbc_acls('true') ->
    list_acls(get_acls(<<"default">>), <<"authoritative">>);
sbc_acls('false') ->    
    list_acls(get_acls(), <<"authoritative">>).

-spec allow_sbc(ne_binary(), ne_binary()) -> 'no_return'.
allow_sbc(Name, IP) ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun sbc_acl/1
                ,fun ecallmgr_config:set/2).

-spec allow_sbc(ne_binary(), ne_binary(), boolean() | text()) -> 'no_return'.
allow_sbc(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    allow_sbc(Name, IP, wh_util:is_true(AsDefault));
allow_sbc(Name, IP, 'true') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls(<<"default">>)
                ,fun sbc_acl/1
                ,fun ecallmgr_config:set_default/2);
allow_sbc(Name, IP, 'false') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun sbc_acl/1
                ,fun ecallmgr_config:set_node/2).

-spec deny_sbc(ne_binary(), ne_binary()) -> 'no_return'.
deny_sbc(Name, IP) ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun(_) -> sbc_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set/2).

-spec deny_sbc(ne_binary(), ne_binary(), boolean() | text()) -> 'no_return'.
deny_sbc(Name, IP, AsDefault) when not is_boolean(AsDefault) ->
    deny_sbc(Name, IP, wh_util:is_true(AsDefault));
deny_sbc(Name, IP, 'true') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls(<<"default">>)
                ,fun(_) -> sbc_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set_default/2);
deny_sbc(Name, IP, 'false') ->
    modify_acls(wh_util:to_binary(Name)
                ,wh_util:to_binary(IP)
                ,get_acls()
                ,fun(_) -> sbc_acl(IP, <<"deny">>) end
                ,fun ecallmgr_config:set_node/2).

-spec acl_summary() -> 'ok'.
acl_summary() ->
    list_acls(get_acls(), 'undefined').

-spec acl_summary(wh_json:object()) -> 'ok'.
acl_summary(AsDefault) when not is_boolean(AsDefault) ->
    acl_summary(wh_util:is_true(AsDefault));
acl_summary('true') ->
    list_acls(get_acls(<<"default">>), 'undefined');
acl_summary('false') ->
    list_acls(get_acls(), 'undefined').

-spec remove_acl(text()) -> 'no_return'.                            
remove_acl(Name) ->
    remove_acl(wh_util:to_binary(Name)
               ,get_acls()
               ,fun ecallmgr_config:set/2).

-spec remove_acl(text(), text() | boolean()) -> 'no_return'.
remove_acl(Name, AsDefault) when not is_boolean(AsDefault) ->
    remove_acl(Name, wh_util:is_true(AsDefault));
remove_acl(Name, 'true') ->
    remove_acl(wh_util:to_binary(Name)
               ,get_acls(<<"default">>)
               ,fun ecallmgr_config:set_default/2);
remove_acl(Name, 'false') ->
    remove_acl(wh_util:to_binary(Name)
               ,get_acls()
               ,fun ecallmgr_config:set_node/2).

-spec reload_acls() -> 'no_return'.
reload_acls() ->
    _ = [begin
             io:format("issued reload ACLs to ~s~n", [Node]),
             freeswitch:bgapi(Node, 'reloadacl', "")
         end
         || Node <- ecallmgr_fs_nodes:connected()
        ],
    'no_return'.

-spec flush_acls() -> 'ok'.
flush_acls() ->
    ecallmgr_config:flush(<<"acls">>).

-spec node_summary() -> 'no_return'.
node_summary() ->
    ecallmgr_fs_nodes:summary(),
    'no_return'.

-spec node_details() -> 'no_return'.
node_details() ->
    ecallmgr_fs_nodes:details(),
    'no_return'.

-spec node_details(text()) -> 'no_return'.
node_details(NodeName) ->
    ecallmgr_fs_nodes:details(NodeName),
    'no_return'.

-spec authz_summary() -> 'no_return'.
authz_summary() ->
    ecallmgr_fs_channels:authz_summary(),
    'no_return'.

-spec channel_summary() -> 'no_return'.
channel_summary() ->
    ecallmgr_fs_channels:summary(),
    'no_return'.

-spec channel_summary(text()) -> 'no_return'.
channel_summary(Node) ->
    ecallmgr_fs_channels:summary(Node),
    'no_return'.

-spec channel_details() -> 'no_return'.
channel_details() ->
    ecallmgr_fs_channels:details(),
    'no_return'.

-spec channel_details(text()) -> 'no_return'.
channel_details(UUID) ->
    ecallmgr_fs_channels:details(UUID),
    'no_return'.

-spec sync_channels() -> 'ok'.
sync_channels() ->
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
        ],
    'ok'.

-spec sync_channels(text()) -> 'ok'.
sync_channels(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec conference_summary() -> 'no_return'.
conference_summary() ->
    ecallmgr_fs_conferences:summary(),
    'no_return'.

-spec conference_summary(text()) -> 'no_return'.
conference_summary(Node) ->
    ecallmgr_fs_conferences:summary(Node),
    'no_return'.

-spec conference_details() -> 'no_return'.
conference_details() ->
    ecallmgr_fs_conferences:details(),
    'no_return'.

-spec conference_details(text()) -> 'no_return'.
conference_details(UUID) ->
    ecallmgr_fs_conferences:details(UUID),
    'no_return'.

-spec sync_conferences() -> 'ok'.
sync_conferences() ->
    _ = [ecallmgr_fs_conferences:sync_node(N)
         || N <- ecallmgr_fs_nodes:connected()
        ],
    'ok'.

-spec sync_conferences(text()) -> 'ok'.
sync_conferences(Node) ->
    N = wh_util:to_atom(Node, 'true'),
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

-spec flush_registrar(text()) -> 'ok'.
flush_registrar(Realm) ->
    ecallmgr_registrar:flush(Realm).

-spec flush_registrar(text(), text()) -> 'ok'.
flush_registrar(Username, Realm) ->
    ecallmgr_registrar:flush(Username, Realm).

-spec registrar_summary() -> 'no_return'.
registrar_summary() ->
    ecallmgr_registrar:summary(),
    'no_return'.

-spec registrar_summary(text()) -> 'no_return'.
registrar_summary(Realm) ->
    ecallmgr_registrar:summary(Realm),
    'no_return'.

-spec registrar_details() -> 'no_return'.
registrar_details() ->
    ecallmgr_registrar:details(),
    'no_return'.

-spec registrar_details(text()) -> 'no_return'.
registrar_details(Realm) ->
    ecallmgr_registrar:details(Realm),
    'no_return'.

-spec registrar_details(text(), text()) -> 'no_return'.
registrar_details(Username, Realm) ->
    ecallmgr_registrar:details(Username, Realm),
    'no_return'.

-spec flush_authn() -> 'ok'.
flush_authn() ->
    wh_cache:flush_local(?ECALLMGR_AUTH_CACHE).

-spec flush_util() -> 'ok'.
flush_util() ->
    wh_cache:flush_local(?ECALLMGR_UTIL_CACHE).

-spec show_channels() -> 'no_return'.
show_channels() ->
    io:format("This function is depreciated, please use channel_summary or channel_detail~n"),
    'no_return'.

-spec show_calls() -> 'no_return'.
show_calls() ->
    io:format("This function is depreciated, please use channel_summary or channel_detail~n"),
    'no_return'.



-spec add_fs_node(text(), ne_binaries(), function()) -> 'ok' | {'error', _}.
add_fs_node(FSNode, FSNodes, ConfigFun) when not is_binary(FSNode) ->
    add_fs_node(wh_util:to_binary(FSNode), FSNodes, ConfigFun);
add_fs_node(FSNode, FSNodes, ConfigFun) ->
    _ = case lists:member(FSNode, FSNodes) of
            'true' -> 'ok';
            'false' ->
                io:format("adding ~s to ecallmgr system config~n"
                          ,[FSNode]),
                ConfigFun(<<"fs_nodes">>, [FSNode | FSNodes])
        end,
    ecallmgr_fs_nodes:add(wh_util:to_atom(FSNode, 'true')).    

-spec remove_fs_node(text(), ne_binaries(), function()) -> 'ok' | {'error', _}.
remove_fs_node(FSNode, FSNodes, ConfigFun) when not is_binary(FSNode) ->
    remove_fs_node(wh_util:to_binary(FSNode), FSNodes, ConfigFun);
remove_fs_node(FSNode, FSNodes, ConfigFun) ->
    _ = case lists:member(FSNode, FSNodes) of
            'false' -> 'ok';
            'true' ->
                io:format("removing ~s from ecallmgr system config~n"
                          ,[FSNode]),
                ConfigFun(<<"fs_nodes">>, lists:delete(FSNode, FSNodes))
        end,
    ecallmgr_fs_nodes:remove(wh_util:to_atom(FSNode, 'true')).

-spec get_fs_nodes(ne_binary()) -> ne_binaries().
get_fs_nodes(Node) ->
    case ecallmgr_config:get(<<"fs_nodes">>, 'undefined', Node) of
        [_|_]=FSNodes -> FSNodes;
        [] -> [];
        _Other ->
            io:format("fs_nodes in ~s was misconfigured(~p), using []~n", [Node, _Other]),
            []
    end.

-spec modify_acls(ne_binary(), ne_binary(), wh_json:object(), function(), function()) -> 'no_return'.
modify_acls(Name, IP, ACLS, ACLFun, ConfigFun) ->
    ACL = ACLFun(IP),
    io:format("updating ~s ACLs ~s(~s) to ~s traffic~n"
              ,[wh_json:get_value(<<"network-list-name">>, ACL)
                ,Name
                ,wh_json:get_value(<<"cidr">>, ACL)
                ,wh_json:get_value(<<"type">>, ACL)
               ]),
    ConfigFun(<<"acls">>, wh_json:set_value(Name, ACL, filter_acls(ACLS))),
    ecallmgr_config:flush(<<"acls">>),
    reload_acls(),
    'no_return'.

remove_acl(Name, ACLs, ConfigFun) ->
    FilteredACLs = filter_acls(ACLs),
    _ = case wh_json:get_value(Name, FilteredACLs) of
            'undefined' -> 'ok';
            ACL ->
                io:format("removing ~s ACLs ~s(~s) from ecallmgr system config~n"
                          ,[wh_json:get_value(<<"network-list-name">>, ACL)
                            ,Name
                            ,wh_json:get_value(<<"cidr">>, ACL)
                           ]),
                ConfigFun(<<"acls">>, wh_json:delete_key(Name, FilteredACLs))
        end,
    ecallmgr_config:flush(<<"acls">>),
    reload_acls(),
    'no_return'.

-spec list_acls(wh_json:object(), api_binary()) -> 'no_return'.
list_acls(ACLs, Network) ->
    io:format("+--------------------------------+-------------------+---------------+-------+------------------+----------------------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-13s | ~-5s | ~-16s | ~-32s |~n",
    io:format(FormatString, [<<"Name">>, <<"CIDR">>, <<"List">>, <<"Type">>, <<"Authorizing Type">>, <<"ID">>]),
    io:format("+================================+===================+===============+=======+==================+==================================+~n", []),
    Props = wh_json:foldl(fun(Name, ACL, Acc) -> 
                                  [{wh_json:get_value(<<"network-list-name">>, ACL)
                                    ,wh_json:set_value(<<"name">>, Name, ACL)} 
                                   | Acc
                                  ]
                          end, [], ACLs),
    _ = [maybe_print_acl(Network, FormatString, ACL) 
         || {_, ACL} <- lists:sort(fun list_acls_sort/2, Props)
        ],
    io:format("+--------------------------------+-------------------+---------------+-------+------------------+----------------------------------+~n", []),
    'no_return'.

list_acls_sort({Network1, _}, {Network2, _}) -> 
    Network1 =< Network2.

maybe_print_acl('undefined', FormatString, ACL) ->
    io:format(FormatString, [wh_json:get_value(<<"name">>, ACL)
                             ,wh_json:get_value(<<"cidr">>, ACL)
                             ,wh_json:get_value(<<"network-list-name">>, ACL)
                             ,wh_json:get_value(<<"type">>, ACL)
                             ,wh_json:get_value(<<"authorizing_type">>, ACL, <<"system_config">>)
                             ,wh_json:get_first_defined([<<"account_id">>
                                                         ,<<"authorizing_id">>
                                                        ], ACL, <<>>)
                            ]);
maybe_print_acl(Network, FormatString, ACL) ->
    case wh_json:get_value(<<"network-list-name">>, ACL) =:= Network of
        'true' -> io:format(FormatString, [wh_json:get_value(<<"name">>, ACL)
                                           ,wh_json:get_value(<<"cidr">>, ACL)
                                           ,wh_json:get_value(<<"network-list-name">>, ACL)
                                           ,wh_json:get_value(<<"type">>, ACL)
                                           ,wh_json:get_value(<<"authorizing_type">>, ACL, <<"system_config">>)
                                           ,wh_json:get_first_defined([<<"account_id">>
                                                                       ,<<"authorizing_id">>
                                                                      ], ACL, <<>>)
                                          ]);
        'false' -> 'ok'
    end.

-spec get_acls() -> wh_json:object().
get_acls() -> get_acls(node()).
    
-spec get_acls(ne_binary()) -> wh_json:object().
get_acls(Node) ->
    ACLs = ecallmgr_config:fetch(<<"acls">>, 'undefined', Node),
    case wh_json:is_json_object(ACLs) of
        'true' -> ACLs;
        'false' ->
            io:format("failed to load json object for ACLs from ~s, got ~p, using empty json object instead~n", [Node, ACLs]),
            wh_json:new()
    end.

-spec filter_acls(wh_json:object()) -> wh_json:object().
filter_acls(ACLs) ->
    wh_json:filter(fun filter_acls_fun/1, ACLs).

-spec filter_acls_fun({wh_json:key(), wh_json:json_term()}) -> boolean().
filter_acls_fun({_Name, ACL}) ->
    wh_json:get_value(<<"authorizing_type">>, ACL) =:= 'undefined'.

-spec carrier_acl(ne_binary()) -> wh_json:object().
carrier_acl(IP) -> carrier_acl(IP, <<"allow">>).

-spec carrier_acl(ne_binary(), ne_binary()) -> wh_json:object().
carrier_acl(IP, Type) ->
    wh_json:from_list([{<<"type">>, Type}
                       ,{<<"network-list-name">>, <<"trusted">>}
                       ,{<<"cidr">>, wh_network_utils:to_cidr(IP)}
                      ]).

-spec sbc_acl(ne_binary()) -> wh_json:object().
sbc_acl(IP) -> sbc_acl(IP, <<"allow">>).

-spec sbc_acl(ne_binary(), ne_binary()) -> wh_json:object().
sbc_acl(IP, Type) ->
    wh_json:from_list([{<<"type">>, Type}
                       ,{<<"network-list-name">>, <<"authoritative">>}
                       ,{<<"cidr">>, wh_network_utils:to_cidr(IP)}
                      ]).
