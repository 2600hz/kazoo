%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_nodes).
-behaviour(gen_listener).

-compile({no_auto_import,[nodes/0]}).

-export([start_link/0]).
-export([is_up/1]).
-export([whapp_count/1, whapp_count/2
        ,whapp_role_count/2, whapp_role_count/3
        ,node_role_count/1, node_role_count/2
        ,whapp_oldest_node/1
        ,whapp_oldest_node/2
        ]).
-export([status/0]).
-export([status_to_json/0]).
-export([flush/0]).
-export([handle_advertise/2]).
-export([notify_new/0
        ,notify_new/1
        ]).
-export([notify_expire/0
        ,notify_expire/1
        ,request/1
        ]).
-export([local_zone/0]).
-export([whapp_zones/1, whapp_zone_count/1]).
-export([globals_scope/0]).
-export([node_encoded/0
        ,node_to_json/1
        ,node_info/0
        ,pool_state_binding/0, bind_for_pool_state/1, bind_for_pool_state/2
        ,unbind_for_pool_state/1, unbind_for_pool_state/2
        ]).

-export([with_role/1, with_role/2]).
-export([print_role/1]).
-export([nodes/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-type request_info() :: {'app', atom()} |
                        {'media_servers', [{kz_term:ne_binary(), kz_json:object()}]} |
                        {'channels', non_neg_integer()} |
                        {'conferences', non_neg_integer()} |
                        {'registrations', non_neg_integer()} |
                        {'info', kz_types:whapp_info()}.
-type request_acc() :: [request_info()].

-export_type([request_acc/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'nodes', ['federate']}
                  ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_advertise'}
                     ,[{<<"nodes">>, <<"advertise">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'no_local', 'true'}]).

-define(HEARTBEAT, 5 * ?MILLISECONDS_IN_SECOND + rand:uniform(10 * ?MILLISECONDS_IN_SECOND)).
-define(EXPIRE_PERIOD, 1 * ?MILLISECONDS_IN_SECOND).
-define(FUDGE_FACTOR, 1.25).
-define(APP_NAME, <<"kz_nodes">>).
-define(APP_VERSION, <<"4.0.0">>).

%% kz_nodes lives in this app
-define(APP_NAME_ATOM, 'kazoo_globals').

-define(MEDIA_SERVERS_HEADER, "Media Servers : ").
-define(MEDIA_SERVERS_LINE, "                ").
-define(MEDIA_SERVERS_DETAIL, "~s (~s)").

-define(HEADER_COL, "~-14s").
-define(SIMPLE_ROW_STR, ?HEADER_COL ": ~s~n").
-define(SIMPLE_ROW_NUM, ?HEADER_COL ": ~B~n").

-record(state, {heartbeat_ref = erlang:make_ref() :: reference()
               ,tab :: ets:tid()
               ,notify_new = sets:new() :: sets:set()
               ,notify_expire = sets:new() :: sets:set()
               ,node = node() :: atom()
               ,md5 :: kz_term:ne_binary()
               ,zone = 'local' :: atom()
               ,version :: kz_term:ne_binary()
               ,zones = [] :: kz_term:proplist()
               ,me = 'undefined' :: kz_types:kz_node() | 'undefined'
               }).
-type nodes_state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

-spec is_up(node()) -> boolean().
is_up(Node) ->
    case ets:match(?MODULE, #kz_node{node = Node
                                    ,expires = '$2'
                                    ,_ = '_'
                                    })
    of
        [] -> 'false';
        [_] -> 'true'
    end.

-spec node_to_json(kz_term:text() | kz_types:kz_node()) -> kz_json:object().
node_to_json(NodeName) when is_atom(NodeName) ->
    [#kz_node{}=Node] = ets:lookup(?MODULE, NodeName),
    node_to_json(Node);
node_to_json(#kz_node{node=NodeName
                     ,zone=Zone
                     ,kapps=Kapps
                     ,media_servers=MediaServers
                     ,version=Version
                     ,channels=Channels
                     ,conferences=Conferences
                     ,registrations=Regs
                     }) ->
    kz_json:from_list([{<<"node">>, kz_term:to_binary(NodeName)}
                      ,{<<"zone">>, kz_term:to_binary(Zone)}
                      ,{<<"kapps">>, [K || {K, _} <- Kapps]}
                      ,{<<"media_servers">>, [K || {K, _} <- MediaServers]}
                      ,{<<"version">>, Version}
                      ,{<<"channels">>, Channels}
                      ,{<<"conferences">>, Conferences}
                      ,{<<"registrations">>, Regs}
                      ]);
node_to_json(NodeName) ->
    node_to_json(kz_term:to_atom(NodeName)).

-spec globals_scope() -> integer().
globals_scope() ->
    MatchSpec = [{#kz_node{globals='$1'
                          ,node='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', node()}}
                   }]
                 ,['$1']
                 }],
    length(ets:select(?MODULE, MatchSpec)).

-spec whapp_count(kz_term:text()) -> integer().
whapp_count(Whapp) ->
    whapp_count(Whapp, 'false').

-spec whapp_count(kz_term:text(), kz_term:text() | boolean() | 'remote') -> integer().
whapp_count(Whapp, Arg) when not is_atom(Arg) ->
    whapp_count(Whapp, kz_term:to_atom(Arg, 'true'));
whapp_count(Whapp, 'false') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone = local_zone()
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'true') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, 'remote') ->
    Zone = local_zone(),
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', Zone}}
                   }]
                 ,['$1']
                 }],
    determine_whapp_count(kz_term:to_binary(Whapp), MatchSpec);
whapp_count(Whapp, Unhandled) ->
    lager:debug("invalid parameters ~p ~p", [Whapp, Unhandled]),
    0.

-spec determine_whapp_count(kz_term:ne_binary(), ets:match_spec()) -> non_neg_integer().
determine_whapp_count(Whapp, MatchSpec) ->
    lists:foldl(fun(Whapps, Acc) when is_list(Whapps) ->
                        determine_whapp_count_fold(Whapps, Acc, Whapp)
                end
               ,0
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec determine_whapp_count_fold(kz_types:kapps_info(), non_neg_integer(), kz_term:ne_binary()) -> non_neg_integer().
determine_whapp_count_fold(Whapps, Acc, Whapp) ->
    case props:is_defined(Whapp, Whapps) of
        'true' -> Acc + 1;
        'false' -> Acc
    end.

-spec whapp_zones(kz_term:text()) -> list().
whapp_zones(Whapp) ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$2', '$1'}}]
                 }],
    determine_whapp_zones(kz_term:to_binary(Whapp), MatchSpec).

-spec determine_whapp_zones(kz_term:ne_binary(), ets:match_spec()) -> list().
determine_whapp_zones(Whapp, MatchSpec) ->
    {Whapp, Zones, _} =
        lists:foldl(fun determine_whapp_zones_fold/2
                   ,{Whapp, [], 0}
                   ,ets:select(?MODULE, MatchSpec)
                   ),
    Zones.

-spec whapp_zone_count(kz_term:text()) -> integer().
whapp_zone_count(Whapp) ->
    length(whapp_zones(Whapp)).

-type fold_zones_acc() :: {atom(), list(), non_neg_integer()}.

-spec determine_whapp_zones_fold({atom(), kz_types:kapps_info()}, fold_zones_acc()) -> fold_zones_acc().
determine_whapp_zones_fold({Zone, Whapps}, {Whapp, Zones, C}=Acc) ->
    case props:is_defined(Whapp, Whapps)
        andalso not lists:member(Zone, Zones)
    of
        'true' -> {Whapp, [Zone | Zones], C+ 1};
        'false' -> Acc
    end.

-spec status_to_json() -> kz_json:object().
status_to_json() ->
    try
        Nodes = lists:sort(fun compare_nodes/2, ets:tab2list(?MODULE)),
        lists:foldl(fun node_status_to_json/2, kz_json:new(), Nodes)
    catch
        'error':'badarg' ->
            kz_json:set_value(<<"error">>
                             ,<<"status unknown until node is fully initialized, try again in a moment\n">>
                             ,kz_json:new()
                             )
    end.

-spec node_status_to_json(kz_types:kz_node(), kz_json:object()) -> kz_json:object().
node_status_to_json(#kz_node{zone=NodeZone
                            ,node=N
                            ,md5=MD5
                            ,version=Version
                            ,processes=Processes
                            ,ports=Ports
                            ,used_memory=UsedMemory
                            ,broker=Broker
                            ,kapps=Kapps
                            ,globals=Globals
                            ,node_info=NodeInfo
                            ,roles=Roles
                            ,media_servers=MediaServers
                            ,registrations=Regs
                            ,channels=Channels
                            ,conferences=Conferences
                            }=_Node
                   ,Acc
                   ) ->
    StatusProps = props:filter_empty(
                    [{<<"node">>, kz_term:to_binary(N)}
                    ,{<<"md5">>, MD5}
                    ,{<<"version">>, Version}
                    ,{<<"used_memory">>, kz_term:to_binary(kz_network_utils:pretty_print_bytes(UsedMemory))}
                    ,{<<"processes">>, Processes}
                    ,{<<"ports">>, Ports}
                    ,{<<"zone">>, kz_term:to_binary(NodeZone)}
                    ,{<<"broker">>, Broker}
                    ,{<<"kapps">>, [K || {K, _} <- Kapps]}
                    ,{<<"globals">>, kz_json:from_list(Globals)}
                    ,{<<"node_info">>, NodeInfo}
                    ,{<<"roles">>, kz_json:from_list(Roles)}
                    ,{<<"media_servers">>, [K || {K, _} <- MediaServers]}
                    ,{<<"channels">>, Channels}
                    ,{<<"conferences">>, Conferences}
                    ,{<<"registrations">>, Regs}
                    ]),
    [NodeType,_] = binary:split(kz_term:to_binary(N), <<"@">>),
    kz_json:set_value([kz_term:to_binary(NodeZone), NodeType, kz_term:to_binary(N)]
                     ,kz_json:from_list(StatusProps)
                     ,Acc
                     ).

-spec status() -> 'no_return'.
status() ->
    try
        Nodes = lists:sort(fun compare_nodes/2, ets:tab2list(?MODULE)),
        print_status(Nodes, gen_listener:call(?SERVER, 'zone')),
        check_node_versions(Nodes)
    catch
        'error':'badarg' ->
            format("status unknown until node is fully initialized, try again in a moment\n"),
            'no_return'
    end.

-spec check_node_versions(kz_types:kz_nodes()) -> 'ok'.
check_node_versions([]) -> 'ok';
check_node_versions([_Node]) -> 'ok';
check_node_versions([#kz_node{version=Version
                             ,node=Node
                             }
                     | Nodes
                    ]) ->
    [Name, _Host] = binary:split(kz_term:to_binary(Node), <<"@">>),
    check_node_versions(Nodes, {Name, Node, Version}).

-spec check_node_versions(kz_types:kz_nodes(), {kz_term:ne_binary(), node(), kz_term:ne_binary()}) -> 'ok'.
check_node_versions([], _) -> 'ok';
check_node_versions([#kz_node{version=Vsn
                             ,node=N
                             }
                     | Nodes
                    ]
                   ,{Name, Node, Version}=Current
                   ) ->
    CheckAgainst =
        case binary:split(kz_term:to_binary(N), <<"@">>) of
            [Name, _Host] when Vsn =:= Version -> Current;
            [Name, _Host] when Vsn > Version ->
                lager:warning("node ~s is running a newer version (~s) than ~s (~s)"
                             ,[N, Vsn, Node, Version]
                             ),
                Current;
            [Name, _Host] ->
                lager:warning("node ~s is running an older version (~s) than ~s (~s)"
                             ,[N, Vsn, Node, Version]
                             ),
                Current;
            [NewName, _Host] ->
                {NewName, N, Vsn}
        end,
    check_node_versions(Nodes, CheckAgainst).

-spec compare_nodes(kz_types:kz_node(), kz_types:kz_node()) -> boolean().
compare_nodes(#kz_node{node = N1}, #kz_node{node = N2}) -> N1 > N2.

-spec print_status(kz_types:kz_nodes(), atom()) -> 'no_return'.
print_status(Nodes, Zone) ->
    F = fun (Node) -> print_node_status(Node, Zone) end,
    lists:foreach(F, Nodes),
    'no_return'.

-spec print_node_status(kz_types:kz_node(), atom()) -> 'ok'.
print_node_status(#kz_node{zone=NodeZone
                          ,node=N
                          ,md5=MD5
                          ,version=Version
                          ,processes=Processes
                          ,ports=Ports
                          ,used_memory=UsedMemory
                          ,broker=Broker
                          ,kapps=Whapps
                          ,globals=Globals
                          ,node_info=NodeInfo
                          ,runtime=RuntimeInfo
                          ,modules=Modules
                          ,roles=Roles
                          }=Node
                 ,Zone
                 ) ->
    MemoryUsage = kz_network_utils:pretty_print_bytes(UsedMemory),
    print_simple_row_str([<<"Node">>, N]),
    _ = maybe_print_md5(MD5),
    print_simple_row_str([<<"Version">>, Version]),
    print_simple_row_str([<<"Memory Usage">>, MemoryUsage]),

    maybe_print_row(Processes > 0, [<<"Processes">>, Processes]),
    maybe_print_row(Ports > 0, [<<"Ports">>, Ports]),

    _ = maybe_print_zone(kz_term:to_binary(NodeZone)
                        ,kz_term:to_binary(Zone)
                        ),

    print_simple_row_str([<<"Broker">>, Broker]),

    _ = maybe_print_globals(Globals),
    _ = maybe_print_node_info(NodeInfo),
    _ = maybe_print_runtime_info(RuntimeInfo),

    _ = maybe_print_kapps(Whapps),
    _ = maybe_print_modules(Modules),
    _ = maybe_print_media_servers(Node),
    _ = maybe_print_roles(Roles),

    format("~n").

-spec maybe_print_row(boolean(), list()) -> 'ok'.
maybe_print_row('false', _Args) -> 'ok';
maybe_print_row('true', Args) ->
    print_simple_row(Args).

-spec print_simple_row(list()) -> 'ok'.
print_simple_row(Args) ->
    format(?SIMPLE_ROW_NUM, Args).

-spec print_simple_row_str(list()) -> 'ok'.
print_simple_row_str(Args) ->
    format(?SIMPLE_ROW_STR, Args).

-spec maybe_print_md5(kz_term:api_binary()) -> 'ok'.
maybe_print_md5('undefined') -> 'ok';
maybe_print_md5(MD5) ->
    print_simple_row_str([<<"md5">>, MD5]).

-spec maybe_print_zone(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_print_zone(Zone, Zone) when Zone =/= <<"local">> ->
    print_simple_row_str([<<"Zone">>, <<Zone/binary, " (local)">>]);
maybe_print_zone(NodeZone, _Zone) ->
    print_simple_row_str([<<"Zone">>, NodeZone]).

-spec maybe_print_globals(kz_term:proplist()) -> 'ok'.
maybe_print_globals([]) -> 'ok';
maybe_print_globals(Globals) ->
    format(?HEADER_COL ++ ":", [<<"Globals">>]),
    lists:foreach(fun print_global/1, Globals),
    format("~n").

-spec print_global({kz_json:key(), integer()}) -> 'ok'.
print_global({K,V}) -> format(" ~s (~B)",[K, V]).

-spec maybe_print_runtime_info(kz_term:api_object()) -> 'ok'.
maybe_print_runtime_info('undefined') -> 'ok';
maybe_print_runtime_info(_RuntimeInfo) ->
    format(?HEADER_COL ++ ": ", [<<"Runtime Info">>]),
    format("~n").

-spec maybe_print_modules(kz_term:api_object()) -> 'ok'.
maybe_print_modules('undefined') -> 'ok';
maybe_print_modules(Modules) ->
    format(?HEADER_COL ++ ": ", [<<"Modules">>]),
    L = kz_json:get_list_value(<<"loaded">>, Modules),
    Mods = lists:sort([N || <<"mod_", N/binary>> <- L]),
    simple_list(Mods, 0).

-spec maybe_print_kapps(kz_term:proplist()) -> 'ok'.
maybe_print_kapps(Whapps) ->
    case lists:sort(fun compare_apps/2, Whapps) of
        []-> 'ok';
        SortedWhapps ->
            format(?HEADER_COL ": ", [<<"WhApps">>]),
            status_list(SortedWhapps, 0)
    end.

-spec compare_apps({binary(), any()}, {binary(), any()}) -> boolean().
compare_apps({K1,_}, {K2,_}) -> K1 < K2.

-spec maybe_print_roles(kz_term:proplist()) -> 'ok'.
maybe_print_roles(Roles) ->
    case lists:sort(fun compare_apps/2, Roles) of
        []-> 'ok';
        SortedRoles ->
            format(?HEADER_COL ": ", [<<"Roles">>]),
            simple_list(props:get_keys(SortedRoles)),
            lists:foreach(fun print_role/1, SortedRoles)
    end.

-spec print_role({kz_term:ne_binary(), kz_json:object()}) -> 'ok'.
print_role({<<"Dispatcher">>, Data}) ->
    Groups = kz_json:get_json_value(<<"Groups">>, Data, kz_json:new()),
    Keys = lists:sort(kz_json:get_keys(Groups)),
    lists:foreach(fun(Group) ->
                          GData = kz_json:get_json_value(Group, Groups),
                          print_dispatcher({Group, GData})
                  end
                 ,Keys
                 );
print_role({<<"Presence">>, Data}) ->
    kz_json:foreach(fun print_presence/1, Data);
print_role({<<"Registrar">>, Data}) ->
    print_simple_row([<<"Registrations">>, kz_json:get_integer_value(<<"Registrations">>, Data, 0)]);
print_role({<<"Proxy">>, Data}) ->
    kz_json:foreach(fun print_proxy/1, Data);
print_role(_) -> 'ok'.

-spec print_proxy({kz_json:key(), kz_json:object()}) -> 'ok'.
print_proxy({<<"Listeners">>, Data}) ->
    Listeners = kz_json:foldl(fun collect_listeners/3, #{}, Data),

    case maps:to_list(Listeners) of
        [] -> 'ok';
        Addrs ->
            S = lists:max([byte_size(A) || {A, _} <- Addrs]),
            Fmt = print_address_format(S),
            [{Address, Info} | Addresses] = lists:keysort(1, Addrs),
            format(?HEADER_COL ++ ": ", [<<"Listening on">>]),
            print_address_info(Address, Info, Fmt),
            _ = lists:foreach(fun(A) -> print_address(A, Fmt) end, Addresses),
            'ok'
    end.

-spec print_address_format(pos_integer()) -> kz_term:ne_binary().
print_address_format(S) ->
    Size = 15 - (15 - S),
    list_to_binary(["~-",io_lib:format("~B", [Size]),"s "]).

-spec print_address({kz_term:ne_binary(), map()}, kz_term:ne_binary()) -> 'ok'.
print_address({Address, Info}, Fmt) ->
    format(?HEADER_COL ++ "  ", [""]),
    print_address_info(Address, Info, Fmt).

-spec print_address_info(kz_term:ne_binary(), map(), kz_term:ne_binary()) -> 'ok'.
print_address_info(Address, Info, Fmt) ->
    format(Fmt, [Address]),
    _ = lists:foreach(fun print_proto/1, lists:keysort(1, maps:to_list(Info))),
    format("~n").

-spec print_proto({kz_term:ne_binary(), kz_term:integers()}) -> 'ok'.
print_proto({Proto, Ports}) ->
    format("~s (~s) ", [Proto, kz_binary:join(lists:usort(Ports), <<" ">>)]).

-spec collect_listeners(kz_json:key(), kz_json:object(), map()) -> map().
collect_listeners(_FullAddress, Info, Acc) ->
    Address = kz_json:get_first_defined([<<"advertise">>, <<"address">>], Info),
    Proto = kz_json:get_ne_binary_value(<<"proto">>, Info),
    Port = kz_json:get_integer_value(<<"port">>, Info),

    AccAddress = maps:get(Address, Acc, #{}),
    AccProto = [Port | maps:get(Proto, AccAddress, [])],

    Acc#{Address => AccAddress#{Proto => AccProto}}.

-spec print_dispatcher({kz_term:ne_binary(), kz_json:object()}) -> 'ok'.
print_dispatcher({Group, Data})->
    format(?HEADER_COL ": ", [<<"Dispatcher ", Group/binary>>]),
    Sets = kz_json:get_keys(Data),
    M = lists:map(fun(S) ->
                          URI = kz_json:get_ne_binary_value([S, <<"destination">>], Data),
                          Flags = kz_json:get_ne_binary_value([S, <<"flags">>], Data),
                          <<URI/binary," (", Flags/binary, ")  ">>
                  end
                 ,Sets
                 ),
    simple_list(M, 0).

-spec print_presence({kz_term:ne_binary(), kz_json:object()}) -> 'ok'.
print_presence({Group, Data}) ->
    format(?HEADER_COL ": ", [Group]),
    simple_list(format_presence_data(Data)).

-spec format_presence_data(kz_json:object()) -> kz_term:ne_binaries().
format_presence_data(Data) ->
    kz_json:foldl(fun format_presence_data/3, [], Data).

-spec format_presence_data(kz_term:ne_binary(), term(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
format_presence_data(K, V, Acc) ->
    [<<K/binary, " (", (kz_term:to_binary(V))/binary, ") ">> | Acc].

-spec maybe_print_media_servers(kz_types:kz_node()) -> 'ok'.
maybe_print_media_servers(#kz_node{media_servers=MediaServers
                                  ,registrations=Registrations
                                  ,channels=Channels
                                  ,conferences=Conferences
                                  }) ->
    case lists:sort(MediaServers) of
        [] when Registrations =:= 0 -> 'ok';
        [] when Registrations > 0 ->
            print_simple_row([<<"Registrations">>, Registrations]);
        [Server|Servers] ->
            print_simple_row([<<"Channels">>, Channels]),
            print_simple_row([<<"Conferences">>, Conferences]),
            print_simple_row([<<"Registrations">>, Registrations]),
            print_media_server(Server, ?MEDIA_SERVERS_HEADER),
            lists:foreach(fun print_media_server/1, Servers)
    end.

-spec print_media_server(kz_types:media_server()) -> 'ok'.
print_media_server(Server) ->
    print_media_server(Server, ?MEDIA_SERVERS_LINE).

-spec print_media_server(kz_types:media_server(), string()) -> 'ok'.
print_media_server({Name, JObj}, Format) ->
    format(lists:flatten([Format, ?MEDIA_SERVERS_DETAIL, "~n"])
          ,[Name
           ,kz_time:pretty_print_elapsed_s(
              kz_time:elapsed_s(kz_json:get_integer_value(<<"Startup">>, JObj))
             )
           ]).

-spec maybe_print_node_info(kz_term:api_object() | kz_json:json_proplist()) -> 'ok'.
maybe_print_node_info('undefined') -> 'ok';
maybe_print_node_info([]) -> 'ok';
maybe_print_node_info([First | Rest]) ->
    format(?HEADER_COL ++ ": ", [<<"Node Info">>]),
    print_node_info(First),
    lists:foreach(fun print_each_node_info/1, Rest);
maybe_print_node_info(NodeInfo) ->
    maybe_print_node_info(kz_json:to_proplist(NodeInfo)).

-spec print_each_node_info({kz_term:ne_binary(), kz_term:ne_binary() | integer()}) -> 'ok'.
print_each_node_info(KV) ->
    format(?HEADER_COL "  ", [<<>>]),
    print_node_info(KV).

-spec print_node_info({kz_term:ne_binary(), kz_term:ne_binary() | integer()}) -> 'ok'.
print_node_info({K, ?NE_BINARY = V}) ->
    print_simple_row_str([K, V]);
print_node_info({K, V}) when is_integer(V) ->
    print_simple_row([K, V]);
print_node_info({K, JObj}) ->
    format("~s: ~s~n", [K, kz_json:encode(JObj)]).

-spec status_list(kz_types:kapps_info(), 0..4) -> 'ok'.
status_list([], _) -> format("~n", []);
status_list(Whapps, Column) when Column > 3 ->
    format("~n" ++ ?HEADER_COL ++ "  ", [""]),
    status_list(Whapps, 0);
status_list([{Whapp, #whapp_info{startup='undefined'}}|Whapps], Column) ->
    format("~-25s", [Whapp]),
    status_list(Whapps, Column + 1);
status_list([{Whapp, #whapp_info{startup=Started,roles=[]}}|Whapps], Column) ->
    Elapsed = kz_time:elapsed_s(Started),
    Print = <<(kz_term:to_binary(Whapp))/binary, "(", (kz_time:pretty_print_elapsed_s(Elapsed))/binary, ")">>,
    format("~-25s", [Print]),
    status_list(Whapps, Column + 1);
status_list([{Whapp, #whapp_info{startup=Started,roles=Roles}}|Whapps], _Column) ->
    Elapsed = kz_time:elapsed_s(Started),
    Print = <<(kz_term:to_binary(Whapp))/binary, "(", (kz_time:pretty_print_elapsed_s(Elapsed))/binary, ")">>,
    format("~-25s", [Print]),
    format("~s", [kz_binary:join(Roles, <<" , ">>)]),
    status_list(Whapps, 4).

-spec simple_list(kz_term:ne_binaries()) -> 'ok'.
simple_list(List) -> simple_list(List, 0).

-spec simple_list(kz_term:ne_binaries(), 0..5) -> 'ok'.
simple_list([], _) -> format("~n", []);
simple_list(List, Column) when Column > 4 ->
    format("~n" ++ ?HEADER_COL ++ "  ", [""]),
    simple_list(List, 0);
simple_list([Item|Items], Column) ->
    format("~s ", [Item]),
    simple_list(Items, Column + 1).

-spec flush() -> 'ok'.
flush() ->
    gen_listener:cast(?SERVER, 'flush').

-spec notify_new() -> 'ok'.
notify_new() ->
    notify_new(self()).

-spec notify_new(pid()) -> 'ok'.
notify_new(Pid) ->
    gen_listener:cast(?SERVER, {'notify_new', Pid}).

-spec notify_expire() -> 'ok'.
notify_expire() ->
    notify_expire(self()).

-spec notify_expire(pid()) -> 'ok'.
notify_expire(Pid) ->
    gen_listener:cast(?SERVER, {'notify_expire', Pid}).

-spec handle_advertise(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_advertise(JObj, Props) ->
    'true' = kapi_nodes:advertise_v(JObj),

    Srv = props:get_value('server', Props),
    Node = props:get_value('node', Props),
    case kz_json:get_value(<<"Node">>, JObj, Node) =:= Node of
        'false' -> gen_listener:cast(Srv, {'advertise', JObj});
        'true' -> 'ok'
    end.

-spec build_advertised_node(kz_json:object(), nodes_state()) -> kz_types:kz_node() | 'ok'.
build_advertised_node(JObj, State) ->
    try
        from_json(JObj, State)
    catch
        _E:_R ->
            lager:warning("error building advertised node : ~p", [{_E, _R}])
    end.

-spec update_advertised_node(kz_types:kz_node(), nodes_state()) -> pid() | 'true'.
update_advertised_node(Node, #state{tab=Tab}=State) ->
    case ets:insert_new(Tab, Node) of
        'true' -> kz_process:spawn(fun notify_new/2, [Node, State]);
        'false' -> ets:insert(Tab, Node)
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', nodes_state()}.
init([]) ->
    lager:debug("starting nodes watcher"),
    erlang:put('kazoo_bindings_silent_apply', 'true'),
    kapi_nodes:declare_exchanges(),
    kapi_self:declare_exchanges(),
    Tab = ets:new(?MODULE, ['set'
                           ,'protected'
                           ,'named_table'
                           ,{'keypos', #kz_node.node}
                           ]),
    lager:debug("started ETS ~p", [Tab]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    _ = net_kernel:monitor_nodes('true', ['nodedown_reason'
                                         ,{'node_type', 'all'}
                                         ]),
    lager:debug("monitoring nodes"),
    Version = list_to_binary([kz_util:kazoo_version()
                             ," - "
                             ,kz_term:to_binary(erlang:system_info('otp_release'))
                             ]),
    State = #state{tab = Tab
                  ,zone = local_zone()
                  ,md5 = node_encoded()
                  ,version = Version
                  },

    self() ! {'heartbeat', State#state.heartbeat_ref},
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), nodes_state()) -> kz_types:handle_call_ret_state(nodes_state()).
handle_call({'print_status', Nodes}, _From, State) ->
    print_status(Nodes, State),
    {'reply', 'ok', State};
handle_call('zone', _From, #state{zone=Zone}=State) ->
    {'reply', Zone, State};
handle_call('nodes', _From, State) ->
    Nodes = lists:sort(fun compare_nodes/2, ets:tab2list(?MODULE)),
    {'reply', Nodes, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), nodes_state()) -> kz_types:handle_cast_ret_state(nodes_state()).
handle_cast({'notify_new', Pid}, #state{notify_new=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_new=sets:add_element(Pid, Set)}};
handle_cast({'notify_expire', Pid}, #state{notify_expire=Set}=State) ->
    _ = erlang:monitor('process', Pid),
    {'noreply', State#state{notify_expire=sets:add_element(Pid, Set)}};
handle_cast({'advertise', JObj}, State) ->
    case build_advertised_node(JObj, State) of
        #kz_node{}=Node ->
            _ = update_advertised_node(Node, State),
            {'noreply', maybe_add_zone(Node, State)};
        'ok' -> {'noreply', State}
    end;
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    lager:info("nodes acquired queue name ~s, starting remote heartbeats", [_Q]),
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', IsConsuming}}, State) ->
    lager:debug("heartbeat from remotes is ~p", [IsConsuming]),
    {'noreply', State};
handle_cast('flush', State) ->
    ets:delete_all_objects(?MODULE),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), nodes_state()) -> kz_types:handle_info_ret_state(nodes_state()).
handle_info('expire_nodes', #state{node=ThisNode, tab=Tab}=State) ->
    Now = kz_time:now_ms(),
    FindSpec = [{#kz_node{node='$1'
                         ,expires='$2'
                         ,last_heartbeat='$3'
                         ,_ = '_'
                         }
                ,[{'andalso'
                  ,{'=/=','$2','undefined'}
                  ,{'andalso'
                   ,{'>',{'const',Now},{'+','$2','$3'}}
                   ,{'=/=','$1',{'const',ThisNode}}
                   }
                  }
                 ]
                ,['$_']
                }
               ],
    Nodes = ets:select(Tab, FindSpec),
    _ = [ets:delete(Tab, Node) || #kz_node{node=Node} <- Nodes],
    _ = kz_process:spawn(fun notify_expire/2, [Nodes, State]),
    _ = erlang:send_after(?EXPIRE_PERIOD, self(), 'expire_nodes'),
    {'noreply', State};

handle_info({'heartbeat', Ref}
           ,#state{heartbeat_ref=Ref
                  ,tab=Tab
                  ,me=Me
                  }=State
           ) ->
    Heartbeat = ?HEARTBEAT,
    Reference = erlang:make_ref(),
    _ = erlang:send_after(Heartbeat, self(), {'heartbeat', Reference}),

    try
        Node = #kz_node{broker=Broker} = create_node(Heartbeat, State),
        _ = ets:insert(Tab, Node),
        _ = Broker =/= <<"disconnected">>
            andalso kapi_nodes:publish_advertise(advertise_payload(Node)),
        {'noreply', State#state{heartbeat_ref=Reference, me=Node}}
    catch
        _:{noproc,_}:_ST ->
            {'noreply', State#state{heartbeat_ref=Reference}, 'hibernate'};
        'exit' : {'timeout' , _}:_ST when Me =/= 'undefined' ->
            NewMe = Me#kz_node{expires=Heartbeat},
            _ = ets:insert(Tab, NewMe),
            lager:notice("timeout creating node sending old data"),
            {'noreply', State#state{heartbeat_ref=Reference, me=NewMe}};
        'exit' : {'timeout' , _}:_ST ->
            lager:warning("timeout creating node, no data to send"),
            {'noreply', State#state{heartbeat_ref=Reference}};
        ?STACKTRACE(_E, _N, ST)
        lager:error("error creating node ~p : ~p", [_E, _N]),
        kz_log:log_stacktrace(ST),
        {'noreply', State#state{heartbeat_ref=Reference}, 'hibernate'}
        end;

handle_info({'DOWN', Ref, 'process', Pid, _}
           ,#state{notify_new=NewSet
                  ,notify_expire=ExpireSet
                  }=State
           ) ->
    erlang:demonitor(Ref, ['flush']),
    {'noreply', State#state{notify_new=sets:del_element(Pid, NewSet)
                           ,notify_expire=sets:del_element(Pid, ExpireSet)
                           }};

handle_info({'nodedown', Node, InfoList}, State) ->
    lager:info("VM ~s is no longer connected:", [Node]),
    _ = [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodedown', Node}, State) ->
    lager:info("VM ~s is no longer connected", [Node]),
    {'noreply', State};
handle_info({'nodeup', Node, InfoList}, State) ->
    lager:info("VM ~s is now connected:", [Node]),
    _ = [lager:info(" ~p: ~p", [K, V]) || {K, V} <- InfoList],
    {'noreply', State};
handle_info({'nodeup', Node}, State) ->
    lager:info("VM ~s is now connected", [Node]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), nodes_state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', kz_term:to_binary(Node)}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), nodes_state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), nodes_state(), any()) -> {'ok', nodes_state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_node('undefined' | 5000..15000, nodes_state()) -> kz_types:kz_node().
create_node(Heartbeat, #state{zone=Zone
                             ,version=Version
                             ,md5=MD5
                             }) ->
    add_kapps_data(#kz_node{expires=Heartbeat
                           ,broker=normalize_amqp_uri(kz_amqp_connections:primary_broker())
                           ,used_memory=erlang:memory('total')
                           ,processes=erlang:system_info('process_count')
                           ,ports=length(erlang:ports())
                           ,version=Version
                           ,zone=Zone
                           ,md5=MD5
                           ,globals=kz_globals:stats()
                           ,node_info=node_info()
                           }).

-spec normalize_amqp_uri(kz_term:api_ne_binary()) -> kz_term:ne_binary().
normalize_amqp_uri('undefined') -> <<"disconnected">>;
normalize_amqp_uri(URI) ->
    kz_term:to_binary(amqp_uri:remove_credentials(kz_term:to_list(URI))).

-spec add_kapps_data(kz_types:kz_node()) -> kz_types:kz_node().
add_kapps_data(Node) ->
    lists:foldl(fun kapp_data/2, Node, kapps_controller:list_apps()).

-spec request(request_acc()) -> request_acc().
request(Acc) ->
    App = props:get_value('app', Acc),
    [{'info', get_whapp_info(App)} | Acc].

-spec kapp_data(atom(), kz_types:kz_node()) -> kz_types:kz_node().
kapp_data(App, Node) ->
    kapp_data(App, Node, kz_nodes_bindings:request(App)).

kapp_data(App
         ,#kz_node{kapps=Kapps
                  ,media_servers=Servers
                  ,channels=Channels
                  ,conferences=Conferences
                  ,registrations=Registrations
                  }=Node
         ,RequestAcc
         ) ->
    Node#kz_node{kapps=maybe_add_info(App, props:get_value('info', RequestAcc), Kapps)
                ,media_servers=props:get_value('media_servers', RequestAcc, []) ++ Servers
                ,channels=props:get_integer_value('channels', RequestAcc, 0) + Channels
                ,conferences=props:get_integer_value('conferences', RequestAcc, 0) + Conferences
                ,registrations=props:get_integer_value('registrations', RequestAcc, 0) + Registrations
                }.

-spec maybe_add_info(atom(), 'undefined' | kz_types:whapp_info(), kz_types:kapps_info()) -> kz_types:kapps_info().
maybe_add_info(_App, 'undefined', Kapps) -> Kapps;
maybe_add_info(App, AppInfo, Kapps) ->
    [{kz_term:to_binary(App), AppInfo} | Kapps].

-spec get_whapp_info(atom() | pid() | kz_term:proplist() | 'undefined') -> kz_types:whapp_info().
get_whapp_info('undefined') -> #whapp_info{};
get_whapp_info(Whapp) when is_atom(Whapp) ->
    try
        get_whapp_info(application_controller:get_master(Whapp))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info(Master) when is_pid(Master) ->
    try
        get_whapp_info(application_master:get_child(Master))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info({Pid, _Module}) when is_pid(Pid) ->
    try
        get_whapp_process_info(erlang:process_info(Pid))
    catch
        _E:_R -> #whapp_info{}
    end;
get_whapp_info(_Arg) ->
    lager:debug("can't get info for ~p", [_Arg]),
    #whapp_info{}.

-spec get_whapp_process_info(kz_term:proplist() | 'undefined') -> kz_types:whapp_info().
get_whapp_process_info('undefined') -> #whapp_info{};
get_whapp_process_info([]) -> #whapp_info{};
get_whapp_process_info(PInfo) ->
    Startup = props:get_value('$startup', props:get_value('dictionary', PInfo, [])),
    #whapp_info{startup=Startup}.

-spec advertise_payload(kz_types:kz_node()) -> kz_term:proplist().
advertise_payload(#kz_node{expires=Expires
                          ,kapps=Whapps
                          ,media_servers=MediaServers
                          ,used_memory=UsedMemory
                          ,processes=Processes
                          ,ports=Ports
                          ,version=Version
                          ,channels=Channels
                          ,conferences=Conferences
                          ,registrations=Registrations
                          ,zone=Zone
                          ,globals=Globals
                          ,md5=MD5
                          }) ->
    props:filter_undefined(
      [{<<"md5">>, MD5}
      ,{<<"Expires">>, Expires}
      ,{<<"WhApps">>, kapps_to_json(Whapps) }
      ,{<<"Media-Servers">>, media_servers_to_json(MediaServers)}
      ,{<<"Used-Memory">>, UsedMemory}
      ,{<<"Processes">>, Processes}
      ,{<<"Ports">>, Ports}
      ,{<<"Version">>, Version}
      ,{<<"Channels">>, Channels}
      ,{<<"Conferences">>, Conferences}
      ,{<<"Registrations">>, Registrations}
      ,{<<"Zone">>, kz_term:to_binary(Zone)}
      ,{<<"Globals">>, kz_json:from_list(Globals)}
      ,{<<"Node-Info">>, node_info()}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec media_servers_to_json(kz_types:media_servers()) -> kz_json:object().
media_servers_to_json([]) -> 'undefined';
media_servers_to_json(Servers) ->
    kz_json:from_list(Servers).

-spec media_servers_from_json(kz_json:object()) -> kz_types:media_servers().
media_servers_from_json(Servers) ->
    [{Key, kz_json:get_value(Key, Servers)}
     || Key <- kz_json:get_keys(Servers)
    ].

-spec from_json(kz_json:object(), nodes_state()) -> kz_types:kz_node().
from_json(JObj, State) ->
    Node = kz_json:get_value(<<"Node">>, JObj),
    #kz_node{node=kz_term:to_atom(Node, 'true')
            ,md5=kz_json:get_value(<<"md5">>, JObj)
            ,expires=kz_term:to_integer(kz_json:get_integer_value(<<"Expires">>, JObj, 0) * ?FUDGE_FACTOR)
            ,kapps=kapps_from_json(kz_json:get_value(<<"WhApps">>, JObj, []))
            ,media_servers=media_servers_from_json(kz_json:get_value(<<"Media-Servers">>, JObj, kz_json:new()))
            ,used_memory=kz_json:get_integer_value(<<"Used-Memory">>, JObj, 0)
            ,processes=kz_json:get_integer_value(<<"Processes">>, JObj, 0)
            ,ports=kz_json:get_integer_value(<<"Ports">>, JObj, 0)
            ,version=kz_json:get_first_defined([<<"Version">>, <<"App-Version">>], JObj, <<"unknown">>)
            ,channels=kz_json:get_integer_value(<<"Channels">>, JObj, 0)
            ,conferences=kz_json:get_integer_value(<<"Conferences">>, JObj, 0)
            ,registrations=kz_json:get_integer_value(<<"Registrations">>, JObj, 0)
            ,broker=get_amqp_broker(JObj)
            ,zone=get_zone(JObj, State)
            ,globals=kz_json:to_proplist(kz_json:get_value(<<"Globals">>, JObj, kz_json:new()))
            ,node_info=kz_json:get_json_value(<<"Node-Info">>, JObj)
            ,runtime=kz_json:get_json_value(<<"Runtime-Info">>, JObj)
            ,modules=kz_json:get_json_value(<<"Modules">>, JObj)
            ,roles=kz_json:to_proplist(kz_json:get_json_value(<<"Roles">>, JObj, kz_json:new()))
            }.


-spec kapps_from_json(kz_term:api_terms()) -> kz_types:kapps_info().
kapps_from_json(Whapps) when is_list(Whapps) ->
    [{Whapp, #whapp_info{}} || Whapp <- Whapps];
kapps_from_json(JObj) ->
    Keys = kz_json:get_keys(JObj),
    [whapp_from_json(Key, JObj) || Key <- Keys].

-spec whapp_from_json(binary(), kz_json:object()) -> {binary(), kz_types:whapp_info()}.
whapp_from_json(Key, JObj) ->
    {Key, whapp_info_from_json(Key, kz_json:get_value(Key, JObj))}.

-spec whapp_info_from_json(kz_term:ne_binary(), kz_json:object()) -> kz_types:whapp_info().
whapp_info_from_json(Key, JObj) ->
    whapp_info_from_json(Key, #whapp_info{}, kz_json:get_values(JObj)).

-spec whapp_info_from_json(kz_term:ne_binary(), kz_types:whapp_info(), {kz_json:json_terms(), kz_json:keys()}) -> kz_types:whapp_info().
whapp_info_from_json(_Key, Info, {[], []}) -> Info;
whapp_info_from_json(Key, Info, {[V | V1], [<<"Roles">> | K1]}) ->
    whapp_info_from_json(Key, Info#whapp_info{roles=V}, {V1, K1});
whapp_info_from_json(<<"kamailio">> = Key, Info, {[V | V1], [<<"Startup">> | K1]}) ->
    whapp_info_from_json(Key, Info#whapp_info{startup=kz_time:unix_seconds_to_gregorian_seconds(V)}, {V1, K1});
whapp_info_from_json(<<"freeswitch">> = Key, Info, {[V | V1], [<<"Startup">> | K1]}) ->
    whapp_info_from_json(Key, Info#whapp_info{startup=kz_time:unix_seconds_to_gregorian_seconds(V)}, {V1, K1});
whapp_info_from_json(Key, Info, {[V | V1], [<<"Startup">> | K1]}) ->
    whapp_info_from_json(Key, Info#whapp_info{startup=V}, {V1, K1});
whapp_info_from_json(Key, Info, {[_V | V1], [_K | K1]}) ->
    whapp_info_from_json(Key, Info, {V1, K1}).

-spec kapps_to_json(kz_types:kapps_info()) -> kz_json:object().
kapps_to_json(Whapps) ->
    List = [whapp_to_json(Whapp) || Whapp <- Whapps],
    kz_json:from_list(List).

-spec whapp_to_json({kz_term:ne_binary(), kz_types:whapp_info()}) -> {kz_term:ne_binary(), kz_json:object()}.
whapp_to_json({K, Info}) ->
    {K, whapp_info_to_json(Info)}.

-spec whapp_info_to_json(kz_types:whapp_info()) -> kz_json:object().
whapp_info_to_json(#whapp_info{startup=Start, roles=Roles}) ->
    kz_json:from_list(
      [{<<"Startup">>, Start}
      ,{<<"Roles">>, Roles}
      ]).

-spec get_zone(kz_json:object(), nodes_state()) -> atom().
get_zone(JObj, #state{zones=Zones, zone=LocalZone}) ->
    case kz_json:get_first_defined([<<"Zone">>, <<"AMQP-Broker-Zone">>], JObj) of
        'undefined' ->
            case kz_json:get_value(<<"AMQP-Broker">>, JObj) of
                'undefined' -> LocalZone;
                Broker ->
                    case props:get_value(Broker, Zones) of
                        'undefined' -> LocalZone;
                        Zone -> kz_term:to_atom(Zone, 'true')
                    end
            end;
        Zone -> kz_term:to_atom(Zone, 'true')
    end.

-spec local_zone() -> atom().
local_zone() -> kz_config:zone().

-spec get_amqp_broker(kz_term:api_ne_binary() | kz_json:object()) -> kz_term:api_ne_binary().
get_amqp_broker('undefined') ->
    normalize_amqp_uri(kz_amqp_connections:primary_broker());
get_amqp_broker(Broker) when is_binary(Broker) -> normalize_amqp_uri(Broker);
get_amqp_broker(JObj) ->
    get_amqp_broker(kz_json:get_ne_value(<<"AMQP-Broker">>, JObj)).

-spec notify_expire(kz_types:kz_nodes(), nodes_state() | kz_term:pids()) -> 'ok'.
notify_expire([], _) -> 'ok';
notify_expire(_, []) -> 'ok';
notify_expire(Nodes, #state{notify_expire=Set}) ->
    notify_expire(Nodes, sets:to_list(Set));
notify_expire([#kz_node{node=NodeName}=Node|Nodes], Pids) ->
    lager:warning("node ~s heartbeat has expired", [NodeName]),
    _ = [gen_listener:cast(Pid, {?MODULE, {'expire', Node}})
         || Pid <- Pids
        ],
    notify_expire(Nodes, Pids).

-spec notify_new(kz_types:kz_node(), nodes_state() | kz_term:pids()) -> 'ok'.
notify_new(Node, #state{notify_new=Set}) ->
    notify_new(Node, sets:to_list(Set));
notify_new(#kz_node{node=NodeName}=Node, Pids) ->
    lager:info("received heartbeat from new node ~s", [NodeName]),
    _ = [gen_listener:cast(Pid, {?MODULE, {'new', Node}})
         || Pid <- Pids
        ],
    'ok'.

-spec whapp_oldest_node(kz_term:text()) -> kz_term:api_integer().
whapp_oldest_node(Whapp) ->
    whapp_oldest_node(Whapp, 'false').

-spec whapp_oldest_node(kz_term:text(), kz_term:text() | boolean() | atom()) -> kz_term:api_integer().
whapp_oldest_node(Whapp, Federated)
  when is_binary(Federated) ->
    whapp_oldest_node(Whapp, kz_term:is_true(Federated));
whapp_oldest_node(Whapp, 'true') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,node='$2'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$1','$2'}}]
                 }],
    determine_whapp_oldest_node(kz_term:to_binary(Whapp), MatchSpec);
whapp_oldest_node(Whapp, 'false') ->
    Zone = gen_listener:call(?SERVER, 'zone'),
    whapp_oldest_node(Whapp, Zone);
whapp_oldest_node(Whapp, Zone)
  when is_atom(Zone) ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,node='$2'
                          ,zone = Zone
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,[{{'$1', '$2'}}]
                 }],
    determine_whapp_oldest_node(kz_term:to_binary(Whapp), MatchSpec).

-spec determine_whapp_oldest_node(kz_term:ne_binary(), ets:match_spec()) ->
          'undefined' | node().
determine_whapp_oldest_node(Whapp, MatchSpec) ->
    case oldest_whapp_node(Whapp, MatchSpec) of
        {Node, _Start} -> Node;
        'undefined' -> 'undefined'
    end.

-type oldest_whapp_node() :: 'undefined' |
                             {node(), kz_time:gregorian_seconds()}.

-spec oldest_whapp_node(kz_term:ne_binary(), ets:match_spec()) ->
          oldest_whapp_node().
oldest_whapp_node(Whapp, MatchSpec) ->
    lists:foldl(fun({Whapps, _Node}=Info, Acc) when is_list(Whapps) ->
                        determine_whapp_oldest_node_fold(Info, Acc, Whapp)
                end
               ,'undefined'
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec determine_whapp_oldest_node_fold({kz_types:kapps_info(), node()}, oldest_whapp_node(), kz_term:ne_binary()) ->
          oldest_whapp_node().
determine_whapp_oldest_node_fold({Whapps, Node}, 'undefined', Whapp) ->
    case props:get_value(Whapp, Whapps) of
        'undefined' -> 'undefined';
        #whapp_info{startup=Start} -> {Node, Start}
    end;
determine_whapp_oldest_node_fold({Whapps, Node}, {_, Startup}=Acc, Whapp) ->
    case props:get_value(Whapp, Whapps) of
        'undefined' -> Acc;
        #whapp_info{startup='undefined'} -> Acc;
        #whapp_info{startup=Start}
          when Start =< Startup -> {Node, Start};
        _ -> Acc
    end.

-spec maybe_add_zone(kz_types:kz_node(), nodes_state()) -> nodes_state().
maybe_add_zone(#kz_node{zone='undefined'}, #state{}=State) -> State;
maybe_add_zone(#kz_node{zone=Zone, broker=B}, #state{zones=Zones}=State) ->
    Broker = normalize_amqp_uri(B),
    case props:get_value(Broker, Zones) of
        'undefined' -> State#state{zones=[{Broker, Zone} | Zones]};
        _ -> State
    end.

-spec node_info() -> kz_json:object().
node_info() ->
    kz_json:from_list(pool_states()
                      ++ amqp_status()
                     ).

-spec amqp_status() -> [{kz_term:ne_binary(), kz_json:object()}].
amqp_status() ->
    Connections = kz_amqp_connections:connections(),
    [amqp_status_connection(Connection) || Connection <- Connections].

-spec amqp_status_connection(kz_amqp_connections:kz_amqp_connections()) -> {kz_term:ne_binary(), kz_json:object()}.
amqp_status_connection(Connection) ->
    Count = kz_amqp_assignments:channel_count(Connection),
    Broker = kz_amqp_connection:broker(Connection),
    {Broker, kz_json:from_list([{<<"channel_count">>, Count}])}.

-spec pool_state_binding() -> kz_term:ne_binary().
pool_state_binding() -> <<?MODULE_STRING, ".amqp.pools">>.

-spec bind_for_pool_state(atom()) -> kazoo_bindings:bind_result().
bind_for_pool_state(Module) ->
    bind_for_pool_state(Module, self()).

-spec bind_for_pool_state(atom(), pid()) -> kazoo_bindings:bind_result().
bind_for_pool_state(Module, Pid) ->
    _ = kazoo_bindings:bind(pool_state_binding(), Module, 'pools', Pid).

-spec unbind_for_pool_state(atom()) -> kazoo_bindings:unbind_result().
unbind_for_pool_state(Module) ->
    unbind_for_pool_state(Module, self()).

-spec unbind_for_pool_state(atom(), kz_term:api_pid()) -> kazoo_bindings:unbind_result().
unbind_for_pool_state(_Module, 'undefined') -> 'ok';
unbind_for_pool_state(Module, Pid) when is_pid(Pid) ->
    kazoo_bindings:unbind(pool_state_binding(), Module, 'pools', Pid).

-spec pool_states() -> kz_term:proplist().
pool_states() ->
    AppPools = kazoo_bindings:map(pool_state_binding(), []),
    lists:keysort(1, [pool_state(Pool)
                      || Pools <- AppPools,
                         {Pool, _Pid} <- Pools
                     ]).

-spec pool_state(atom()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
pool_state(Name) ->
    {PoolState, Workers, OverFlow, Monitors} = poolboy:status(Name),
    pool_state(Name, PoolState, Workers, OverFlow, Monitors).

-spec pool_state(atom(), atom(), integer(), integer(), integer()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
pool_state(Name, State, Workers, Overflow, Monitors) ->
    {kz_term:to_binary(Name)
    ,iolist_to_binary(
       io_lib:format("~p/~p/~p (~p)", [Workers, Monitors, Overflow, State])
      )
    }.

-spec node_encoded() -> kz_term:ne_binary().
node_encoded() ->
    case application:get_env(?APP_NAME_ATOM, 'node_encoded') of
        'undefined' ->
            Encoded = kz_base64url:encode(crypto:hash('md5', kz_term:to_binary(node()))),
            application:set_env(?APP_NAME_ATOM, 'node_encoded', Encoded),
            Encoded;
        {'ok', Encoded} -> Encoded
    end.

-spec whapp_role_count(kz_term:text(), kz_term:text()) -> integer().
whapp_role_count(Whapp, Role) ->
    whapp_role_count(Whapp, Role, 'false').

-spec whapp_role_count(kz_term:text(), kz_term:text(), kz_term:text() | boolean() | 'remote') -> integer().
whapp_role_count(Whapp, Role, Arg) when not is_atom(Arg) ->
    whapp_role_count(Whapp, Role, kz_term:to_atom(Arg, 'true'));
whapp_role_count(Whapp, Role, 'false') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone = local_zone()
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_role_count(kz_term:to_binary(Whapp), kz_term:to_binary(Role), MatchSpec);
whapp_role_count(Whapp, Role, 'true') ->
    MatchSpec = [{#kz_node{kapps='$1'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$1']
                 }],
    determine_whapp_role_count(kz_term:to_binary(Whapp), kz_term:to_binary(Role), MatchSpec);
whapp_role_count(Whapp, Role, 'remote') ->
    Zone = local_zone(),
    MatchSpec = [{#kz_node{kapps='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', Zone}}
                   }]
                 ,['$1']
                 }],
    determine_whapp_role_count(kz_term:to_binary(Whapp), kz_term:to_binary(Role), MatchSpec);
whapp_role_count(Whapp, Role, Unhandled) ->
    lager:debug("invalid parameters ~p , ~p , ~p", [Whapp, Role, Unhandled]),
    0.

-spec determine_whapp_role_count(kz_term:ne_binary(), kz_term:ne_binary(), ets:match_spec()) -> non_neg_integer().
determine_whapp_role_count(Whapp, Role, MatchSpec) ->
    lists:foldl(fun(Whapps, Acc) when is_list(Whapps) ->
                        determine_whapp_role_count_fold(Whapps, Role, Acc, Whapp)
                end
               ,0
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec determine_whapp_role_count_fold(kz_types:kapps_info(), kz_term:ne_binary(), non_neg_integer(), kz_term:ne_binary()) -> non_neg_integer().
determine_whapp_role_count_fold(Whapps, Role, Acc, Whapp) ->
    case props:is_defined(Whapp, Whapps)
        andalso lists:member(Role, (props:get_value(Whapp, Whapps))#whapp_info.roles)
    of
        'true' -> Acc + 1;
        'false' -> Acc
    end.

-spec node_role_count(kz_term:text()) -> integer().
node_role_count(Role) ->
    length(with_role(Role)).

-spec node_role_count(kz_term:text(), kz_term:text() | boolean() | 'remote') -> integer().
node_role_count(Role, Arg) ->
    length(with_role(Role, Arg)).

-spec with_role(kz_term:text()) -> kz_types:kz_nodes().
with_role(Role) ->
    with_role(Role, 'false').

-spec with_role(kz_term:text(), kz_term:text() | boolean() | 'remote') -> kz_types:kz_nodes().
with_role(Role, Arg) when not is_atom(Arg) ->
    with_role(Role, kz_term:to_atom(Arg, 'true'));
with_role(Role, 'false') ->
    MatchSpec = [{#kz_node{roles='$1'
                          ,zone = local_zone()
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$_']
                 }],
    with_role_filter(kz_term:to_binary(Role), MatchSpec);
with_role(Role, 'true') ->
    MatchSpec = [{#kz_node{roles='$1'
                          ,_ = '_'
                          }
                 ,[{'=/=', '$1', []}]
                 ,['$_']
                 }],
    with_role_filter(kz_term:to_binary(Role), MatchSpec);
with_role(Role, 'remote') ->
    Zone = local_zone(),
    MatchSpec = [{#kz_node{roles='$1'
                          ,zone='$2'
                          ,_ = '_'
                          }
                 ,[{'andalso'
                   ,{'=/=', '$1', []}
                   ,{'=/=', '$2', {'const', Zone}}
                   }]
                 ,['$_']
                 }],
    with_role_filter(kz_term:to_binary(Role), MatchSpec);
with_role(Role, Unhandled) ->
    lager:debug("invalid parameters ~p , ~p , ~p", [Role, Unhandled]),
    [].

-spec with_role_filter(kz_term:ne_binary(), ets:match_spec()) -> kz_types:kz_nodes().
with_role_filter(Role, MatchSpec) ->
    lists:foldl(fun(#kz_node{roles=Roles}=Node, Acc) when is_list(Roles) ->
                        case props:is_defined(Role, Roles) of
                            'true' -> [Node | Acc];
                            'false' -> Acc
                        end
                end
               ,[]
               ,ets:select(?MODULE, MatchSpec)
               ).

-spec nodes() -> kz_types:kz_nodes().
nodes() ->
    gen_listener:call(?MODULE, 'nodes').

format_output() ->
    case erlang:get('io_output') of
        'undefined' -> group_leader();
        Pid -> Pid
    end.

format(A) ->
    io:format(format_output(), A, []).

format(A, B) ->
    io:format(format_output(), A, B).
