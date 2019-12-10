%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(waveguide_reducer).

-behaviour(gen_reducer).

-export([reduce/0
        ,ping/0, ping/1
        ]).

-include("waveguide.hrl").

-define(RT_STATS_KEY, <<"runtime_stats.">>).
-define(TOPO_APPS_KEY, <<"topology.apps.">>).
-define(TOPO_DB_KEY, <<"topology.database.">>).
-define(TOPO_NODES_KEY, <<"topology.nodes.">>).
-define(SERVICES_KEY, <<"services.">>).

%%------------------------------------------------------------------------------
%% @doc reduce the extracted data into waveguide request data object
%% @end
%%------------------------------------------------------------------------------
-spec reduce() -> kz_json:objects().
reduce() ->
    Extractors = [{'kztm_nodes_extractor', <<"topology">>}
                 ,{'kztm_services_extractor', <<"services">>}
                 ,{'kztm_database_extractor', <<"db_clusters">>}
                 ],
    JObj = lists:foldl(fun extract_foldl/2, kz_json:new(), Extractors),
    reduce(JObj).

-spec extract_foldl({atom(), kz_term:ne_binary()}, kz_json:object()) -> kz_json:object().
extract_foldl({E, K}, A) ->
    kz_json:set_value(K, (E):extract(), A).

%%------------------------------------------------------------------------------
%% @doc reduce the extracted data into waveguide request data object
%% @end
%%------------------------------------------------------------------------------
-spec reduce(kz_json:object()) -> kz_json:objects().
reduce(JObj) ->
    Reducers = [{fun reduce_database_jobj/2, JObj}
               ,{fun reduce_services_jobj/2, JObj}
               ,{fun reduce_runtime_stats/2, JObj}
               ,{fun resolve_node_topology/2, JObj}
               ,{fun normalize_summaries/2, JObj}
               ],
    lists:foldl(fun reduce_fold_fun/2, [], Reducers).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reduce_fold_fun({fun(), kz_json:object()}, kz_json:objects()) -> kz_json:objects().
reduce_fold_fun({F, Obj}, Acc) -> F(Obj, Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ping() -> {'ok', kz_json:object()} | {'error', 'invalid_ping_type'}.
ping() -> ping(?WG_MAIN_PING).

%%------------------------------------------------------------------------------
%% @doc generate waveguide json payload for given ping type
%% @end
%%------------------------------------------------------------------------------
-spec ping(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', 'invalid_ping_type'}.
ping(?WG_ACTIVATION_PING=Ping) ->
    {'ok', info_ping(Ping)};
ping(?WG_HANDSHAKE_PING=Ping) ->
    {'ok', info_ping(Ping)};
ping(?WG_MAIN_PING=Ping) ->
    {'ok', data_ping(Ping)};
ping(_Other) ->
    {'error', 'invalid_ping_type'}.

%%------------------------------------------------------------------------------
%% @doc simple ping envelope for status or info purposes
%% @end
%%------------------------------------------------------------------------------
-spec info_ping(kz_term:ne_binary()) -> kz_json:object().
info_ping(Ping) ->
    kz_json:from_list([{<<"cluster_id">>, wg_util:cluster_id()}
                      ,{<<"timestamp">>, kz_time:current_tstamp()}
                      ,{<<"type">>, Ping}
                      ]).

%%------------------------------------------------------------------------------
%% @doc construct a ping envelope from the reducer object
%% @end
%%------------------------------------------------------------------------------
-spec data_ping(kz_term:ne_binary()) -> kz_json:object().
data_ping(Ping) ->
    Obj = reduce(),
    kz_json:from_list([{<<"cluster_id">>, wg_util:cluster_id()}
                      ,{<<"data">>, Obj}
                      ,{<<"timestamp">>, kz_time:current_tstamp()}
                      ,{<<"type">>, Ping}
                      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reduce_runtime_stats(kz_json:object(), kz_json:object()) -> kz_term:ne_binary().
reduce_runtime_stats(Obj, Acc) ->
    Nodes = kz_json:get_value([<<"topology">>, <<"nodes">>], Obj),
    Routines = [{fun node_summary/2, Nodes}
               ,{fun freeswitch_summary/2, Nodes}
               ,{fun calculate_aggregates/2, Nodes}
               ],
    lists:foldl(fun reduce_fold_fun/2, Acc, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_summary(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
node_summary(Nodes, Acc) ->
    lists:foldl(fun node_apps_foldl/2, Acc, Nodes).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_apps_foldl(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_apps_foldl(Node, Acc) ->
    Routines = [{fun node_type_rollup/2, Node}
               ,{fun node_version_rollup/2, Node}
               ,{fun node_apps_rollup/2, Node}
               ,{fun node_kamailio_stats/2, Node}
               ],
    lists:foldl(fun reduce_fold_fun/2, Acc, Routines).

%%------------------------------------------------------------------------------
%% @doc aggregate node types (kapps, kamailio, ecallmgr)
%% @end
%%------------------------------------------------------------------------------
-spec node_type_rollup(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_type_rollup(Node, Acc) ->
    Type = kz_json:get_ne_binary_value(<<"type">>, Node),
    Key = <<(?TOPO_NODES_KEY)/binary,(format_wg_id(Type))/binary,".count">>,
    Count = kz_json:get_integer_value(Key, Acc, 0),
    kz_json:set_value(Key, Count+1, Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_version_rollup(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_version_rollup(Node, Acc) ->
    Type = kz_json:get_ne_binary_value(<<"type">>, Node),
    Version = kz_json:get_ne_binary_value(<<"version">>, Node),
    Key = <<(?TOPO_NODES_KEY)/binary,(format_wg_id([Type, <<"version">>]))/binary,".",(format_wg_id(Version))/binary>>,
    Cnt = kz_json:get_value(Key, Acc, 0),
    kz_json:set_value(Key, Cnt + 1, Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_apps_rollup(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_apps_rollup(Node, Acc) ->
    node_apps_rollup(Node, Acc, ?INCLUDE_APPS).

-spec node_apps_rollup(kz_json:object(), kz_json:objects(), boolean()) -> kz_json:objects().
node_apps_rollup(_Node, Acc, 'false') -> Acc;
node_apps_rollup(Node, Acc, 'true') ->
    Apps = kz_json:get_value([<<"metadata">>, <<"apps">>], Node),
    lists:foldl(fun node_apps_rollup_foldl/2, Acc, Apps).

%%------------------------------------------------------------------------------
%% @doc aggregate app specific stats
%% @end
%%------------------------------------------------------------------------------
-spec node_apps_rollup_foldl(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_apps_rollup_foldl(App, Acc) ->
    Routines = [{fun node_apps_rollup_name/2, App}
               ,{fun node_apps_rollup_runtime/2, App}
               ],
    lists:foldl(fun reduce_fold_fun/2, Acc, Routines).

%%------------------------------------------------------------------------------
%% @doc increment app name count
%% @end
%%------------------------------------------------------------------------------
-spec node_apps_rollup_name(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_apps_rollup_name(App, Acc) ->
    Name = kz_json:get_value(<<"name">>, App),
    Key = <<(?TOPO_APPS_KEY)/binary,Name/binary,".count">>,
    Count = kz_json:get_integer_value(Key, Acc, 0),
    kz_json:set_value(Key, Count+1, Acc).

%%------------------------------------------------------------------------------
%% @doc append apps uptime to list
%% @end
%%------------------------------------------------------------------------------
-spec node_apps_rollup_runtime(kz_json:object(), kz_json:objects()) -> kz_json:objects().
node_apps_rollup_runtime(App, Acc) ->
    Name = kz_json:get_value(<<"name">>, App),
    Startup = kz_json:get_value(<<"startup">>, App),
    Elapsed = kz_time:elapsed_s(Startup),
    Key = <<(?RT_STATS_KEY)/binary,"apps.",(format_wg_id([Name]))/binary,".uptime">>,
    Val = kz_json:get_value(Key, Acc, []),
    kz_json:set_value(Key, [Elapsed | Val], Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_kamailio_stats(kz_json:object(), kz_json:object()) -> kz_json:objects().
node_kamailio_stats(Node, Acc) ->
    node_kamailio_stats(Node, Acc, ?INCLUDE_KAMAILIO).

-spec node_kamailio_stats(kz_json:object(), kz_json:object(), boolean() ) -> kz_json:objects().
node_kamailio_stats(_Node, Acc, 'false') -> Acc;
node_kamailio_stats(Node, Acc, 'true') ->
    case kz_json:get_ne_binary_value(<<"type">>, Node) of
        <<"kamailio">> ->
            Registrations = kz_json:get_integer_value([<<"metadata">>, <<"registrations">>], Node),
            Key = <<(?RT_STATS_KEY)/binary,"kamailio.registrations.count">>,
            Count = kz_json:get_integer_value(Key, Acc, 0),
            kz_json:set_value(Key, Count + Registrations, Acc);
        _ -> Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec freeswitch_summary(kz_json:objects(), kz_json:object()) -> kz_json:objects().
freeswitch_summary(Nodes, Acc) ->
    freeswitch_summary(Nodes, Acc, ?INCLUDE_FREESWITCH).

-spec freeswitch_summary(kz_json:objects(), kz_json:object(), boolean()) -> kz_json:objects().
freeswitch_summary(_Nodes, Acc, 'false') -> Acc;
freeswitch_summary(Nodes, Acc, 'true') ->
    MediaServers = dedupe_media_servers(Nodes),
    kz_json:foldl(fun node_freeswitch_summary_foldl/3, Acc, MediaServers).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_aggregates(kz_json:objects(), kz_json:object()) -> kz_json:object().
calculate_aggregates(_Nodes, Acc) ->
    Stats = kz_json:foldl(fun calculate_aggregate/3, kz_json:new(), Acc),
    kz_json:merge(Stats, Acc).

-spec calculate_aggregate(kz_term:binary(), kz_term:binaries() | any(), kz_json:object()) -> kz_json:object().
calculate_aggregate(K, V, Acc)
  when is_list(V)
       andalso length(V) > 0 ->
    [A, B, C, _D] = binary:split(K, <<".">>, ['global']),
    Key0 = <<A/binary,".",B/binary,".",(format_wg_id([C]))/binary>>,
    Stats = kz_json:from_list([{<<Key0/binary,".Min">>, lists:min(V)}
                              ,{<<Key0/binary,".Max">>, lists:max(V)}
                              ,{<<Key0/binary,".Avg">>, lists:sum(V) / length(V)}
                              ]),
    kz_json:merge(Acc, Stats);
calculate_aggregate(_K, _V, Acc) -> Acc.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dedupe_media_servers(kz_json:objects()) -> kz_json:object().
dedupe_media_servers(Nodes) ->
    dedupe_media_servers(Nodes, kz_json:new()).

-spec dedupe_media_servers(kz_json:objects(), kz_json:object()) -> kz_json:object().
dedupe_media_servers([], Acc) -> Acc;
dedupe_media_servers([Node | Nodes], Acc) ->
    case kz_json:get_ne_binary_value(<<"type">>, Node) of
        <<"ecallmgr">> ->
            MediaServers = kz_json:get_value([<<"metadata">>, <<"media_servers">>], Node, kz_json:new()),
            dedupe_media_servers(Nodes, kz_json:merge(MediaServers, Acc));
        _ -> dedupe_media_servers(Nodes, Acc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_freeswitch_summary_foldl(kz_term:api_binaries(), kz_term:non_neg_integer() | kz_term:binary(), kz_json:object()) -> kz_json:object().
node_freeswitch_summary_foldl(_K, V, Acc) ->
    Values = kz_json:flatten(V),
    kz_json:foldl(fun rollup_freeswitch_jobj/3, Acc, Values).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollup_freeswitch_jobj(kz_term:ne_binaries(), non_neg_integer() | kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
rollup_freeswitch_jobj(_K, V, Acc) when is_binary(V) ->
    Version = binary:replace(V, <<".">>, <<"_">>, ['global']),
    Key = <<(?TOPO_NODES_KEY)/binary,(format_wg_id([<<"freeswitch">>, <<"version">>]))/binary,".",Version/binary>>,
    Cnt = kz_json:get_value(Key, Acc, 0),
    kz_json:set_value(Key, Cnt + 1, Acc);
rollup_freeswitch_jobj([<<"startup">>]=_ObjK, V, Acc)
  when is_integer(V) ->
    Key = <<(?RT_STATS_KEY)/binary,"freeswitch.uptime.count">>,
    Uptimes = kz_json:get_value(Key, Acc, []),
    kz_json:set_value(Key, [kz_time:elapsed_s(V) | Uptimes], Acc);
rollup_freeswitch_jobj(K, V, Acc) when is_integer(V) ->
    Key = <<(?RT_STATS_KEY)/binary,"freeswitch.",(format_wg_id(K))/binary,".count">>,
    Stat = kz_json:get_value(Key, Acc, []),
    kz_json:set_value(Key, [V | Stat], Acc).

%%------------------------------------------------------------------------------
%% @doc aggregate and reduce database topology objects to scalar values
%% @end
%%------------------------------------------------------------------------------
-spec reduce_database_jobj(kz_json:object(), kz_json:objects()) -> kz_json:object().
reduce_database_jobj(Obj, Acc) ->
    reduce_database_jobj(Obj, Acc, ?INCLUDE_DATABASE).

-spec reduce_database_jobj(kz_json:object(), kz_json:objects(), boolean()) -> kz_json:object().
reduce_database_jobj(_Obj, Acc, 'false') -> Acc;
reduce_database_jobj(Obj, Acc, 'true') ->
    Clusters = kz_json:get_value(<<"db_clusters">>, Obj),
    lists:foldl(fun rollup_db_cluster_jobjs/2, Acc, kz_json:flatten(Clusters)).

%%------------------------------------------------------------------------------
%% @doc normalize each cluster's stats to flat waveguide K/V pairs
%% @end
%%------------------------------------------------------------------------------
-spec rollup_db_cluster_jobjs(kz_json:objects(), kz_json:object()) -> kz_json:object().
rollup_db_cluster_jobjs([], Acc) -> Acc;
rollup_db_cluster_jobjs(Cluster, Acc) ->
    kz_json:foldl(fun normalize_db_cluster_stats/3, Acc, Cluster).

%%------------------------------------------------------------------------------
%% @doc normalize stats to flat waveguide scalar K/V pairs
%% @end
%%------------------------------------------------------------------------------
-spec normalize_db_cluster_stats(kz_term:binary(), kz_term:binary() | kz_term:binaries() | non_neg_integer(), kz_json:object()) -> kz_json:object().
normalize_db_cluster_stats(K, V, Acc) when is_list(V) ->
    normalize_db_cluster_stats(K, length(V), Acc);
normalize_db_cluster_stats([Tag, Stat], V, Acc) when is_binary(V) ->
    kz_json:set_value(<<(?TOPO_DB_KEY)/binary,(format_wg_id([?ANONYMIZE(Tag), Stat]))/binary,".",(format_wg_id(V))/binary>>, 1, Acc);
normalize_db_cluster_stats([Tag, Stat], V, Acc) ->
    kz_json:set_value(<<(?TOPO_DB_KEY)/binary,(format_wg_id([?ANONYMIZE(Tag)]))/binary,".",Stat/binary>>, V, Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reduce_services_jobj(kz_term:proplist(), kz_json:object()) -> kz_json:objects().
reduce_services_jobj(Obj, Acc) ->
    reduce_services_jobj(Obj, Acc, ?INCLUDE_SERVICES).

-spec reduce_services_jobj(kz_term:proplist(), kz_json:object(), boolean()) -> kz_json:objects().
reduce_services_jobj(_Obj, Acc, 'false') -> Acc;
reduce_services_jobj(Obj, Acc, _IncludeServices) ->
    ServiceItems = kz_json:get_json_value([<<"services">>, <<"service_items">>], Obj),
    normalize_service_items(kz_json:flatten(ServiceItems), Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_service_items(kz_json:object(), kz_json:object()) -> kz_json:object().
normalize_service_items(Obj, Acc) ->
    kz_json:foldl(fun normalize_services_items_foldl/3, Acc, Obj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_services_items_foldl(kz_term:binary(), kz_term:ne_binary() | kz_term:binaries() | non_neg_integer(), kz_json:object()) -> kz_json:object().
normalize_services_items_foldl(K, V, Acc) ->
    [Category | Rest] = K,
    kz_json:set_value(<<(?SERVICES_KEY)/binary,Category/binary,".",(format_wg_id(Rest))/binary,".count">>, V, Acc).

%%------------------------------------------------------------------------------
%% @doc extract and count unique fqdns to assert physical node counts
%% @end
%%------------------------------------------------------------------------------
-spec resolve_node_topology(kz_json:object(), kz_json:object()) -> kz_json:object().
resolve_node_topology(Obj, Acc) ->
    Nodes = kz_json:get_value([<<"topology">>, <<"nodes">>], Obj),
    Fqdns = parse_node_fqdns(Nodes, []),
    kz_json:set_value(<<(?TOPO_NODES_KEY)/binary,"physical.count">>, length(Fqdns), Acc).

-spec parse_node_fqdns(kz_json:objects(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
parse_node_fqdns([], Acc) -> lists:usort(Acc);
parse_node_fqdns([Node | Nodes], Acc) ->
    Name = kz_json:get_atom_value(<<"node">>, Node),
    [_Host, Fqdn] = binary:split(kz_term:to_binary(Name), <<"@">>),
    parse_node_fqdns(Nodes, [Fqdn | Acc]).

%%------------------------------------------------------------------------------
%% @doc normalize flattened K/V pairs to waveguide schema
%% @end
%%------------------------------------------------------------------------------
-spec normalize_summaries(kz_json:objects(), kz_json:object()) -> kz_json:objects().
normalize_summaries(_Nodes, Acc) ->
    kz_json:foldl(fun normalize_summaries_foldl/3, [], Acc).

-spec normalize_summaries_foldl(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:binaries() | non_neg_integer(), kz_json:objects()) -> kz_json:objects().
normalize_summaries_foldl(Key, Value, Acc) ->
    JObj = kz_json:from_list([{<<"id">>, Key}
                             ,{<<"value">>, Value}
                             ]),
    [JObj | Acc].

%%------------------------------------------------------------------------------
%% @doc format a period seperated key from a flattened list
%% @end
%%------------------------------------------------------------------------------
-spec format_wg_id(kz_term:binary() | kz_term:binaries()) -> kz_term:binary().
format_wg_id(A) when is_binary(A) ->
    Temp = binary:split(A, <<".">>, ['global']),
    kz_binary:join(Temp, <<"_">>);
format_wg_id([A]) -> A;
format_wg_id([A, B]) -> <<A/binary,"_",B/binary>>;
format_wg_id([A, B, C]) -> <<A/binary,"_",B/binary,"_",C/binary>>.
