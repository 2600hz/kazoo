%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kztm_nodes_extractor).

-behavior(gen_extractor).

-export([extract/0]).

-include("../../kazoo_stdlib/include/kz_records.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec extract() -> kz_json:object().
extract() ->
    Nodes = normalize_kz_nodes(),
    kz_json:from_list([{<<"nodes">>, Nodes}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_kz_nodes() -> kz_json:objects().
normalize_kz_nodes() ->
    RawNodes = kz_nodes:nodes(),
    normalize_kz_nodes(RawNodes).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_kz_nodes(kz_types:kz_nodes()) -> kz_json:objects().
normalize_kz_nodes(Nodes) ->
    lists:foldl(fun extract_node_metadata/2, [], Nodes).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec extract_node_metadata(kz_types:kz_node(), kz_json:objects()) -> kz_json:objects().
extract_node_metadata(#kz_node{node=NodeName, version=Version, zone=Zone}=Node, Acc) ->
    Routines = [{fun maybe_extract_apps_metadata/2, Node}
               ,{fun maybe_extract_freeswitch_metadata/2, Node}
               ,{fun maybe_extract_kamailio_metadata/2, Node}
               ],
    NodeMeta = lists:foldl(fun({F, N}, A) -> F(N, A) end, kz_json:new(), Routines),
    JObj = kz_json:from_list([{<<"node">>, NodeName}
                             ,{<<"zone">>, Zone}
                             ,{<<"type">>, node_type(Node)}
                             ,{<<"version">>, Version}
                             ,{<<"metadata">>, NodeMeta}
                             ]),
    [JObj | Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node_type(kz_types:kz_node()) -> kz_term:ne_binary().
node_type(#kz_node{kapps=[{<<"kamailio">>=NodeType, _}]}) -> NodeType;
node_type(#kz_node{kapps=Kapps}) ->
    case proplists:is_defined(<<"ecallmgr">>, Kapps) of
        'true' -> <<"ecallmgr">>;
        _ -> <<"kapps">>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_extract_apps_metadata(kz_types:kz_node(), kz_json:objects()) -> kz_json:objects().
maybe_extract_apps_metadata(#kz_node{kapps=Apps}, Acc) ->
    AppsMeta = lists:foldl(fun app_metadata/2, [], Apps),
    kz_json:set_value(<<"apps">>, AppsMeta, Acc).

%%------------------------------------------------------------------------------
%% @doc extract_node_metadata
%% @end
%%------------------------------------------------------------------------------
-spec app_metadata({kz_term:ne_binary(), #whapp_info{}}, kz_json:objects()) -> kz_json:objects().
app_metadata({AppName, {'whapp_info', StartTs, _}}, Acc) ->
    JObj = kz_json:from_list([{<<"name">>, AppName}
                             ,{<<"startup">>, StartTs}
                             ]),
    [JObj | Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_extract_freeswitch_metadata(kz_types:node(), kz_json:objects()) -> kz_json:objects().
maybe_extract_freeswitch_metadata(#kz_node{media_servers=[]}, Acc) -> Acc;
maybe_extract_freeswitch_metadata(#kz_node{media_servers=Servers}, Acc) ->
    MediaServers = media_servers(Servers),
    kz_json:set_value(<<"media_servers">>, MediaServers, Acc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec media_servers(kz_types:media_servers()) -> kz_json:objects().
media_servers(Servers) ->
    lists:foldl(fun media_servers_foldl/2, [], Servers).

%%------------------------------------------------------------------------------
%% @doc extract runtime stats and version info from media server entry
%% @end
%%------------------------------------------------------------------------------
-spec media_servers_foldl({kz_term:ne_binary(), kz_json:object()}, kz_json:objects()) -> kz_json:objects().
media_servers_foldl({Server, Meta}, Acc) ->
    Stats = kz_json:from_list(props:filter_empty([{<<"sessions">>, kz_json:get_value(<<"Sessions">>, Meta)}
                                                 ,{<<"startup">>, kz_json:get_value(<<"Startup">>, Meta)}
                                                 ,{<<"version">>, binary:replace(kz_json:get_ne_binary_value(<<"Version">>, Meta, <<"">>), <<" ">>, <<"-">>, ['global'])}
                                                 ])),
    kz_json:set_value(Server, Stats, Acc).

%%------------------------------------------------------------------------------
%% @doc extract current registration count from kamailio server
%% @end
%%------------------------------------------------------------------------------
-spec maybe_extract_kamailio_metadata(kz_types:kz_node(), kz_json:objects()) -> kz_json:objects().
maybe_extract_kamailio_metadata(#kz_node{roles=[]}, Acc) -> Acc;
maybe_extract_kamailio_metadata(#kz_node{roles=_Roles, registrations=Registrations}, Acc) ->
    kz_json:set_value(<<"registrations">>, Registrations, Acc).
