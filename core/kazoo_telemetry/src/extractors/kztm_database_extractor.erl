%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kztm_database_extractor).

-behaviour(gen_extractor).

-export([extract/0]).

-define(MEMBERSHIP_DOC, <<"_membership">>).
-define(MEMBERSHIP_DB, <<"">>).

%%------------------------------------------------------------------------------
%% @doc gen_extractor callback
%% @end
%%------------------------------------------------------------------------------
-spec extract() -> kz_json:objects().
extract() ->
    resolve_database_topology(kzs_plan:plan()).

%%------------------------------------------------------------------------------
%% @doc dataplan and normalize servers for summary generation
%% @end
%%------------------------------------------------------------------------------
-spec resolve_database_topology(map()) -> kz_json:objects().
resolve_database_topology(Plan) ->
    Clusters = maybe_extract_servers(Plan),
    maybe_resolve_clusters(Clusters, []).

%%------------------------------------------------------------------------------
%% @doc extract server maps into a list
%% @end
%%------------------------------------------------------------------------------
-spec maybe_extract_servers(map()) -> [map()].
maybe_extract_servers(#{'server' := _Server, 'others' := Others}=Plan) ->
    Others0 = [S || {_Tag, S} <- Others],
    [Plan | Others0].

%%------------------------------------------------------------------------------
%% @doc maybe generate a summary for each server in the plan
%% @end
%%------------------------------------------------------------------------------
-spec maybe_resolve_clusters([map()], kz_json:objects()) -> kz_json:objects().
maybe_resolve_clusters(Servers, Acc) ->
    lists:foldl(fun generate_cluster_summary/2, Acc, Servers).

%%------------------------------------------------------------------------------
%% @doc generates a summary for a given server map
%% @end
%%------------------------------------------------------------------------------
-spec generate_cluster_summary(map(), kz_json:objects()) -> kz_json:objects().
generate_cluster_summary(#{'server' := {_App, Conn}}=Server, Acc) ->
    Summary = resolve_nodes(Server),
    JObj = kz_json:from_list([{server_tag(Conn), Summary}]),
    [JObj | Acc].

%%------------------------------------------------------------------------------
%% @doc helper to fetch tag from connection data
%% @end
%%------------------------------------------------------------------------------
-spec server_tag({'server', kz_term:ne_binary(), kz_term:proplist()}) -> kz_term:ne_binary().
server_tag({'server', _, Props}) ->
    proplists:get_value('tag', Props).

%%------------------------------------------------------------------------------
%% @doc extract all_node and cluster_nodes values from cluster metadata
%% @end
%%------------------------------------------------------------------------------
-spec resolve_nodes(map()) -> kz_json:object().
resolve_nodes(#{'server' := {'kazoo_couch'=App, Conn}}=Server) ->
    {'ok', Nodes} = App:open_doc(Conn, ?MEMBERSHIP_DB, ?MEMBERSHIP_DOC, []),
    kz_json:from_list([{<<"all_nodes">>, kz_json:get_value(<<"all_nodes">>, Nodes)}
                      ,{<<"cluster_nodes">>, kz_json:get_value(<<"cluster_nodes">>, Nodes)}
                      ,{<<"version">>, resolve_version(Server)}
                      ]).

%%------------------------------------------------------------------------------
%% @doc extract version from cluster_metadata
%% @end
%%------------------------------------------------------------------------------
-spec resolve_version(map()) -> kz_term:ne_binary().
resolve_version(#{'server' := {'kazoo_couch'=App, Conn}}) ->
    {'ok', ServerInfo} = App:server_info(Conn),
    kz_json:get_value(<<"version">>, ServerInfo).
