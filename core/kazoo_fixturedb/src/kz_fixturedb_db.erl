%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_db).

%% DB operations
-export([db_create/3
        ,db_delete/2
        ,db_view_cleanup/2
        ,db_info/1, db_info/2
        ,db_exists/2
        ,db_archive/3
        ,db_list/2
        ]).

-include("kz_fixturedb.hrl").

%%%=============================================================================
%%% DB operations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_create(server_map(), kz_term:ne_binary(), kz_data:options()) -> boolean().
db_create(_Server, _DbName, _Options) ->
    'true'.

-spec db_delete(server_map(), kz_term:ne_binary()) -> boolean().
db_delete(_Server, _DbName) ->
    'true'.

-spec db_view_cleanup(server_map(), kz_term:ne_binary()) -> boolean().
db_view_cleanup(_Server, _DbName) ->
    'true'.

-spec db_info(server_map()) -> {'ok', kz_term:ne_binaries()}.
db_info(#{url := ServerUrl}=Server) ->
    #{url := AppUrl} = kz_fixturedb_server:get_app_connection(Server),
    {ok, get_dbs_list(ServerUrl, AppUrl)}.


-spec db_info(server_map(), kz_term:ne_binary()) -> docs_resp().
db_info(Server, DbName) ->
    case db_exists(Server, DbName) of
        'false' -> {'error', 'not_found'};
        'true' -> {'ok', kz_json:from_list([{<<"db_name">>, DbName}])}
    end.

-spec db_exists(server_map(), kz_term:ne_binary()) -> boolean().
db_exists(Server, DbName) ->
    #{url := Url} = kz_fixturedb_server:maybe_use_app_connection(Server, DbName),
    filelib:is_dir(filename:join([Url ++ "/", kz_term:to_binary(DbName)])).

-spec db_archive(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
db_archive(_, _, _) ->
    'ok'.

-spec db_list(server_map(), kz_data:options()) -> {'ok', kz_term:ne_binaries()}.
db_list(Server, _Options) ->
    db_info(Server).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_dbs_list(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binaries().
get_dbs_list(ServerUrl, ServerUrl) ->
    [kz_term:to_binary(filename:basename(Db)) || Db <- get_dbs_list(ServerUrl)];
get_dbs_list(ServerUrl, AppUrl) ->
    lists:usort([kz_term:to_binary(Db)
                 || Db <- get_dbs_list(ServerUrl) ++ get_dbs_list(AppUrl)
                ]).

-spec get_dbs_list(kz_term:ne_binary()) -> [string()].
get_dbs_list(Url) ->
    filelib:wildcard(kz_term:to_list(Url) ++ "/*").
