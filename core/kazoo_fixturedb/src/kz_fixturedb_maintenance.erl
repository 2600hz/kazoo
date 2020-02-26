%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_maintenance).

-export([dummy_plan/0
        ,new_connection/0, new_connection/1
        ,db_to_disk/1, db_to_disk/2
        ]).

-include("kz_fixturedb.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dummy_plan() -> map().
dummy_plan() ->
    #{server => {kazoo_fixturedb, new_connection()}}.

-spec new_connection() -> server_map().
new_connection() -> new_connection(#{}).

-spec new_connection(map()) -> server_map().
new_connection(Map) ->
    {'ok', Server} = kz_fixturedb_server:new_connection(Map),
    Server.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_to_disk(kz_term:ne_binary()) -> 'ok' | kz_datamgr:data_error().
db_to_disk(Database) ->
    db_to_disk(Database, fun kz_term:always_true/1).

-type filter_fun() :: fun((kz_json:object()) -> boolean()).

-spec db_to_disk(kz_term:ne_binary(), filter_fun()) -> 'ok' | kz_datamgr:data_error().
db_to_disk(Database, FilterFun) ->
    case kz_datamgr:db_exists(Database) of
        'true' -> db_to_disk_persist(Database, FilterFun);
        'false' -> {'error', 'not_found'}
    end.

-spec db_to_disk_persist(kz_term:ne_binary(), filter_fun()) -> 'ok' | kz_datamgr:data_error().
db_to_disk_persist(Database, FilterFun) ->
    db_to_disk_persist(Database, FilterFun, get_page(Database, 'undefined')).

-spec get_page(kz_term:ne_binary(), kz_json:api_json_term()) -> kazoo:paginate_results().
get_page(Database, 'undefined') ->
    query(Database, []);
get_page(Database, StartKey) ->
    query(Database, [{'startkey', StartKey}]).

-spec query(kz_term:ne_binary(), kazoo_data:view_options()) -> kazoo:paginate_results().
query(Database, Options) ->
    kz_datamgr:paginate_results(Database, 'all_docs', ['include_docs' | Options]).

-spec db_to_disk_persist(kz_term:ne_binary(), filter_fun(), kz_datamgr:paginate_results()) ->
          'ok' | kz_datamgr:data_error().
db_to_disk_persist(Database, FilterFun, {'ok', ViewResults, 'undefined'}) ->
    _ = filter_and_persist(Database, FilterFun, ViewResults),
    lager:info("finished persisting ~s", [Database]);
db_to_disk_persist(Database, FilterFun, {'ok', ViewResults, NextStartKey}) ->
    _ = filter_and_persist(Database, FilterFun, ViewResults),
    db_to_disk_persist(Database, FilterFun, get_page(Database, NextStartKey));
db_to_disk_persist(_Database, _FilterFun, {'error', _E}=Error) ->
    Error.

-spec filter_and_persist(kz_term:ne_binary(), filter_fun(), kz_json:objects()) -> 'ok'.
filter_and_persist(Database, FilterFun, ViewResults) ->
    _ = [persist(Database, Document) || ViewResult <- ViewResults,
                                        Document <- [kz_json:get_json_value(<<"doc">>, ViewResult)],
                                        FilterFun(Document)
        ],
    'ok'.

-spec persist(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
persist(Database, Document) ->
    Path = kz_fixturedb_util:get_doc_path(Database, kz_doc:id(Document)),
    'ok' = filelib:ensure_dir(Path),

    lager:info("  persisting doc ~s to ~s", [kz_doc:id(Document), Path]),
    'ok' = file:write_file(Database, kz_json:encode(Document, ['pretty'])).
