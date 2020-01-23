%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Manage data connections.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_datamgr).

-export([db_classification/1]).

%% Settings-related
-export([max_bulk_insert/0
        ,max_bulk_read/0
        ]).
-export([init_dbs/1]).
%% format
-export([format_error/1]).

%% System manipulation
-export([db_exists/1, db_exists/2, db_exists_all/1
        ,db_info/0, db_info/1
        ,db_create/1, db_create/2
        ,db_compact/1
        ,db_view_cleanup/1
        ,db_view_update/2, db_view_update/3
        ,db_delete/1
        ,db_replicate/1
        ,db_archive/1, db_archive/2
        ,db_import/2
        ,db_list/0, db_list/1
        ]).

%% Document manipulation
-export([save_doc/2, save_doc/3
        ,save_docs/2, save_docs/3
        ,open_cache_doc/2, open_cache_doc/3
        ,open_cache_docs/2, open_cache_docs/3
        ,update_cache_doc/3
        ,flush_cache_doc/2, flush_cache_doc/3
        ,flush_cache_docs/0, flush_cache_docs/1
        ,add_to_doc_cache/3
        ,open_doc/2,open_doc/3
        ,open_docs/2, open_docs/3
        ,del_doc/2, del_docs/2
        ,del_doc/3, del_docs/3
        ,lookup_doc_rev/2, lookup_doc_rev/3
        ,update_doc/3
        ,revise_doc_from_file/3
        ,revise_docs_from_folder/3, revise_docs_from_folder/4
        ,revise_views_from_folder/2
        ,ensure_saved/2, ensure_saved/3
        ,load_fixtures_from_folder/2
        ,all_docs/1
        ,all_design_docs/1
        ,all_docs/2
        ,all_design_docs/2
        ,copy_doc/4, copy_doc/5
        ,move_doc/4, move_doc/5
        ,maybe_update_doc/2
        ]).

%% attachments
-export([fetch_attachment/3, fetch_attachment/4
        ,stream_attachment/3, stream_attachment/4, stream_attachment/5
        ,put_attachment/4, put_attachment/5
        ,delete_attachment/3, delete_attachment/4
        ,attachment_url/3, attachment_url/4
        ]).

%% Views
-export([get_all_results/2
        ,get_results/2, get_results/3
        ,get_results_count/3
        ,get_result_keys/1, get_result_keys/3, get_result_keys/2
        ,get_result_ids/1, get_result_ids/2, get_result_ids/3
        ,get_single_result/3
        ,get_result_doc/3, get_result_docs/3
        ,show/2, show/3, show/4
        ,paginate_results/3
        ,design_info/2
        ,design_compact/2
        ,maybe_adapt_multilines/1
        ]).

-export([get_uuid/0, get_uuid/1
        ,get_uuids/1, get_uuids/2
        ]).

-export([suppress_change_notice/0
        ,enable_change_notice/0
        ,change_notice/0
        ]).

-export([refresh_views/1
        ,register_view/2
        ,register_views/2
        ,register_views_from_folder/1, register_views_from_folder/2
        ]).

-type update_option() :: {'update', kz_json:flat_proplist()} |
                         {'create', kz_json:flat_proplist()} |
                         {'should_create', boolean()} |
                         {'extra_update', kz_json:flat_proplist()} |
                         {'ensure_saved', boolean()}.
-type update_options() :: [update_option()].

-export_type([view_option/0, view_options/0
             ,view_listing/0, views_listing/0
             ,data_error/0, data_errors/0
             ,db_classification/0
             ,update_option/0, update_options/0
             ,get_results_return/0
             ,paginated_results/0
             ]).

-deprecated({'ensure_saved', '_', 'eventually'}).

-include("kz_data.hrl").

-type database_name() :: kz_term:ne_binary().

-define(UUID_SIZE, 16).

%%%=============================================================================
%%% Couch Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Overwrite the existing contents of a document with the contents of
%% a file.
%% @end
%%------------------------------------------------------------------------------
-spec update_doc_from_file(database_name(), nonempty_string() | kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
update_doc_from_file(DbName, Path) ->
    lager:debug("update db ~s from CouchDB file: ~s", [DbName, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        JObj = maybe_adapt_multilines(kz_json:decode(Bin)),
        maybe_update_doc_from_file(DbName, JObj, open_doc(DbName, kz_doc:id(JObj)))
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("bad match: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end.

-spec maybe_update_doc_from_file(database_name(), kz_json:object(), any()) ->
          {'ok', kz_json:object()} |
          data_error().
maybe_update_doc_from_file(DbName, JObj, {'ok', Doc}=OK) ->
    case should_update(DbName, JObj, OK) of
        'false' ->
            {'ok', Doc};
        'undefined' ->
            save_doc(DbName, kz_doc:update_pvt_parameters(JObj, DbName));
        'true' ->
            PrivateFields = kz_json:to_proplist(kz_doc:private_fields(Doc)),
            NewJObj = kz_doc:update_pvt_parameters(kz_json:set_values(PrivateFields, JObj), DbName),
            save_doc(DbName, kz_doc:set_revision(NewJObj, kz_doc:revision(Doc)))
    end;
maybe_update_doc_from_file(DbName, JObj, {'error', 'not_found'}) ->
    save_doc(DbName, kz_doc:update_pvt_parameters(JObj, DbName));
maybe_update_doc_from_file(_, _, {'error', _}=Error) ->
    Error.

%%------------------------------------------------------------------------------
%% @doc Create or overwrite the existing contents of a document with the
%% contents of a file.
%% @end
%%------------------------------------------------------------------------------
-spec revise_doc_from_file(database_name(), atom(), kz_term:ne_binary() | nonempty_string()) ->
          {'ok', kz_json:object()} |
          data_error().
revise_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    case update_doc_from_file(DbName, Path) of
        {'error', _E}=R ->
            lager:debug("failed to update doc: ~p", [_E]),
            R;
        {'ok', _}=Resp ->
            lager:debug("revised ~s", [File]),
            Resp
    end.

%%------------------------------------------------------------------------------
%% @doc Loads all `.json' files in an applications `priv/couchdb/views/' folder
%% into a given database.
%% @end
%%------------------------------------------------------------------------------
-spec revise_views_from_folder(database_name(), atom()) -> 'ok'.
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%------------------------------------------------------------------------------
%% @doc Loads all `.json' files in an applications folder, relative to
%% `priv/couchdb/' into a given database
%% @end
%%------------------------------------------------------------------------------
-spec revise_docs_from_folder(database_name(), atom(), kz_term:ne_binary() | nonempty_string()) -> 'ok'.
revise_docs_from_folder(DbName, App, Folder) ->
    revise_docs_from_folder(DbName, App, Folder, 'false').

-spec revise_docs_from_folder(database_name(), atom(), kz_term:ne_binary() | nonempty_string(), boolean()) -> 'ok'.
revise_docs_from_folder(DbName, App, Folder, Sleep) ->
    Database = kzs_util:to_database(DbName),
    case code:priv_dir(App) of
        {'error', 'bad_name'} ->
            lager:error("tried to revise docs for db ~p for invalid priv directory. app: ~p", [Database, App]);
        ValidDir ->
            Files = filelib:wildcard([ValidDir, "/couchdb/", kz_term:to_list(Folder), "/*.json"]),
            lager:debug("refreshing ~B documents in db ~s for ~s", [length(Files), Database, App]),
            do_revise_docs_from_folder(Database, Sleep, Files)
    end.

-spec do_revise_docs_from_folder(database_name(), boolean(), kz_term:ne_binaries()) -> 'ok'.
do_revise_docs_from_folder(_, _, []) -> 'ok';
do_revise_docs_from_folder(DbName, Sleep, [H|T]) ->
    try
        {'ok', _} = update_doc_from_file(DbName, H),
        Sleep
            andalso timer:sleep(250),
        do_revise_docs_from_folder(DbName, Sleep, T)
    catch
        ?STACKTRACE(_, _, ST)
        kz_log:log_stacktrace(ST),
        do_revise_docs_from_folder(DbName, Sleep, T)
        end.

-spec maybe_update_doc(database_name(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          data_error().
maybe_update_doc(DbName, JObj) ->
    case should_update(DbName, JObj) of
        'false' ->
            {'ok', JObj};
        'undefined' ->
            save_doc(DbName, JObj);
        'true' ->
            Updates = kz_json:to_proplist(kz_json:flatten(JObj)),
            Update = [{'update', Updates}
                     ,{'ensure_saved', 'true'}
                     ],
            update_doc(DbName, kz_doc:id(JObj), Update)
    end.

-spec should_update(database_name(), kz_json:object()) -> kz_term:api_boolean().
should_update(DbName, JObj) ->
    should_update(DbName, JObj, open_doc(DbName, kz_doc:id(JObj))).

-spec should_update(database_name(), kz_json:object(), any()) -> kz_term:api_boolean().
should_update(_, JObj, {'ok', Doc}) ->
    kz_doc:document_hash(JObj) =/= kz_doc:document_hash(Doc);
should_update(_, _, {'error', 'not_found'}) ->
    'undefined';
should_update(_, _, {'error', _}) ->
    'true'.

%%------------------------------------------------------------------------------
%% @doc Replaces multi-line Javascript into single line, on the fly
%% while loading views from files.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_adapt_multilines(kz_json:object()) -> kz_json:object().
maybe_adapt_multilines(JObj) ->
    kz_json:map(fun maybe_adapt_multilines/2, JObj).

-spec maybe_adapt_multilines(kz_json:key(), kz_json:json_term()) ->
          {kz_json:key(), kz_json:json_term()}.
maybe_adapt_multilines(Key, JObj)
  when Key =:= <<"filters">>
       orelse Key =:= <<"lists">>
       orelse Key =:= <<"shows">>
       orelse Key =:= <<"updates">> ->
    Prop = kz_json:foldl(fun inline_js_fun/3, kz_json:new(), JObj),
    {Key, Prop};
maybe_adapt_multilines(<<"views">> = Key, JObj) ->
    NewViews =
        [{View, kz_json:foldl(fun inline_js_fun/3, kz_json:new(), Pairs)}
         || {View, Pairs} <- kz_json:to_proplist(JObj)
        ],
    {Key, kz_json:from_list(NewViews)};
maybe_adapt_multilines(<<"rewrites">> = Key, Code=[<<"function", _/binary>>|_]) ->
    {Key, iolist_to_binary(Code)};
maybe_adapt_multilines(<<"validate_doc_update">> = Key, Code=[<<"function", _/binary>>|_]) ->
    {Key, iolist_to_binary(Code)};
maybe_adapt_multilines(Key, Value) ->
    {Key, Value}.

-spec inline_js_fun(kz_term:ne_binary(), kz_term:ne_binaries() | kz_json:json_term(), kz_json:object()) ->
          kz_json:object().
inline_js_fun(Type, Code=[<<"function",_/binary>>|_], Acc) ->
    kz_json:set_value(Type, iolist_to_binary(Code), Acc);
inline_js_fun(Type, Code, Acc) ->
    kz_json:set_value(Type, Code, Acc).

%%------------------------------------------------------------------------------
%% @doc Load fixture files from a folder into a database, only if the ID
%% isn't already exists.
%% @end
%%------------------------------------------------------------------------------
-spec load_fixtures_from_folder(database_name(), atom()) -> 'ok'.
load_fixtures_from_folder(DbName, App) ->
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", ?FIXTURES_FOLDER, "/*.json"]),
    Database = kzs_util:to_database(DbName),
    do_load_fixtures_from_folder(Database, Files).

-spec do_load_fixtures_from_folder(database_name(), kz_term:ne_binaries()) -> 'ok'.
do_load_fixtures_from_folder(Database, Files) ->
    lists:foreach(fun(Filename) -> do_load_fixture_from_folder(Database, Filename) end
                 ,Files
                 ).

-spec do_load_fixture_from_folder(database_name(), file:filename_all()) -> 'ok'.
do_load_fixture_from_folder(Database, Filename) ->
    try
        {'ok', Bin} = file:read_file(Filename),
        FixJObj = kz_json:decode(Bin),
        FixId = kz_doc:id(FixJObj),
        case lookup_doc_rev(Database, FixId) of
            {'ok', _Rev} ->
                lager:debug("fixture ~s exists in ~s: ~s", [FixId, Database, _Rev]);
            {'error', 'not_found'} ->
                lager:debug("saving fixture ~s to ~s", [FixId, Database]),
                _ = save_doc(Database, FixJObj),
                lager:debug("saved fixture");
            {'error', _Reason} ->
                lager:debug("failed to lookup rev for fixture: ~p: ~s in ~s", [_Reason, FixId, Database])
        end
    catch
        _C:_R ->
            lager:debug("failed to check fixture: ~s: ~p", [_C, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc Determines if a database exists.
%% @end
%%------------------------------------------------------------------------------
-spec db_exists(database_name()) -> boolean().
db_exists(DbName) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_exists(kzs_plan:plan(Database), Database).

-spec db_exists(database_name(), kz_term:api_binary() | kz_term:proplist()) -> boolean().
db_exists(DbName, 'undefined') ->
    db_exists(DbName);
db_exists(DbName, <<Type/binary>>) ->
    case add_doc_type_from_view(Type, []) of
        [] -> db_exists(DbName, [{'doc_type', Type}]);
        Options -> db_exists(DbName, Options)
    end;
db_exists(DbName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_exists(kzs_plan:plan(Database, Options), Database).

%%------------------------------------------------------------------------------
%% @doc Determines if a database exists, also checks other connections.
%% @end
%%------------------------------------------------------------------------------
-spec db_exists_all(database_name()) -> boolean().
db_exists_all(DbName) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_exists_all(kzs_plan:plan(Database), Database).

%%------------------------------------------------------------------------------
%% @doc Retrieve information regarding all databases.
%% @end
%%------------------------------------------------------------------------------
-spec db_info() -> {'ok', kz_term:ne_binaries()} |
          data_error().
db_info() ->
    kzs_db:db_info(kzs_plan:plan()).

%%------------------------------------------------------------------------------
%% @doc Retrieve information regarding a database.
%% @end
%%------------------------------------------------------------------------------
-spec db_info(database_name()) -> {'ok', kz_json:object()} |
          data_error().
db_info(DbName) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_info(kzs_plan:plan(Database), Database).

%%------------------------------------------------------------------------------
%% @doc Retrieve information regarding a database design doc.
%% @end
%%------------------------------------------------------------------------------
-spec design_info(database_name(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
design_info(DbName, DesignName) ->
    Database = kzs_util:to_database(DbName),
    kzs_view:design_info(kzs_plan:plan(Database, DesignName), Database, DesignName).

-spec design_compact(database_name(), kz_term:ne_binary()) -> boolean().
design_compact(DbName, DesignName)->
    Database = kzs_util:to_database(DbName),
    kzs_view:design_compact(kzs_plan:plan(Database, DesignName), Database, DesignName).

-spec db_view_cleanup(database_name()) -> boolean().
db_view_cleanup(DbName) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_view_cleanup(kzs_plan:plan(Database), Database).

-spec db_view_update(database_name(), views_listing()) ->
          boolean() |
          {'error', 'db_not_found'} |
          {'error', 'invalid_db_name'}.
db_view_update(DbName, Views) ->
    db_view_update(DbName, Views, 'false').

-spec db_view_update(database_name(), views_listing(), boolean()) ->
          boolean() |
          {'error', 'db_not_found'} |
          {'error', 'invalid_db_name'}.
db_view_update(DbName, Views0, Remove) ->
    case lists:keymap(fun maybe_adapt_multilines/1, 2, Views0) of
        [] -> 'false';
        Views ->
            Database = kzs_util:to_database(DbName),
            kzs_db:db_view_update(kzs_plan:plan(Database), Database, Views, Remove)
    end.

%%------------------------------------------------------------------------------
%% @doc Replicate a DB from one host to another.
%% IMPORTANT: Use the atom true, not binary `<<"true">>' (though it may be changing in couch to allow `<<"true">>')
%%
%% Proplist description:
%% <dl>
%%   <dt>`{<<"source">>, <<"http://some.couch.server:5984/source_db">>}'</dt><dd>URL to source CouchDB database.</dd>
%%   <dt>`{<<"target">>, <<"target_db">>}'</dt><dd>Target database.</dd>
%%   <dt>`{<<"create_target">>, true}'</dt><dd>Optional, creates the DB on target if it is existent already.</dd>
%%   <dt>`{<<"continuous">>, true}'</dt><dd>Optional, continuously update target from source.</dd>
%%   <dt>`{<<"cancel">>, true}'</dt><dd>Optional, will cancel a replication (one-time or continuous).</dd>
%%   <dt>`{<<"filter">>, <<"source_design_doc/source_filter_name">>}'</dt><dd>Optional, filter what documents are sent from source to target.</dd>
%%   <dt>`<<"query_params">>, {struct, [{<<"key1">>, <<"value1">>}, {<<"key2">>, <<"value2">>}]}}'</dt>
%%     <dd>Optional, send params to filter function
%%       `filter_fun: function(Doc, Req) -> boolean();' passed Key-Value pairs in `query_params' are in `Req' in filter function
%%     </dd>
%%   <dt>`{<<"doc_ids">>, [<<"source_doc_id_1">>, <<"source_doc_id_2">>]}'</dt><dd>Optional, if you only want specific docs, no need for a filter.</dd>
%%   <dt>`{<<"proxy">>, <<"http://some.proxy.server:12345">>}'</dt><dd>Optional, if you need to pass the replication via proxy to target HTTPS support for proxying is suspect.</dd>
%% </dl>
%%
%% If authentication is needed at the source's end:
%% `{<<"source">>, <<"http://user:password@some.couch.server:5984/source_db">>}'.
%%
%% If source or target DB is on the current connection, you can just put the DB name, e.g:
%% `[{<<"source">>, <<"source_db">>}, {<<"target">>, <<"target_db">>}, ...]'.
%% Then you don't have to specify the credentials (if any) for the connection/
%% @end
%%------------------------------------------------------------------------------
-spec db_replicate(kz_term:proplist() | kz_json:object()) ->
          {'ok', kz_json:object()} |
          data_error().
db_replicate(Prop) when is_list(Prop) ->
    db_replicate(kz_json:from_list(Prop));
db_replicate(JObj) ->
    kzs_db:db_replicate(kzs_plan:plan(), JObj).

%%------------------------------------------------------------------------------
%% @doc Determines if a database exists.
%% @end
%%------------------------------------------------------------------------------
-spec db_create(database_name()) -> boolean().
db_create(DbName) ->
    db_create(DbName, []).

-spec db_create(database_name(), kzs_db:db_create_options()) -> boolean().
db_create(DbName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_create(kzs_plan:plan(Database), Database, Options).

%%------------------------------------------------------------------------------
%% @doc Compact a database.
%% @end
%%------------------------------------------------------------------------------
-spec db_compact(database_name()) -> boolean().

db_compact(DbName) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_compact(kzs_plan:plan(Database), Database).

%%------------------------------------------------------------------------------
%% @doc Delete a database (takes an `encoded' DbName).
%% @end
%%------------------------------------------------------------------------------
-spec db_delete(database_name()) -> boolean().
db_delete(DbName) ->
    db_delete(DbName, []).

-spec db_delete(database_name(), db_delete_options()) -> boolean().
db_delete(DbName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_delete(kzs_plan:plan(Database), Database, Options).

%%------------------------------------------------------------------------------
%% @doc Archive a database (takes an `encoded' DbName).
%% @end
%%------------------------------------------------------------------------------
-spec db_archive(database_name()) -> 'ok' | data_error().
db_archive(DbName) ->
    Folder = kazoo_data_config:get_ne_binary(<<"default_archive_folder">>, <<"/tmp">>),
    db_archive(DbName, filename:join([<<Folder/binary, "/", DbName/binary, ".json">>])).

-spec db_archive(database_name(), kz_term:ne_binary()) -> 'ok' | data_error().
db_archive(DbName, Filename) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_archive(kzs_plan:plan(Database), Database, Filename).

-spec db_import(database_name(), file:filename_all()) -> 'ok' | data_error().
db_import(DbName, ArchiveFile) ->
    Database = kzs_util:to_database(DbName),
    kzs_db:db_import(kzs_plan:plan(Database), Database, ArchiveFile).

%%%=============================================================================
%%% Document Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc fetch a cached doc or open it if not available.
%% @end
%%------------------------------------------------------------------------------
-spec open_cache_doc(database_name(), docid()) ->
          {'ok', kz_json:object()} |
          data_error().
open_cache_doc(DbName, {DocType, DocId}) ->
    open_cache_doc(DbName, DocId, [{'doc_type', DocType}]);
open_cache_doc(DbName, DocId) ->
    open_cache_doc(DbName, DocId, []).

-spec open_cache_doc(database_name(), docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
open_cache_doc(DbName, {DocType, DocId}, Options) ->
    open_cache_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_cache_doc(DbName, DocId, Options) ->
    kzs_cache:open_cache_doc(kzs_util:to_database(DbName), DocId, Options).

-spec add_to_doc_cache(database_name(), kz_term:ne_binary(), kz_json:object()) ->
          'ok' |
          data_error().
add_to_doc_cache(DbName, DocId, Doc) ->
    Database = kzs_util:to_database(DbName),
    kzs_cache:add_to_doc_cache(Database, DocId, Doc).

-spec update_cache_doc(database_name(), kz_term:ne_binary(), fun((kz_json:object()) -> kz_json:object() | 'skip')) ->
          {'ok', kz_json:object()} |
          data_error().
update_cache_doc(DbName, DocId, Fun) when is_function(Fun, 1) ->
    case open_cache_doc(DbName, DocId) of
        {'ok', JObj} ->
            NewJObj = Fun(JObj),
            maybe_save_doc(DbName, NewJObj, JObj);
        {'error', _Reason} = Else ->
            lager:error("can't open doc ~s/~s coz ~p", [DbName, DocId, _Reason]),
            Else
    end.

-spec maybe_save_doc(database_name(), kz_json:object() | 'skip', kz_json:object()) ->
          {'ok', kz_json:object() | kz_json:objects()} |
          data_error().
maybe_save_doc(_DbName, 'skip', Jobj) ->
    {'ok', Jobj};
maybe_save_doc(DbName, JObj, _OldJobj) ->
    save_doc(DbName, JObj).

-spec flush_cache_doc(database_name(), kz_term:ne_binary() | kz_json:object()) ->
          'ok' |
          {'error', 'invalid_db_name'}.
flush_cache_doc(DbName, Doc) ->
    flush_cache_doc(DbName, Doc, []).

-spec flush_cache_doc(database_name(), kz_term:ne_binary() | kz_json:object(), kz_term:proplist()) ->
          'ok' |
          {'error', 'invalid_db_name'}.
flush_cache_doc(DbName, Doc, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_cache:flush_cache_doc(Database, Doc, Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> kzs_cache:flush_cache_docs().

-spec flush_cache_docs(database_name()) -> 'ok' | {'error', 'invalid_db_name'}.
flush_cache_docs(DbName) ->
    kzs_cache:flush_cache_docs(kzs_util:to_database(DbName)).

-ifdef(TEST).
%% -define(OPEN_DOC_LOG(DbName, DocId, Options),
%%         begin
%%             {_, ST} = erlang:process_info(self(), current_stacktrace),
%%             kz_log:log_stacktrace(ST),
%%             ?LOG_DEBUG("~s:open_doc(~p, ~p, ~p)", [?MODULE, DbName, DocId, Options])
%%         end
%%        ).
-define(OPEN_DOC_LOG(DbName, DocId, Options), ok).
-else.
-define(OPEN_DOC_LOG(DbName, DocId, Options), ok).
-endif.

%%------------------------------------------------------------------------------
%% @doc open a document given a doc id returns an error tuple or the json.
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(database_name(), docid()) ->
          {'ok', kz_json:object()} |
          data_error() |
          {'error', 'not_found'}.
open_doc(DbName, {DocType, DocId}) ->
    open_doc(DbName, DocId, [{'doc_type', DocType}]);
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec open_doc(database_name(), docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error() |
          {'error', 'not_found'}.
open_doc(DbName, {DocType, DocId}, Options) ->
    open_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_doc(DbName, DocId, Options) ->
    Database = kzs_util:to_database(DbName),
    ?OPEN_DOC_LOG(Database, DocId, Options),
    kzs_doc:open_doc(kzs_plan:plan(Database, Options), Database, DocId, Options).

%%------------------------------------------------------------------------------
%% @doc Open documents given doc ids returns an error tuple or the json.
%% Each returned JObj contains either an `<<"doc">>' or `<<"error">>' field.
%% So: match both error tuple and each JSON of the list.
%% @end
%%------------------------------------------------------------------------------
-spec open_docs(database_name(), docids()) ->
          {'ok', kz_json:objects()} |
          data_error() |
          {'error', 'not_found'}.
open_docs(DbName, DocIds) ->
    open_docs(DbName, DocIds, []).

-spec open_docs(database_name(), docids(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error() |
          {'error', 'not_found'}.
open_docs(DbName, DocIds, Options) ->
    read_chunked(fun do_open_docs/3, kzs_util:to_database(DbName), DocIds, Options).

do_open_docs(DbName, DocIds, Options) ->
    NewOptions = [{'keys', DocIds}, 'include_docs' | Options],
    all_docs(DbName, NewOptions).

read_chunked(Opener, Database, DocIds, Options) ->
    read_chunked(Opener, Database, DocIds, Options, []).
read_chunked(Opener, Database, DocIds, Options, Acc) ->
    try lists:split(max_bulk_read(Options), DocIds) of
        {NewDocIds, DocIdsLeft} ->
            NewAcc = read_chunked_results(Opener, Database, NewDocIds, Options, Acc),
            read_chunked(Opener, Database, DocIdsLeft, Options, NewAcc)
    catch 'error':'badarg' ->
            case read_chunked_results(Opener, Database, DocIds, Options, Acc) of
                {'error', _R}=E -> E;
                JObjs -> {'ok', lists:flatten(lists:reverse(JObjs))}
            end
    end.

read_chunked_results(_, _, _, _, {'error', _}=Acc) -> Acc;
read_chunked_results(Opener, Database, DocIds, Options, Acc) ->
    read_chunked_results(DocIds, Opener(Database, DocIds, Options), Acc).

read_chunked_results(_DocIds, {'ok', JObjs}, Acc) ->
    [JObjs | Acc];
read_chunked_results(_DocIds, {'error', _}=Reason, []) ->
    Reason;
read_chunked_results(DocIds, {'error', Reason}, Acc) ->
    [kz_json:from_list(
       [{<<"id">>, DocId}
       ,{<<"error">>, Reason}
       ])
     || DocId <- DocIds
    ] ++ Acc.

%%------------------------------------------------------------------------------
%% @doc Open documents given doc ids returns an error tuple or the JSON.
%% Attempts to fetch from cache before making an ad-hoc bulk read.
%% Each returned JObj contains either an `<<"doc">>' or `<<"error">>' field.
%% So: match both error tuple and each JSON of the list.
%%
%% <div class="notice">No guaranty on order of results is provided.</div>
%% @end
%%------------------------------------------------------------------------------

-spec open_cache_docs(database_name(), docids()) ->
          {'ok', kz_json:objects()} |
          data_error() |
          {'error', 'not_found'}.
open_cache_docs(DbName, DocIds) ->
    open_cache_docs(DbName, DocIds, []).

-spec open_cache_docs(database_name(), docids(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error() |
          {'error', 'not_found'}.
open_cache_docs(DbName, DocIds, Options) ->
    Database = kzs_util:to_database(DbName),
    read_chunked(fun kzs_cache:open_cache_docs/3, Database, DocIds, Options).

-spec all_docs(database_name()) ->
          {'ok', kz_json:objects()} |
          data_error().
all_docs(DbName) ->
    all_docs(DbName, []).

-spec all_docs(database_name(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
all_docs(DbName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_view:all_docs(kzs_plan:plan(Database, Options), Database, Options).

-spec db_list() -> {'ok', kz_term:ne_binaries()} | data_error().
db_list() ->
    db_list([]).

-spec db_list(kz_term:proplist()) -> {'ok', kz_term:ne_binaries()} | data_error().
db_list(Options) ->
    kzs_db:db_list(kzs_plan:plan(), Options).

-spec all_design_docs(database_name()) -> {'ok', kz_json:objects()} |
          data_error().
all_design_docs(DbName) ->
    all_design_docs(DbName, []).

-spec all_design_docs(database_name(), kz_term:proplist()) -> {'ok', kz_json:objects()} |
          data_error().
all_design_docs(DbName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_view:all_design_docs(kzs_plan:plan(Database), Database, Options).

%%------------------------------------------------------------------------------
%% @doc Get the revision of a document (much faster than requesting the whole document).
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev(database_name(), docid()) ->
          {'ok', kz_term:ne_binary()} |
          data_error().
lookup_doc_rev(DbName, {DocType, DocId}) ->
    lookup_doc_rev(DbName, DocId, [{'doc_type', DocType}]);
lookup_doc_rev(DbName, DocId) ->
    lookup_doc_rev(DbName, DocId, []).

-spec lookup_doc_rev(database_name(), docid(), kz_term:proplist()) ->
          {'ok', kz_term:ne_binary()} | data_error().
lookup_doc_rev(DbName, {DocType, DocId}, Options) ->
    lookup_doc_rev(DbName, DocId, maybe_add_doc_type(DocType, Options));
lookup_doc_rev(DbName, DocId, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_doc:lookup_doc_rev(kzs_plan:plan(Database, Options), Database, DocId).

%%------------------------------------------------------------------------------
%% @doc Save document to database.
%% @end
%%------------------------------------------------------------------------------
-spec save_doc(database_name(), kz_json:object() | kz_json:objects()) ->
          {'ok', kz_json:object() | kz_json:objects()} |
          data_error().
save_doc(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).

%%------------------------------------------------------------------------------
%% @doc Save a document. If it fails because of conflict, pulls latest
%% revision and tries saving again. Otherwise return.
%% @end
%%------------------------------------------------------------------------------

-spec ensure_saved(database_name(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          data_error().
ensure_saved(DbName, Doc) ->
    ensure_saved(DbName, Doc, []).

-spec ensure_saved(database_name(), kz_json:object(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
ensure_saved(DbName, Doc, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_doc:ensure_saved(kzs_plan:plan(Database, Doc), Database, Doc, Options).

-spec save_doc(database_name(), kz_json:object(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
save_doc(DbName, Doc, Options) ->
    Database = kzs_util:to_database(DbName),
    OldSetting = maybe_toggle_publish(Options),
    Result = kzs_doc:save_doc(kzs_plan:plan(Database, Doc), Database, Doc, Options),
    maybe_revert_publish(OldSetting),
    Result.

-spec maybe_toggle_publish(kz_term:proplist()) -> boolean().
maybe_toggle_publish(Options) ->
    Old = change_notice(),
    case props:get_value('publish_change_notice', Options) of
        'true' -> enable_change_notice();
        'false' -> suppress_change_notice();
        'undefined' -> 'ok'
    end,
    Old.

-spec maybe_revert_publish(boolean()) -> boolean().
maybe_revert_publish('true') ->
    enable_change_notice();
maybe_revert_publish('false') ->
    suppress_change_notice().

-spec save_docs(database_name(), kz_json:objects()) ->
          {'ok', kz_json:objects()} |
          data_error().
save_docs(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []).

-spec save_docs(database_name(), kz_json:objects(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
save_docs(DbName, [Doc|_]=Docs, Options) ->
    Database = kzs_util:to_database(DbName),
    OldSetting = maybe_toggle_publish(Options),
    Result = kzs_doc:save_docs(kzs_plan:plan(Database, Doc), Database, Docs, Options),
    maybe_revert_publish(OldSetting),
    Result;
save_docs(_DbName, []=Docs, _Options)
  when is_list(Docs) ->
    {'ok', Docs}.

%%------------------------------------------------------------------------------
%% @doc Fetch, update and save a doc (creating if not present).
%% @end
%%------------------------------------------------------------------------------
-spec update_doc(database_name(), docid(), update_options()) ->
          {'ok', kz_json:object()} |
          data_error().
update_doc(DbName, Id, Options) ->
    Database = kzs_util:to_database(DbName),
    case open_doc(Database, Id) of
        {'error', 'not_found'} ->
            update_not_found(Database, Id, Options, props:is_true('should_create', Options, 'true'));
        {'error', _}=E -> E;
        {'ok', CurrentDoc} ->
            apply_updates_and_save(Database, Id, Options, CurrentDoc)
    end.

-spec apply_updates_and_save(database_name(), docid(), update_options(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          data_error().
apply_updates_and_save(Database, Id, Options, CurrentDoc) ->
    apply_updates_and_save(Database, Id, Options, CurrentDoc, props:get_value('update', Options)).

apply_updates_and_save(_Database, _Id, _Options, CurrentDoc, []) ->
    lager:debug("no updates to apply, returning current doc ~s", [_Id]),
    {'ok', CurrentDoc};
apply_updates_and_save(Database, Id, Options, CurrentDoc, UpdateProps) ->
    UpdatedDoc = kz_json:set_values(UpdateProps, CurrentDoc),

    case kz_json:are_equal(CurrentDoc, UpdatedDoc) of
        'true' ->
            lager:debug("updates to ~s result in the same doc", [Id]),
            {'ok', CurrentDoc};
        'false' ->
            lager:debug("attempting to update to ~s: ~s", [Database, kz_json:encode(UpdatedDoc)]),
            save_update(Database, Id, Options, UpdatedDoc)
    end.

-spec save_update(database_name(), docid(), update_options(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          data_error().
save_update(Database, Id, Options, UpdatedDoc) ->
    ExtraProps = props:get_value('extra_update', Options, []),
    ExtraUpdatedDoc = kz_json:set_values(ExtraProps, UpdatedDoc),

    EnsureSaved = props:is_true('ensure_saved', Options, 'false'),

    case save_doc(Database, ExtraUpdatedDoc) of
        {'ok', _Saved}=OK ->
            lager:debug("saved ~s/~s: ~s", [Database, Id, kz_json:encode(_Saved)]),
            OK;
        {'error', 'conflict'} when EnsureSaved ->
            lager:debug("saving ~s to ~s resulted in a conflict, trying again", [Id, Database]),
            Updates = props:delete(kz_doc:path_revision(), props:get_value('update', Options)),
            update_doc(Database, Id, props:set_value('update', Updates, Options));
        {'error', _E}=Error ->
            lager:debug("failed to save ~s: ~p", [Id, _E]),
            Error
    end.

-spec update_not_found(database_name(), docid(), update_options(), boolean()) ->
          {'ok', kz_json:object()} |
          data_error().
update_not_found(_Database, _Id, _Options, 'false') ->
    {'error', 'not_found'};
update_not_found(Database, Id, Options, 'true') ->
    CreateProps = props:get_value('create', Options, []),

    JObj = kz_json:set_values(CreateProps, kz_json:new()),
    Updated = kz_json:set_values([{kz_doc:path_id(), Id}
                                  | props:get_value('update', Options)
                                 ]
                                 ++ props:get_value('extra_update', Options, [])
                                ,JObj
                                ),
    lager:debug("attempting to create ~s/~s: ~s", [Database, Id, kz_json:encode(Updated)]),
    save_doc(Database, Updated).

%%------------------------------------------------------------------------------
%% @doc Remove document from the db.
%% @end
%%------------------------------------------------------------------------------
-spec del_doc(database_name(), kz_json:object() | kz_json:objects() | kz_term:ne_binary()) ->
          {'ok', kz_json:objects()} |
          data_error().
del_doc(DbName, Doc) ->
    del_doc(DbName, Doc, []).

-spec del_doc(database_name(), kz_json:object() | kz_json:objects() | kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
del_doc(DbName, Doc, Options) when is_list(Doc) ->
    del_docs(DbName, Doc, Options);
del_doc(DbName, Doc, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_doc:del_doc(kzs_plan:plan(Database, Doc), Database, Doc, Options).

%%------------------------------------------------------------------------------
%% @doc Remove documents from the db.
%% @end
%%------------------------------------------------------------------------------
-spec del_docs(database_name(), kz_json:objects() | kz_term:ne_binaries()) ->
          {'ok', kz_json:objects()} |
          data_error().
del_docs(DbName, Docs) ->
    del_docs(DbName, Docs, []).

-spec del_docs(database_name(), kz_json:objects() | kz_term:ne_binaries(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
del_docs(DbName, Docs, Options)
  when is_list(Docs) ->
    Database = kzs_util:to_database(DbName),
    kzs_doc:del_docs(kzs_plan:plan(Database), Database, Docs, Options).

%%%=============================================================================
%%% Attachment Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Fetch attachment with `AName' from document `DocId'.
%% @end
%%------------------------------------------------------------------------------
-spec fetch_attachment(database_name(), docid(), kz_term:ne_binary()) ->
          {'ok', binary()} |
          data_error() |
          kz_att_error:error().
fetch_attachment(DbName, {DocType, DocId}, AName) ->
    fetch_attachment(DbName, DocId, AName, [{'doc_type', DocType}]);
fetch_attachment(DbName, DocId, AName) ->
    fetch_attachment(DbName, DocId, AName, []).

-spec fetch_attachment(database_name(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', binary()} |
          data_error() |
          kz_att_error:error().
fetch_attachment(DbName, {DocType, DocId}, AName, Options) ->
    fetch_attachment(DbName, DocId, AName, maybe_add_doc_type(DocType, Options));
fetch_attachment(DbName, DocId, AName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_attachments:fetch_attachment(kzs_plan:plan(Database, Options), Database, DocId, AName, Options).

-spec stream_attachment(database_name(), docid(), kz_term:ne_binary()) ->
          {'ok', reference()} |
          {'error', any()}.
stream_attachment(DbName, DocId, AName) ->
    stream_attachment(DbName, DocId, AName, []).

-spec stream_attachment(database_name(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', reference()} |
          {'error', any()}.
stream_attachment(DbName, DocId, AName, Options) ->
    stream_attachment(DbName, DocId, AName, Options, self()).

-spec stream_attachment(database_name(), docid(), kz_term:ne_binary(), kz_term:proplist(), pid()) ->
          {'ok', reference()} |
          {'error', any()}.
stream_attachment(DbName, {DocType, DocId}, AName, Options, Pid) ->
    stream_attachment(DbName, DocId, AName, maybe_add_doc_type(DocType, Options), Pid);
stream_attachment(DbName, DocId, AName, Options, Pid) ->
    Database = kzs_util:to_database(DbName),
    kzs_attachments:stream_attachment(kzs_plan:plan(Database, Options), Database, DocId, AName, Pid).

%%------------------------------------------------------------------------------
%% @doc Fetch attachment with `AName' from document `DocId'.
%% Options:
%% <dl>
%%   <dt>`{content_type, kz_term:ne_binary()}'</dt><dd>Content type of attachment.</dd>
%%   <dt>`{content_length, integer()}'</dt><dd>Attachment size in byte</dd>
%%   <dt>`{rev, kz_term:ne_binary()}'</dt><dd>Current document's revision</dd>
%% </dl>
%%
%% <div class="notice">Note atoms as keys in proplist.</div>
%% @end
%%------------------------------------------------------------------------------
-spec put_attachment(database_name(), docid(), kz_term:ne_binary(), iodata()) ->
          {'ok', kz_json:object()} |
          data_error() |
          kz_att_error:error().
put_attachment(DbName, DocId, AName, Contents) ->
    put_attachment(DbName, DocId, AName, Contents, []).

-spec put_attachment(database_name(), docid(), kz_term:ne_binary(), iodata(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'ok', kz_json:object(), kz_term:proplist()} |
          data_error() |
          kz_att_error:error().
put_attachment(DbName, {DocType, DocId}, AName, Contents, Options) ->
    put_attachment(DbName, DocId, AName, Contents, maybe_add_doc_type(DocType, Options));
put_attachment(DbName, DocId, AName, Contents, Options) ->
    Database = kzs_util:to_database(DbName),
    case attachment_options(Database, DocId, Options) of
        {'ok', NewOpts} ->
            NewOptions = props:delete('plan_override', NewOpts),
            kzs_attachments:put_attachment(kzs_plan:plan(Database, NewOpts), Database, DocId, AName, Contents, NewOptions);
        {'error', _} = Error -> Error
    end.

-spec delete_attachment(database_name(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
delete_attachment(DbName, DocId, AName) ->
    delete_attachment(DbName, DocId, AName, []).

-spec delete_attachment(database_name(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
delete_attachment(DbName, DocId, AName, Options) ->
    Database = kzs_util:to_database(DbName),
    kzs_attachments:delete_attachment(kzs_plan:plan(Database, DocId), Database, DocId, AName, Options).

-spec attachment_url(database_name(), docid(), kz_term:ne_binary()) ->
          kz_term:ne_binary() |
          {'proxy', tuple()} |
          {'error', any()}.
attachment_url(DbName, DocId, AttachmentId) ->
    attachment_url(DbName, DocId, AttachmentId, []).

-spec attachment_url(database_name(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
          kz_term:ne_binary() |
          {'proxy', tuple()} |
          {'error', any()}.
attachment_url(DbName, {DocType, DocId}, AttachmentId, Options) ->
    attachment_url(DbName, DocId, AttachmentId, maybe_add_doc_type(DocType, Options));
attachment_url(DbName, DocId, AttachmentId, Options) ->
    Database = kzs_util:to_database(DbName),
    Plan = kzs_plan:plan(Database, Options),
    case kzs_doc:open_doc(Plan, Database, DocId, props:delete('plan_override', Options)) of
        {'ok', JObj} ->
            NewOptions = [{'rev', kz_doc:revision(JObj)}
                          | maybe_add_doc_type(kz_doc:type(JObj), Options)
                         ],
            Handler = kz_doc:attachment_property(JObj, AttachmentId, <<"handler">>),
            kzs_attachments:attachment_url(Plan, Database, DocId, AttachmentId, Handler, NewOptions);
        {'error', _} = Error -> Error
    end.

%%%=============================================================================
%%% Attachment Helper Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
attachment_options(DbName, DocId, Options) ->
    RequiredOptions = [{'doc_type', fun kz_doc:type/1}
                      ,{'rev', fun kz_doc:revision/1}
                      ],
    attachment_options(DbName, DocId, Options, RequiredOptions).

attachment_options(DbName, DocId, Options, RequiredOptions) ->
    Database = kzs_util:to_database(DbName),
    Fun = fun() -> case open_cache_doc(Database, DocId, props:delete('plan_override', Options)) of
                       {'ok', JObj} -> JObj;
                       _ -> kz_json:new()
                   end
          end,
    case maybe_add_required_options(Options, RequiredOptions, Fun) of
        {'ok', _}=Ok -> Ok;
        {'error', _Missing}=Error ->
            lager:error("missing required options: ~p", [_Missing]),
            Error
    end.

maybe_add_required_options(Options, RequiredOptions, Fun) ->
    case has_required_options(Options, RequiredOptions) of
        'true' -> {'ok', Options};
        'false' -> add_required_options(Options, RequiredOptions, Fun())
    end.

has_required_options(Options, RequiredOptions) ->
    missing_required_options(Options, RequiredOptions) =:= [].

missing_required_options(Options, RequiredOptions) ->
    lists:foldl(fun({Key, _}, Acc) ->
                        case props:is_defined(Key, Options) of
                            'true' -> Acc;
                            'false' -> [Key | Acc]
                        end
                end
               ,[]
               ,RequiredOptions
               ).

add_required_options(Options, RequiredOptions, JObj) ->
    {_, NewOptions} = lists:foldl(fun add_required_option/2, {JObj, Options}, RequiredOptions),
    case missing_required_options(NewOptions, RequiredOptions) of
        [] -> {'ok', NewOptions};
        Missing -> {'error', Missing}
    end.

add_required_option({Key, Fun}, {JObj, Options}=Acc) ->
    case props:is_defined(Key, Options) of
        'true' -> Acc;
        'false' ->
            Value = case Fun(JObj) of
                        'undefined' -> <<"UNKNOWN">>;
                        V -> V
                    end,
            {JObj, [{Key, Value} | Options]}
    end.

%%%=============================================================================
%%% View Functions
%%%=============================================================================

-ifdef(TEST).
%% -define(GET_RESULTS(DbName, DesignId, Options)
%%        ,?LOG_DEBUG("~s:get_results(~p, ~p, ~p)", [?MODULE, DbName, DesignId, Options])
%%        ).
-define(GET_RESULTS(DbName, DesignId, Options), ok).
-else.
-define(GET_RESULTS(DbName, DesignId, Options), ok).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get the results of the view.
%% Returns `{Total, Offset, Meta, Rows}'
%% @end
%%------------------------------------------------------------------------------
-spec get_all_results(database_name(), 'all_docs' | kz_term:ne_binary()) -> get_results_return().
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec get_results(database_name(), 'all_docs' | kz_term:ne_binary()) -> get_results_return().
get_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec get_results(database_name(), 'all_docs' | kz_term:ne_binary(), view_options()) -> get_results_return().
get_results(DbName, DesignDoc, Options) ->
    Database = kzs_util:to_database(DbName),
    ?GET_RESULTS(Database, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    Plan = kzs_plan:plan(Database, Opts),
    case kzs_view:get_results(Plan, Database, DesignDoc, Options) of
        {'error', 'not_found'} -> maybe_create_view(Plan, Database, DesignDoc, Options);
        Other -> Other
    end.

-spec show(database_name(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
show(DbName, DesignDoc) ->
    show(DbName, DesignDoc, 'null').

-spec show(database_name(), kz_term:ne_binary(), kz_term:ne_binary() | 'null') ->
          {'ok', kz_json:object()} |
          data_error().
show(DbName, DesignDoc, DocId) ->
    show(DbName, DesignDoc, DocId, []).

-spec show(database_name(), kz_term:ne_binary(), kz_term:ne_binary() | 'null', kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
show(DbName, DesignDoc, DocId, Options) ->
    Database = kzs_util:to_database(DbName),
    Plan = kzs_plan:plan(Database),
    kzs_view:show(Plan, Database, DesignDoc, DocId, Options).

-spec get_results_count(database_name(), kz_term:ne_binary(), view_options()) ->
          {'ok', integer()} |
          data_error().
get_results_count(DbName, DesignDoc, Options) ->
    Database = kzs_util:to_database(DbName),
    ?GET_RESULTS(Database, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    kzs_view:get_results_count(kzs_plan:plan(Database, Opts), Database, DesignDoc, Options).

-spec get_registered_view(map(), database_name(), kz_term:ne_binary()) ->
          'not_registered' | kz_json:object().
get_registered_view(Plan, DbName, DesignDoc) ->
    Database = kzs_util:to_database(DbName),
    Classification = kz_term:to_binary(kzs_util:db_classification(Database)),

    Keys = registration_keys(Database, DesignDoc, Classification),
    Options = registration_options(Keys),

    case query_registered_views(Plan, Options) of
        {'ok', []} -> 'not_registered';
        {'ok', [JObj | _]} -> kz_json:get_json_value(<<"doc">>, JObj);
        {'error', _Err} ->
            lager:error("error getting registered view : ~p", [_Err]),
            'not_registered'
    end.

-spec query_registered_views(map(), view_options()) -> {'ok', kz_json:objects()} |
          data_error().
query_registered_views(Plan, Options) ->
    kzs_view:get_results(Plan, ?KZ_DATA_DB, <<"views/registered">>, Options).

-spec registration_options([[kz_term:ne_binary()]]) -> view_options().
registration_options(Keys) ->
    ['include_docs'
    ,{'keys', Keys}
    ].

-spec registration_keys(database_name(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          [[kz_term:ne_binary()]].
registration_keys(Database, DesignDoc, Classification) ->
    [[Classification, DesignDoc]
    ,[Database, DesignDoc]
    ].

-spec maybe_create_view(map(), database_name(), kz_term:ne_binary(), view_options()) -> get_results_return().
maybe_create_view(Plan, Database, DesignDoc, Options) ->
    maybe_create_view(Plan, Database, DesignDoc, Options, kzs_db:db_exists(Plan, Database)).

-spec maybe_create_view(map(), database_name(), kz_term:ne_binary(), view_options(), boolean()) -> get_results_return().
maybe_create_view(_Plan, _Database, _DesignDoc, _Options, 'false') ->
    {'error', 'not_found'};
maybe_create_view(Plan, Database, DesignDoc, Options, 'true') ->
    maybe_create_registered_view(Plan, Database, DesignDoc, Options
                                ,get_registered_view(Plan, Database, DesignDoc)
                                ).

-spec maybe_create_registered_view(map(), database_name(), 'all_docs' | kz_term:ne_binary(), view_options(), kz_json:object() | 'not_registered') ->
          get_results_return().
maybe_create_registered_view(_Plan, _Database, _DesignDoc, _Options, 'not_registered') ->
    {'error', 'not_found'};
maybe_create_registered_view(Plan, Database, DesignDoc, Options, ViewJObj) ->
    ViewDoc = kz_json:get_json_value(<<"view_definition">>, ViewJObj),
    case kzs_doc:save_doc(Plan, Database, ViewDoc, []) of
        {'ok', _ViewDoc} -> kzs_view:get_results(Plan, Database, DesignDoc, Options);
        {'error', 'conflict'} ->
            _ = maybe_update_view(Plan, Database, DesignDoc, ViewDoc),
            kzs_view:get_results(Plan, Database, DesignDoc, Options);
        {'error', _Err} ->
            lager:error("error saving registered view ~s to database ~s : ~p", [DesignDoc, Database, _Err]),
            {'error', 'not_found'}
    end.

-spec maybe_update_view(map(), database_name(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_update_view(Plan, Database, DesignDoc, ViewDoc) ->
    case binary:split(DesignDoc, <<$/>>) of
        [_, View] ->
            case kz_json:get_value([<<"views">>, View], ViewDoc) of
                'undefined' -> 'ok';
                _View ->
                    CurrentDoc = kzs_doc:open_doc(Plan, Database, kz_doc:id(ViewDoc), []),
                    maybe_update_view(Plan, Database, DesignDoc, ViewDoc, CurrentDoc)
            end;
        _Else -> 'ok'
    end.

-spec maybe_update_view(map(), database_name(), kz_term:ne_binary(), kz_json:object(), {'ok', kz_json:object()}|data_error()) -> 'ok'.
maybe_update_view(Plan, Database, DesignDoc, ViewDoc, {'ok', CurrentDoc}) ->
    case kz_json:are_equal(kz_doc:delete_revision(CurrentDoc), ViewDoc) of
        'false' ->
            UpdateDoc = kz_doc:set_revision(ViewDoc, kz_doc:revision(CurrentDoc)),
            case kzs_doc:save_doc(Plan, Database, UpdateDoc, []) of
                {'ok', _ViewDoc} ->
                    lager:debug("updated design doc ~s on database ~s", [DesignDoc, Database]);
                {'error', _Err} ->
                    lager:error("error updating design doc ~s to database ~s : ~p", [DesignDoc, Database, _Err])
            end;
        'true' -> 'ok'
    end;
maybe_update_view(_Plan, _Database, _DesignDoc, _ViewDoc, _Error) -> 'ok'.

-spec get_result_keys(database_name(), kz_term:ne_binary()) ->
          {'ok', kz_json:path() | kz_json:paths()} | data_error().
get_result_keys(DbName, DesignDoc) ->
    get_result_keys(DbName, DesignDoc, []).

-spec get_result_keys(database_name(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_json:path() | kz_json:paths()} | data_error().
get_result_keys(DbName, DesignDoc, Options) ->
    Database = kzs_util:to_database(DbName),
    ?GET_RESULTS(Database, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    case kzs_view:get_results(kzs_plan:plan(Database, Opts), Database, DesignDoc, Options) of
        {'ok', JObjs} -> {'ok', get_result_keys(JObjs)};
        {'error', _} = Error -> Error
    end.

-spec get_result_keys(kz_json:objects()) -> kz_json:path() | kz_json:paths().
get_result_keys(JObjs) ->
    [kz_json:get_value(<<"key">>, JObj)
     || JObj <- JObjs
    ].

-spec get_result_ids(database_name(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binaries()} | data_error().
get_result_ids(DbName, DesignDoc) ->
    get_result_ids(DbName, DesignDoc, []).

-spec get_result_ids(database_name(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_term:ne_binaries()} | data_error().
get_result_ids(DbName, DesignDoc, Options) ->
    Database = kzs_util:to_database(DbName),
    ?GET_RESULTS(Database, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    case kzs_view:get_results(kzs_plan:plan(Database, Opts), Database, DesignDoc, Options) of
        {'ok', JObjs} -> {'ok', get_result_ids(JObjs)};
        {'error', _} = Error -> Error
    end.

-spec get_result_ids(kz_json:objects()) -> kz_term:ne_binaries().
get_result_ids(JObjs) ->
    [kz_doc:id(JObj) || JObj <- JObjs].

%%------------------------------------------------------------------------------
%% @doc Gets the only result of a view.
%% If no result is found: returns `{'error', not_found}'.
%% If more than one result is found, either:
%% - if `Options' contains `first_when_multiple'
%%     then the first one will be returned;
%% - otherwise `{'error', multiple_results}' is returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_single_result(database_name(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_json:object()} |
          {'error', 'multiple_results'} |
          data_error().
get_single_result(DbName, DesignDoc, Options) ->
    case get_results(DbName, DesignDoc, Options) of
        {'ok', [Result]} -> {'ok', Result};
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Results} ->
            case props:is_true('first_when_multiple', Options, 'false') of
                'true' -> {'ok', hd(Results)};
                'false' -> {'error', 'multiple_results'}
            end;
        {'error', _}=E -> E
    end.

-spec get_result_doc(database_name(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', 'multiple_results'} |
          data_error().
get_result_doc(DbName, DesignDoc, Key) ->
    Options = ['include_docs'
              ,{'key', Key}
              ],
    case get_results(DbName, DesignDoc, Options) of
        {'ok', [Result]} -> {'ok', kz_json:get_json_value(<<"doc">>, Result)};
        {'ok', []} -> {'error', 'not_found'};
        {'ok', _Results} -> {'error', 'multiple_results'};
        {'error', _}=E -> E
    end.

-spec get_result_docs(database_name(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
          {'ok', kz_json:objects()} |
          data_error().
get_result_docs(DbName, DesignDoc, Keys) ->
    Options = ['include_docs'
              ,{'keys', Keys}
              ],
    case get_results(DbName, DesignDoc, Options) of
        {'ok', []} -> {'error', 'no_results'};
        {'ok', Results} -> {'ok', [kz_json:get_json_value(<<"doc">>, Result) || Result <- Results]};
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a page of results and the next 'startkey'
%% @end
%%------------------------------------------------------------------------------
-type paginate_options() :: [{'page_size', pos_integer()}] | view_options().
-type paginated_results() :: {'ok', kz_json:objects(), kz_json:api_json_term()} |
                             data_error().

-spec paginate_results(database_name(), 'all_docs' | kz_term:ne_binary(), paginate_options()) ->
          {'ok', kz_json:objects(), kz_json:api_json_term()} |
          data_error().
paginate_results(DbName, DesignDoc, Options) ->
    {PageSize, ViewOptions} = props:take_value('page_size', Options, 50),
    GetResultsOptions = props:set_value('limit', PageSize+1, ViewOptions),
    case get_results(DbName, DesignDoc, GetResultsOptions) of
        {'ok', Results} -> paginate_results(PageSize, Results);
        {'error', _E}=Error -> Error
    end.

-spec paginate_results(pos_integer(), kz_json:objects()) ->
          {'ok', kz_json:objects(), kz_json:api_json_term()} |
          data_error().
paginate_results(_PageSize, []) -> {'ok', [], 'undefined'};
paginate_results(PageSize, Results) ->
    try lists:split(PageSize, Results) of
        {Page, []} -> {'ok', Page, 'undefined'};
        {Page, [Next]} -> {'ok', Page, kz_json:get_value(<<"key">>, Next)}
    catch
        'error':'badarg' -> {'ok', Results, 'undefined'}
    end.

-spec get_uuid() -> kz_term:ne_binary().
get_uuid() -> get_uuid(?UUID_SIZE).

-spec get_uuid(pos_integer()) -> kz_term:ne_binary().
get_uuid(N) -> kz_binary:rand_hex(N).

-spec get_uuids(pos_integer()) -> kz_term:ne_binaries().
get_uuids(Count) -> get_uuids(Count, ?UUID_SIZE).

-spec get_uuids(pos_integer(), pos_integer()) -> kz_term:ne_binaries().
get_uuids(Count, Size) -> [get_uuid(Size) || _ <- lists:seq(1, Count)].

%%%=============================================================================
%%% Misc functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec suppress_change_notice() -> 'false'.
suppress_change_notice() ->
    put('$kz_data_change_notice', 'false').

-spec enable_change_notice() -> 'true'.
enable_change_notice() ->
    put('$kz_data_change_notice', 'true').

-spec change_notice() -> boolean().
change_notice() ->
    case get('$kz_data_change_notice') of
        'false' -> 'false';
        _Else -> 'true'
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec copy_doc(database_name(), docid(), database_name(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
copy_doc(FromDB, FromId, ToDB, Options) ->
    copy_doc(FromDB, FromId, ToDB, FromId, Options).

-spec copy_doc(database_name(), docid(), database_name(), docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
copy_doc(FromDB, {DocType, FromId}, ToDB, ToId, Options) ->
    copy_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
copy_doc(FromDB, FromId, ToDB, {DocType, ToId}, Options) ->
    copy_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
copy_doc(FromDB, FromId, ToDB, ToId, Options) ->
    FromDatabase = kzs_util:to_database(FromDB),
    ToDatabase = kzs_util:to_database(ToDB),

    Src = kzs_plan:plan(FromDatabase, Options),
    Dst = kzs_plan:plan(ToDatabase, Options),
    CopySpec = #copy_doc{source_dbname=FromDatabase
                        ,source_doc_id=FromId
                        ,dest_dbname=ToDatabase
                        ,dest_doc_id=ToId
                        },
    kzs_doc:copy_doc(Src, Dst, CopySpec, Options).

-spec move_doc(database_name(), docid(), database_name(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
move_doc(FromDB, FromId, ToDB, Options) ->
    move_doc(FromDB, FromId, ToDB, FromId, Options).

-spec move_doc(database_name(), docid(), database_name(), docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
move_doc(FromDB, {DocType, FromId}, ToDB, ToId, Options) ->
    move_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
move_doc(FromDB, FromId, ToDB, {DocType, ToId}, Options) ->
    move_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
move_doc(FromDB, FromId, ToDB, ToId, Options) ->
    FromDatabase = kzs_util:to_database(FromDB),
    ToDatabase = kzs_util:to_database(ToDB),

    Src = kzs_plan:plan(FromDatabase, Options),
    Dst = kzs_plan:plan(ToDatabase, Options),
    CopySpec = #copy_doc{source_dbname=FromDatabase
                        ,source_doc_id=FromId
                        ,dest_dbname=ToDatabase
                        ,dest_doc_id=ToId
                        },
    kzs_doc:move_doc(Src, Dst, CopySpec, Options).

%%------------------------------------------------------------------------------
%% @doc How many documents are chunked when doing a bulk save.
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert() -> pos_integer().
max_bulk_insert() ->
    kazoo_data_config:get_pos_integer(<<"max_bulk_insert">>, 2000).

%%------------------------------------------------------------------------------
%% @doc How many documents are chunked when doing a bulk read.
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_read() -> pos_integer().
max_bulk_read() ->
    kazoo_data_config:get_pos_integer(<<"max_bulk_read">>, 2000).

-spec max_bulk_read(view_options()) -> pos_integer().
max_bulk_read(ViewOptions) ->
    AskedFor = props:get_integer_value('max_bulk_read', ViewOptions, max_bulk_read()),
    UpperBound = min(AskedFor, max_bulk_read()),
    max(UpperBound, 1).

-spec db_classification(database_name()) -> db_classification().
db_classification(DBName) -> kzs_util:db_classification(DBName).

-spec format_error(any()) -> any().
format_error(Error) -> kzs_server:format_error(Error).

-spec maybe_add_doc_type(kz_term:ne_binary(), view_options()) -> view_options().
maybe_add_doc_type(DocType, Options) ->
    case props:get_value('doc_type', Options) of
        'undefined' -> [{'doc_type', DocType} | Options];
        _ -> Options
    end.

-spec maybe_add_doc_type_from_view(kz_term:ne_binary(), view_options()) -> view_options().
maybe_add_doc_type_from_view(ViewName, Options) ->
    case props:get_value('doc_type', Options) of
        'undefined' -> add_doc_type_from_view(ViewName, Options);
        _ -> Options
    end.

-spec add_doc_type_from_view(kz_term:ne_binary() | 'all_docs', view_options()) -> view_options().
add_doc_type_from_view('all_docs', Options) -> Options;
add_doc_type_from_view(View, Options) ->
    case binary:split(View, <<"/">>, ['global']) of
        [ViewType, ViewName] ->
            DocType = kzs_view:doc_type_from_view(ViewType, ViewName),
            [{'doc_type', DocType} | Options];
        _ -> Options
    end.

-spec init_dbs(map()) -> boolean().
init_dbs(Server) ->
    _ = suppress_change_notice(),
    Fun = fun(DB, Acc) -> [init_db(Server, DB) | Acc] end,
    Result = lists:foldl(Fun, [], ?KZ_SYSTEM_DBS),
    revise_docs_from_folder(?KZ_DATA_DB, ?APP, <<"views">>),
    _ = enable_change_notice(),
    lists:any(fun kz_term:is_true/1, Result).

-spec init_db(map(), database_name()) -> boolean().
init_db(Server, DbName) ->
    Database = kzs_util:to_database(DbName),

    case kzs_db:db_exists(Server, Database) of
        'true' -> 'false';
        'false' ->
            lager:info("creating database ~s", [Database]),
            kzs_db:db_create(Server, Database, [])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_views(atom(), views_listing() | [string()]) -> 'ok'.
register_views(App, Views) ->
    lists:foreach(fun(View) -> register_view(App, View) end, Views).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_view(atom(), view_listing() | string() | kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
register_view(App, {_, ViewJObj}=View) ->
    Validate = validate_view_map(kz_json:get_ne_json_value(<<"kazoo">>, ViewJObj)),
    maybe_register_view(View, App, Validate);
register_view(App, ViewName) ->
    register_view(App, kzs_util:get_view_json(App, ViewName)).

-spec maybe_register_view(view_listing(), atom(), {'error', any()} | {kz_term:ne_binary() | kz_json:objects()}) ->
          {'ok', kz_json:object()} |
          data_error().
maybe_register_view({<<"_design/", _Name/binary>>, _}, _App, {'error', _Reason}=Error) ->
    lager:error("can not register the view ~s for app ~s: ~s", [_Name, _App, _Reason]),
    Error;
maybe_register_view({<<"_design/", Name/binary>>, View}, App, {ClassId, ViewMaps}) ->
    Version = kz_util:application_version(App),
    AppName = kz_term:to_binary(App),
    DocId = <<ClassId/binary, "-", AppName/binary, "-", Name/binary>>,

    log_register_views(Name, DocId, App, ViewMaps),

    Update = [{[<<"kazoo">>], kz_json:from_list([{<<"view_map">>, ViewMaps}])}
             ,{[<<"view_definition">>], maybe_adapt_multilines(kz_json:delete_key(<<"kazoo">>, View))}
             ],
    ExtraUpdate = [{[<<"version">>], Version}],
    Create = [{[<<"application">>], AppName}
             ,{[<<"name">>], Name}
             ,{[<<"pvt_type">>], <<"view_definition">>}
             ],

    UpdateOptions = [{'update', Update}
                    ,{'extra_update', ExtraUpdate}
                    ,{'create', Create}
                    ],

    update_doc(?KZ_DATA_DB, DocId, UpdateOptions).

log_register_views(_, _, _, []) -> 'ok';
log_register_views(Name, DocId, App, [ViewMap | ViewMaps]) ->
    Dest = kz_json:get_value(<<"database">>, ViewMap, kz_json:get_value(<<"classification">>, ViewMap)),
    lager:debug("trying to register view ~s with id ~s for app ~s, with destination ~s"
               ,[Name, DocId, App, Dest]
               ),
    log_register_views(Name, DocId, App, ViewMaps).

-spec validate_view_map(kz_term:api_object()) ->
          {kz_term:ne_binary(), kz_json:objects()} |
          {'error', kz_term:ne_binary()}.
validate_view_map('undefined') ->
    {'error', <<"no_view_registration_info">>};
validate_view_map(JObj) ->
    ViewMap = kz_json:get_list_value(<<"view_map">>, JObj, []),
    validate_view_map(ViewMap, []).

-spec validate_view_map(kz_json:objects(), kz_json:objects() | {'error', kz_term:ne_binary()}) ->
          {kz_term:ne_binary(), kz_json:objects()} |
          {'error', any()}.
validate_view_map(_, {'error', _}=Error) ->
    Error;
validate_view_map([], []) ->
    validate_view_map('undefined');
validate_view_map([], [JObj]) ->
    DbOrClass = kz_json:get_value(<<"database">>
                                 ,JObj
                                 ,kz_json:get_value(<<"classification">>, JObj)
                                 ),
    {DbOrClass, [JObj]};
validate_view_map([], [_|_]=ViewMap) ->
    {<<"multi_db">>, ViewMap};
validate_view_map([JObj | JObjs], ViewMaps) ->
    Db = kz_json:get_ne_binary_value(<<"database">>, JObj),
    Class = kz_json:get_ne_binary_value(<<"classification">>, JObj),
    case kz_json:is_json_object(JObj) of
        'true' ->
            validate_view_map(JObjs, only_one_of(Db, Class, [JObj | ViewMaps]));
        'false' ->
            {'error', <<"not_valid_registration_info">>}
    end.

only_one_of('undefined', 'undefined', _Acc) ->
    {'error', <<"not_valid_registration_info">>};
only_one_of('undefined', _Class, Acc) ->
    Acc;
only_one_of(_DbName, 'undefined', Acc) ->
    Acc;
only_one_of(_DbName, _Class, _Acc) ->
    {'error', <<"database_and_classification_are_exclusive">>}.

%% @equiv register_views_from_folder(App, "views")
-spec register_views_from_folder(atom()) -> 'ok'.
register_views_from_folder(App) ->
    register_views_from_folder(App, "views").

%%------------------------------------------------------------------------------
%% @doc Read all database view JSON files from the private folder of the calling
%% application and register them in `system_data' database.
%% @end
%%------------------------------------------------------------------------------
-spec register_views_from_folder(atom(), nonempty_string()) -> 'ok'.
register_views_from_folder(App, Folder) ->
    Views = kzs_util:get_views_json(App, Folder),
    register_views(App, Views).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh_views(database_name()) ->
          boolean() |
          {'error', 'db_not_found' | 'invalid_db_name'}.
refresh_views(DbName) ->
    Database = kzs_util:to_database(DbName),
    maybe_refresh_system_db(Database, lists:member(Database, ?KZ_SYSTEM_DBS)).

-spec maybe_refresh_system_db(database_name(), boolean()) ->
          boolean() |
          {'error', 'db_not_found' | 'invalid_db_name'}.
maybe_refresh_system_db(Database, 'false') ->
    do_refresh_views(Database);
maybe_refresh_system_db(Database, 'true') ->
    case kzs_db:db_exists(kzs_plan:plan(Database), Database) of
        'true' ->
            do_refresh_views(Database);
        'false' ->
            lager:info("creating system database ~s", [Database]),
            kzs_db:db_create(kzs_plan:plan(Database), Database, []),
            do_refresh_views(Database)
    end.

-spec do_refresh_views(database_name()) ->
          boolean() |
          {'error', 'db_not_found' | 'invalid_db_name'}.
do_refresh_views(DbName) ->
    Database = kzs_util:to_database(DbName),

    suppress_change_notice(),
    Classification = kzs_util:db_classification(Database),
    lager:debug("updating views for db ~s:~s", [Classification, Database]),
    Updated = case view_definitions(Database, Classification) of
                  [] -> 'false';
                  Views ->
                      Encoded = kz_http_util:urlencode(kz_http_util:urldecode(Database)),
                      db_view_update(Encoded, Views)
              end,

    _ = case Updated of
            'true' ->
                lager:debug("~s:~s views updated", [Classification, Database]),
                kzs_publish:publish_db(Database, 'edited');
            'false' ->
                lager:debug("~s:~s no views updated", [Classification, Database]);
            {'error', 'db_not_found'}=Error ->
                lager:debug("~s: db '~s' not found", [Classification, Database]),
                Error
        end,
    enable_change_notice(),
    Updated.

-spec view_definitions(database_name(), atom() | kz_term:ne_binary()) -> views_listing().
view_definitions(DbName, Classification) ->
    ViewOptions = [kz_http_util:urldecode(DbName), kz_term:to_binary(Classification)],
    case get_result_docs(?KZ_DATA_DB, <<"views/views_by_classification">>, ViewOptions) of
        {'error', _} -> [];
        {'ok', JObjs} ->
            ViewDefs = [kz_json:get_json_value(<<"view_definition">>, JObj) || JObj <- JObjs],
            [{kz_doc:id(ViewDef), ViewDef} || ViewDef <- ViewDefs]
    end.
