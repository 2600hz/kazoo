%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Manage data connections
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_datamgr).

-export([db_classification/1]).

%% Settings-related
-export([max_bulk_insert/0]).

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
        ,update_cache_doc/3
        ,flush_cache_doc/2, flush_cache_doc/3
        ,flush_cache_docs/0, flush_cache_docs/1
        ,add_to_doc_cache/3
        ,open_doc/2,open_doc/3
        ,del_doc/2, del_docs/2
        ,del_doc/3, del_docs/3
        ,lookup_doc_rev/2, lookup_doc_rev/3
        ,update_doc/3, update_doc/4
        ,load_doc_from_file/3
        ,update_doc_from_file/3
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
        ,get_result_keys/1, get_result_keys/3
        ,get_single_result/3
        ,design_info/2
        ,design_compact/2
        ]).

-export([get_uuid/0, get_uuid/1
        ,get_uuids/1, get_uuids/2
        ]).
-export([suppress_change_notice/0
        ,enable_change_notice/0
        ,change_notice/0
        ]).

-export_type([view_option/0, view_options/0
             ,view_listing/0, views_listing/0
             ,data_error/0
             ]).

-include("kz_data.hrl").

-define(VALID_DBNAME(DbName),
        is_binary(DbName)
        andalso byte_size(DbName) > 0).

-define(UUID_SIZE, 16).

%%%===================================================================
%%% Couch Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a file into couch as a document (not an attachement)
%% @end
%%--------------------------------------------------------------------
-spec load_doc_from_file(ne_binary(), atom(), nonempty_string() | ne_binary()) ->
                                {'ok', kz_json:object()} |
                                data_error().
load_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", kz_util:to_list(File)]),
    lager:debug("read into db ~s from CouchDB JSON file: ~s", [DbName, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        save_doc(DbName, kz_json:decode(Bin)) %% if it crashes on the match, the catch will let us know
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("badmatch error reading ~s: ~p", [Path, Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception reading ~s: ~p", [Path, Reason]),
            {'error', Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Overwrite the existing contents of a document with the contents of
%% a file
%% @end
%%--------------------------------------------------------------------
-spec update_doc_from_file(ne_binary(), atom(), nonempty_string() | ne_binary()) ->
                                  {'ok', kz_json:object()} |
                                  data_error().
update_doc_from_file(DbName, App, File) when ?VALID_DBNAME(DbName) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    lager:debug("update db ~s from CouchDB file: ~s", [DbName, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        JObj = kz_json:decode(Bin),
        ensure_saved(DbName, JObj)
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("bad match: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end;
update_doc_from_file(DbName, App, File) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> load_doc_from_file(Db, App, File);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create or overwrite the existing contents of a document with the
%% contents of a file
%% @end
%%--------------------------------------------------------------------
-spec revise_doc_from_file(ne_binary(), atom(), ne_binary() | nonempty_string()) ->
                                  {'ok', kz_json:object()} |
                                  data_error().
revise_doc_from_file(DbName, App, File) ->
    case update_doc_from_file(DbName, App, File) of
        {'error', _E} ->
            lager:debug("failed to update doc: ~p", [_E]),
            load_doc_from_file(DbName, App, File);
        {'ok', _}=Resp ->
            lager:debug("revised ~s", [File]),
            Resp
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications priv/couchdb/views/ folder
%% into a given database
%% @end
%%--------------------------------------------------------------------
-spec revise_views_from_folder(ne_binary(), atom()) -> 'ok'.
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications folder, relative to
%% priv/couchdb/ into a given database
%% @end
%%--------------------------------------------------------------------
-spec revise_docs_from_folder(ne_binary(), atom(), ne_binary() | nonempty_string()) -> 'ok'.
-spec revise_docs_from_folder(ne_binary(), atom(), ne_binary() | nonempty_string(), boolean()) -> 'ok'.

revise_docs_from_folder(DbName, App, Folder) ->
    revise_docs_from_folder(DbName, App, Folder, 'true').

revise_docs_from_folder(DbName, App, Folder, Sleep) ->
    case code:priv_dir(App) of
        {'error', 'bad_name'} ->
            lager:error("tried to revise docs for db ~p for invalid priv directory. app: ~p", [DbName, App]);

        ValidDir ->
            Files = filelib:wildcard([ValidDir, "/couchdb/", kz_util:to_list(Folder), "/*.json"]),
            do_revise_docs_from_folder(DbName, Sleep, Files)
    end.

-spec do_revise_docs_from_folder(ne_binary(), boolean(), ne_binaries()) -> 'ok'.
do_revise_docs_from_folder(_, _, []) -> 'ok';
do_revise_docs_from_folder(DbName, Sleep, [H|T]) ->
    try
        {'ok', Bin} = file:read_file(H),
        JObj = maybe_adapt_multilines(kz_json:decode(Bin)),
        Sleep
            andalso timer:sleep(250),
        _ = ensure_saved(DbName, JObj),
        do_revise_docs_from_folder(DbName, Sleep, T)
    catch
        _:_ ->
            kz_util:log_stacktrace(),
            do_revise_docs_from_folder(DbName, Sleep, T)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Replaces multiline Javascript into single line, on the fly
%% while loading views from files.
%% @end
%%--------------------------------------------------------------------
-spec maybe_adapt_multilines(kz_json:object()) -> kz_json:object().
maybe_adapt_multilines(JObj) ->
    case kz_json:get_value(<<"views">>, JObj) of
        'undefined' -> JObj;
        Views ->
            NewViews =
                [{View, kz_json:foldl(fun inline_js_fun/3, kz_json:new(), Pairs)}
                 || {View, Pairs} <- kz_json:to_proplist(Views)
                ],
            kz_json:set_value(<<"views">>, kz_json:from_list(NewViews), JObj)
    end.

%% @private
-spec inline_js_fun(ne_binary(), ne_binaries() | kz_json:json_term(), kz_json:object()) ->
                           kz_json:object().
inline_js_fun(Type, Code=[<<"function",_/binary>>|_], Acc) ->
    kz_json:set_value(Type, iolist_to_binary(Code), Acc);
inline_js_fun(Type, Code, Acc) ->
    kz_json:set_value(Type, Code, Acc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load fixture files from a folder into a database, only if the id
%% isn't already existant
%% @end
%%--------------------------------------------------------------------
-spec load_fixtures_from_folder(ne_binary(), atom()) -> 'ok'.
load_fixtures_from_folder(DbName, App) ->
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", ?FIXTURES_FOLDER, "/*.json"]),
    do_load_fixtures_from_folder(DbName, Files).

-spec do_load_fixtures_from_folder(ne_binary(), ne_binaries()) -> 'ok'.
do_load_fixtures_from_folder(_, []) -> 'ok';
do_load_fixtures_from_folder(DbName, [F|Fs]) ->
    try
        {'ok', Bin} = file:read_file(F),
        FixJObj = kz_json:decode(Bin),
        FixId = kz_doc:id(FixJObj),
        case lookup_doc_rev(DbName, FixId) of
            {'ok', _Rev} ->
                lager:debug("fixture ~s exists in ~s: ~s", [FixId, DbName, _Rev]);
            {'error', 'not_found'} ->
                lager:debug("saving fixture ~s to ~s", [FixId, DbName]),
                save_doc(DbName, FixJObj);
            {'error', _Reason} ->
                lager:debug("failed to lookup rev for fixture: ~p: ~s in ~s", [_Reason, FixId, DbName])
        end
    catch
        _C:_R ->
            lager:debug("failed to check fixture: ~s: ~p", [_C, _R])
    end,
    do_load_fixtures_from_folder(DbName, Fs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_exists(text()) -> boolean().
db_exists(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_exists(kzs_plan:plan(DbName), DbName);
db_exists(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_exists(Db);
        {'error', _}=E -> E
    end.

-spec db_exists(text(), api_binary() | kz_proplist()) -> boolean().
db_exists(DbName, 'undefined') ->
    db_exists(DbName);
db_exists(DbName, Type)
  when ?VALID_DBNAME(DbName), is_binary(Type) ->
    case add_doc_type_from_view(Type, []) of
        [] -> db_exists(DbName, [{'doc_type', Type}]);
        Options -> db_exists(DbName, Options)
    end;
db_exists(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_exists(kzs_plan:plan(DbName, Options), DbName);
db_exists(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_exists(Db, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists, also checks other connections
%% @end
%%--------------------------------------------------------------------
-spec db_exists_all(text()) -> boolean().
db_exists_all(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_exists_all(kzs_plan:plan(DbName), DbName);
db_exists_all(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_exists_all(Db);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding all databases
%% @end
%%--------------------------------------------------------------------
-spec db_info() -> {'ok', ne_binaries()} |
                   data_error().
db_info() ->
    kzs_db:db_info(kzs_plan:plan()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec db_info(text()) -> {'ok', kz_json:object()} |
                         data_error().
db_info(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_info(kzs_plan:plan(DbName), DbName);
db_info(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_info(Db);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database design doc
%% @end
%%--------------------------------------------------------------------
-spec design_info(text(), ne_binary()) ->
                         {'ok', kz_json:object()} |
                         data_error().

design_info(DbName, DesignName) when ?VALID_DBNAME(DbName) ->
    kzs_view:design_info(kzs_plan:plan(DbName, DesignName), DbName, DesignName);
design_info(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> design_info(Db, DesignName);
        {'error', _}=E -> E
    end.

-spec design_compact(ne_binary(), ne_binary()) -> boolean().

design_compact(DbName, DesignName) when ?VALID_DBNAME(DbName)->
    kzs_view:design_compact(kzs_plan:plan(DbName, DesignName), DbName, DesignName);
design_compact(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> design_compact(Db, DesignName);
        {'error', _}=E -> E
    end.

-spec db_view_cleanup(ne_binary()) -> boolean().

db_view_cleanup(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_view_cleanup(kzs_plan:plan(DbName), DbName);
db_view_cleanup(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_view_cleanup(Db);
        {'error', _}=E -> E
    end.

-spec db_view_update(ne_binary(), views_listing()) -> boolean().
-spec db_view_update(ne_binary(), views_listing(), boolean()) -> boolean().

db_view_update(DbName, Views) ->
    db_view_update(DbName, Views, 'false').

db_view_update(DbName, Views0, Remove) when ?VALID_DBNAME(DbName) ->
    Views = lists:keymap(fun maybe_adapt_multilines/1, 2, Views0),
    kzs_db:db_view_update(kzs_plan:plan(DbName), DbName, Views, Remove);
db_view_update(DbName, Views, Remove) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_view_update(Db, Views, Remove);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Replicate a DB from one host to another
%%
%% Proplist:
%% [{<<"source">>, <<"http://some.couch.server:5984/source_db">>}
%%  ,{<<"target">>, <<"target_db">>}
%%
%%   IMPORTANT: Use the atom true, not binary <<"true">> (though it may be changing in couch to allow <<"true">>)
%%  ,{<<"create_target">>, true} % optional, creates the DB on target if non-existent
%%  ,{<<"continuous">>, true} % optional, continuously update target from source
%%  ,{<<"cancel">>, true} % optional, will cancel a replication (one-time or continuous)
%%
%%  ,{<<"filter">>, <<"source_design_doc/source_filter_name">>} % optional, filter what documents are sent from source to target
%%  ,{<<"query_params">>, {struct, [{<<"key1">>, <<"value1">>}, {<<"key2">>, <<"value2">>}]} } % optional, send params to filter function
%%  filter_fun: function(doc, req) -> boolean(); passed K/V pairs in query_params are in req in filter function
%%
%%  ,{<<"doc_ids">>, [<<"source_doc_id_1">>, <<"source_doc_id_2">>]} % optional, if you only want specific docs, no need for a filter
%%
%%  ,{<<"proxy">>, <<"http://some.proxy.server:12345">>} % optional, if you need to pass the replication via proxy to target
%%   https support for proxying is suspect
%% ].
%%
%% If authentication is needed at the source's end:
%% {<<"source">>, <<"http://user:password@some.couch.server:5984/source_db">>}
%%
%% If source or target DB is on the current connection, you can just put the DB name, e.g:
%% [{<<"source">>, <<"source_db">>}, {<<"target">>, <<"target_db">>}, ...]
%% Then you don't have to specify the auth creds (if any) for the connection
%%
%% @end
%%--------------------------------------------------------------------
-spec db_replicate(kz_proplist() | kz_json:object()) ->
                          {'ok', kz_json:object()} |
                          data_error().
db_replicate(Prop) when is_list(Prop) ->
    db_replicate(kz_json:from_list(Prop));
db_replicate(JObj) ->
    kzs_db:db_replicate(kzs_plan:plan(), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_create(text()) -> boolean().
-spec db_create(text(), kzs_db:db_create_options()) -> boolean().

db_create(DbName) ->
    db_create(DbName, []).

db_create(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_create(kzs_plan:plan(DbName), DbName, Options);
db_create(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_create(Db, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec db_compact(text()) -> boolean().

db_compact(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_compact(kzs_plan:plan(DbName), DbName);
db_compact(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_compact(Db);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete a database (takes an 'encoded' DbName)
%% @end
%%--------------------------------------------------------------------
-spec db_delete(text()) -> 'ok' | data_error().
db_delete(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_delete(kzs_plan:plan(DbName), DbName);
db_delete(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_delete(Db);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Archive a database (takes an 'encoded' DbName)
%% @end
%%--------------------------------------------------------------------
-spec db_archive(ne_binary()) -> 'ok' | data_error().
-spec db_archive(ne_binary(), ne_binary()) -> 'ok' | data_error().
db_archive(DbName) ->
    Folder = kapps_config:get(?CONFIG_CAT, <<"default_archive_folder">>, <<"/tmp">>),
    db_archive(DbName, filename:join([<<Folder/binary, "/", DbName/binary, ".json">>])).

db_archive(DbName, Filename) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_archive(kzs_plan:plan(DbName), DbName, Filename);
db_archive(DbName, Filename) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_archive(Db, Filename);
        {'error', _}=E -> E
    end.

-spec db_import(ne_binary(), file:filename_all()) -> 'ok' | data_error().
db_import(DbName, ArchiveFile) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_import(kzs_plan:plan(DbName), DbName, ArchiveFile);
db_import(DbName, ArchiveFile) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_import(Db, ArchiveFile);
        {'error', _}=E -> E
    end.

%%%===================================================================
%%% Document Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch a cached doc or open it if not available.
%% @end
%%--------------------------------------------------------------------
-spec open_cache_doc(text(), docid()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.
-spec open_cache_doc(text(), docid(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            {'error', 'not_found'}.

open_cache_doc(DbName, {DocType, DocId}) ->
    open_cache_doc(DbName, DocId, [{'doc_type', DocType}]);
open_cache_doc(DbName, DocId) ->
    open_cache_doc(DbName, DocId, []).

open_cache_doc(DbName, {DocType, DocId}, Options) ->
    open_cache_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_cache_doc(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    kzs_cache:open_cache_doc(DbName, DocId, Options);
open_cache_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> open_cache_doc(Db, DocId, Options);
        {'error', _}=E -> E
    end.

-spec add_to_doc_cache(text(), ne_binary(), kz_json:object()) ->
                              {'ok', kz_json:objects()} |
                              data_error().
add_to_doc_cache(DbName, DocId, Doc) when ?VALID_DBNAME(DbName) ->
    kzs_cache:add_to_doc_cache(DbName, DocId, Doc);
add_to_doc_cache(DbName, DocId, Doc) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> add_to_doc_cache(Db, DocId, Doc);
        {'error', _}=E -> E
    end.

-spec update_cache_doc(text(), ne_binary(), fun((kz_json:object()) -> kz_json:object() | 'skip')) ->
                              {'ok', kz_json:object()}
                                  | data_error().
update_cache_doc(DbName, DocId, Fun) when is_function(Fun, 1) ->
    case open_cache_doc(DbName, DocId) of
        {'ok', JObj} ->
            NewJObj = Fun(JObj),
            maybe_save_doc(DbName, NewJObj, JObj);
        {'error', _Reason} = Else ->
            lager:error("Can't open doc ~s/~s coz ~p", [DbName, DocId, _Reason]),
            Else
    end.

-spec maybe_save_doc(text(), kz_json:object() | 'skip', kz_json:object()) ->
                            {'ok', kz_json:object() | kz_json:objects()} |
                            data_error().
maybe_save_doc(_DbName, 'skip', Jobj) ->
    {'ok', Jobj};
maybe_save_doc(DbName, JObj, _OldJobj) ->
    save_doc(DbName, JObj).

-spec flush_cache_doc(ne_binary(), ne_binary() | kz_json:object()) ->
                             'ok' |
                             {'error', 'invalid_db_name'}.
flush_cache_doc(DbName, Doc) ->
    flush_cache_doc(DbName, Doc, []).

-spec flush_cache_doc(ne_binary(), ne_binary() | kz_json:object(), kz_proplist()) ->
                             'ok' |
                             {'error', 'invalid_db_name'}.
flush_cache_doc(DbName, Doc, Options) when ?VALID_DBNAME(DbName) ->
    kzs_cache:flush_cache_doc(DbName, Doc, Options);
flush_cache_doc(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> flush_cache_doc(Db, Doc, Options);
        {'error', _}=E -> E
    end.

-spec flush_cache_docs() -> 'ok'.
-spec flush_cache_docs(text()) -> 'ok' | {'error', 'invalid_db_name'}.
flush_cache_docs() -> kzs_cache:flush_cache_docs().
flush_cache_docs(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> kzs_cache:flush_cache_docs(Db);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------
-spec open_doc(text(), docid()) ->
                      {'ok', kz_json:object()} |
                      data_error() |
                      {'error', 'not_found'}.
-spec open_doc(text(), docid(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error() |
                      {'error', 'not_found'}.

open_doc(DbName, {DocType, DocId}) ->
    open_doc(DbName, DocId, [{'doc_type', DocType}]);
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

open_doc(DbName, {DocType, DocId}, Options) ->
    open_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_doc(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:open_doc(kzs_plan:plan(DbName, Options), DbName, DocId, Options);
open_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> open_doc(Db, DocId, Options);
        {'error', _}=E -> E
    end.

-spec all_docs(text()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
-spec all_docs(text(), kz_proplist()) ->
                      {'ok', kz_json:objects()} |
                      data_error().

all_docs(DbName) ->
    all_docs(DbName, []).

all_docs(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_view:all_docs(kzs_plan:plan(DbName, Options), DbName, Options);
all_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> all_docs(Db, Options);
        {'error', _}=E -> E
    end.

-spec db_list() -> {'ok', ne_binaries()} | data_error().
-spec db_list(kz_proplist()) -> {'ok', ne_binaries()} | data_error().

db_list() ->
    db_list([]).

db_list(Options) ->
    kzs_db:db_list(kzs_plan:plan(), Options).

-spec all_design_docs(text()) -> {'ok', kz_json:objects()} |
                                 data_error().
-spec all_design_docs(text(), kz_proplist()) -> {'ok', kz_json:objects()} |
                                                data_error().

all_design_docs(DbName) ->
    all_design_docs(DbName, []).

all_design_docs(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_view:all_design_docs(kzs_plan:plan(DbName), DbName, Options);
all_design_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> all_design_docs(Db, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec lookup_doc_rev(text(), docid()) ->
                            {'ok', ne_binary()} |
                            data_error().
lookup_doc_rev(DbName, {DocType, DocId}) ->
    lookup_doc_rev(DbName, DocId, [{'doc_type', DocType}]);
lookup_doc_rev(DbName, DocId) ->
    lookup_doc_rev(DbName, DocId, []).

-spec lookup_doc_rev(text(), docid(), kz_proplist()) ->
                            {'ok', ne_binary()} | data_error().
lookup_doc_rev(DbName, {DocType, DocId}, Options) ->
    lookup_doc_rev(DbName, DocId, maybe_add_doc_type(DocType, Options));
lookup_doc_rev(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:lookup_doc_rev(kzs_plan:plan(DbName, Options), DbName, DocId);
lookup_doc_rev(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> lookup_doc_rev(Db, DocId, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save document to database
%% @end
%%--------------------------------------------------------------------
-spec save_doc(text(), kz_json:object() | kz_json:objects()) ->
                      {'ok', kz_json:object() | kz_json:objects()} |
                      data_error().
save_doc(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save a document. If it fails because of conflict, pulls latest
%% revision and tries saving again. Otherwise return.
%% @end
%%--------------------------------------------------------------------
-spec ensure_saved(text(), kz_json:object()) ->
                          {'ok', kz_json:object()} |
                          data_error().
-spec ensure_saved(text(), kz_json:object(), kz_proplist()) ->
                          {'ok', kz_json:object()} |
                          data_error().

ensure_saved(DbName, Doc) ->
    ensure_saved(DbName, Doc, []).

ensure_saved(DbName, Doc, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:ensure_saved(kzs_plan:plan(DbName, Doc), DbName, Doc, Options);
ensure_saved(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> ensure_saved(Db, Doc, Options);
        {'error', _}=E -> E
    end.

-spec save_doc(text(), kz_json:object(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
save_doc(DbName, Doc, Options) when ?VALID_DBNAME(DbName) ->
    OldSetting = maybe_toggle_publish(Options),
    Result = kzs_doc:save_doc(kzs_plan:plan(DbName, Doc), DbName, Doc, Options),
    maybe_revert_publish(OldSetting),
    Result;
save_doc(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> save_doc(Db, Doc, Options);
        {'error', _}=E -> E
    end.

-spec maybe_toggle_publish(kz_proplist()) -> boolean().
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

-spec save_docs(text(), kz_json:objects()) ->
                       {'ok', kz_json:objects()} |
                       data_error().
-spec save_docs(text(), kz_json:objects(), kz_proplist()) ->
                       {'ok', kz_json:objects()} |
                       data_error().

save_docs(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []).

save_docs(DbName, [Doc|_]=Docs, Options)
  when is_list(Docs), ?VALID_DBNAME(DbName) ->
    OldSetting = maybe_toggle_publish(Options),
    Result = kzs_doc:save_docs(kzs_plan:plan(DbName, Doc), DbName, Docs, Options),
    maybe_revert_publish(OldSetting),
    Result;
save_docs(DbName, Docs, Options) when is_list(Docs) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> save_docs(Db, Docs, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch, update and save a doc (creating if not present)
%% @end
%%--------------------------------------------------------------------
-spec update_doc(ne_binary(), docid(), kz_proplist()) ->
                        {'ok', kz_json:object()} |
                        data_error().
-spec update_doc(ne_binary(), docid(), kz_proplist(), kz_proplist()) ->
                        {'ok', kz_json:object()} |
                        data_error().

update_doc(DbName, Id, UpdateProps) ->
    update_doc(DbName, Id, UpdateProps, []).

update_doc(DbName, Id, UpdateProps, CreateProps) when is_list(UpdateProps),
                                                      is_list(CreateProps) ->
    case open_doc(DbName, Id) of
        {'error', 'not_found'} ->
            JObj = kz_json:from_list(lists:append([[{<<"_id">>, Id} | CreateProps], UpdateProps])),
            save_doc(DbName, JObj);
        {'error', _}=E -> E;
        {'ok', JObj}=Ok ->
            case kz_json:set_values(UpdateProps, JObj) of
                JObj -> Ok;
                UpdatedJObj -> save_doc(DbName, UpdatedJObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec del_doc(text(), kz_json:object() | kz_json:objects() | ne_binary()) ->
                     {'ok', kz_json:objects()} |
                     data_error().
del_doc(DbName, Doc) ->
    del_doc(DbName, Doc, []).

-spec del_doc(text(), kz_json:object() | kz_json:objects() | ne_binary(), kz_proplist()) ->
                     {'ok', kz_json:objects()} |
                     data_error().
del_doc(DbName, Doc, Options) when is_list(Doc) ->
    del_docs(DbName, Doc, Options);
del_doc(DbName, Doc, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:del_doc(kzs_plan:plan(DbName, Doc), DbName, Doc, Options);
del_doc(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> del_doc(Db, Doc, Options);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
-spec del_docs(text(), kz_json:objects() | ne_binaries()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
del_docs(DbName, Docs) ->
    del_docs(DbName, Docs, []).

-spec del_docs(text(), kz_json:objects() | ne_binaries(), kz_proplist()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
del_docs(DbName, Docs, Options)
  when is_list(Docs), ?VALID_DBNAME(DbName) ->
    kzs_doc:del_docs(kzs_plan:plan(DbName), DbName, Docs, Options);
del_docs(DbName, Docs, Options) when is_list(Docs) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> del_docs(Db, Docs, Options);
        {'error', _}=E -> E
    end.

%%%===================================================================
%%% Attachment Functions
%%%===================================================================

-spec fetch_attachment(text(), docid(), ne_binary()) ->
                              {'ok', binary()} |
                              data_error().

-spec fetch_attachment(text(), docid(), ne_binary(), kz_proplist()) ->
                              {'ok', binary()} |
                              data_error().
fetch_attachment(DbName, {DocType, DocId}, AName) ->
    fetch_attachment(DbName, DocId, AName, [{'doc_type', DocType}]);
fetch_attachment(DbName, DocId, AName) ->
    fetch_attachment(DbName, DocId, AName, []).

fetch_attachment(DbName, {DocType, DocId}, AName, Options) when ?VALID_DBNAME(DbName) ->
    fetch_attachment(DbName, DocId, AName, maybe_add_doc_type(DocType, Options));
fetch_attachment(DbName, DocId, AName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_attachments:fetch_attachment(kzs_plan:plan(DbName, Options), DbName, DocId, AName);
fetch_attachment(DbName, DocId, AName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> fetch_attachment(Db, DocId, AName, Options);
        {'error', _}=E -> E
    end.

-spec stream_attachment(text(), docid(), ne_binary()) ->
                               {'ok', reference()} |
                               {'error', any()}.
stream_attachment(DbName, DocId, AName) ->
    stream_attachment(DbName, DocId, AName, []).

-spec stream_attachment(text(), docid(), ne_binary(), kz_proplist()) ->
                               {'ok', reference()} |
                               {'error', any()}.
stream_attachment(DbName, DocId, AName, Options) ->
    stream_attachment(DbName, DocId, AName, Options, self()).

-spec stream_attachment(text(), docid(), ne_binary(), kz_proplist(), pid()) ->
                               {'ok', reference()} |
                               {'error', any()}.
stream_attachment(DbName, {DocType, DocId}, AName, Options, Pid) when ?VALID_DBNAME(DbName) ->
    stream_attachment(DbName, DocId, AName, maybe_add_doc_type(DocType, Options), Pid);
stream_attachment(DbName, DocId, AName, Options, Pid) when ?VALID_DBNAME(DbName) ->
    kzs_attachments:stream_attachment(kzs_plan:plan(DbName, Options), DbName, DocId, AName, Pid);
stream_attachment(DbName, DocId, AName, Options, Pid) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> stream_attachment(Db, DocId, AName, Options, Pid);
        {'error', _}=E -> E
    end.

-spec put_attachment(text(), docid(), ne_binary(), ne_binary()) ->
                            {'ok', kz_json:object()} |
                            data_error().
%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec put_attachment(text(), docid(), ne_binary(), ne_binary(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error().
put_attachment(DbName, DocId, AName, Contents) ->
    put_attachment(DbName, DocId, AName, Contents, []).

put_attachment(DbName, {DocType, DocId}, AName, Contents, Options) ->
    put_attachment(DbName, DocId, AName, Contents, maybe_add_doc_type(DocType, Options));
put_attachment(DbName, DocId, AName, Contents, Options) when ?VALID_DBNAME(DbName) ->
    case attachment_options(DbName, DocId, Options) of
        {'ok', NewOptions} -> kzs_attachments:put_attachment(kzs_plan:plan(DbName, NewOptions)
                                                            ,DbName
                                                            ,DocId
                                                            ,AName
                                                            ,Contents
                                                            ,props:delete('plan_override', NewOptions)
                                                            );
        {'error', _} = Error -> Error
    end;
put_attachment(DbName, DocId, AName, Contents, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> put_attachment(Db, DocId, AName, Contents, Options);
        {'error', _}=E -> E
    end.

-spec delete_attachment(text(), ne_binary(), ne_binary()) ->
                               {'ok', kz_json:object()} |
                               data_error().
-spec delete_attachment(text(), ne_binary(), ne_binary(), kz_proplist()) ->
                               {'ok', kz_json:object()} |
                               data_error().
delete_attachment(DbName, DocId, AName) ->
    delete_attachment(DbName, DocId, AName, []).

delete_attachment(DbName, DocId, AName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_attachments:delete_attachment(kzs_plan:plan(DbName, DocId), DbName, DocId, AName, Options);
delete_attachment(DbName, DocId, AName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> delete_attachment(Db, DocId, AName, Options);
        {'error', _}=E -> E
    end.

-spec attachment_url(text(), docid(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'proxy', tuple()} |
                            {'error', any()}.
-spec attachment_url(text(), docid(), ne_binary(), kz_proplist()) ->
                            {'ok', ne_binary()} |
                            {'proxy', tuple()} |
                            {'error', any()}.
attachment_url(DbName, DocId, AttachmentId) ->
    attachment_url(DbName, DocId, AttachmentId, []).

attachment_url(DbName, {DocType, DocId}, AttachmentId, Options) when ?VALID_DBNAME(DbName) ->
    attachment_url(DbName, DocId, AttachmentId, maybe_add_doc_type(DocType, Options));
attachment_url(DbName, DocId, AttachmentId, Options) when ?VALID_DBNAME(DbName) ->
    Plan = kzs_plan:plan(DbName, Options),
    case kzs_doc:open_doc(Plan, DbName, DocId, props:delete('plan_override', Options)) of
        {'ok', JObj} ->
            NewOptions = [{'rev', kz_doc:revision(JObj)}
                          | maybe_add_doc_type(kz_doc:type(JObj), Options)
                         ],
            Handler = kz_doc:attachment_property(JObj, AttachmentId, <<"handler">>),
            kzs_attachments:attachment_url(Plan, DbName, DocId, AttachmentId, Handler, NewOptions);
        {'error', _} = Error -> Error
    end;
attachment_url(DbName, DocId, AttachmentId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> attachment_url(Db, DocId, AttachmentId, Options);
        {'error', _}=E -> E
    end.

%%%===================================================================
%%% Attachment Helper Functions
%%%===================================================================

attachment_options(DbName, DocId, Options) ->
    RequiredOptions = [{'doc_type', fun kz_doc:type/1}
                      ,{'rev', fun kz_doc:revision/1}
                      ],
    attachment_options(DbName, DocId, Options, RequiredOptions).

attachment_options(DbName, DocId, Options, RequiredOptions) ->
    Fun = fun() -> case open_cache_doc(DbName, DocId, props:delete('plan_override', Options)) of
                       {'ok', JObj} -> JObj;
                       _ -> kz_json:new()
                   end
          end,
    case maybe_add_required_options(Options, RequiredOptions, Fun) of
        {'ok', _} = Ok -> Ok;
        {'error', _} = Error -> log_attachment_options(Error)
    end.

log_attachment_options({'error', Missing}=Error) ->
    lager:error("missing required options : ~p", [Missing]),
    Error.

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
                end, [], RequiredOptions).

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

%%%===================================================================
%%% View Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the results of the view
%% {Total, Offset, Meta, Rows}
%% @end
%%--------------------------------------------------------------------
-spec get_all_results(ne_binary(), ne_binary()) -> get_results_return().
-spec get_results(ne_binary(), ne_binary()) -> get_results_return().
-spec get_results(ne_binary(), ne_binary(), view_options()) -> get_results_return().
-spec get_results_count(ne_binary(), ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               data_error().
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

get_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

get_results(DbName, DesignDoc, Options) when ?VALID_DBNAME(DbName) ->
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    Plan = kzs_plan:plan(DbName, Opts),

    case kzs_view:get_results(Plan, DbName, DesignDoc, Options) of
        {'error', 'not_found'} ->
            maybe_create_view(DbName, Plan, DesignDoc, Options);

        Other ->
            Other
    end;

get_results(DbName, DesignDoc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> get_results(Db, DesignDoc, Options);
        {'error', _}=E -> E
    end.

get_results_count(DbName, DesignDoc, Options) ->
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    kzs_view:get_results_count(kzs_plan:plan(DbName, Opts), DbName, DesignDoc, Options).

-spec maybe_create_view(ne_binary(), map(), ne_binary(), view_options()) -> get_results_return().
maybe_create_view(DbName, Plan, DesignDoc, Options) ->
    case props:get_value('view_json', Options) of
        'undefined' ->
            {'error', 'not_found'};

        ViewJson ->
            db_view_update(DbName, ViewJson),
            kzs_view:get_results(Plan, DbName, DesignDoc, Options)
    end.

-spec get_result_keys(ne_binary(), ne_binary(), view_options()) ->
                             {'ok', ne_binaries()} | data_error().
get_result_keys(DbName, DesignDoc, Options) ->
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    case kzs_view:get_results(kzs_plan:plan(DbName, Opts), DbName, DesignDoc, Options) of
        {'ok', JObjs} -> {'ok', get_result_keys(JObjs)};
        {'error', _} = Error -> Error
    end.

-spec get_result_keys(kz_json:objects()) -> kz_json:path().
get_result_keys(JObjs) ->
    [kz_json:get_value(<<"key">>, JObj)
     || JObj <- JObjs
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets the only result of a view.
%% If no result is found: returns `{error, not_found}'.
%% If more than one result is found, either:
%% - if `Options' contains `first_when_multiple'
%%     then the first one will be returned;
%% - otherwise `{error, multiple_results}' is returned.
%% @end
%%--------------------------------------------------------------------
-spec get_single_result(ne_binary(), ne_binary(), view_options()) ->
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

-spec get_uuid() -> ne_binary().
-spec get_uuid(pos_integer()) -> ne_binary().
get_uuid() -> get_uuid(?UUID_SIZE).
get_uuid(N) -> kz_util:rand_hex_binary(N).

-spec get_uuids(pos_integer()) -> ne_binaries().
-spec get_uuids(pos_integer(), pos_integer()) -> ne_binaries().
get_uuids(Count) -> get_uuids(Count, ?UUID_SIZE).
get_uuids(Count, Size) -> [get_uuid(Size) || _ <- lists:seq(1, Count)].

%%%===================================================================
%%% Misc functions
%%%===================================================================

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


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTE: the attempt to correct the dbname is not very erlang like, but
%%  since there are more places that expect an error and do not
%%  handle a crash appropriately/gracefully this is a quick solution...
%% @end
%%--------------------------------------------------------------------
-spec maybe_convert_dbname(text()) ->
                                  {'ok', ne_binary()} |
                                  {'error', 'invalid_db_name'}.
maybe_convert_dbname(DbName) ->
    case kz_util:is_empty(DbName) of
        'true' -> {'error', 'invalid_db_name'};
        'false' -> {'ok', kz_util:to_binary(DbName)}
    end.

-spec copy_doc(ne_binary(), docid(), ne_binary(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
-spec copy_doc(ne_binary(), docid(), ne_binary(), docid(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
copy_doc(FromDB, FromId, ToDB, Options) ->
    copy_doc(FromDB, FromId, ToDB, FromId, Options).

copy_doc(FromDB, {DocType, FromId}, ToDB, ToId, Options) ->
    copy_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
copy_doc(FromDB, FromId, ToDB, {DocType, ToId}, Options) ->
    copy_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
copy_doc(FromDB, FromId, ToDB, ToId, Options) ->
    Src = kzs_plan:plan(FromDB, Options),
    Dst = kzs_plan:plan(ToDB, Options),
    CopySpec = #copy_doc{source_dbname=FromDB
                        ,source_doc_id=FromId
                        ,dest_dbname=ToDB
                        ,dest_doc_id=ToId
                        },
    kzs_doc:copy_doc(Src, Dst, CopySpec, Options).

-spec move_doc(ne_binary(), docid(), ne_binary(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
-spec move_doc(ne_binary(), docid(), ne_binary(), docid(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
move_doc(FromDB, FromId, ToDB, Options) ->
    move_doc(FromDB, FromId, ToDB, FromId, Options).

move_doc(FromDB, {DocType, FromId}, ToDB, ToId, Options) ->
    move_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
move_doc(FromDB, FromId, ToDB, {DocType, ToId}, Options) ->
    move_doc(FromDB, FromId, ToDB, ToId, maybe_add_doc_type(DocType, Options));
move_doc(FromDB, FromId, ToDB, ToId, Options) ->
    Src = kzs_plan:plan(FromDB, Options),
    Dst = kzs_plan:plan(ToDB, Options),
    CopySpec = #copy_doc{source_dbname=FromDB
                        ,source_doc_id=FromId
                        ,dest_dbname=ToDB
                        ,dest_doc_id=ToId
                        },
    kzs_doc:move_doc(Src, Dst, CopySpec, Options).

%%------------------------------------------------------------------------------
%% @public
%% @doc How many documents are chunked when doing a bulk save
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert() -> ?MAX_BULK_INSERT.
max_bulk_insert() -> ?MAX_BULK_INSERT.

-spec db_classification(text()) -> db_classifications().
db_classification(DBName) -> kzs_util:db_classification(DBName).

-spec format_error(any()) -> any().
format_error(Error) -> kzs_server:format_error(Error).

-spec maybe_add_doc_type(ne_binary(), view_options()) -> view_options().
maybe_add_doc_type(DocType, Options) ->
    case props:get_value('doc_type', Options) of
        'undefined' -> [{'doc_type', DocType} | Options];
        _ -> Options
    end.

-spec maybe_add_doc_type_from_view(ne_binary(), view_options()) -> view_options().
maybe_add_doc_type_from_view(ViewName, Options) ->
    case props:get_value('doc_type', Options) of
        'undefined' -> add_doc_type_from_view(ViewName, Options);
        _ -> Options
    end.

-spec add_doc_type_from_view(ne_binary(), view_options()) -> view_options().
add_doc_type_from_view(View, Options) ->
    case binary:split(View, <<"/">>, ['global']) of
        [ViewType, ViewName] ->
            DocType = kzs_view:doc_type_from_view(ViewType, ViewName),
            [{'doc_type', DocType} | Options];
        _ -> Options
    end.
