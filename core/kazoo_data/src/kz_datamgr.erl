%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Manage data connections.
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

-export([refresh_views/1]).
-export([register_view/2
        ]).
-export([register_views/2
        ]).
-export([register_views_from_folder/1
        ,register_views_from_folder/2
        ]).

-type update_option() :: {'update', kz_json:flat_proplist()} |
                         {'create', kz_json:flat_proplist()} |
                         {'extra_update', kz_json:flat_proplist()} |
                         {'ensure_saved', boolean()}.
-type update_options() :: [update_option()].

-export_type([view_option/0, view_options/0
             ,view_listing/0, views_listing/0
             ,data_error/0, data_errors/0
             ,db_classification/0
             ,update_option/0, update_options/0
             ]).

-deprecated({'ensure_saved', '_', 'eventually'}).

-include("kz_data.hrl").

-define(VALID_DBNAME(DbName), is_binary(DbName), byte_size(DbName) > 0).

-define(UUID_SIZE, 16).

%%%=============================================================================
%%% Couch Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Overwrite the existing contents of a document with the contents of
%% a file.
%% @end
%%------------------------------------------------------------------------------
-spec update_doc_from_file(kz_term:ne_binary(), nonempty_string() | kz_term:ne_binary()) ->
                                  {'ok', kz_json:object()} |
                                  data_error().
update_doc_from_file(DbName, Path) when ?VALID_DBNAME(DbName) ->
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
    end;
update_doc_from_file(DbName, File) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> update_doc_from_file(Db, File);
        {'error', _}=E -> E
    end.

-spec maybe_update_doc_from_file(kz_term:ne_binary(), kz_json:object(), any()) ->
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
-spec revise_doc_from_file(kz_term:ne_binary(), atom(), kz_term:ne_binary() | nonempty_string()) ->
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
-spec revise_views_from_folder(kz_term:ne_binary(), atom()) -> 'ok'.
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%------------------------------------------------------------------------------
%% @doc Loads all `.json' files in an applications folder, relative to
%% `priv/couchdb/' into a given database
%% @end
%%------------------------------------------------------------------------------
-spec revise_docs_from_folder(kz_term:ne_binary(), atom(), kz_term:ne_binary() | nonempty_string()) -> 'ok'.
revise_docs_from_folder(DbName, App, Folder) ->
    revise_docs_from_folder(DbName, App, Folder, 'false').

-spec revise_docs_from_folder(kz_term:ne_binary(), atom(), kz_term:ne_binary() | nonempty_string(), boolean()) -> 'ok'.
revise_docs_from_folder(DbName, App, Folder, Sleep) ->
    case code:priv_dir(App) of
        {'error', 'bad_name'} ->
            lager:error("tried to revise docs for db ~p for invalid priv directory. app: ~p", [DbName, App]);
        ValidDir ->
            Files = filelib:wildcard([ValidDir, "/couchdb/", kz_term:to_list(Folder), "/*.json"]),
            lager:debug("refreshing ~B documents in db ~s for ~s", [length(Files), DbName, App]),
            do_revise_docs_from_folder(DbName, Sleep, Files)
    end.

-spec do_revise_docs_from_folder(kz_term:ne_binary(), boolean(), kz_term:ne_binaries()) -> 'ok'.
do_revise_docs_from_folder(_, _, []) -> 'ok';
do_revise_docs_from_folder(DbName, Sleep, [H|T]) ->
    try
        {'ok', _} = update_doc_from_file(DbName, H),
        Sleep
            andalso timer:sleep(250),
        do_revise_docs_from_folder(DbName, Sleep, T)
    catch
        _:_ ->
            kz_util:log_stacktrace(),
            do_revise_docs_from_folder(DbName, Sleep, T)
    end.

-spec maybe_update_doc(kz_term:ne_binary(), kz_json:object()) ->
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

-spec should_update(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_boolean().
should_update(DbName, JObj) ->
    should_update(DbName, JObj, open_doc(DbName, kz_doc:id(JObj))).

-spec should_update(kz_term:ne_binary(), kz_json:object(), any()) -> kz_term:api_boolean().
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
    case kz_json:get_value(<<"views">>, JObj) of
        'undefined' -> JObj;
        Views ->
            NewViews =
                [{View, kz_json:foldl(fun inline_js_fun/3, kz_json:new(), Pairs)}
                 || {View, Pairs} <- kz_json:to_proplist(Views)
                ],
            kz_json:set_value(<<"views">>, kz_json:from_list(NewViews), JObj)
    end.

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
-spec load_fixtures_from_folder(kz_term:ne_binary(), atom()) -> 'ok'.
load_fixtures_from_folder(DbName, App) ->
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", ?FIXTURES_FOLDER, "/*.json"]),
    do_load_fixtures_from_folder(DbName, Files).

-spec do_load_fixtures_from_folder(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
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
                _ = save_doc(DbName, FixJObj),
                lager:debug("saved fixture");
            {'error', _Reason} ->
                lager:debug("failed to lookup rev for fixture: ~p: ~s in ~s", [_Reason, FixId, DbName])
        end
    catch
        _C:_R ->
            lager:debug("failed to check fixture: ~s: ~p", [_C, _R])
    end,
    do_load_fixtures_from_folder(DbName, Fs).

%%------------------------------------------------------------------------------
%% @doc Determines if a database exists.
%% @end
%%------------------------------------------------------------------------------
-spec db_exists(kz_term:text()) -> boolean().
db_exists(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_exists(kzs_plan:plan(DbName), DbName);
db_exists(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_exists(Db);
        {'error', _}=E -> E
    end.

-spec db_exists(kz_term:text(), kz_term:api_binary() | kz_term:proplist()) -> boolean().
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

%%------------------------------------------------------------------------------
%% @doc Determines if a database exists, also checks other connections.
%% @end
%%------------------------------------------------------------------------------
-spec db_exists_all(kz_term:text()) -> boolean().
db_exists_all(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_exists_all(kzs_plan:plan(DbName), DbName);
db_exists_all(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_exists_all(Db);
        {'error', _}=E -> E
    end.

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
-spec db_info(kz_term:text()) -> {'ok', kz_json:object()} |
                                 data_error().
db_info(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_info(kzs_plan:plan(DbName), DbName);
db_info(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_info(Db);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Retrieve information regarding a database design doc.
%% @end
%%------------------------------------------------------------------------------
-spec design_info(kz_term:text(), kz_term:ne_binary()) ->
                         {'ok', kz_json:object()} |
                         data_error().

design_info(DbName, DesignName) when ?VALID_DBNAME(DbName) ->
    kzs_view:design_info(kzs_plan:plan(DbName, DesignName), DbName, DesignName);
design_info(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> design_info(Db, DesignName);
        {'error', _}=E -> E
    end.

-spec design_compact(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().

design_compact(DbName, DesignName) when ?VALID_DBNAME(DbName)->
    kzs_view:design_compact(kzs_plan:plan(DbName, DesignName), DbName, DesignName);
design_compact(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> design_compact(Db, DesignName);
        {'error', _}=E -> E
    end.

-spec db_view_cleanup(kz_term:ne_binary()) -> boolean().

db_view_cleanup(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_view_cleanup(kzs_plan:plan(DbName), DbName);
db_view_cleanup(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_view_cleanup(Db);
        {'error', _Reason} ->
            lager:debug("failed to convert db name ~s: ~p", [DbName, _Reason]),
            'false'
    end.

-spec db_view_update(kz_term:ne_binary(), views_listing()) ->
                            boolean() |
                            {'error', 'invalid_db_name'}.
db_view_update(DbName, Views) ->
    db_view_update(DbName, Views, 'false').

-spec db_view_update(kz_term:ne_binary(), views_listing(), boolean()) ->
                            boolean() |
                            {'error', 'invalid_db_name'}.
db_view_update(DbName, Views0, Remove) when ?VALID_DBNAME(DbName) ->
    case lists:keymap(fun maybe_adapt_multilines/1, 2, Views0) of
        [] -> 'false';
        Views ->
            kzs_db:db_view_update(kzs_plan:plan(DbName), DbName, Views, Remove)
    end;
db_view_update(DbName, Views, Remove) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_view_update(Db, Views, Remove);
        {'error', _}=E -> E
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
-spec db_create(kz_term:text()) -> boolean().
db_create(DbName) ->
    db_create(DbName, []).

-spec db_create(kz_term:text(), kzs_db:db_create_options()) -> boolean().
db_create(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_create(kzs_plan:plan(DbName), DbName, Options);
db_create(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_create(Db, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Compact a database.
%% @end
%%------------------------------------------------------------------------------
-spec db_compact(kz_term:text()) -> boolean().

db_compact(DbName) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_compact(kzs_plan:plan(DbName), DbName);
db_compact(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_compact(Db);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a database (takes an `encoded' DbName).
%% @end
%%------------------------------------------------------------------------------
-spec db_delete(kz_term:text()) -> boolean().
db_delete(DbName) ->
    db_delete(DbName, []).

-spec db_delete(kz_term:text(), db_delete_options()) -> boolean().
db_delete(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_delete(kzs_plan:plan(DbName), DbName, Options);
db_delete(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_delete(Db, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Archive a database (takes an `encoded' DbName).
%% @end
%%------------------------------------------------------------------------------
-spec db_archive(kz_term:ne_binary()) -> 'ok' | data_error().
db_archive(DbName) ->
    Folder = kazoo_data_config:get_ne_binary(<<"default_archive_folder">>, <<"/tmp">>),
    db_archive(DbName, filename:join([<<Folder/binary, "/", DbName/binary, ".json">>])).

-spec db_archive(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | data_error().
db_archive(DbName, Filename) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_archive(kzs_plan:plan(DbName), DbName, Filename);
db_archive(DbName, Filename) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_archive(Db, Filename);
        {'error', _}=E -> E
    end.

-spec db_import(kz_term:ne_binary(), file:filename_all()) -> 'ok' | data_error().
db_import(DbName, ArchiveFile) when ?VALID_DBNAME(DbName) ->
    kzs_db:db_import(kzs_plan:plan(DbName), DbName, ArchiveFile);
db_import(DbName, ArchiveFile) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> db_import(Db, ArchiveFile);
        {'error', _}=E -> E
    end.

%%%=============================================================================
%%% Document Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc fetch a cached doc or open it if not available.
%% @end
%%------------------------------------------------------------------------------
-spec open_cache_doc(kz_term:text(), docid()) ->
                            {'ok', kz_json:object()} |
                            data_error().
open_cache_doc(DbName, {DocType, DocId}) ->
    open_cache_doc(DbName, DocId, [{'doc_type', DocType}]);
open_cache_doc(DbName, DocId) ->
    open_cache_doc(DbName, DocId, []).

-spec open_cache_doc(kz_term:text(), docid(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            data_error().
open_cache_doc(DbName, {DocType, DocId}, Options) ->
    open_cache_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_cache_doc(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    kzs_cache:open_cache_doc(DbName, DocId, Options);
open_cache_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> open_cache_doc(Db, DocId, Options);
        {'error', _}=E -> E
    end.

-spec add_to_doc_cache(kz_term:text(), kz_term:ne_binary(), kz_json:object()) ->
                              'ok' |
                              data_error().
add_to_doc_cache(DbName, DocId, Doc) when ?VALID_DBNAME(DbName) ->
    kzs_cache:add_to_doc_cache(DbName, DocId, Doc);
add_to_doc_cache(DbName, DocId, Doc) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> add_to_doc_cache(Db, DocId, Doc);
        {'error', _}=E -> E
    end.

-spec update_cache_doc(kz_term:text(), kz_term:ne_binary(), fun((kz_json:object()) -> kz_json:object() | 'skip')) ->
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

-spec maybe_save_doc(kz_term:text(), kz_json:object() | 'skip', kz_json:object()) ->
                            {'ok', kz_json:object() | kz_json:objects()} |
                            data_error().
maybe_save_doc(_DbName, 'skip', Jobj) ->
    {'ok', Jobj};
maybe_save_doc(DbName, JObj, _OldJobj) ->
    save_doc(DbName, JObj).

-spec flush_cache_doc(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
                             'ok' |
                             {'error', 'invalid_db_name'}.
flush_cache_doc(DbName, Doc) ->
    flush_cache_doc(DbName, Doc, []).

-spec flush_cache_doc(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), kz_term:proplist()) ->
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
flush_cache_docs() -> kzs_cache:flush_cache_docs().

-spec flush_cache_docs(kz_term:text()) -> 'ok' | {'error', 'invalid_db_name'}.
flush_cache_docs(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> kzs_cache:flush_cache_docs(Db);
        {'error', _}=E -> E
    end.

-ifdef(TEST).
%% -define(OPEN_DOC_LOG(DbName, DocId, Options),
%%         begin
%%             {_, ST} = erlang:process_info(self(), current_stacktrace),
%%             kz_util:log_stacktrace(ST),
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
-spec open_doc(kz_term:text(), docid()) ->
                      {'ok', kz_json:object()} |
                      data_error() |
                      {'error', 'not_found'}.
open_doc(DbName, {DocType, DocId}) ->
    open_doc(DbName, DocId, [{'doc_type', DocType}]);
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec open_doc(kz_term:text(), docid(), kz_term:proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error() |
                      {'error', 'not_found'}.
open_doc(DbName, {DocType, DocId}, Options) ->
    open_doc(DbName, DocId, maybe_add_doc_type(DocType, Options));
open_doc(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    ?OPEN_DOC_LOG(DbName, DocId, Options),
    kzs_doc:open_doc(kzs_plan:plan(DbName, Options), DbName, DocId, Options);
open_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> open_doc(Db, DocId, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Open documents given doc ids returns an error tuple or the json.
%% Each returned JObj contains either an `<<"doc">>' or `<<"error">>' field.
%% So: match both error tuple and each JSON of the list.
%% @end
%%------------------------------------------------------------------------------
-spec open_docs(kz_term:text(), docids()) ->
                       {'ok', kz_json:objects()} |
                       data_error() |
                       {'error', 'not_found'}.
open_docs(DbName, DocIds) ->
    open_docs(DbName, DocIds, []).

-spec open_docs(kz_term:text(), docids(), kz_term:proplist()) ->
                       {'ok', kz_json:objects()} |
                       data_error() |
                       {'error', 'not_found'}.
open_docs(DbName, DocIds, Options) ->
    read_chunked(fun do_open_docs/3, DbName, DocIds, Options).

do_open_docs(DbName, DocIds, Options) ->
    NewOptions = [{'keys', DocIds}, 'include_docs' | Options],
    all_docs(DbName, NewOptions).

read_chunked(Opener, DbName, DocIds, Options) ->
    read_chunked(Opener, DbName, DocIds, Options, []).
read_chunked(Opener, DbName, DocIds, Options, Acc) ->
    try lists:split(max_bulk_read(Options), DocIds) of
        {NewDocIds, DocIdsLeft} ->
            NewAcc = read_chunked_results(Opener, DbName, NewDocIds, Options, Acc),
            read_chunked(Opener, DbName, DocIdsLeft, Options, NewAcc)
    catch 'error':'badarg' ->
            case read_chunked_results(Opener, DbName, DocIds, Options, Acc) of
                {'error', _R}=E -> E;
                JObjs -> {'ok', lists:flatten(lists:reverse(JObjs))}
            end
    end.

read_chunked_results(_, _, _, _, {'error',_}=Acc) -> Acc;
read_chunked_results(Opener, DbName, DocIds, Options, Acc) ->
    read_chunked_results(DocIds, Opener(DbName, DocIds, Options), Acc).

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

-spec open_cache_docs(kz_term:text(), docids()) ->
                             {'ok', kz_json:objects()} |
                             data_error() |
                             {'error', 'not_found'}.
open_cache_docs(DbName, DocIds) ->
    open_cache_docs(DbName, DocIds, []).

-spec open_cache_docs(kz_term:text(), docids(), kz_term:proplist()) ->
                             {'ok', kz_json:objects()} |
                             data_error() |
                             {'error', 'not_found'}.
open_cache_docs(DbName, DocIds, Options) when ?VALID_DBNAME(DbName) ->
    read_chunked(fun kzs_cache:open_cache_docs/3, DbName, DocIds, Options);
open_cache_docs(DbName, DocIds, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> open_cache_docs(Db, DocIds, Options);
        {'error', _}=E -> E
    end.



-spec all_docs(kz_term:text()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
all_docs(DbName) ->
    all_docs(DbName, []).

-spec all_docs(kz_term:text(), kz_term:proplist()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
all_docs(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_view:all_docs(kzs_plan:plan(DbName, Options), DbName, Options);
all_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> all_docs(Db, Options);
        {'error', _}=E -> E
    end.


-spec db_list() -> {'ok', kz_term:ne_binaries()} | data_error().
db_list() ->
    db_list([]).

-spec db_list(kz_term:proplist()) -> {'ok', kz_term:ne_binaries()} | data_error().
db_list(Options) ->
    kzs_db:db_list(kzs_plan:plan(), Options).


-spec all_design_docs(kz_term:text()) -> {'ok', kz_json:objects()} |
                                         data_error().
all_design_docs(DbName) ->
    all_design_docs(DbName, []).

-spec all_design_docs(kz_term:text(), kz_term:proplist()) -> {'ok', kz_json:objects()} |
                                                             data_error().
all_design_docs(DbName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_view:all_design_docs(kzs_plan:plan(DbName), DbName, Options);
all_design_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> all_design_docs(Db, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Get the revision of a document (much faster than requesting the whole document).
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev(kz_term:text(), docid()) ->
                            {'ok', kz_term:ne_binary()} |
                            data_error().
lookup_doc_rev(DbName, {DocType, DocId}) ->
    lookup_doc_rev(DbName, DocId, [{'doc_type', DocType}]);
lookup_doc_rev(DbName, DocId) ->
    lookup_doc_rev(DbName, DocId, []).

-spec lookup_doc_rev(kz_term:text(), docid(), kz_term:proplist()) ->
                            {'ok', kz_term:ne_binary()} | data_error().
lookup_doc_rev(DbName, {DocType, DocId}, Options) ->
    lookup_doc_rev(DbName, DocId, maybe_add_doc_type(DocType, Options));
lookup_doc_rev(DbName, DocId, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:lookup_doc_rev(kzs_plan:plan(DbName, Options), DbName, DocId);
lookup_doc_rev(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> lookup_doc_rev(Db, DocId, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Save document to database.
%% @end
%%------------------------------------------------------------------------------
-spec save_doc(kz_term:text(), kz_json:object() | kz_json:objects()) ->
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

-spec ensure_saved(kz_term:text(), kz_json:object()) ->
                          {'ok', kz_json:object()} |
                          data_error().
ensure_saved(DbName, Doc) ->
    ensure_saved(DbName, Doc, []).

-spec ensure_saved(kz_term:text(), kz_json:object(), kz_term:proplist()) ->
                          {'ok', kz_json:object()} |
                          data_error().
ensure_saved(DbName, Doc, Options) when ?VALID_DBNAME(DbName) ->
    kzs_doc:ensure_saved(kzs_plan:plan(DbName, Doc), DbName, Doc, Options);
ensure_saved(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> ensure_saved(Db, Doc, Options);
        {'error', _}=E -> E
    end.

-spec save_doc(kz_term:text(), kz_json:object(), kz_term:proplist()) ->
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


-spec save_docs(kz_term:text(), kz_json:objects()) ->
                       {'ok', kz_json:objects()} |
                       data_error().
save_docs(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []).

-spec save_docs(kz_term:text(), kz_json:objects(), kz_term:proplist()) ->
                       {'ok', kz_json:objects()} |
                       data_error().
save_docs(DbName, [Doc|_]=Docs, Options)
  when is_list(Docs), ?VALID_DBNAME(DbName) ->
    OldSetting = maybe_toggle_publish(Options),
    Result = kzs_doc:save_docs(kzs_plan:plan(DbName, Doc), DbName, Docs, Options),
    maybe_revert_publish(OldSetting),
    Result;
save_docs(DbName, []=Docs, _Options)
  when is_list(Docs), ?VALID_DBNAME(DbName) ->
    {'ok', Docs};
save_docs(DbName, Docs, Options) when is_list(Docs) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> save_docs(Db, Docs, Options);
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Fetch, update and save a doc (creating if not present).
%% @end
%%------------------------------------------------------------------------------
-spec update_doc(kz_term:ne_binary(), docid(), update_options()) ->
                        {'ok', kz_json:object()} |
                        data_error().
update_doc(DbName, Id, Options) ->
    case open_doc(DbName, Id) of
        {'error', 'not_found'} ->
            update_not_found(DbName, Id, Options);
        {'error', _}=E -> E;
        {'ok', CurrentDoc} ->
            apply_updates_and_save(DbName, Id, Options, CurrentDoc)
    end.

-spec apply_updates_and_save(kz_term:ne_binary(), docid(), update_options(), kz_json:object()) ->
                                    {'ok', kz_json:object()} |
                                    data_error().
apply_updates_and_save(DbName, Id, Options, CurrentDoc) ->
    apply_updates_and_save(DbName, Id, Options, CurrentDoc, props:get_value('update', Options)).

apply_updates_and_save(_DbName, _Id, _Options, CurrentDoc, []) ->
    lager:debug("no updates to apply, returning current doc ~s", [_Id]),
    {'ok', CurrentDoc};
apply_updates_and_save(DbName, Id, Options, CurrentDoc, UpdateProps) ->
    UpdatedDoc = kz_json:set_values(UpdateProps, CurrentDoc),

    case kz_json:are_equal(CurrentDoc, UpdatedDoc) of
        'true' ->
            lager:debug("updates to ~s result in the same doc", [Id]),
            {'ok', CurrentDoc};
        'false' ->
            lager:debug("attempting to save ~s", [kz_json:encode(UpdatedDoc)]),
            save_update(DbName, Id, Options, UpdatedDoc)
    end.

-spec save_update(kz_term:ne_binary(), docid(), update_options(), kz_json:object()) ->
                         {'ok', kz_json:object()} |
                         data_error().
save_update(DbName, Id, Options, UpdatedDoc) ->
    ExtraProps = props:get_value('extra_update', Options, []),
    ExtraUpdatedDoc = kz_json:set_values(ExtraProps, UpdatedDoc),

    EnsureSaved = props:is_true('ensure_saved', Options, 'false'),

    case save_doc(DbName, ExtraUpdatedDoc) of
        {'ok', _Saved}=OK ->
            lager:debug("saved ~s/~s: ~s", [DbName, Id, kz_json:encode(_Saved)]),
            OK;
        {'error', 'conflict'} when EnsureSaved ->
            lager:debug("saving ~s to ~s resulted in a conflict, trying again", [Id, DbName]),
            update_doc(DbName, Id, Options);
        {'error', _E}=Error ->
            lager:debug("failed to save ~s: ~p", [Id, _E]),
            Error
    end.

-spec update_not_found(kz_term:ne_binary(), docid(), update_options()) ->
                              {'ok', kz_json:object()} |
                              data_error().
update_not_found(DbName, Id, Options) ->
    CreateProps = props:get_value('create', Options, []),

    JObj = kz_json:set_values(CreateProps, kz_json:new()),
    Updated = kz_json:set_values([{kz_doc:path_id(), Id}
                                  | props:get_value('update', Options)
                                 ]
                                 ++ props:get_value('extra_update', Options, [])
                                ,JObj
                                ),
    lager:debug("attempting to create ~s: ~s", [Id, kz_json:encode(Updated)]),
    save_doc(DbName, Updated).

%%------------------------------------------------------------------------------
%% @doc Remove document from the db.
%% @end
%%------------------------------------------------------------------------------
-spec del_doc(kz_term:text(), kz_json:object() | kz_json:objects() | kz_term:ne_binary()) ->
                     {'ok', kz_json:objects()} |
                     data_error().
del_doc(DbName, Doc) ->
    del_doc(DbName, Doc, []).

-spec del_doc(kz_term:text(), kz_json:object() | kz_json:objects() | kz_term:ne_binary(), kz_term:proplist()) ->
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

%%------------------------------------------------------------------------------
%% @doc Remove documents from the db.
%% @end
%%------------------------------------------------------------------------------
-spec del_docs(kz_term:text(), kz_json:objects() | kz_term:ne_binaries()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
del_docs(DbName, Docs) ->
    del_docs(DbName, Docs, []).

-spec del_docs(kz_term:text(), kz_json:objects() | kz_term:ne_binaries(), kz_term:proplist()) ->
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

%%%=============================================================================
%%% Attachment Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Fetch attachment with `AName' from document `DocId'.
%% @end
%%------------------------------------------------------------------------------
-spec fetch_attachment(kz_term:text(), docid(), kz_term:ne_binary()) ->
                              {'ok', binary()} |
                              data_error() |
                              kz_att_error:error().
fetch_attachment(DbName, {DocType, DocId}, AName) ->
    fetch_attachment(DbName, DocId, AName, [{'doc_type', DocType}]);
fetch_attachment(DbName, DocId, AName) ->
    fetch_attachment(DbName, DocId, AName, []).

-spec fetch_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
                              {'ok', binary()} |
                              data_error() |
                              kz_att_error:error().
fetch_attachment(DbName, {DocType, DocId}, AName, Options) when ?VALID_DBNAME(DbName) ->
    fetch_attachment(DbName, DocId, AName, maybe_add_doc_type(DocType, Options));
fetch_attachment(DbName, DocId, AName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_attachments:fetch_attachment(kzs_plan:plan(DbName, Options), DbName, DocId, AName, Options);
fetch_attachment(DbName, DocId, AName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> fetch_attachment(Db, DocId, AName, Options);
        {'error', _}=E -> E
    end.

-spec stream_attachment(kz_term:text(), docid(), kz_term:ne_binary()) ->
                               {'ok', reference()} |
                               {'error', any()}.
stream_attachment(DbName, DocId, AName) ->
    stream_attachment(DbName, DocId, AName, []).

-spec stream_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', reference()} |
                               {'error', any()}.
stream_attachment(DbName, DocId, AName, Options) ->
    stream_attachment(DbName, DocId, AName, Options, self()).

-spec stream_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:proplist(), pid()) ->
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
-spec put_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            data_error() |
                            kz_att_error:error().
put_attachment(DbName, DocId, AName, Contents) ->
    put_attachment(DbName, DocId, AName, Contents, []).

-spec put_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            {'ok', kz_json:object(), kz_term:proplist()} |
                            data_error() |
                            kz_att_error:error().
put_attachment(DbName, {DocType, DocId}, AName, Contents, Options) ->
    put_attachment(DbName, DocId, AName, Contents, maybe_add_doc_type(DocType, Options));
put_attachment(DbName, DocId, AName, Contents, Options) when ?VALID_DBNAME(DbName) ->
    case attachment_options(DbName, DocId, Options) of
        {'ok', NewOpts} ->
            NewOptions = props:delete('plan_override', NewOpts),
            kzs_attachments:put_attachment(kzs_plan:plan(DbName, NewOpts), DbName, DocId, AName, Contents, NewOptions);
        {'error', _} = Error -> Error
    end;
put_attachment(DbName, DocId, AName, Contents, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> put_attachment(Db, DocId, AName, Contents, Options);
        {'error', _}=E -> E
    end.

-spec delete_attachment(kz_term:text(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               {'ok', kz_json:object()} |
                               data_error().
delete_attachment(DbName, DocId, AName) ->
    delete_attachment(DbName, DocId, AName, []).

-spec delete_attachment(kz_term:text(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', kz_json:object()} |
                               data_error().
delete_attachment(DbName, DocId, AName, Options) when ?VALID_DBNAME(DbName) ->
    kzs_attachments:delete_attachment(kzs_plan:plan(DbName, DocId), DbName, DocId, AName, Options);
delete_attachment(DbName, DocId, AName, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> delete_attachment(Db, DocId, AName, Options);
        {'error', _}=E -> E
    end.

-spec attachment_url(kz_term:text(), docid(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary()} |
                            {'proxy', tuple()} |
                            {'error', any()}.
attachment_url(DbName, DocId, AttachmentId) ->
    attachment_url(DbName, DocId, AttachmentId, []).

-spec attachment_url(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
                            kz_term:ne_binary() |
                            {'proxy', tuple()} |
                            {'error', any()}.
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
    Fun = fun() -> case open_cache_doc(DbName, DocId, props:delete('plan_override', Options)) of
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
-spec get_all_results(kz_term:ne_binary(), kz_term:ne_binary()) -> get_results_return().
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec get_results(kz_term:ne_binary(), kz_term:ne_binary()) -> get_results_return().
get_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec get_results(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) -> get_results_return().
get_results(DbName, DesignDoc, Options) when ?VALID_DBNAME(DbName) ->
    ?GET_RESULTS(DbName, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    Plan = kzs_plan:plan(DbName, Opts),
    case kzs_view:get_results(Plan, DbName, DesignDoc, Options) of
        {'error', 'not_found'} -> maybe_create_view(Plan, DbName, DesignDoc, Options);
        Other -> Other
    end;
get_results(DbName, DesignDoc, Options) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> get_results(Db, DesignDoc, Options);
        {'error', _}=E -> E
    end.

-spec get_results_count(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               data_error().
get_results_count(DbName, DesignDoc, Options) ->
    ?GET_RESULTS(DbName, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    kzs_view:get_results_count(kzs_plan:plan(DbName, Opts), DbName, DesignDoc, Options).

-spec get_registered_view(map(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 'not_registered' | kz_json:object().
get_registered_view(Plan, DbName, DesignDoc) ->
    Classification = kz_term:to_binary(kzs_util:db_classification(DbName)),
    Keys = [[Classification, DesignDoc]
           ,[DbName, DesignDoc]
           ],
    Opts = ['include_docs'
           ,{'keys', Keys}
           ],
    case kzs_view:get_results(Plan, ?KZ_DATA_DB, <<"views/registered">>, Opts) of
        {'ok', []} -> 'not_registered';
        {'ok', [JObj | _]} -> kz_json:get_json_value(<<"doc">>, JObj);
        {'error', _Err} ->
            lager:error("error getting registered view : ~p", [_Err]),
            'not_registered'
    end.

-spec maybe_create_view(map(), kz_term:ne_binary(), kz_term:ne_binary(), view_options()) -> get_results_return().
maybe_create_view(Plan, DbName, DesignDoc, Options) ->
    maybe_create_view(Plan, DbName, DesignDoc, Options, kzs_db:db_exists(Plan, DbName)).

-spec maybe_create_view(map(), kz_term:ne_binary(), kz_term:ne_binary(), view_options(), boolean()) -> get_results_return().
maybe_create_view(_Plan, _DbName, _DesignDoc, _Options, 'false') ->
    {'error', 'not_found'};
maybe_create_view(Plan, DbName, DesignDoc, Options, 'true') ->
    maybe_create_registered_view(Plan, DbName, DesignDoc, Options
                                ,get_registered_view(Plan, DbName, DesignDoc)
                                ).

-spec maybe_create_registered_view(map(), kz_term:ne_binary(), kz_term:ne_binary(), view_options(), kz_json:object() | 'not_registered') ->
                                          get_results_return().
maybe_create_registered_view(_Plan, _DbName, _DesignDoc, _Options, 'not_registered') ->
    {'error', 'not_found'};
maybe_create_registered_view(Plan, DbName, DesignDoc, Options, ViewJObj) ->
    ViewDoc = kz_json:get_json_value(<<"view_definition">>, ViewJObj),
    case kzs_doc:save_doc(Plan, DbName, ViewDoc, []) of
        {'ok', _ViewDoc} -> kzs_view:get_results(Plan, DbName, DesignDoc, Options);
        {'error', 'conflict'} -> kzs_view:get_results(Plan, DbName, DesignDoc, Options);
        {'error', _Err} ->
            lager:error("error saving registered view ~s to database ~s : ~p", [DesignDoc, DbName, _Err]),
            {'error', 'not_found'}
    end.

-spec get_result_keys(kz_term:ne_binary(), kz_term:ne_binary()) ->
                             {'ok', kz_term:ne_binaries() | [kz_term:ne_binaries()]} | data_error().
get_result_keys(DbName, DesignDoc) ->
    get_result_keys(DbName, DesignDoc, []).

-spec get_result_keys(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
                             {'ok', kz_term:ne_binaries() | [kz_term:ne_binaries()]} | data_error().
get_result_keys(DbName, DesignDoc, Options) ->
    ?GET_RESULTS(DbName, DesignDoc, Options),
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

-spec get_result_ids(kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binaries()} | data_error().
get_result_ids(DbName, DesignDoc) ->
    get_result_ids(DbName, DesignDoc, []).

-spec get_result_ids(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
                            {'ok', kz_term:ne_binaries()} | data_error().
get_result_ids(DbName, DesignDoc, Options) ->
    ?GET_RESULTS(DbName, DesignDoc, Options),
    Opts = maybe_add_doc_type_from_view(DesignDoc, Options),
    case kzs_view:get_results(kzs_plan:plan(DbName, Opts), DbName, DesignDoc, Options) of
        {'ok', JObjs} -> {'ok', get_result_ids(JObjs)};
        {'error', _} = Error -> Error
    end.

-spec get_result_ids(kz_json:objects()) -> kz_term:ne_binaries().
get_result_ids(JObjs) ->
    [kz_doc:id(JObj) || JObj <- JObjs].

%%------------------------------------------------------------------------------
%% @doc Gets the only result of a view.
%% If no result is found: returns `{error, not_found}'.
%% If more than one result is found, either:
%% - if `Options' contains `first_when_multiple'
%%     then the first one will be returned;
%% - otherwise `{error, multiple_results}' is returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_single_result(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
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

-spec get_result_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
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

-spec get_result_docs(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
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
-spec paginate_results(kz_term:ne_binary(), kz_term:ne_binary(), paginate_options()) ->
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
%% @doc Attempt to correct the database name.
%%
%% <div class="notice">The attempt to correct the dbname is not very Erlang like,
%% but since there are more places that expect an error and do not
%% handle a crash appropriately/gracefully this is a quick solution.</div>
%% @end
%%------------------------------------------------------------------------------
-spec maybe_convert_dbname(kz_term:text()) ->
                                  {'ok', kz_term:ne_binary()} |
                                  {'error', 'invalid_db_name'}.
maybe_convert_dbname(DbName) ->
    case kz_term:is_empty(DbName) of
        'true' -> {'error', 'invalid_db_name'};
        'false' -> {'ok', kz_term:to_binary(DbName)}
    end.

-spec copy_doc(kz_term:ne_binary(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
copy_doc(FromDB, FromId, ToDB, Options) ->
    copy_doc(FromDB, FromId, ToDB, FromId, Options).

-spec copy_doc(kz_term:ne_binary(), docid(), kz_term:ne_binary(), docid(), kz_term:proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
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

-spec move_doc(kz_term:ne_binary(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
move_doc(FromDB, FromId, ToDB, Options) ->
    move_doc(FromDB, FromId, ToDB, FromId, Options).

-spec move_doc(kz_term:ne_binary(), docid(), kz_term:ne_binary(), docid(), kz_term:proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
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

-spec db_classification(kz_term:text()) -> db_classification().
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

-spec add_doc_type_from_view(kz_term:ne_binary(), view_options()) -> view_options().
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
    Fun = fun(DB, Acc) ->
                  [init_db(Server, DB) | Acc]
          end,
    Result = lists:foldl(Fun, [], ?KZ_SYSTEM_DBS),
    revise_docs_from_folder(?KZ_DATA_DB, ?APP, <<"views">>),
    _ = enable_change_notice(),
    lists:any(fun kz_term:is_true/1, Result).

-spec init_db(map(), kz_term:ne_binary()) -> boolean().
init_db(Server, Db) ->
    case kzs_db:db_exists(Server, Db) of
        'true' -> 'false';
        'false' ->
            lager:info("creating database ~s", [Db]),
            kzs_db:db_create(Server, Db, [])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_views(atom(), views_listing() | [string()]) -> 'ok'.
register_views(_, []) ->
    'ok';
register_views(App, [View | Other]) ->
    _ = register_view(App, View),
    register_views(App, Other).

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

    Update = [{<<"kazoo">>, kz_json:from_list([{<<"view_map">>, ViewMaps}])}
             ,{<<"view_definition">>, maybe_adapt_multilines(kz_json:delete_key(<<"kazoo">>, View))}
             ],
    ExtraUpdate = [{<<"version">>, Version}],
    Create = [{<<"application">>, AppName}
             ,{<<"name">>, Name}
             ,{<<"pvt_type">>, <<"view_definition">>}
             ],

    UpdateOptions = [{'update', Update}
                    ,{'extra_update', ExtraUpdate}
                    ,{'create', Create}
                    ],

    update_doc(?KZ_DATA_DB, DocId, UpdateOptions).

log_register_views(_, _, _, []) ->
    'ok';
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
    Class= kz_json:get_ne_binary_value(<<"classification">>, JObj),
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
-spec refresh_views(kz_term:ne_binary()) -> boolean() | {'error', 'invalid_db_name'}.
refresh_views(DbName) when ?VALID_DBNAME(DbName) ->
    suppress_change_notice(),
    Classification = kzs_util:db_classification(DbName),
    lager:debug("updating views for db ~s:~s", [Classification, DbName]),
    Updated = case view_definitions(DbName, Classification) of
                  [] -> 'false';
                  Views ->
                      Database = kz_util:uri_encode(kz_util:uri_decode(DbName)),
                      db_view_update(Database, Views)
              end,

    _ = case Updated of
            'true' ->
                lager:debug("~s:~s views updated", [Classification, DbName]),
                kzs_publish:publish_db(DbName, 'edited');
            'false' ->
                lager:debug("~s:~s no views updated", [Classification, DbName])
        end,
    enable_change_notice(),
    Updated;
refresh_views(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> refresh_views(Db);
        {'error', _}=E -> E
    end.

-spec view_definitions(kz_term:ne_binary(), atom() | kz_term:ne_binary()) -> views_listing().
view_definitions(DbName, Classification) ->
    ViewOptions = [kz_util:uri_decode(DbName), kz_term:to_binary(Classification)],
    case get_result_docs(?KZ_DATA_DB, <<"views/views_by_classification">>, ViewOptions) of
        {'error', _} -> [];
        {'ok', JObjs} ->
            ViewDefs = [kz_json:get_json_value(<<"view_definition">>, JObj) || JObj <- JObjs],
            [{kz_doc:id(ViewDef), ViewDef} || ViewDef <- ViewDefs]
    end.
