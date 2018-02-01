%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz INC
%%% @doc
%%% Manage data connections
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_datamgr).

-export([db_classification/1]).

%% Settings-related
-export([max_bulk_insert/0
        ,max_bulk_read/0
        ]).
-export([init_dbs/0]).
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
        ,update_doc/3, update_doc/4
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
        ,get_result_keys/1, get_result_keys/3, get_result_keys/2
        ,get_result_ids/1, get_result_ids/2, get_result_ids/3
        ,get_single_result/3
        ,get_result_doc/3, get_result_docs/3
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

-export([register_view/2, register_view/3
        ,register_views/2,register_views/3
        ,refresh_views/1
        ,register_views_from_folder/1
        ,register_views_from_folder/2
        ,register_views_from_folder/3
        ]).

-export_type([view_option/0, view_options/0
             ,view_listing/0, views_listing/0
             ,data_error/0, data_errors/0
             ,db_classification/0
             ]).

-include("kz_data.hrl").

-define(VALID_DBNAME(DbName), is_binary(DbName), byte_size(DbName) > 0).

-define(UUID_SIZE, 16).

%%%===================================================================
%%% Couch Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overwrite the existing contents of a document with the contents of
%% a file
%% @end
%%--------------------------------------------------------------------
-spec update_doc_from_file(kz_term:ne_binary(), atom(), nonempty_string() | kz_term:ne_binary()) ->
                                  {'ok', kz_json:object()} |
                                  data_error().
update_doc_from_file(DbName, App, File) when ?VALID_DBNAME(DbName) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    lager:debug("update db ~s from CouchDB file: ~s", [DbName, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        JObj = maybe_adapt_multilines(kz_json:decode(Bin)),
        maybe_update_doc(DbName, JObj)
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
        {'ok', Db} -> update_doc_from_file(Db, App, File);
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create or overwrite the existing contents of a document with the
%% contents of a file
%% @end
%%--------------------------------------------------------------------
-spec revise_doc_from_file(kz_term:ne_binary(), atom(), kz_term:ne_binary() | nonempty_string()) ->
                                  {'ok', kz_json:object()} |
                                  data_error().
revise_doc_from_file(DbName, App, File) ->
    case update_doc_from_file(DbName, App, File) of
        {'error', _E}=R ->
            lager:debug("failed to update doc: ~p", [_E]),
            R;
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
-spec revise_views_from_folder(kz_term:ne_binary(), atom()) -> 'ok'.
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications folder, relative to
%% priv/couchdb/ into a given database
%% @end
%%--------------------------------------------------------------------

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
            do_revise_docs_from_folder(DbName, Sleep, Files)
    end.

-spec do_revise_docs_from_folder(kz_term:ne_binary(), boolean(), kz_term:ne_binaries()) -> 'ok'.
do_revise_docs_from_folder(_, _, []) -> 'ok';
do_revise_docs_from_folder(DbName, Sleep, [H|T]) ->
    try
        {'ok', Bin} = file:read_file(H),
        JObj = maybe_adapt_multilines(kz_json:decode(Bin)),
        _ = maybe_update_doc(DbName, JObj),
        Sleep
            andalso timer:sleep(250),
        do_revise_docs_from_folder(DbName, Sleep, T)
    catch
        _:_ ->
            kz_util:log_stacktrace(),
            do_revise_docs_from_folder(DbName, Sleep, T)
    end.

maybe_update_doc(DbName, JObj) ->
    case should_update(DbName, JObj) of
        true -> ensure_saved(DbName, JObj);
        false -> {'ok', JObj}
    end.

should_update(DbName, JObj) ->
    case open_doc(DbName, kz_doc:id(JObj)) of
        {'ok', Doc} -> kz_doc:document_hash(JObj) =/= kz_doc:document_hash(Doc);
        _ -> true
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
-spec inline_js_fun(kz_term:ne_binary(), kz_term:ne_binaries() | kz_json:json_term(), kz_json:object()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists, also checks other connections
%% @end
%%--------------------------------------------------------------------
-spec db_exists_all(kz_term:text()) -> boolean().
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
-spec db_info() -> {'ok', kz_term:ne_binaries()} |
                   data_error().
db_info() ->
    kzs_db:db_info(kzs_plan:plan()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec db_info(kz_term:text()) -> {'ok', kz_json:object()} |
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
        Views ->  kzs_db:db_view_update(kzs_plan:plan(DbName), DbName, Views, Remove)
    end;
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
%%  ,{<<"create_target">>, true} % optional, creates the DB on target if nonexistent
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
-spec db_replicate(kz_term:proplist() | kz_json:object()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec db_compact(kz_term:text()) -> boolean().

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Archive a database (takes an 'encoded' DbName)
%% @end
%%--------------------------------------------------------------------

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

%%%===================================================================
%%% Document Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch a cached doc or open it if not available.
%% @end
%%--------------------------------------------------------------------

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
            lager:error("Can't open doc ~s/~s coz ~p", [DbName, DocId, _Reason]),
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Open documents given doc ids returns an error tuple or the json.
%% Each returned JObj contains either an <<"doc">> or <<"error">> field.
%% So: match both error tuple & each JSON of the list.
%% @end
%%--------------------------------------------------------------------

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
    NewOptions = [{keys, DocIds}, include_docs | Options],
    all_docs(DbName, NewOptions).

read_chunked(Opener, DbName, DocIds, Options) ->
    read_chunked(Opener, DbName, DocIds, Options, []).
read_chunked(Opener, DbName, DocIds, Options, Acc) ->
    try lists:split(max_bulk_read(Options), DocIds) of
        {NewDocIds, DocIdsLeft} ->
            NewAcc = read_chunked_results(Opener, DbName, NewDocIds, Options, Acc),
            read_chunked(Opener, DbName, DocIdsLeft, Options, NewAcc)
    catch error:badarg ->
            case read_chunked_results(Opener, DbName, DocIds, Options, Acc) of
                {error, _R}=E -> E;
                JObjs -> {ok, lists:flatten(lists:reverse(JObjs))}
            end
    end.

read_chunked_results(_, _, _, _, {error,_}=Acc) -> Acc;
read_chunked_results(Opener, DbName, DocIds, Options, Acc) ->
    read_chunked_results(DocIds, Opener(DbName, DocIds, Options), Acc).

read_chunked_results(_DocIds, {ok, JObjs}, Acc) ->
    [JObjs | Acc];
read_chunked_results(_DocIds, {error,_}=Reason, []) ->
    Reason;
read_chunked_results(DocIds, {error, Reason}, Acc) ->
    [kz_json:from_list(
       [{<<"id">>, DocId}
       ,{<<"error">>, Reason}
       ])
     || DocId <- DocIds
    ] ++ Acc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Open documents given doc ids returns an error tuple or the json.
%% Attempts to fetch from cache before making an ad-hoc bulk read.
%% Each returned JObj contains either an <<"doc">> or <<"error">> field.
%% So: match both error tuple & each JSON of the list.
%% Note: no guaranty on order of results is provided.
%% @end
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save document to database
%% @end
%%--------------------------------------------------------------------
-spec save_doc(kz_term:text(), kz_json:object() | kz_json:objects()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch, update and save a doc (creating if not present)
%% @end
%%--------------------------------------------------------------------
-type update_props() :: [{kz_json:path(), kz_json:json_term()}].

-spec update_doc(kz_term:ne_binary(), docid(), update_props()) ->
                        {'ok', kz_json:object()} |
                        data_error().
update_doc(DbName, Id, UpdateProps) ->
    update_doc(DbName, Id, UpdateProps, []).

-spec update_doc(kz_term:ne_binary(), docid(), update_props(), kz_term:proplist()) ->
                        {'ok', kz_json:object()} |
                        data_error().
update_doc(DbName, Id, UpdateProps, CreateProps) ->
    update_doc(DbName, Id, UpdateProps, CreateProps, []).

-spec update_doc(kz_term:ne_binary(), docid(), update_props(), kz_term:proplist(), kz_term:proplist()) ->
                        {'ok', kz_json:object()} |
                        data_error().
update_doc(DbName, Id, UpdateProps, CreateProps, ExtraUpdateProps)
  when is_list(UpdateProps),
       is_list(CreateProps),
       is_list(ExtraUpdateProps) ->
    case open_doc(DbName, Id) of
        {'error', 'not_found'} ->
            JObj = kz_json:from_list([{<<"_id">>, Id}]
                                     ++ CreateProps
                                     ++ UpdateProps
                                     ++ ExtraUpdateProps
                                    ),
            save_doc(DbName, JObj);
        {'error', _}=E -> E;
        {'ok', JObj}=OK ->
            UpdatedJObj = kz_json:set_values(UpdateProps, JObj),
            case kz_json:are_equal(JObj, UpdatedJObj) of
                'true' -> OK;
                'false' -> save_doc(DbName, kz_json:set_values(ExtraUpdateProps, UpdatedJObj))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
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

%%%===================================================================
%%% Attachment Functions
%%%===================================================================

-spec fetch_attachment(kz_term:text(), docid(), kz_term:ne_binary()) ->
                              {'ok', binary()} |
                              data_error().

-spec fetch_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:proplist()) ->
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

-spec put_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            data_error().
%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec put_attachment(kz_term:text(), docid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            {'ok', kz_json:object(), kz_term:proplist()} |
                            data_error().
put_attachment(DbName, DocId, AName, Contents) ->
    put_attachment(DbName, DocId, AName, Contents, []).

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
                            {'ok', kz_term:ne_binary()} |
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

-ifdef(TEST).
%% -define(GET_RESULTS(DbName, DesignId, Options)
%%        ,?LOG_DEBUG("~s:get_results(~p, ~p, ~p)", [?MODULE, DbName, DesignId, Options])
%%        ).
-define(GET_RESULTS(DbName, DesignId, Options), ok).
-else.
-define(GET_RESULTS(DbName, DesignId, Options), ok).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the results of the view
%% {Total, Offset, Meta, Rows}
%% @end
%%--------------------------------------------------------------------

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
        {'error', 'not_found'} ->
            maybe_create_view(DbName, Plan, DesignDoc, Options);
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

-spec maybe_create_view(kz_term:ne_binary(), map(), kz_term:ne_binary(), view_options()) -> get_results_return().
maybe_create_view(DbName, Plan, DesignDoc, Options) ->
    case props:get_value('view_json', Options) of
        'undefined' -> {'error', 'not_found'};
        ViewJson ->
            'true' = db_view_update(DbName, ViewJson),
            kzs_view:get_results(Plan, DbName, DesignDoc, Options)
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

-spec get_uuid() -> kz_term:ne_binary().
get_uuid() -> get_uuid(?UUID_SIZE).

-spec get_uuid(pos_integer()) -> kz_term:ne_binary().
get_uuid(N) -> kz_binary:rand_hex(N).

-spec get_uuids(pos_integer()) -> kz_term:ne_binaries().
get_uuids(Count) -> get_uuids(Count, ?UUID_SIZE).

-spec get_uuids(pos_integer(), pos_integer()) -> kz_term:ne_binaries().
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
%% @public
%% @doc
%% How many documents are chunked when doing a bulk save
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert() -> pos_integer().
max_bulk_insert() ->
    kazoo_data_config:get_pos_integer(<<"max_bulk_insert">>, 2000).

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% How many documents are chunked when doing a bulk read
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

-spec init_dbs() -> boolean().
init_dbs() ->
    Result = case db_exists(?KZ_ACCOUNTS_DB) of
                 'true' -> 'false';
                 'false' -> [db_create(DbName) || DbName <- ?KZ_SYSTEM_DBS],
                            'true'
             end,
    revise_docs_from_folder(?KZ_DATA_DB, 'kazoo_data', <<"views">>),
    Result.

-spec register_views(kz_term:ne_binary() | db_classification(), views_listing()) -> 'ok'.
register_views(Classification, Views) ->
    App = kz_util:calling_app(),
    register_views(Classification, kz_term:to_atom(App, 'true'), Views).


-spec register_views(kz_term:ne_binary() | db_classification(), atom(), views_listing()) -> 'ok'.
register_views(_Classification, _App, []) -> 'ok';
register_views(Classification, App, [View | Other]) ->
    _ = register_view(Classification, App, View),
    register_views(Classification, App, Other).

-spec register_view(kz_term:ne_binary() | db_classification(), view_listing()) ->
                           {'ok', kz_json:object()} |
                           data_error().
register_view(Classification, View) ->
    App = kz_util:calling_app(),
    register_view(Classification, kz_term:to_atom(App, 'true'), View).

-spec register_view(kz_term:ne_binary() | db_classification(), atom(), view_listing() | string() | kz_term:ne_binary()) ->
                           {'ok', kz_json:object()} |
                           data_error().
register_view(Classification, App, {<<"_design/", Name/binary>>, View}) ->
    Version = kz_util:application_version(App),
    AppName = kz_term:to_binary(App),
    DocId = <<(kz_term:to_binary(Classification))/binary, "-", AppName/binary, "-", Name/binary>>,
    Update = [{<<"view_definition">>, View}],
    ExtraUpdate = [{<<"version">>, Version}],
    Create = [{<<"application">>, AppName}
             ,{<<"classification">>, kz_term:to_binary(Classification)}
             ,{<<"name">>, Name}
             ,{<<"pvt_type">>, <<"view_definition">>}
             ],
    update_doc(?KZ_DATA_DB, DocId, Update, Create, ExtraUpdate);
register_view(Classification, App, ViewName) ->
    register_view(Classification, App, kzs_util:get_view_json(App, ViewName)).

-spec register_views_from_folder(kz_term:ne_binary() | db_classification()) -> 'ok'.
register_views_from_folder(Classification) ->
    register_views_from_folder(Classification, kz_term:to_atom(kz_util:calling_app(), 'true')).

-spec register_views_from_folder(kz_term:ne_binary() | db_classification(), atom()) -> 'ok'.
register_views_from_folder(Classification, App) ->
    register_views_from_folder(Classification, App, "views").

-spec register_views_from_folder(kz_term:ne_binary() | db_classification(), atom(), kz_term:ne_binary() | nonempty_string()) -> 'ok'.
register_views_from_folder(Classification, App, Folder) ->
    Views = kzs_util:get_views_json(App, Folder),
    register_views(Classification, App, Views).

-spec refresh_views(kz_term:ne_binary()) -> boolean() | {'error', 'invalid_db_name'}.
refresh_views(DbName) when ?VALID_DBNAME(DbName) ->
    suppress_change_notice(),
    Classification = kz_term:to_binary(kzs_util:db_classification(DbName)),
    lager:debug("updating views for db ~s:~s", [Classification, DbName]),
    Updated = case get_result_docs(?KZ_DATA_DB, <<"views/views_by_classification">>, [Classification]) of
                  {'error', _} -> 'false';
                  {'ok', JObjs} ->
                      ViewDefs = [kz_json:get_json_value(<<"view_definition">>, JObj) || JObj <- JObjs],
                      Views = [{kz_doc:id(ViewDef), ViewDef} || ViewDef <- ViewDefs],
                      Database = kz_util:uri_encode(kz_util:uri_decode(DbName)),
                      db_view_update(Database, Views)
              end,
    _ = case Updated of
            'true' -> lager:debug("~s:~s views updated", [Classification, DbName]),
                      kzs_publish:publish_db(DbName, 'edited');
            'false' -> lager:debug("~s:~s no views needed updating", [Classification, DbName])
        end,
    enable_change_notice(),
    Updated;
refresh_views(DbName) ->
    case maybe_convert_dbname(DbName) of
        {'ok', Db} -> refresh_views(Db);
        {'error', _}=E -> E
    end.

