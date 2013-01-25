%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Manage CouchDB connections
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(couch_mgr).

%% System manipulation
-export([db_exists/1
         ,db_info/0, db_info/1
         ,db_create/1, db_create/2
         ,db_compact/1
         ,db_view_cleanup/1
         ,db_delete/1
         ,db_replicate/1
        ]).
-export([admin_db_info/0, admin_db_info/1
         ,admin_db_compact/1
         ,admin_db_view_cleanup/1
         ,design_info/2
         ,admin_design_info/2
         ,design_compact/2
         ,admin_design_compact/2
        ]).

%% Document manipulation
-export([save_doc/2, save_doc/3
         ,save_docs/2, save_docs/3
         ,open_cache_doc/2, open_cache_doc/3
         ,flush_cache_doc/2, flush_cache_doc/3
         ,open_doc/2, admin_open_doc/2
         ,open_doc/3, admin_open_doc/3
         ,del_doc/2, del_docs/2
         ,lookup_doc_rev/2
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
         ,admin_all_docs/1
         ,all_docs/2
         ,all_design_docs/2, admin_all_docs/2
        ]).

%% attachments
-export([fetch_attachment/3
         ,stream_attachment/3
         ,put_attachment/4, put_attachment/5
         ,delete_attachment/3, delete_attachment/4
        ]).

%% Views
-export([get_all_results/2
         ,get_results/3
         ,get_results_count/3
         ,get_result_keys/1
        ]).

-export([get_uuid/0, get_uuid/1
         ,get_uuids/1, get_uuids/2
        ]).

%% Types
-export_type([get_results_return/0]).

-include_lib("wh_couch.hrl").

-define(VALID_DBNAME, is_binary(DbName) andalso byte_size(DbName) > 0).

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
-spec load_doc_from_file/3 :: (ne_binary(), atom(), nonempty_string() | ne_binary()) ->
                                      {'ok', wh_json:object()} |
                                      couchbeam_error().
load_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", wh_util:to_list(File)]),
    lager:debug("read into db ~s from CouchDB JSON file: ~s", [DbName, Path]),
    try
        {ok, Bin} = file:read_file(Path),
        ?MODULE:save_doc(DbName, wh_json:decode(Bin)) %% if it crashes on the match, the catch will let us know
    catch
        _Type:{badmatch,{error,Reason}} ->
            lager:debug("badmatch error: ~p", [Reason]),
            {error, Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Overwrite the existing contents of a document with the contents of
%% a file
%% @end
%%--------------------------------------------------------------------
-spec update_doc_from_file/3 :: (ne_binary(), atom(), nonempty_string() | ne_binary()) ->
                                        {'ok', wh_json:object()} |
                                        couchbeam_error().
update_doc_from_file(DbName, App, File) when ?VALID_DBNAME ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    lager:debug("update db ~s from CouchDB file: ~s", [DbName, Path]),
    try
        {ok, Bin} = file:read_file(Path),
        JObj = wh_json:decode(Bin),
        {ok, Rev} = ?MODULE:lookup_doc_rev(DbName, wh_json:get_value(<<"_id">>, JObj)),
        ?MODULE:save_doc(DbName, wh_json:set_value(<<"_rev">>, Rev, JObj))
    catch
        _Type:{badmatch,{error,Reason}} ->
            lager:debug("bad match: ~p", [Reason]),
            {error, Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {error, Reason}
    end;
update_doc_from_file(DbName, App, File) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> load_doc_from_file(Db, App, File);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create or overwrite the existing contents of a document with the
%% contents of a file
%% @end
%%--------------------------------------------------------------------
-spec revise_doc_from_file/3 :: (ne_binary(), atom(), ne_binary() | nonempty_string()) ->
                                        {'ok', wh_json:object()} |
                                        couchbeam_error().
revise_doc_from_file(DbName, App, File) ->
    case ?MODULE:update_doc_from_file(DbName, App, File) of
        {error, _E} ->
            lager:debug("failed to update doc: ~p", [_E]),
            ?MODULE:load_doc_from_file(DbName, App, File);
        {ok, _}=Resp ->
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
-spec revise_views_from_folder/2 :: (ne_binary(), atom()) -> 'ok'.
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications folder, relative to
%% priv/couchdb/ into a given database
%% @end
%%--------------------------------------------------------------------
-spec revise_docs_from_folder/3 :: (ne_binary(), atom(), ne_binary() | nonempty_string()) -> 'ok'.
-spec revise_docs_from_folder/4 :: (ne_binary(), atom(), ne_binary() | nonempty_string(), boolean()) -> 'ok'.

revise_docs_from_folder(DbName, App, Folder) ->
    revise_docs_from_folder(DbName, App, Folder, true).

revise_docs_from_folder(DbName, App, Folder, Sleep) ->
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", wh_util:to_list(Folder), "/*.json"]),
    do_revise_docs_from_folder(DbName, Sleep, Files).

-spec do_revise_docs_from_folder/3 :: (ne_binary(), boolean(), [ne_binary() | nonempty_string(),...]) -> 'ok'.
do_revise_docs_from_folder(_, _, []) -> ok;
do_revise_docs_from_folder(DbName, Sleep, [H|T]) ->
    try
        {ok, Bin} = file:read_file(H),
        JObj = wh_json:decode(Bin),
        Sleep andalso timer:sleep(250),
        case lookup_doc_rev(DbName, wh_json:get_value(<<"_id">>, JObj)) of
            {ok, Rev} ->
                lager:debug("update doc from file ~s in ~s", [H, DbName]),
                save_doc(DbName, wh_json:set_value(<<"_rev">>, Rev, JObj));
            {error, not_found} ->
                lager:debug("import doc from file ~s in ~s", [H, DbName]),
                save_doc(DbName, JObj);
            {error, Reason} ->
                lager:debug("failed to load doc ~s into ~s, ~p", [H, DbName, Reason])
        end,
        do_revise_docs_from_folder(DbName, Sleep, T)
    catch
        _:_ ->
            do_revise_docs_from_folder(DbName, Sleep, T)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load fixture files from a folder into a database, only if the id
%% isn't already existant
%% @end
%%--------------------------------------------------------------------
-spec load_fixtures_from_folder/2 :: (ne_binary(), atom()) -> 'ok'.
load_fixtures_from_folder(DbName, App) ->
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", ?FIXTURES_FOLDER, "/*.json"]),
    do_load_fixtures_from_folder(DbName, Files).

-spec do_load_fixtures_from_folder/2 :: (ne_binary(), [ne_binary(),...] | []) -> 'ok'.
do_load_fixtures_from_folder(_, []) -> ok;
do_load_fixtures_from_folder(DbName, [F|Fs]) ->
    try
        {ok, Bin} = file:read_file(F),
        FixJObj = wh_json:decode(Bin),
        FixId = wh_json:get_value(<<"_id">>, FixJObj),
        case lookup_doc_rev(DbName, FixId) of
            {ok, _Rev} ->
                lager:debug("fixture ~s exists in ~s: ~s", [FixId, DbName, _Rev]);
            {error, not_found} ->
                lager:debug("saving fixture ~s to ~s", [FixId, DbName]),
                save_doc(DbName, FixJObj);
            {error, _Reason} ->
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
-spec db_exists/1 :: (text()) -> boolean().
db_exists(DbName) when ?VALID_DBNAME ->
    couch_util:db_exists(wh_couch_connections:get_server(), DbName);
db_exists(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_exists(Db);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding all databases
%% @end
%%--------------------------------------------------------------------
-spec db_info/0 :: () -> {'ok', [ne_binary(),...] | []} |
                         couchbeam_error().
-spec admin_db_info/0 :: () -> {'ok', [ne_binary(),...] | []} |
                               couchbeam_error().

db_info() ->
    couch_util:db_info(wh_couch_connections:get_server()).

admin_db_info() ->
    couch_util:db_info(wh_couch_connections:get_admin_server()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec db_info/1 :: (text()) -> {'ok', wh_json:object()} |
                               couchbeam_error().
-spec admin_db_info/1 :: (text()) -> {'ok', wh_json:object()} |
                                     couchbeam_error().

db_info(DbName) when ?VALID_DBNAME ->
    couch_util:db_info(wh_couch_connections:get_server(), DbName);
db_info(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_info(Db);
        {error, _}=E -> E
    end.

admin_db_info(DbName) when ?VALID_DBNAME ->
    couch_util:db_info(wh_couch_connections:get_admin_server(), DbName);
admin_db_info(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_db_info(Db);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database design doc
%% @end
%%--------------------------------------------------------------------
-spec design_info/2 :: (text(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
-spec admin_design_info/2 :: (text(), ne_binary()) ->
                                     {'ok', wh_json:object()} |
                                     couchbeam_error().

design_info(DbName, DesignName) when ?VALID_DBNAME ->
    couch_util:design_info(wh_couch_connections:get_server(), DbName, DesignName);
design_info(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> design_info(Db, DesignName);
        {error, _}=E -> E
    end.

admin_design_info(DbName, DesignName) when ?VALID_DBNAME ->
    couch_util:design_info(wh_couch_connections:get_admin_server(), DbName, DesignName);
admin_design_info(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_design_info(Db, DesignName);
        {error, _}=E -> E
    end.

-spec design_compact/2 :: (ne_binary(), ne_binary()) -> boolean().
-spec admin_design_compact/2 :: (ne_binary(), ne_binary()) -> boolean().

design_compact(DbName, DesignName) when ?VALID_DBNAME->
    couch_util:design_compact(wh_couch_connections:get_server(), DbName, DesignName);
design_compact(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> design_compact(Db, DesignName);
        {error, _}=E -> E
    end.

admin_design_compact(DbName, DesignName) when ?VALID_DBNAME ->
    couch_util:design_compact(wh_couch_connections:get_admin_server(), DbName, DesignName);
admin_design_compact(DbName, DesignName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_design_compact(Db, DesignName);
        {error, _}=E -> E
    end.

-spec db_view_cleanup/1 :: (ne_binary()) -> boolean().
-spec admin_db_view_cleanup/1 :: (ne_binary()) -> boolean().

db_view_cleanup(DbName) when ?VALID_DBNAME ->
    couch_util:db_view_cleanup(wh_couch_connections:get_server(), DbName);
db_view_cleanup(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_view_cleanup(Db);
        {error, _}=E -> E
    end.

admin_db_view_cleanup(DbName) when ?VALID_DBNAME ->
    couch_util:db_view_cleanup(wh_couch_connections:get_admin_server(), DbName);
admin_db_view_cleanup(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_db_view_cleanup(Db);
        {error, _}=E -> E
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
-spec db_replicate/1 :: (proplist() | wh_json:object()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
db_replicate(Prop) when is_list(Prop) ->
    db_replicate(wh_json:from_list(Prop));
db_replicate(JObj) ->
    couch_util:db_replicate(wh_couch_connections:get_server(), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_create/1 :: (text()) -> boolean().
-spec db_create/2 :: (text(), couch_util:db_create_options()) -> boolean().

db_create(DbName) ->
    db_create(DbName, []).

db_create(DbName, Options) when ?VALID_DBNAME ->
    couch_util:db_create(wh_couch_connections:get_server(), DbName, Options);
db_create(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_create(Db, Options);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec db_compact/1 :: (text()) -> boolean().
-spec admin_db_compact/1 :: (text()) -> boolean().

db_compact(DbName) when ?VALID_DBNAME ->
    couch_util:db_compact(wh_couch_connections:get_server(), DbName);
db_compact(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_compact(Db);
        {error, _}=E -> E
    end.

admin_db_compact(DbName) when ?VALID_DBNAME ->
    couch_util:db_compact(wh_couch_connections:get_admin_server(), DbName);
admin_db_compact(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_db_compact(Db);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete a database
%% @end
%%--------------------------------------------------------------------
-spec db_delete/1 :: (text()) -> boolean().
db_delete(DbName) when ?VALID_DBNAME ->
    couch_util:db_delete(wh_couch_connections:get_server(), DbName);
db_delete(DbName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> db_delete(Db);
        {error, _}=E -> E
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
-spec open_cache_doc/2 :: (text(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error() |
                                  {'error', 'not_found'}.
-spec open_cache_doc/3 :: (text(), ne_binary(), proplist()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error() |
                                  {'error', 'not_found'}.

open_cache_doc(DbName, DocId) ->
    open_cache_doc(DbName, DocId, []).

open_cache_doc(DbName, DocId, Options) when ?VALID_DBNAME ->
    couch_util:open_cache_doc(wh_couch_connections:get_server(), DbName, DocId, Options);
open_cache_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> open_cache_doc(Db, DocId, Options);
        {error, _}=E -> E
    end.

-spec flush_cache_doc/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec flush_cache_doc/3 :: (ne_binary(), ne_binary(), proplist()) -> 'ok'.
flush_cache_doc(DbName, DocId) ->
    flush_cache_doc(DbName, DocId, []).
flush_cache_doc(DbName, DocId, Options) when ?VALID_DBNAME ->
    couch_util:flush_cache_doc(wh_couch_connections:get_server(), DbName, DocId, Options);
flush_cache_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> flush_cache_doc(Db, DocId, Options);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------
-spec open_doc/2 :: (text(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error() |
                            {'error', 'not_found'}.
-spec open_doc/3 :: (text(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error() |
                            {'error', 'not_found'}.
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

open_doc(DbName, DocId, Options) when ?VALID_DBNAME ->
    couch_util:open_doc(wh_couch_connections:get_server(), DbName, DocId, Options);
open_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> open_doc(Db, DocId, Options);
        {error, _}=E -> E
    end.


-spec admin_open_doc/2 :: (text(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error() |
                                  {'error', 'not_found'}.
-spec admin_open_doc/3 :: (text(), ne_binary(), wh_proplist()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error() |
                                  {'error', 'not_found'}.
admin_open_doc(DbName, DocId) ->
    admin_open_doc(DbName, DocId, []).

admin_open_doc(DbName, DocId, Options) when ?VALID_DBNAME->
    couch_util:open_doc(wh_couch_connections:get_admin_server(), DbName, DocId, Options);
admin_open_doc(DbName, DocId, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_open_doc(Db, DocId, Options);
        {error, _}=E -> E
    end.

-spec all_docs/1 :: (text()) -> {'ok', wh_json:objects()} |
                                couchbeam_error().
-spec all_docs/2 :: (text(), proplist()) -> {'ok', wh_json:objects()} |
                                            couchbeam_error().

all_docs(DbName) ->
    all_docs(DbName, []).

all_docs(DbName, Options) when ?VALID_DBNAME ->
    couch_util:all_docs(wh_couch_connections:get_server(), DbName, Options);
all_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> all_docs(Db, Options);
        {error, _}=E -> E
    end.

-spec admin_all_docs/1 :: (text()) -> {'ok', wh_json:objects()} |
                                      couchbeam_error().

-spec admin_all_docs/2 :: (text(), proplist()) -> {'ok', wh_json:objects()} |
                                                  couchbeam_error().

admin_all_docs(DbName) ->
    admin_all_docs(DbName, []).

admin_all_docs(DbName, Options) when ?VALID_DBNAME ->
    couch_util:all_docs(wh_couch_connections:get_admin_server(), DbName, Options);
admin_all_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> admin_all_docs(Db, Options);
        {error, _}=E -> E
    end.

-spec all_design_docs/1 :: (text()) -> {'ok', wh_json:objects()} |
                                       couchbeam_error().
-spec all_design_docs/2 :: (text(), proplist()) -> {'ok', wh_json:objects()} |
                                                   couchbeam_error().

all_design_docs(DbName) ->
    all_design_docs(DbName, []).

all_design_docs(DbName, Options) when ?VALID_DBNAME ->
    couch_util:all_design_docs(wh_couch_connections:get_server(), DbName, Options);
all_design_docs(DbName, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> all_design_docs(Db, Options);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec lookup_doc_rev/2 :: (text(), api_binary()) ->
                                  {'ok', ne_binary()} |
                                  couchbeam_error().
lookup_doc_rev(_DbName, undefined) -> {error, not_found};
lookup_doc_rev(DbName, DocId) when ?VALID_DBNAME ->
    couch_util:lookup_doc_rev(wh_couch_connections:get_server(), DbName, DocId);
lookup_doc_rev(DbName, DocId) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> lookup_doc_rev(Db, DocId);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec save_doc/2 :: (text(), wh_json:object() | wh_json:objects()) ->
                            {'ok', wh_json:object() | wh_json:objects()} |
                            couchbeam_error().
save_doc(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).

%% save a document; if it fails to save because of conflict, pull the latest revision and try saving again.
%% any other error is returned
-spec ensure_saved/2 :: (text(), wh_json:object()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
-spec ensure_saved/3 :: (text(), wh_json:object(), proplist()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().

ensure_saved(DbName, Doc) ->
    ensure_saved(DbName, Doc, []).

ensure_saved(DbName, Doc, Options) when ?VALID_DBNAME ->
    couch_util:ensure_saved(wh_couch_connections:get_server(), DbName, Doc, Options);
ensure_saved(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> ensure_saved(Db, Doc, Options);
        {error, _}=E -> E
    end.

-spec save_doc/3 :: (text(), wh_json:object(), proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
save_doc(DbName, Doc, Options) when ?VALID_DBNAME ->
    couch_util:save_doc(wh_couch_connections:get_server(), DbName, Doc, Options);
save_doc(DbName, Doc, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> save_doc(Db, Doc, Options);
        {error, _}=E -> E
    end.

-spec save_docs/2 :: (text(), wh_json:objects()) ->
                             {'ok', wh_json:objects()} |
                             couchbeam_error().
-spec save_docs/3 :: (text(), wh_json:objects(), proplist()) ->
                             {'ok', wh_json:objects()} |
                             couchbeam_error().

save_docs(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []).

save_docs(DbName, Docs, Options) when is_list(Docs) andalso ?VALID_DBNAME ->
    couch_util:save_docs(wh_couch_connections:get_server(), DbName, Docs, Options);
save_docs(DbName, Docs, Options) when is_list(Docs) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> save_docs(Db, Docs, Options);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch, update and save a doc (creating if not present)
%% @end
%%--------------------------------------------------------------------
-spec update_doc/3 :: (ne_binary(), ne_binary(), proplist()) ->
                              {'ok', wh_json:object()} |
                              couchbeam_error().
-spec update_doc/4 :: (ne_binary(), ne_binary(), proplist(), proplist()) ->
                              {'ok', wh_json:object()} |
                              couchbeam_error().

update_doc(DbName, Id, UpdateProps) ->
    update_doc(DbName, Id, UpdateProps, []).

update_doc(DbName, Id, UpdateProps, CreateProps) ->
    case open_doc(DbName, Id) of
        {error, not_found} ->
            JObj = wh_json:from_list(lists:append([[{<<"_id">>, Id}], CreateProps, UpdateProps])),
            save_doc(DbName, JObj);
        {error, _}=E -> E;
        {ok, JObj}=Ok ->
            case wh_json:set_values(UpdateProps, JObj) of
                JObj -> Ok;
                UpdatedJObj ->
                    save_doc(DbName, UpdatedJObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec del_doc/2 :: (text(), wh_json:object() | wh_json:objects() | ne_binary()) ->
                           {'ok', wh_json:objects()} |
                           couchbeam_error().
del_doc(DbName, Doc) when is_list(Doc) ->
    del_docs(DbName, Doc);
del_doc(DbName, Doc) when ?VALID_DBNAME ->
    couch_util:del_doc(wh_couch_connections:get_server(), DbName, Doc);
del_doc(DbName, Doc) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> del_doc(Db, Doc);
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
-spec del_docs/2 :: (text(), wh_json:objects()) ->
                            {'ok', wh_json:objects()}.
del_docs(DbName, Docs) when is_list(Docs) andalso ?VALID_DBNAME ->
    couch_util:del_docs(wh_couch_connections:get_server(), DbName, Docs);
del_docs(DbName, Docs) when is_list(Docs) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> del_docs(Db, Docs);
        {error, _}=E -> E
    end.

%%%===================================================================
%%% Attachment Functions
%%%===================================================================
-spec fetch_attachment/3 :: (text(), ne_binary(), ne_binary()) ->
                                    {'ok', ne_binary()} |
                                    couchbeam_error().
fetch_attachment(DbName, DocId, AName) when ?VALID_DBNAME ->
    couch_util:fetch_attachment(wh_couch_connections:get_server(), DbName, DocId, AName);
fetch_attachment(DbName, DocId, AName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> fetch_attachment(Db, DocId, AName);
        {error, _}=E -> E
    end.

-spec stream_attachment/3 :: (text(), ne_binary(), ne_binary()) ->
                                     {'ok', reference()} |
                                     {'error', term()}.
stream_attachment(DbName, DocId, AName) when ?VALID_DBNAME ->
    couch_util:stream_attachment(wh_couch_connections:get_server(), DbName, DocId, AName, self());
stream_attachment(DbName, DocId, AName) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> stream_attachment(Db, DocId, AName);
        {error, _}=E -> E
    end.

-spec put_attachment/4 :: (text(), ne_binary(), ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error().
%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec put_attachment/5 :: (text(), ne_binary(), ne_binary(), ne_binary(), proplist()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error().
put_attachment(DbName, DocId, AName, Contents) ->
    put_attachment(DbName, DocId, AName, Contents, []).

put_attachment(DbName, DocId, AName, Contents, Options) when ?VALID_DBNAME ->
    couch_util:put_attachment(wh_couch_connections:get_server(), DbName, DocId, AName, Contents, Options);
put_attachment(DbName, DocId, AName, Contents, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> put_attachment(Db, DocId, AName, Contents, Options);
        {error, _}=E -> E
    end.

-spec delete_attachment/3 :: (text(), ne_binary(), ne_binary()) ->
                                     {'ok', wh_json:object()} |
                                     couchbeam_error().
-spec delete_attachment/4 :: (text(), ne_binary(), ne_binary(), proplist()) ->
                                     {'ok', wh_json:object()} |
                                     couchbeam_error().
delete_attachment(DbName, DocId, AName) ->
    delete_attachment(DbName, DocId, AName, []).

delete_attachment(DbName, DocId, AName, Options) when ?VALID_DBNAME ->
    couch_util:delete_attachment(wh_couch_connections:get_server(), DbName, DocId, AName, Options);
delete_attachment(DbName, DocId, AName, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> delete_attachment(Db, DocId, AName, Options);
        {error, _}=E -> E
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
-type get_results_return() :: {'ok', wh_json:objects() | wh_json:json_strings()} |
                              couchbeam_error().
-spec get_all_results/2 :: (text(), ne_binary()) -> get_results_return().
-spec get_results/3 :: (text(), ne_binary(), wh_proplist()) -> get_results_return().
-spec get_results_count/3 :: (text(), ne_binary(), wh_proplist()) ->
                                     {'ok', integer()} |
                                     couchbeam_error().

get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

get_results(DbName, DesignDoc, Options) when ?VALID_DBNAME ->
    couch_util:get_results(wh_couch_connections:get_server(), DbName, DesignDoc, Options);
get_results(DbName, DesignDoc, Options) ->
    case maybe_convert_dbname(DbName) of
        {ok, Db} -> get_results(Db, DesignDoc, Options);
        {error, _}=E -> E
    end.

get_results_count(DbName, DesignDoc, Options) ->
    couch_util:get_results_count(wh_couch_connections:get_server(), DbName, DesignDoc, Options).

-spec get_result_keys/1 :: (wh_json:objects()) -> [wh_json:json_string(),...] | [].
get_result_keys(JObjs) ->
    lists:map(fun get_keys/1, JObjs).

-spec get_keys/1 :: (wh_json:object()) -> wh_json:json_string().
get_keys(JObj) ->
    wh_json:get_value(<<"key">>, JObj).

-spec get_uuid/0 :: () -> ne_binary().
-spec get_uuid/1 :: (pos_integer()) -> ne_binary().
get_uuid() -> get_uuid(?UUID_SIZE).
get_uuid(N) -> wh_util:rand_hex_binary(N).

-spec get_uuids/1 :: (pos_integer()) -> ne_binaries().
-spec get_uuids/2 :: (pos_integer(), pos_integer()) -> ne_binaries().
get_uuids(Count) -> get_uuids(Count, ?UUID_SIZE).
get_uuids(Count, Size) -> [get_uuid(Size) || _ <- lists:seq(1, Count)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTE: the attempt to correct the dbname is not very erlang like, but 
%%  when since there are more places that expect an error and do not
%%  handle a crash appropriately/gracefully this is a quick solution....
%% @end
%%--------------------------------------------------------------------
-spec maybe_convert_dbname/1 :: (text()) ->
                                        {'ok', ne_binary()} |
                                        {'error', 'invalid_db_name'}.
maybe_convert_dbname(DbName) ->
    case wh_util:is_empty(DbName) of
        true -> {error, invalid_db_name};
        false -> {ok, wh_util:to_binary(DbName)}
    end.
