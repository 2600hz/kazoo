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

-behaviour(gen_server).

%% API
-export([start_link/0
         ,set_host/1
         ,set_host/2
         ,set_host/3
         ,set_host/4
         ,set_host/5
         ,get_host/0
         ,get_port/0
         ,get_creds/0
         ,get_url/0
         ,get_uuid/0
         ,get_uuids/1
        ]).
-export([get_admin_port/0
         ,get_admin_conn/0
         ,get_admin_url/0
         ,get_node_cookie/0
         ,set_node_cookie/1
         ,load_configs/0
        ]).

%% System manipulation
-export([db_exists/1
         ,db_info/0
         ,db_info/1
         ,db_create/1
         ,db_create/2
         ,db_compact/1
         ,db_view_cleanup/1
         ,db_delete/1
         ,db_replicate/1
        ]).
-export([admin_db_info/0
         ,admin_db_info/1
         ,admin_db_compact/1
         ,admin_db_view_cleanup/1
        ]).

-export([design_info/2
         ,admin_design_info/2
         ,design_compact/2
         ,admin_design_compact/2
        ]).

%% Document manipulation
-export([save_doc/2
         ,save_doc/3
         ,save_docs/2
         ,save_docs/3
         ,open_cache_doc/2
         ,open_cache_doc/3
         ,open_doc/2
         ,open_doc/3
         ,del_doc/2
         ,del_docs/2
         ,lookup_doc_rev/2
        ]).
-export([update_doc/3
        ,update_doc/4
        ]).
-export([add_change_handler/2
         ,add_change_handler/3
         ,rm_change_handler/2
        ]).
-export([load_doc_from_file/3
         ,update_doc_from_file/3
        ]).
-export([revise_doc_from_file/3]).
-export([revise_docs_from_folder/3
         ,revise_docs_from_folder/4
        ]).
-export([revise_views_from_folder/2
         ,ensure_saved/2
         ,ensure_saved/3
        ]).

-export([all_docs/1
         ,all_design_docs/1
         ,admin_all_docs/1
        ]).
-export([all_docs/2
         ,all_design_docs/2
         ,admin_all_docs/2
        ]).

%% attachments
-export([fetch_attachment/3
         ,stream_attachment/3
         ,put_attachment/4
         ,put_attachment/5
         ,delete_attachment/3
         ,delete_attachment/4
        ]).

%% Views
-export([get_all_results/2
         ,get_results/3
        ]).
-export([get_result_keys/1]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("wh_couch.hrl").

-define(SERVER, ?MODULE).

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% Change handler {DBName :: string(), {Srv :: pid(), SrvRef :: reference()}
-record(state, {
          host = {"", ?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT} :: {nonempty_string(), pos_integer(), pos_integer()}
          ,connection = #server{} :: server()
          ,admin_connection = #server{} :: server()
          ,creds = {"", ""} :: {string(), string()} % {User, Pass}
          ,change_handlers = dict:new() :: dict()
         }).

%%%===================================================================
%%% Couch Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a file into couch as a document (not an attachement)
%% @end
%%--------------------------------------------------------------------
-spec load_doc_from_file/3 :: (ne_binary(), atom(), nonempty_string() | ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                        {'error', atom()}.
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
-spec update_doc_from_file/3 :: (ne_binary(), atom(), nonempty_string() | ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                          {'error', atom()}.
update_doc_from_file(DbName, App, File) ->
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
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create or overwrite the existing contents of a document with the
%% contents of a file
%% @end
%%--------------------------------------------------------------------
-spec revise_doc_from_file/3 :: (ne_binary(), atom(), ne_binary() | nonempty_string()) -> {'ok', wh_json:json_object()} |
                                                                                          {'error', atom()}.
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
    Files = filelib:wildcard([code:priv_dir(App), "/couchdb/", Folder, "/*.json"]),
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
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_exists/1 :: (ne_binary()) -> boolean().
db_exists(DbName) ->
    couch_util:db_exists(get_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding all databases
%% @end
%%--------------------------------------------------------------------
-spec db_info/0 :: () -> {'ok', [ne_binary(),...] | []} |
                         {'error', atom()}.
-spec admin_db_info/0 :: () -> {'ok', [ne_binary(),...] | []} |
                               {'error', atom()}.
db_info() ->
    couch_util:db_info(get_conn()).
admin_db_info() ->
    couch_util:db_info(get_admin_conn()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec db_info/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                    {'error', atom()}.
-spec admin_db_info/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                          {'error', atom()}.
db_info(DbName) ->
    couch_util:db_info(get_conn(), DbName).
admin_db_info(DbName) ->
    couch_util:db_info(get_admin_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database design doc
%% @end
%%--------------------------------------------------------------------
-spec design_info/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                     {'error', atom()}.
-spec admin_design_info/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                           {'error', atom()}.
design_info(DbName, DesignName) ->
    couch_util:design_info(get_conn(), DbName, DesignName).
admin_design_info(DbName, DesignName) ->
    couch_util:design_info(get_admin_conn(), DbName, DesignName).

-spec design_compact/2 :: (ne_binary(), ne_binary()) -> boolean().
-spec admin_design_compact/2 :: (ne_binary(), ne_binary()) -> boolean().
design_compact(DbName, Design) ->
    couch_util:design_compact(get_conn(), DbName, Design).
admin_design_compact(DbName, Design) ->
    couch_util:design_compact(get_admin_conn(), DbName, Design).

-spec db_view_cleanup/1 :: (ne_binary()) -> boolean().
-spec admin_db_view_cleanup/1 :: (ne_binary()) -> boolean().
db_view_cleanup(DbName) ->
    couch_util:db_view_cleanup(get_conn(), DbName).
admin_db_view_cleanup(DbName) ->
    couch_util:db_view_cleanup(get_admin_conn(), DbName).

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
-spec db_replicate/1 :: (proplist() | wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                                {'error', atom()}.
db_replicate(Prop) when is_list(Prop) ->
    db_replicate(wh_json:from_list(Prop));
db_replicate(JObj) ->
    couch_util:db_replicate(get_conn(), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_create/1 :: (ne_binary()) -> boolean().
db_create(DbName) ->
    db_create(DbName, []).

-spec db_create/2 :: (ne_binary(), couch_util:db_create_options()) -> boolean().
db_create(DbName, Options) ->
    couch_util:db_create(get_conn(), DbName, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec db_compact/1 :: (ne_binary()) -> boolean().
-spec admin_db_compact/1 :: (ne_binary()) -> boolean().
db_compact(DbName) ->
    couch_util:db_compact(get_conn(), DbName).
admin_db_compact(DbName) ->
    couch_util:db_compact(get_admin_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete a database
%% @end
%%--------------------------------------------------------------------
-spec db_delete/1 :: (ne_binary()) -> boolean().
db_delete(DbName) ->
    couch_util:db_delete(get_conn(), DbName).

%%%===================================================================
%%% Document Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch a cached doc or open it if not available.
%% @end
%%--------------------------------------------------------------------
-spec open_cache_doc/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                        {'error', 'not_found' | 'db_not_reachable'}.
-spec open_cache_doc/3 :: (ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                    {'error', 'not_found' | 'db_not_reachable'}.

open_cache_doc(DbName, DocId) ->
    open_cache_doc(DbName, DocId, []).

open_cache_doc(DbName, DocId, Options) ->
    couch_util:open_cache_doc(get_conn(), DbName, DocId, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------
-spec open_doc/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                  {'error', 'not_found' | 'db_not_reachable'}.
-spec open_doc/3 :: (ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} |
                                                              {'error', 'not_found' | 'db_not_reachable'}.
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).
open_doc(DbName, DocId, Options) ->
    couch_util:open_doc(get_conn(), DbName, DocId, Options).

-spec all_docs/1 :: (ne_binary()) -> {'ok', wh_json:json_objects()} |
                                     {'error', atom()}.
-spec admin_all_docs/1 :: (ne_binary()) -> {'ok', wh_json:json_objects()} |
                                           {'error', atom()}.
all_docs(DbName) ->
    couch_util:all_docs(get_conn(), DbName, []).
admin_all_docs(DbName) ->
    couch_util:all_docs(get_admin_conn(), DbName, []).

-spec all_docs/2 :: (ne_binary(), proplist()) -> {'ok', wh_json:json_objects()} |
                                                 {'error', atom()}.
-spec admin_all_docs/2 :: (ne_binary(), proplist()) -> {'ok', wh_json:json_objects()} |
                                                       {'error', atom()}.
all_docs(DbName, Options) ->
    couch_util:all_docs(get_conn(), DbName, Options).
admin_all_docs(DbName, Options) ->
    couch_util:all_docs(get_admin_conn(), DbName, Options).

-spec all_design_docs/1 :: (ne_binary()) -> {'ok', wh_json:json_objects()} |
                                            {'error', atom()}.
all_design_docs(DbName) ->
    couch_util:all_design_docs(get_conn(), DbName, []).

-spec all_design_docs/2 :: (ne_binary(), proplist()) -> {'ok', wh_json:json_objects()} |
                                                        {'error', atom()}.
all_design_docs(DbName, Options) ->
    couch_util:all_design_docs(get_conn(), DbName, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec lookup_doc_rev/2 :: (ne_binary(), ne_binary()) -> {'ok', ne_binary()} |
                                                        {'error', atom()}.
lookup_doc_rev(DbName, DocId) ->
    couch_util:lookup_doc_rev(get_conn(), DbName, DocId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec save_doc/2 :: (ne_binary(), wh_json:json_object() | wh_json:json_objects()) -> {'ok', wh_json:json_object() | wh_json:json_objects()} |
                                                                                     {'error', atom()}.
save_doc(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).

%% save a document; if it fails to save because of conflict, pull the latest revision and try saving again.
%% any other error is returned
-spec ensure_saved/2 :: (ne_binary(), wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                                {'error', atom()}.
-spec ensure_saved/3 :: (ne_binary(), wh_json:json_object(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', atom()}.

ensure_saved(DbName, Doc) ->
    ensure_saved(DbName, Doc, []).

ensure_saved(DbName, Doc, Options) ->
    couch_util:ensure_saved(get_conn(), DbName, Doc, Options).

-spec save_doc/3 :: (ne_binary(), wh_json:json_object(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                        {'error', atom()}.
save_doc(DbName, Doc, Opts) ->
    couch_util:save_doc(get_conn(), DbName, Doc, Opts).

-spec save_docs/2 :: (ne_binary(), wh_json:json_objects()) -> {'ok', wh_json:json_objects()}.
-spec save_docs/3 :: (ne_binary(), wh_json:json_objects(), proplist()) -> {'ok', wh_json:json_objects()}.
save_docs(DbName, Docs) when is_list(Docs) ->
    save_docs(DbName, Docs, []).
save_docs(DbName, Docs, Opts) when is_list(Docs) ->
    couch_util:save_docs(get_conn(), DbName, Docs, Opts).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch, update and save a doc (creating if not present)
%% @end
%%--------------------------------------------------------------------
-spec update_doc/3 :: (ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                {'error', atom()}.
-spec update_doc/4 :: (ne_binary(), ne_binary(), proplist(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                            {'error', atom()}.

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
-spec del_doc/2 :: (ne_binary(), wh_json:json_object() | ne_binary()) -> {'ok', wh_json:json_objects()} |
                                                                         couchbeam_error().
del_doc(DbName, Doc) ->
    couch_util:del_doc(get_conn(), DbName, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
-spec del_docs/2 :: (ne_binary(), wh_json:json_objects()) -> {'ok', wh_json:json_objects()}.
del_docs(DbName, Docs) ->
    couch_util:del_docs(get_conn(), DbName, Docs).

%%%===================================================================
%%% Attachment Functions
%%%===================================================================
-spec fetch_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', ne_binary()} |
                                                                       {'error', atom()}.
fetch_attachment(DbName, DocId, AName) ->
    couch_util:fetch_attachment(get_conn(), DbName, DocId, AName).

-spec stream_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', reference()} |
                                                                        {'error', term()}.
stream_attachment(DbName, DocId, AName) ->
    couch_util:stream_attachment(get_conn(), DbName, DocId, AName, self()).

-spec put_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                  {'error', atom()}.
%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec put_attachment/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                                              {'error', atom()}.
put_attachment(DbName, DocId, AName, Contents) ->
    couch_util:put_attachment(get_conn(), DbName, DocId, AName, Contents).
put_attachment(DbName, DocId, AName, Contents, Options) ->
    couch_util:put_attachment(get_conn(), DbName, DocId, AName, Contents, Options).

-spec delete_attachment/3 :: (ne_binary(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                        {'error', atom()}.
-spec delete_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), proplist()) -> {'ok', wh_json:json_object()} |
                                                                                    {'error', atom()}.
delete_attachment(DbName, DocId, AName) ->
    couch_util:delete_attachment(get_conn(), DbName, DocId, AName).
delete_attachment(DbName, DocId, AName, Options) ->
    couch_util:delete_attachment(get_conn(), DbName, DocId, AName, Options).

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
-spec get_all_results/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_objects() | wh_json:json_strings()} |
                                                         {'error', atom()}.
-spec get_results/3 :: (ne_binary(), ne_binary(), wh_proplist()) -> {'ok', wh_json:json_objects() | wh_json:json_strings()} |
                                                                    {'error', atom()}.
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).
get_results(DbName, DesignDoc, ViewOptions) ->
    couch_util:get_results(get_conn(), DbName, DesignDoc, ViewOptions).

-spec get_result_keys/1 :: (wh_json:json_objects()) -> [wh_json:json_string(),...] | [].
get_result_keys(JObjs) ->
    lists:map(fun get_keys/1, JObjs).

-spec get_keys/1 :: (wh_json:json_object()) -> wh_json:json_string().
get_keys(JObj) ->
    wh_json:get_value(<<"key">>, JObj).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% set the host to connect to
-spec set_host/1 :: (string()) -> 'ok' |
                                  {'error', 'unable_to_connect'}.
set_host(HostName) ->
    set_host(HostName, ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT).

-spec set_host/2 :: (string(), non_neg_integer()) -> 'ok' |
                                                     {'error', 'unable_to_connect'}.
set_host(HostName, Port) ->
    set_host(HostName, Port, "", "", ?DEFAULT_ADMIN_PORT).

-spec set_host/3 :: (string(), string(), string()) -> 'ok' |
                                                      {'error', 'unable_to_connect'}.
set_host(HostName, UserName, Password) ->
    set_host(HostName, ?DEFAULT_PORT, UserName, Password, ?DEFAULT_ADMIN_PORT).

-spec set_host/4 :: (string(), non_neg_integer(), string(), string()) -> 'ok' |
                                                                         {'error', 'unable_to_connect'}.
set_host(HostName, Port, UserName, Password) ->
    set_host(HostName, Port, UserName, Password, ?DEFAULT_ADMIN_PORT).

-spec set_host/5 :: (string(), non_neg_integer(), string(), string(), non_neg_integer()) -> 'ok' |
                                                                                            {'error', 'unable_to_connect'}.
set_host(HostName, Port, UserName, Password, AdminPort) ->
    gen_server:call(?SERVER, {set_host, HostName, Port, UserName, Password, AdminPort}, infinity).

get_host() ->
    gen_server:call(?SERVER, get_host).

get_port() ->
    gen_server:call(?SERVER, get_port).

get_admin_port() ->
    gen_server:call(?SERVER, get_admin_port).

get_creds() ->
    gen_server:call(?SERVER, get_creds).

get_conn() ->
    case whereis(?SERVER) of
        Srv when is_pid(Srv) ->
            gen_server:call(?SERVER, get_conn);
        _E ->
            lager:debug("no server by the name of ~s", [?SERVER]),
            ST = erlang:get_stacktrace(),
            _ = [lager:debug("st: ~p", [T]) || T <- ST],
            exit(couch_mgr_died)
    end.

get_admin_conn() ->
    gen_server:call(?SERVER, get_admin_conn).

-spec get_uuid/0 :: () -> ne_binary().
get_uuid() ->
    wh_util:rand_hex_binary(16).

-spec get_uuids/1 :: (pos_integer()) -> [ne_binary(),...].
get_uuids(Count) ->
    [get_uuid() || _ <- lists:seq(1, Count)].

-spec get_node_cookie/0 :: () -> atom().
get_node_cookie() ->
    couch_config:fetch(bigcouch_cookie, monster).

-spec set_node_cookie/1 :: (atom()) -> {'ok', wh_json:json_object()}.
set_node_cookie(Cookie) when is_atom(Cookie) ->
    couch_config:store(bigcouch_cookie, Cookie).

-spec load_configs/0 :: () -> 'ok'.
load_configs() ->
    gen_server:cast(?SERVER, load_configs).

-spec get_url/0 :: () -> ne_binary() | 'undefined'.
-spec get_admin_url/0 :: () -> ne_binary() | 'undefined'.
get_url() ->
    get_url(wh_util:to_binary(get_host()), get_creds(), get_port()).
get_admin_url() ->
    get_url(wh_util:to_binary(get_host()), get_creds(), get_admin_port()).

-spec get_url/3 :: (binary(), {string(), string()}, pos_integer()) -> ne_binary() | 'undefined'.
get_url(<<>>, _, _) -> 'undefined';
get_url(H, {[], []}, P) ->
    list_to_binary(["http://", H, ":", wh_util:to_binary(P), "/"]);
get_url(H, {User, Pwd}, P) ->
    list_to_binary(["http://", wh_util:to_binary(User), ":", wh_util:to_binary(Pwd)
                    ,"@", H, ":", wh_util:to_binary(P), "/"
                   ]).

add_change_handler(DBName, DocID) ->
    lager:debug("add change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), self()}).

add_change_handler(DBName, DocID, Pid) ->
    lager:debug("add change handler for Pid: ~p for DB: ~s and Doc: ~s", [Pid, DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), Pid}).

rm_change_handler(DBName, DocID) ->
    lager:debug("rm change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
    gen_server:call(?SERVER, {rm_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID)}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init/1 :: ([]) -> {'ok', #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting couch_mgr"),
    {ok, init_state()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_host, _From, #state{host={H,_,_}}=State) ->
    {reply, H, State};

handle_call(get_port, _From, #state{host={_,P,_}}=State) ->
    {reply, P, State};

handle_call(get_admin_port, _From, #state{host={_,_,P}}=State) ->
    {reply, P, State};

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, #state{host={OldHost,_,_}}=State) ->
    lager:debug("updating host from ~p to ~p", [OldHost, Host]),
    try {couch_util:get_new_connection(Host, Port, User, Pass)
         ,couch_util:get_new_connection(Host, AdminPort, User, Pass)} of
        {Conn, AdminConn} ->
            _ = couch_config:store(couch_host, {Host, Port, User, Pass, AdminPort}),

            {reply, ok, State#state{host={Host, Port, AdminPort}
                                    ,connection=Conn
                                    ,admin_connection=AdminConn
                                    ,change_handlers=dict:new()
                                    ,creds={User,Pass}
                                   }}
    catch
        _:_ ->
            lager:debug("failed to connect to ~s:~p or ~p", [Host, Port, AdminPort]),
            lager:info("We tried to connect to BigCouch at ~s:~p and ~p but were refused. Is BigCouch/HAProxy running at these host:port combos?", [Host, Port, AdminPort]),
            {reply, {error, unable_to_connect}, State}
    end;

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, State) ->
    lager:debug("setting host for the first time to ~p", [Host]),

    try {couch_util:get_new_connection(Host, Port, User, Pass)
         ,couch_util:get_new_connection(Host, AdminPort, User, Pass)} of
        {Conn, AdminConn} ->
            _ = couch_config:store(couch_host, {Host, Port, User, Pass, AdminPort}),

            {reply, ok, State#state{host={Host,Port,AdminPort}
                                    ,connection=Conn
                                    ,admin_connection=AdminConn
                                    ,change_handlers=dict:new()
                                    ,creds={User,Pass}
                                   }}
    catch
        _:_ ->
            lager:debug("failed to connect to ~s:~p or ~p", [Host, Port, AdminPort]),
            lager:info("We tried to connect to BigCouch at ~s:~p and ~p but were refused. Is BigCouch/HAProxy running at these host:port combos?", [Host, Port, AdminPort]),
            {reply, {error, unable_to_connect}, State}
    end;

handle_call(get_conn, _, #state{connection=S}=State) ->
    {reply, S, State};

handle_call(get_admin_conn, _, #state{admin_connection=ACon}=State) ->
    {reply, ACon, State};

handle_call(get_creds, _, #state{creds=Cred}=State) ->
    {reply, Cred, State};

handle_call({rm_change_handler, DBName, DocID}, {Pid, _Ref}, #state{change_handlers=CH}=State) ->
    spawn(fun() ->
                  {ok, {Srv, _}} = dict:find(DBName, CH),
                  lager:debug("found CH(~p): Rm listener(~p) for db:doc ~s:~s", [Srv, Pid, DBName, DocID]),
                  change_handler:rm_listener(Srv, Pid, DocID)
          end),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(load_configs, State) ->
    couch_config:ready(),
    {noreply, State};
handle_cast({add_change_handler, DBName, DocID, Pid}, #state{change_handlers=CH, connection=S}=State) ->
    case dict:find(DBName, CH) of
        {ok, {Srv, _}} ->
            lager:debug("found change handler(~p): adding listener(~p) for ~s:~s", [Srv, Pid, DBName, DocID]),
            change_handler:add_listener(Srv, Pid, DocID),
            {noreply, State};
        error ->
            {ok, Srv} = change_mgr_sup:start_handler(couch_util:get_db(S, DBName), []),
            lager:debug("started change handler(~p): adding listener(~p) for ~s:~s", [Srv, Pid, DBName, DocID]),
            SrvRef = erlang:monitor(process, Srv),
            change_handler:add_listener(Srv, Pid, DocID),
            {noreply, State#state{change_handlers=dict:store(DBName, {Srv, SrvRef}, CH)}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Srv, complete}, #state{change_handlers=CH}=State) ->
    lager:debug("srv ~p down after complete", [Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info({'DOWN', Ref, process, Srv, {error,connection_closed}}, #state{change_handlers=CH}=State) ->
    lager:debug("srv ~p down after conn closed", [Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info(_Info, State) ->
    lager:debug("unexpected message ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_state/0 :: () -> #state{}.
init_state() ->
    case couch_config:fetch(<<"couch_host">>) of
        undefined ->
            lager:debug("starting conns with default_couch_host"),
            init_state_from_config(couch_config:fetch(<<"default_couch_host">>));
        HostData ->
            lager:debug("starting conns with couch_host"),
            init_state_from_config(HostData)
    end.

init_state_from_config(undefined) ->
    init_state_from_config({"localhost", ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT});
init_state_from_config(null) ->
    init_state_from_config({"localhost", ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT});
init_state_from_config(H) when not is_tuple(H) ->
    init_state_from_config({H, ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT});
init_state_from_config({H, Port}) ->
    init_state_from_config({H, Port, "", "", ?DEFAULT_ADMIN_PORT});
init_state_from_config({H, User, Pass}) ->
    init_state_from_config({H, ?DEFAULT_PORT, User, Pass, ?DEFAULT_ADMIN_PORT});
init_state_from_config({H, Port, User, Pass}) ->
    init_state_from_config({H, Port, User, Pass, ?DEFAULT_ADMIN_PORT});
init_state_from_config({H, Port, User, Pass, AdminPort}) ->
    try {couch_util:get_new_connection(H, wh_util:to_integer(Port), User, Pass)
         ,couch_util:get_new_connection(H, wh_util:to_integer(AdminPort), User, Pass)} of
        {Conn, AdminConn} ->
            lager:debug("returning state record"),
            couch_config:ready(),
            #state{connection=Conn
                   ,admin_connection=AdminConn
                   ,host={H, wh_util:to_integer(Port), wh_util:to_integer(AdminPort)}
                   ,creds={User, Pass}
                  }
    catch
        error:{badmatch,{error,{conn_failed,{error,econnrefused}}}} ->
            lager:debug("connection to ~s:~p refused", [H, Port]),
            lager:info("We tried to connect to BigCouch at ~s:~p and ~p but were refused. Is BigCouch/HAProxy running at these host:port combos?", [H, Port, AdminPort]),
            exit(couch_connection_failed);
        A:B ->
            ST = erlang:get_stacktrace(),
            lager:debug("init failed to connect: ~p:~p", [A, B]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            A(B)
    end.

-spec remove_ref/2 :: (reference(), dict()) -> dict().
remove_ref(Ref, CH) ->
    dict:filter(fun(_, {_, Ref1}) when Ref1 =:= Ref -> false;
                   (_, _) -> true end, CH).
