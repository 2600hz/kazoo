%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage CouchDB connections
%%% @end
%%% Created : 16 Sep 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0, set_host/1, set_host/2, set_host/3, set_host/4, set_host/5, get_host/0, get_port/0, get_creds/0, get_url/0, get_uuid/0, get_uuids/1]).
-export([get_admin_port/0, get_admin_conn/0, get_admin_url/0, get_node_cookie/0, set_node_cookie/1]).

%% System manipulation
-export([db_exists/1, db_info/0, db_info/1, db_create/1, db_create/2, db_compact/1, db_view_cleanup/1, db_delete/1, db_replicate/1]).
-export([admin_db_info/0, admin_db_info/1, admin_db_compact/1, admin_db_view_cleanup/1]).

-export([design_info/2, admin_design_info/2, design_compact/2, admin_design_compact/2]).

%% Document manipulation
-export([save_doc/2, save_doc/3, save_docs/2, save_docs/3, open_doc/2, open_doc/3, del_doc/2, del_docs/2, lookup_doc_rev/2]).
-export([add_change_handler/2, add_change_handler/3, rm_change_handler/2, load_doc_from_file/3, update_doc_from_file/3, revise_doc_from_file/3]).
-export([revise_docs_from_folder/3, revise_views_from_folder/2, ensure_saved/2]).

-export([all_docs/1, all_design_docs/1, admin_all_docs/1]).
-export([all_docs/2, all_design_docs/2, admin_all_docs/2]).

%% attachments
-export([fetch_attachment/3, put_attachment/4, put_attachment/5, delete_attachment/3, delete_attachment/4]).

%% Views
-export([get_all_results/2, get_results/3]).
-export([get_result_keys/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("wh_couch.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_FILE, [code:lib_dir(whistle_couch, priv), "/startup.config"]).

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% Change handler {DBName :: string(), {Srv :: pid(), SrvRef :: reference()}
-record(state, {
          host = {"", ?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT} :: tuple(string(), integer(), integer())
	  ,connection = #server{} :: #server{}
	  ,admin_connection = #server{} :: #server{}
	  ,creds = {"", ""} :: tuple(string(), string()) % {User, Pass}
	  ,change_handlers = dict:new() :: dict()
	  ,cache = undefined :: undefined | pid()
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
-spec load_doc_from_file/3 :: (DbName, App, File) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      App :: atom(),
      File :: list() | binary().
load_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", wh_util:to_list(File)]),
    ?LOG_SYS("Read into db ~s from CouchDB JSON file: ~s", [DbName, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	?MODULE:save_doc(DbName, mochijson2:decode(Bin)) %% if it crashes on the match, the catch will let us know
    catch
        _Type:{badmatch,{error,Reason}} ->
	    ?LOG_SYS("badmatch error: ~p", [Reason]),
            {error, Reason};
 	_Type:Reason ->
	    ?LOG_SYS("exception: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Overwrite the existing contents of a document with the contents of
%% a file
%% @end
%%--------------------------------------------------------------------
-spec update_doc_from_file/3 :: (DbName, App, File) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      App :: atom(),
      File :: list() | binary().
update_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    ?LOG_SYS("Update db ~s from CouchDB file: ~s", [DbName, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	JObj = mochijson2:decode(Bin),
	{ok, Rev} = ?MODULE:lookup_doc_rev(DbName, wh_json:get_value(<<"_id">>, JObj)),
	?MODULE:save_doc(DbName, wh_json:set_value(<<"_rev">>, Rev, JObj))
    catch
        _Type:{badmatch,{error,Reason}} ->
	    ?LOG_SYS("bad match: ~p", [Reason]),
            {error, Reason};
 	_Type:Reason ->
	    ?LOG_SYS("exception: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create or overwrite the existing contents of a document with the
%% contents of a file
%% @end
%%--------------------------------------------------------------------
-spec revise_doc_from_file/3 :: (DbName, App, File) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      App :: atom(),
      File :: list() | binary().
revise_doc_from_file(DbName, App, File) ->
    case ?MODULE:update_doc_from_file(DbName, App, File) of
        {error, _E} ->
	    ?LOG_SYS("failed to update doc: ~p", [_E]),
            ?MODULE:load_doc_from_file(DbName, App, File);
        {ok, _}=Resp ->
	    ?LOG_SYS("revised ~s", [File]),
	    Resp
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications priv/couchdb/views/ folder
%% into a given database
%% @end
%%--------------------------------------------------------------------
-spec revise_views_from_folder/2 :: (DbName, App) -> ok when
      DbName :: binary(),
      App :: atom().
revise_views_from_folder(DbName, App) ->
    revise_docs_from_folder(DbName, App, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Loads all .json files in an applications folder, relative to
%% priv/couchdb/ into a given database
%% @end
%%--------------------------------------------------------------------
-spec revise_docs_from_folder/3 :: (DbName, App, Folder) -> ok when
      DbName :: binary(),
      App :: atom(),
      Folder :: list().
revise_docs_from_folder(DbName, App, Folder) ->
    Files = filelib:wildcard(lists:flatten([code:priv_dir(App), "/couchdb/", Folder, "/*.json"])),
    do_revise_docs_from_folder(DbName, Files).

-spec do_revise_docs_from_folder/2 :: (Db, Docs) -> ok when
      Db :: binary(),
      Docs :: [string(),...] | [].
do_revise_docs_from_folder(_, []) ->
    ok;
do_revise_docs_from_folder(DbName, [H|T]) ->
    try
        {ok, Bin} = file:read_file(H),
        JObj = mochijson2:decode(Bin),
        timer:sleep(250),
        case lookup_doc_rev(DbName, wh_json:get_value(<<"_id">>, JObj)) of
            {ok, Rev} ->
                ?LOG_SYS("update doc from file ~s in ~s", [H, DbName]),
                save_doc(DbName, wh_json:set_value(<<"_rev">>, Rev, JObj));
            {error, not_found} ->
                ?LOG_SYS("import doc from file ~s in ~s", [H, DbName]),
                save_doc(DbName, JObj);
            {error, Reason} ->
                ?LOG_SYS("failed to load doc ~s into ~s, ~p", [H, DbName, Reason])
        end,
        do_revise_docs_from_folder(DbName, T)
    catch
        _:_ ->
            do_revise_docs_from_folder(DbName, T)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_exists/1 :: (DbName) -> boolean() when
      DbName :: binary().
db_exists(DbName) ->
    couch_util:db_exists(get_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding all databases
%% @end
%%--------------------------------------------------------------------
-spec db_info/0 :: () -> {ok, [binary(),...] | []} | {error, atom()}.
db_info() ->
    couch_util:db_info(get_conn()).

-spec admin_db_info/0 :: () -> {ok, [binary(),...] | []} | {error, atom()}.
admin_db_info() ->
    couch_util:db_info(get_admin_conn()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec db_info/1 :: (DbName) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary().
db_info(DbName) ->
    couch_util:db_info(get_conn(), DbName).

-spec admin_db_info/1 :: (DbName) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary().
admin_db_info(DbName) ->
    couch_util:db_info(get_admin_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database design doc
%% @end
%%--------------------------------------------------------------------
-spec design_info/2 :: (DbName, DesignName) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DesignName :: binary().
design_info(DbName, DesignName) ->
    couch_util:design_info(get_conn(), DbName, DesignName).

-spec admin_design_info/2 :: (DbName, DesignName) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DesignName :: binary().
admin_design_info(DbName, DesignName) ->
    couch_util:design_info(get_admin_conn(), DbName, DesignName).

-spec design_compact/2 :: (DbName, Design) -> boolean() when
      DbName :: binary(),
      Design :: binary().
design_compact(DbName, Design) ->
    couch_util:design_compact(get_conn(), DbName, Design).

-spec admin_design_compact/2 :: (DbName, Design) -> boolean() when
      DbName :: binary(),
      Design :: binary().
admin_design_compact(DbName, Design) ->
    couch_util:design_compact(get_admin_conn(), DbName, Design).

-spec db_view_cleanup/1 :: (DbName) -> boolean() when
      DbName :: binary().
db_view_cleanup(DbName) ->
    couch_util:db_view_cleanup(get_conn(), DbName).

-spec admin_db_view_cleanup/1 :: (DbName) -> boolean() when
      DbName :: binary().
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
-spec db_replicate/1 :: (Prop :: tuple(struct, proplist()) | proplist()) -> {ok, json_object()} | {error, atom()}.
db_replicate(Prop) when is_list(Prop) ->
    db_replicate({struct, Prop});
db_replicate({struct, _}=MochiJson) ->
    couch_util:db_replicate(get_conn(), MochiJson).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec db_create/1 :: (DbName) -> boolean() when
      DbName :: binary().
db_create(DbName) ->
    db_create(DbName, []).

-spec db_create/2 :: (DbName, Options) -> boolean() when
      DbName :: binary(),
      Options :: [{q,integer()} | {n,integer()},...] | [].
db_create(DbName, Options) ->
    couch_util:db_create(get_conn(), DbName, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec db_compact/1 :: (DbName) -> boolean() when
      DbName :: binary().
db_compact(DbName) ->
    couch_util:db_compact(get_conn(), DbName).

-spec admin_db_compact/1 :: (DbName) -> boolean() when
      DbName :: binary().
admin_db_compact(DbName) ->
    couch_util:db_compact(get_admin_conn(), DbName).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete a database
%% @end
%%--------------------------------------------------------------------
-spec db_delete/1 :: (DbName) -> boolean() when
      DbName :: binary().
db_delete(DbName) ->
    couch_util:db_delete(get_conn(), DbName).

%%%===================================================================
%%% Document Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------
-spec open_doc/2 :: (DbName, DocId) -> {ok, json_object()} | {error, not_found | db_not_reachable} when
      DbName :: binary(),
      DocId :: binary().
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec open_doc/3 :: (DbName, DocId, Options) -> {ok, json_object()} | {error, not_found | db_not_reachable} when
      DbName :: binary(),
      DocId :: binary(),
      Options :: proplist().
open_doc(DbName, DocId, Options) ->
    couch_util:open_doc(get_conn(), DbName, DocId, Options).

-spec all_docs/1 :: (DbName) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary().
-spec admin_all_docs/1 :: (DbName) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary().
all_docs(DbName) ->
    couch_util:all_docs(get_conn(), DbName, []).
admin_all_docs(DbName) ->
    couch_util:all_docs(get_admin_conn(), DbName, []).

-spec all_docs/2 :: (DbName, Options) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Options :: proplist().
-spec admin_all_docs/2 :: (DbName, Options) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Options :: proplist().
all_docs(DbName, Options) ->
    couch_util:all_docs(get_conn(), DbName, Options).
admin_all_docs(DbName, Options) ->
    couch_util:all_docs(get_admin_conn(), DbName, Options).

-spec all_design_docs/1 :: (DbName) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary().
all_design_docs(DbName) ->
    couch_util:all_design_docs(get_conn(), DbName, []).

-spec all_design_docs/2 :: (DbName, Options) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Options :: proplist().
all_design_docs(DbName, Options) ->
    couch_util:all_design_docs(get_conn(), DbName, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec lookup_doc_rev/2 :: (DbName, DocId) -> {error, atom()} | {ok, binary()} when
      DbName :: binary(),
      DocId :: binary().
lookup_doc_rev(DbName, DocId) ->
    couch_util:lookup_doc_rev(get_conn(), DbName, DocId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec save_doc/2 :: (DbName, Doc) -> {ok, json_object()} | {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Doc :: proplist() | json_object() | json_objects().
save_doc(DbName, [{struct, [_|_]}=Doc]) ->
    save_doc(DbName, Doc, []);
save_doc(DbName, [{struct, _}|_]=Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) when is_list(Doc) ->
    save_doc(DbName, {struct, Doc}, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).

%% save a document; if it fails to save because of conflict, pull the latest revision and try saving again.
%% any other error is returned
-spec ensure_saved/2 :: (DbName, Doc) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      Doc :: json_object().
ensure_saved(DbName, Doc) ->
    couch_util:ensure_saved(get_conn(), DbName, Doc, []).

-spec save_doc/3 :: (DbName, Doc, Opts) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      Doc :: json_object(),
      Opts :: proplist().
save_doc(DbName, {struct, _}=Doc, Opts) ->
    couch_util:save_doc(get_conn(), DbName, Doc, Opts).

-spec save_docs/2 :: (DbName, Docs) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Docs :: json_objects().
save_docs(DbName, Docs) ->
    save_docs(DbName, Docs, []).

-spec save_docs/3 :: (DbName, Docs, Opts) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Docs :: json_objects(),
      Opts :: proplist().
save_docs(DbName, Docs, Opts) ->
    couch_util:save_docs(get_conn(), DbName, Docs, Opts).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec del_doc/2 :: (DbName, Doc) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      Doc :: json_object() | binary().
del_doc(DbName, Doc) ->
    couch_util:del_doc(get_conn(), DbName, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
-spec del_docs/2 :: (DbName, Docs) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      Docs :: json_objects().
del_docs(DbName, Docs) ->
    couch_util:del_docs(get_conn(), DbName, Docs).

%%%===================================================================
%%% Attachment Functions
%%%===================================================================
-spec fetch_attachment/3 :: (DbName, DocId, AName) -> {ok, binary()} | {error, atom()} when
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary().
fetch_attachment(DbName, DocId, AName) ->
    couch_util:fetch_attachment(get_conn(), DbName, DocId, AName).

-spec put_attachment/4 :: (DbName, DocId, AName, Contents) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Contents :: binary().
put_attachment(DbName, DocId, AName, Contents) ->
    couch_util:put_attachment(get_conn(), DbName, DocId, AName, Contents).

%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec put_attachment/5 :: (DbName, DocId, AName, Contents, Options) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Contents :: binary(),
      Options :: proplist().
put_attachment(DbName, DocId, AName, Contents, Options) ->
    couch_util:put_attachment(get_conn(), DbName, DocId, AName, Contents, Options).

-spec delete_attachment/3 :: (DbName, DocId, AName) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary().
delete_attachment(DbName, DocId, AName) ->
    couch_util:delete_attachment(get_conn(), DbName, DocId, AName).

-spec delete_attachment/4 :: (DbName, DocId, AName, Options) -> {ok, json_object()} | {error, atom()} when
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Options :: proplist().
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
-spec get_all_results/2 :: (DbName, DesignDoc) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      DesignDoc :: binary().
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec get_results/3 :: (DbName, DesignDoc, ViewOptions) -> {ok, json_objects()} | {error, atom()} when
      DbName :: binary(),
      DesignDoc :: binary(),
      ViewOptions :: proplist().
get_results(DbName, DesignDoc, ViewOptions) ->
    couch_util:get_results(get_conn(), DbName, DesignDoc, ViewOptions).

-spec get_result_keys/1 :: (JObjs) -> [binary(),...] | [] when
      JObjs :: json_objects().
get_result_keys(JObjs) ->
    lists:map(fun get_keys/1, JObjs).

-spec get_keys/1 :: (JObj) -> binary() when
      JObj :: json_object().
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
-spec start_link/0 :: () -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% set the host to connect to
-spec set_host/1 :: (HostName) -> ok | {error, term()} when
      HostName :: string().
set_host(HostName) ->
    set_host(HostName, ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT).

-spec(set_host/2 :: (HostName :: string(), Port :: integer()) -> ok | tuple(error, term())).
set_host(HostName, Port) ->
    set_host(HostName, Port, "", "", ?DEFAULT_ADMIN_PORT).

-spec(set_host/3 :: (HostName :: string(), UserName :: string(), Password :: string()) -> ok | tuple(error, term())).
set_host(HostName, UserName, Password) ->
    set_host(HostName, ?DEFAULT_PORT, UserName, Password, ?DEFAULT_ADMIN_PORT).

-spec(set_host/4 :: (HostName :: string(), Port :: integer(), UserName :: string(), Password :: string()) -> ok | tuple(error, term())).
set_host(HostName, Port, UserName, Password) ->
    set_host(HostName, Port, UserName, Password, ?DEFAULT_ADMIN_PORT).

-spec(set_host/5 :: (HostName :: string(), Port :: integer(), UserName :: string(), Password :: string(), AdminPort :: integer()) -> ok | tuple(error, term())).
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
    gen_server:call(?SERVER, get_conn).

get_admin_conn() ->
    gen_server:call(?SERVER, get_admin_conn).

-spec get_uuid/0 :: () -> binary().
get_uuid() ->
    [UUID] = couchbeam:get_uuid(get_conn()),
    wh_util:to_binary(UUID).

-spec get_uuids/1 :: (Count) -> [binary(),...] when
      Count :: pos_integer().
get_uuids(Count) ->
    Conn = get_conn(),
    [wh_util:to_binary(UUID) || UUID <- couchbeam:get_uuids(Conn, Count)].

-spec get_node_cookie/0 :: () -> atom().
get_node_cookie() ->
    case wh_cache:fetch_local(get_cache_pid(), bigcouch_cookie) of
	{ok, Cookie} -> Cookie;
	{error, not_found} -> set_node_cookie(monster), monster
    end.

-spec set_node_cookie/1 :: (Cookie) -> ok when
      Cookie :: atom().
set_node_cookie(Cookie) when is_atom(Cookie) ->
    wh_cache:store_local(get_cache_pid(), bigcouch_cookie, Cookie, 24 * 3600).

-spec get_url/0 :: () -> binary().
get_url() ->
    case {wh_util:to_binary(get_host()), get_creds(), get_port()} of
        {<<"">>, _, _} ->
            undefined;
        {H, {[], []}, P} ->
            <<"http://", H/binary, ":", (wh_util:to_binary(P))/binary, $/>>;
        {H, {User, Pwd}, P} ->
            <<"http://"
              ,(wh_util:to_binary(User))/binary, $: ,(wh_util:to_binary(Pwd))/binary
              ,$@, H/binary
              ,":", (wh_util:to_binary(P))/binary, $/>>
    end.

-spec(get_admin_url/0 :: () -> binary()).
get_admin_url() ->
    case {wh_util:to_binary(get_host()), get_creds(), get_admin_port()} of
        {<<"">>, _, _} ->
            undefined;
        {H, {[], []}, P} ->
            <<"http://", H/binary, ":", (wh_util:to_binary(P))/binary, $/>>;
        {H, {User, Pwd}, P} ->
            <<"http://"
              ,(wh_util:to_binary(User))/binary, $: ,(wh_util:to_binary(Pwd))/binary
              ,$@, H/binary
              ,":", (wh_util:to_binary(P))/binary, $/>>
    end.

-spec get_cache_pid/0 :: () -> pid() | undefined.
get_cache_pid() ->
    gen_server:call(?SERVER, get_cache_pid).

add_change_handler(DBName, DocID) ->
    ?LOG_SYS("Add change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), self()}).

add_change_handler(DBName, DocID, Pid) ->
    ?LOG_SYS("Add change handler for Pid: ~p for DB: ~s and Doc: ~s", [Pid, DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), Pid}).

rm_change_handler(DBName, DocID) ->
    ?LOG_SYS("RM change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
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
-spec(init/1 :: (Args :: list()) -> tuple(ok, tuple())).
init(_) ->
    process_flag(trap_exit, true),
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
handle_call(get_cache_pid, _From, #state{cache=C}=State) ->
    {reply, C, State};

handle_call(get_host, _From, #state{host={H,_,_}}=State) ->
    {reply, H, State};

handle_call(get_port, _From, #state{host={_,P,_}}=State) ->
    {reply, P, State};

handle_call(get_admin_port, _From, #state{host={_,_,P}}=State) ->
    {reply, P, State};

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, #state{host={OldHost,_,_}}=State) ->
    ?LOG_SYS("Updating host from ~p to ~p", [OldHost, Host]),
    Conn = couch_util:get_new_connection(Host, Port, User, Pass),
    AdminConn = couch_util:get_new_connection(Host, AdminPort, User, Pass),
    spawn(fun() -> save_config(Host, Port, User, Pass, AdminPort) end),

    {reply, ok, State#state{host={Host, Port, AdminPort}
			    ,connection=Conn
			    ,admin_connection=AdminConn
			    ,change_handlers=dict:new()
			    ,creds={User,Pass}
			   }};

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, State) ->
    ?LOG_SYS("Setting host for the first time to ~p", [Host]),
    Conn = couch_util:get_new_connection(Host, Port, User, Pass),
    AdminConn = couch_util:get_new_connection(Host, AdminPort, User, Pass),
    spawn(fun() -> save_config(Host, Port, User, Pass, AdminPort) end),

    {reply, ok, State#state{host={Host,Port,AdminPort}
			    ,connection=Conn
			    ,admin_connection=AdminConn
			    ,change_handlers=dict:new()
			    ,creds={User,Pass}
			   }};

handle_call(get_conn, _, #state{connection=S}=State) ->
    {reply, S, State};

handle_call(get_admin_conn, _, #state{admin_connection=ACon}=State) ->
    {reply, ACon, State};

handle_call(get_creds, _, #state{creds=Cred}=State) ->
    {reply, Cred, State};

handle_call({rm_change_handler, DBName, DocID}, {Pid, _Ref}, #state{change_handlers=CH}=State) ->
    spawn(fun() ->
		  {ok, {Srv, _}} = dict:find(DBName, CH),
		  ?LOG_SYS("Found CH(~p): Rm listener(~p) for db:doc ~s:~s", [Srv, Pid, DBName, DocID]),
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
handle_cast({add_change_handler, DBName, DocID, Pid}, #state{change_handlers=CH, connection=S}=State) ->
    case dict:find(DBName, CH) of
	{ok, {Srv, _}} ->
	    ?LOG_SYS("Found CH(~p): Adding listener(~p) for db:doc ~s:~s", [Srv, Pid, DBName, DocID]),
	    change_handler:add_listener(Srv, Pid, DocID),
	    {noreply, State};
	error ->
	    {ok, Srv} = change_handler:start_link(couch_util:get_db(S, DBName), []),
	    ?LOG_SYS("Started CH(~p): added listener(~p) for db:doc ~s:~s", [Srv, Pid, DBName, DocID]),
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
    ?LOG_SYS("Srv ~p down after complete", [Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info({'DOWN', Ref, process, Srv, {error,connection_closed}}, #state{change_handlers=CH}=State) ->
    ?LOG_SYS("Srv ~p down after conn closed", [Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info(_Info, State) ->
    ?LOG_SYS("Unexpected message ~p", [_Info]),
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
-spec(init_state/0 :: () -> #state{}).
init_state() ->
    Pid = whereis(wh_couch_cache),
    case get_startup_config() of
	{ok, Ts} ->
	    {_, Host, NormalPort, User, Password, AdminPort} = case lists:keyfind(couch_host, 1, Ts) of
								   false ->
								       case lists:keyfind(default_couch_host, 1, Ts) of
									   false -> {ok, net_adm:localhost(), ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT};
									   {default_couch_host,H} -> {ok, H, ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT};
									   {default_couch_host,H,U,P} -> {ok, H, ?DEFAULT_PORT, U, P, ?DEFAULT_ADMIN_PORT};
									   {default_couch_host,H,Port,U,Pass} -> {ok, H, Port, U, Pass, ?DEFAULT_ADMIN_PORT};
									   {default_couch_host,H,Port,U,Pass,AdminP} -> {ok, H, Port, U, Pass, AdminP}
								       end;
								   {couch_host,H} -> {ok, H, ?DEFAULT_PORT, "", "", ?DEFAULT_ADMIN_PORT};
								   {couch_host,H,U,P} -> {ok, H, ?DEFAULT_PORT, U, P, ?DEFAULT_ADMIN_PORT};
								   {couch_host,H,Port,U,Pass} -> {ok, H, Port, U, Pass, ?DEFAULT_ADMIN_PORT};
								   {couch_host,H,Port,U,Pass,AdminP} -> {ok, H, Port, U, Pass, AdminP}
							       end,
	    Conn = couch_util:get_new_connection(Host, wh_util:to_integer(NormalPort), User, Password),
	    AdminConn = couch_util:get_new_connection(Host, wh_util:to_integer(AdminPort), User, Password),

	    Cookie = case lists:keyfind(bigcouch_cookie, 1, Ts) of
			 false -> monster;
			 {_, C} -> C
		     end,
	    wh_cache:store_local(Pid, bigcouch_cookie, Cookie, 24*3600), % store for a day

	    #state{connection=Conn
		   ,admin_connection=AdminConn
		   ,host={Host, wh_util:to_integer(NormalPort), wh_util:to_integer(AdminPort)}
		   ,creds={User, Password}
		   ,cache=Pid
		  };
	_ -> #state{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_startup_config/0 :: () -> tuple(ok, list(tuple())) | tuple(error, atom() | tuple())).
get_startup_config() ->
    file:consult(?STARTUP_FILE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(save_config/5 :: (H :: string(), Port :: integer(), U :: string(), P :: string(), AdminPort :: integer()) -> no_return()).
save_config(H, Port, U, P, AdminPort) ->
    {ok, Config} = get_startup_config(),
    {ok, Cookie} = wh_cache:fetch_local(whereis(wh_couch_cache), bigcouch_cookie),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun(Item, Acc) -> [io_lib:format("~p.~n", [Item]) | Acc] end
				 , "", [{bigcouch_cookie, Cookie}
					,{couch_host, H, Port, U, P, AdminPort}
					| lists:keydelete(couch_host, 1, Config)
				       ])
		   ).

-spec(remove_ref/2 :: (Ref :: reference(), CH :: dict()) -> dict()).
remove_ref(Ref, CH) ->
    dict:filter(fun(_, {_, Ref1}) when Ref1 =:= Ref -> false;
		   (_, _) -> true end, CH).
