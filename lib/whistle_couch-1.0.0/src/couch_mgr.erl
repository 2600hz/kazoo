%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage CouchDB connections
%%% @end
%%% Created : 16 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(couch_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0, set_host/1, set_host/2, set_host/3, set_host/4, set_host/5, get_host/0, get_port/0, get_creds/0, get_url/0, get_uuid/0, get_uuids/1]).
-export([get_admin_port/0, get_admin_conn/0, get_admin_url/0]).

%% System manipulation
-export([db_exists/1, db_info/0, db_info/1, db_create/1, db_compact/1, db_delete/1, db_replicate/1]).
-export([admin_db_info/0, admin_db_info/1, admin_db_compact/1]).

-export([design_info/2, admin_design_info/2, design_compact/2, admin_design_compact/2]).

%% Document manipulation
-export([save_doc/2, save_doc/3, save_docs/2, save_docs/3, open_doc/2, open_doc/3, del_doc/2, del_docs/2, lookup_doc_rev/2]).
-export([add_change_handler/2, add_change_handler/3, rm_change_handler/2, load_doc_from_file/3, update_doc_from_file/3]).

-export([all_docs/1, all_design_docs/1, admin_all_docs/1, admin_all_design_docs/1]).

%% attachments
-export([fetch_attachment/3, put_attachment/4, put_attachment/5, delete_attachment/3]).

%% Views
-export([get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("whistle/include/whistle_types.hrl"). % get the whistle types
-include_lib("couchbeam/include/couchbeam.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_FILE, [code:lib_dir(whistle_couch, priv), "/startup.config"]).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_ADMIN_PORT, 5986).
-define(IBROWSE_OPTS, [{max_sessions, 1024}, {max_pipeline_size, 10}]).

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% Change handler {DBName :: string(), {Srv :: pid(), SrvRef :: reference()}
-record(state, {
          host = {"", ?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT} :: tuple(string(), integer(), integer())
	  ,connection = #server{} :: #server{}
	  ,admin_connection = #server{} :: #server{}
	  ,creds = {"", ""} :: tuple(string(), string()) % {User, Pass}
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
-spec(load_doc_from_file/3 :: (DbName :: binary(), App :: atom(), File :: list() | binary()) -> tuple(ok, json_object()) | tuple(error, term())).
load_doc_from_file(DbName, App, File) ->
    Path = lists:flatten([code:priv_dir(App), "/couchdb/", whistle_util:to_list(File)]),
    logger:format_log(info, "Read into ~p from CouchDB dir: ~p~n", [DbName, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	?MODULE:save_doc(DbName, mochijson2:decode(Bin)) %% if it crashes on the match, the catch will let us know
    catch
        _Type:{badmatch,{error,Reason}} ->
            {error, Reason};
 	_Type:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Overwrite the existing contents of a document with the contents of
%% a file
%% @end
%%--------------------------------------------------------------------
-spec(update_doc_from_file/3 :: (DbName :: binary(), App :: atom(), File :: list() | binary()) -> tuple(ok, json_object()) | tuple(error, term())).
update_doc_from_file(DbName, App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    logger:format_log(info, "Update into ~p from CouchDB dir: ~p~n", [DbName, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	{struct, Prop} = mochijson2:decode(Bin),
	DocId = props:get_value(<<"_id">>, Prop),
	{ok, Rev} = ?MODULE:lookup_doc_rev(DbName, DocId),
	?MODULE:save_doc(DbName, {struct, [{<<"_rev">>, Rev} | Prop]})
    catch
        _Type:{badmatch,{error,Reason}} ->
            {error, Reason};
 	_Type:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec(db_exists/1 :: (DbName :: binary()) -> boolean()).
db_exists(DbName) ->
    case get_conn() of
        {} -> false;
        Conn -> couchbeam:db_exists(Conn, whistle_util:to_list(DbName))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding all databases
%% @end
%%--------------------------------------------------------------------
-spec(db_info/0 :: () -> tuple(ok, list(binary())) | tuple(error, atom())).
db_info() ->
    case get_conn() of
        {} -> {error, db_not_reachable};
        Conn ->
            case couchbeam:all_dbs(Conn) of
                {error, _Error}=E -> E;
                {ok, Info} -> {ok, Info}
            end
    end.

-spec(admin_db_info/0 :: () -> tuple(ok, list(binary())) | tuple(error, atom())).
admin_db_info() ->
    case get_admin_conn() of
        {} -> {error, db_not_reachable};
        Conn ->
            case couchbeam:all_dbs(Conn) of
                {error, _Error}=E -> E;
                {ok, Info} -> {ok, Info}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database
%% @end
%%--------------------------------------------------------------------
-spec(db_info/1 :: (DbName :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
db_info(DbName) ->
    case get_conn() of
        {} -> {error, db_not_reachable};
        Conn ->
            case couchbeam:db_info(#db{server=Conn, name=whistle_util:to_list(DbName)}) of
                {error, _Error}=E -> E;
                {ok, Info} -> {ok, Info}
            end
    end.

-spec(admin_db_info/1 :: (DbName :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
admin_db_info(DbName) ->
    case get_admin_conn() of
        {} -> {error, db_not_reachable};
        Conn ->
            couchbeam:db_info(#db{server=Conn, name=whistle_util:to_list(DbName)})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieve information regarding a database design doc
%% @end
%%--------------------------------------------------------------------
design_info(DbName, DesignName) ->
    case get_db(DbName) of
	{error, db_not_reachable}=E -> E;
	Conn ->
	    couchbeam:design_info(Conn, DesignName)
    end.

admin_design_info(DbName, DesignName) ->
    case get_admin_db(DbName) of
	{error, db_not_reachable}=E -> E;
	Conn ->
	    couchbeam:design_info(Conn, DesignName)
    end.

-spec(design_compact/2 :: (DbName :: binary(), Design :: binary()) -> boolean()).
design_compact(DbName, Design) ->
    case get_conn() of
        {} -> false;
        Conn ->
            case couchbeam:compact(#db{server=Conn, name=whistle_util:to_list(DbName)}, Design) of
                {error, _} -> false;
                ok -> true
            end
    end.

-spec(admin_design_compact/2 :: (DbName :: binary(), Design :: binary()) -> boolean()).
admin_design_compact(DbName, Design) ->
    case get_admin_conn() of
        {} -> false;
        Conn ->
            case couchbeam:compact(#db{server=Conn, name=whistle_util:to_list(DbName)}, Design) of
                {error, _} -> false;
                ok -> true
            end
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
-spec(db_replicate/1 :: (Prop :: tuple(struct, proplist()) | proplist()) -> tuple(ok, term()) | tuple(error, term())).
db_replicate(Prop) when is_list(Prop) ->
    db_replicate({struct, Prop});
db_replicate({struct, _}=MochiJson) ->
    couchbeam:replicate(get_conn(), MochiJson).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Detemine if a database exists
%% @end
%%--------------------------------------------------------------------
-spec(db_create/1 :: (DbName :: binary()) -> boolean()).
db_create(DbName) ->
    case get_conn() of
        {} -> false;
        Conn ->
            case couchbeam:create_db(Conn, whistle_util:to_list(DbName)) of
                {error, _} -> false;
                {ok, _} -> true
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Compact a database
%% @end
%%--------------------------------------------------------------------
-spec(db_compact/1 :: (DbName :: binary()) -> boolean()).
db_compact(DbName) ->
    case get_conn() of
        {} -> false;
        Conn ->
            case couchbeam:compact(#db{server=Conn, name=whistle_util:to_list(DbName)}) of
                {error, _} -> false;
                ok -> true
            end
    end.

-spec(admin_db_compact/1 :: (DbName :: binary()) -> boolean()).
admin_db_compact(DbName) ->
    case get_admin_conn() of
        {} -> false;
        Conn ->
            case couchbeam:compact(#db{server=Conn, name=whistle_util:to_list(DbName)}) of
                {error, _} -> false;
                ok -> true
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete a database
%% @end
%%--------------------------------------------------------------------
-spec(db_delete/1 :: (DbName :: binary()) -> boolean()).
db_delete(DbName) ->
    case get_conn() of
        {} -> false;
        Conn ->
            case couchbeam:delete_db(Conn, whistle_util:to_list(DbName)) of
                {error, _} -> false;
                {ok, _} -> true
            end
    end.

%%%===================================================================
%%% Document Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a doc id returns an error tuple or the json
%% @end
%%--------------------------------------------------------------------
-spec(open_doc/2 :: (DbName :: binary(), DocId :: binary()) -> tuple(ok, json_object()) | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec(open_doc/3 :: (DbName :: binary(), DocId :: binary(), Options :: proplist()) -> tuple(ok, json_object()) | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId, Options) when not is_binary(DocId) ->
    open_doc(DbName, whistle_util:to_binary(DocId), Options);
open_doc(DbName, DocId, Options) ->
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db -> couchbeam:open_doc(Db, DocId, Options)
    end.

all_docs(DbName) ->
    get_all_docs(get_db(DbName)).
admin_all_docs(DbName) ->
    get_all_docs(get_admin_db(DbName)).

get_all_docs({error, _}) -> {error, db_not_reachable};
get_all_docs(Db) ->
    {ok, View} = couchbeam:all_docs(Db),
    case couchbeam_view:fetch(View) of
	{ok, {struct, Prop}} ->
	    Rows = props:get_value(<<"rows">>, Prop, []),
	    {ok, Rows};
	{error, _Error}=E -> E
    end.

all_design_docs(DbName) ->
    get_all_design_docs(get_db(DbName)).
admin_all_design_docs(DbName) ->
    get_all_design_docs(get_admin_db(DbName)).

get_all_design_docs({error, _}) ->
    {error, db_not_reachable};
get_all_design_docs(Db) ->
    {ok, View} = couchbeam:view(Db, "_design_docs", []),
    case couchbeam_view:fetch(View) of
	{ok, {struct, Prop}} ->
	    Rows = props:get_value(<<"rows">>, Prop, []),
	    {ok, Rows};
	{error, _Error}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec(lookup_doc_rev/2 :: (DbName :: binary(), DocId :: binary()) -> tuple(error, term()) | tuple(ok, binary())).
lookup_doc_rev(DbName, DocId) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:lookup_doc_rev(Db, DocId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec(save_doc/2 :: (DbName :: binary(), Doc :: proplist() | json_object() | json_objects()) -> tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
save_doc(DbName, [{struct, [_|_]}=Doc]) ->
    save_doc(DbName, Doc, []);
save_doc(DbName, [{struct, _}|_]=Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) when is_list(Doc) ->
    save_doc(DbName, {struct, Doc}, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).


-spec(save_doc/3 :: (DbName :: binary(), Doc :: json_object(), Opts :: proplist()) -> tuple(ok, json_object()) | tuple(error, atom())).
save_doc(DbName, {struct, _}=Doc, Opts) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:save_doc(Db, Doc, Opts) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, Doc1}
            end
    end.

save_docs(DbName, Docs) ->
    save_docs(DbName, Docs, []).

-spec(save_docs/3 :: (DbName :: binary(), Docs :: json_objects(), Opts :: proplist()) -> tuple(ok, json_objects()) | tuple(error, atom())).
save_docs(DbName, Docs, Opts) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db -> couchbeam:save_docs(Db, Docs, Opts)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec(del_doc/2 :: (DbName :: binary(), Doc :: json_object()) -> tuple(ok, term()) | tuple(error, atom())).
del_doc(DbName, Doc) ->
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db ->
	    case couchbeam:delete_doc(Db, Doc) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, Doc1}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove documents from the db
%% @end
%%--------------------------------------------------------------------
-spec(del_docs/2 :: (DbName :: binary(), Docs :: json_objects()) -> tuple(ok, json_objects()) | tuple(error, atom())).
del_docs(DbName, Docs) ->
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db ->
	    couchbeam:delete_docs(Db, Docs)
    end.

%%%===================================================================
%%% Attachment Functions
%%%===================================================================
-spec(fetch_attachment/3 :: (DbName :: binary(), DocId :: binary(), AttachmentName :: binary()) -> tuple(ok, binary()) | tuple(error, term())).
fetch_attachment(DbName, DocId, AName) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:fetch_attachment(Db, DocId, AName)
    end.

%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec(put_attachment/4 :: (DbName :: binary(), DocId :: binary(), AttachmentName :: binary(), Contents :: binary()) -> tuple(ok, binary()) | tuple(error, term())).
put_attachment(DbName, DocId, AName, Contents) ->
    {ok, Rev} = ?MODULE:lookup_doc_rev(DbName, DocId),
    put_attachment(DbName, DocId, AName, Contents, [{rev, Rev}]).

-spec(put_attachment/5 :: (DbName :: binary(), DocId :: binary(), AttachmentName :: binary(), Contents :: binary(), Options :: proplist()) -> tuple(ok, binary()) | tuple(error, term())).
put_attachment(DbName, DocId, AName, Contents, Options) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:put_attachment(Db, DocId, AName, Contents, Options)
    end.

-spec(delete_attachment/3 :: (DbName :: binary(), DocId :: binary(), AttachmentName :: binary()) -> tuple(ok, binary()) | tuple(error, term())).
delete_attachment(DbName, DocId, AName) ->
    {ok, Rev} = ?MODULE:lookup_doc_rev(DbName, DocId),
    delete_attachment(DbName, DocId, AName, [{rev, Rev}]).

-spec(delete_attachment/4 :: (DbName :: binary(), DocId :: binary(), AttachmentName :: binary(), Options :: proplist()) -> tuple(ok, binary()) | tuple(error, term())).
delete_attachment(DbName, DocId, AName, Options) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:delete_attachment(Db, DocId, AName, Options)
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
-spec(get_all_results/2 :: (DbName :: binary(), DesignDoc :: binary()) ->
				tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec(get_results/3 :: (DbName :: binary(), DesignDoc :: binary(), ViewOptions :: proplist()) ->
			    tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
get_results(DbName, DesignDoc, ViewOptions) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
	    case get_view(Db, DesignDoc, ViewOptions) of
		{error, _Error}=E -> E;
		View ->
		    case couchbeam_view:fetch(View) of
			{ok, {struct, Prop}} ->
			    Rows = props:get_value(<<"rows">>, Prop, []),
                            {ok, Rows};
			{error, _Error}=E -> E
		    end
	    end
    end.

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
-spec(start_link/0 :: () -> tuple(ok, pid()) | ignore | tuple(error, term())).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% set the host to connect to
-spec(set_host/1 :: (HostName :: string()) -> ok | tuple(error, term())).
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

-spec(set_host/5 :: (HostName :: string(), Port :: integer(), UserName :: string(), Password :: string(), AdmingPort :: integer()) -> ok | tuple(error, term())).
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

get_db(DbName) ->
    Conn = gen_server:call(?SERVER, get_conn),
    open_db(whistle_util:to_list(DbName), Conn).

get_admin_db(DbName) ->
    Conn = gen_server:call(?SERVER, get_admin_conn),
    open_db(whistle_util:to_list(DbName), Conn).

get_uuid() ->
    Conn = gen_server:call(?SERVER, get_conn),
    [UUID] = couchbeam:get_uuid(Conn),   
    UUID.

get_uuids(Count) ->
    Conn = gen_server:call(?SERVER, get_conn),
    couchbeam:get_uuids(Conn, Count).

-spec(get_url/0 :: () -> binary()).
get_url() ->
    case {whistle_util:to_binary(get_host()), get_creds(), get_port()} of
        {<<"">>, _, _} ->
            undefined;
        {H, {[], []}, P} ->
            <<"http://", H/binary, ":", (whistle_util:to_binary(P))/binary, $/>>;
        {H, {User, Pwd}, P} ->
            <<"http://"
              ,(whistle_util:to_binary(User))/binary, $: ,(whistle_util:to_binary(Pwd))/binary
              ,$@, H/binary
              ,":", (whistle_util:to_binary(P))/binary, $/>>
    end.

-spec(get_admin_url/0 :: () -> binary()).
get_admin_url() ->
    case {whistle_util:to_binary(get_host()), get_creds(), get_admin_port()} of
        {<<"">>, _, _} ->
            undefined;
        {H, {[], []}, P} ->
            <<"http://", H/binary, ":", (whistle_util:to_binary(P))/binary, $/>>;
        {H, {User, Pwd}, P} ->
            <<"http://"
              ,(whistle_util:to_binary(User))/binary, $: ,(whistle_util:to_binary(Pwd))/binary
              ,$@, H/binary
              ,":", (whistle_util:to_binary(P))/binary, $/>>
    end.

add_change_handler(DBName, DocID) ->
    logger:format_log(info, "ADD_CHANGE_HANDLER for Pid: ~p for DB: ~p and Doc: ~p~n", [self(), DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, whistle_util:to_binary(DBName), whistle_util:to_binary(DocID), self()}).

add_change_handler(DBName, DocID, Pid) ->
    logger:format_log(info, "ADD_CHANGE_HANDLER req by ~p for Pid: ~p for DB: ~p and Doc: ~p~n", [self(), Pid, DBName, DocID]),
    gen_server:cast(?SERVER, {add_change_handler, whistle_util:to_binary(DBName), whistle_util:to_binary(DocID), Pid}).

rm_change_handler(DBName, DocID) ->
    gen_server:call(?SERVER, {rm_change_handler, whistle_util:to_binary(DBName), whistle_util:to_binary(DocID)}).

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
handle_call(get_host, _From, #state{host={H,_,_}}=State) ->
    {reply, H, State};

handle_call(get_port, _From, #state{host={_,P,_}}=State) ->
    {reply, P, State};

handle_call(get_admin_port, _From, #state{host={_,_,P}}=State) ->
    {reply, P, State};

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, #state{host={OldHost,_,_}}=State) ->
    logger:format_log(info, "WHISTLE_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    Conn = get_new_connection(Host, Port, User, Pass),
    AdminConn = get_new_connection(Host, AdminPort, User, Pass),
    spawn(fun() -> save_config(Host, Port, User, Pass, AdminPort) end),

    {reply, ok, State#state{host={Host, Port, AdminPort}
			    ,connection=Conn
			    ,admin_connection=AdminConn
			    ,change_handlers=dict:new()
			    ,creds={User,Pass}
			   }};

handle_call({set_host, Host, Port, User, Pass, AdminPort}, _From, State) ->
    logger:format_log(info, "WHISTLE_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    Conn = get_new_connection(Host, Port, User, Pass),
    AdminConn = get_new_connection(Host, AdminPort, User, Pass),
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
		  logger:format_log(info, "COUCH_MGR(~p): Found CH(~p): Rm listener(~p) for doc ~p:~p~n", [self(), Srv, Pid, DBName, DocID]),
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
	    logger:format_log(info, "COUCH_MGR(~p): Found CH(~p): Adding listener(~p) for doc ~p:~p~n", [self(), Srv, Pid, DBName, DocID]),
	    change_handler:add_listener(Srv, Pid, DocID),
	    {noreply, State};
	error ->
	    {ok, Srv} = change_handler:start_link(open_db(whistle_util:to_list(DBName), S), []),
	    logger:format_log(info, "COUCH_MGR(~p): started CH(~p): Adding listener(~p) for doc ~p:~p~n", [self(), Srv, Pid, DBName, DocID]),
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
    logger:format_log(error, "WHISTLE_COUCH(~p): Srv ~p down after complete~n", [self(), Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info({'DOWN', Ref, process, Srv, {error,connection_closed}}, #state{change_handlers=CH}=State) ->
    logger:format_log(error, "WHISTLE_COUCH(~p): Srv ~p down after conn closed~n", [self(), Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info(_Info, State) ->
    logger:format_log(error, "WHISTLE_COUCH(~p): Unexpected info ~p~n", [self(), _Info]),
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
-spec(get_new_connection/4 :: (Host :: string(), Port :: integer(), User :: string(), Pass :: string()) -> #server{}).
get_new_connection(Host, Port, "", "") -> get_new_conn(Host, Port, ?IBROWSE_OPTS);
get_new_connection(Host, Port, User, Pass) -> get_new_conn(Host, Port, [{basic_auth, {User, Pass}} | ?IBROWSE_OPTS]).

-spec(get_new_conn/3 :: (Host :: string(), Port :: integer(), Opts :: proplist()) -> #server{}).
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(Host, Port, "", Opts),
    logger:format_log(info, "WHISTLE_COUCH(~p): Host ~p Opts ~p has conn ~p~n", [self(), Host, Opts, Conn]),
    {ok, _Version} = couchbeam:server_info(Conn),
    logger:format_log(info, "WHISTLE_COUCH(~p): Connected to ~p~n~p~n", [self(), Host, _Version]),
    Conn.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% open_db, if DbName is known, returns the {#db{}, DBs}, else returns {#db{}, [{DbName, #db{}} | DBs]}
%% an error in opening the db will cause a {{error, Err}, DBs} to be returned
%% @end
%%--------------------------------------------------------------------
-spec(open_db/2 :: (DbName :: binary(), Conn :: #server{}) -> tuple(error, db_not_reachable) | #db{}).
open_db(DbName, Conn) ->
    case couchbeam:open_db(Conn, DbName) of
        {error, _Error}=E -> E;
        {ok, Db} -> Db
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get_view, if Db/DesignDoc is known, return {#view{}, Views},
%% else returns {#view{}, [{{#db{}, DesignDoc, ViewOpts}, #view{}} | Views]}
%% @end
%%--------------------------------------------------------------------
-spec(get_view/3 :: (Db :: #db{}, DesignDoc :: binary(), ViewOptions :: list()) -> #view{} | tuple(error, view_not_found)).
get_view(Db, DesignDoc, ViewOptions) ->
    case couchbeam:view(Db, DesignDoc, ViewOptions) of
	{error, _Error}=E -> E;
	{ok, View} -> View
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(init_state/0 :: () -> #state{}).
init_state() ->
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
	    Conn = get_new_connection(Host, whistle_util:to_integer(NormalPort), User, Password),
	    AdminConn = get_new_connection(Host, whistle_util:to_integer(AdminPort), User, Password),
	    #state{connection=Conn
		   ,admin_connection=AdminConn
		   ,host={Host, whistle_util:to_integer(NormalPort), whistle_util:to_integer(AdminPort)}
		   ,creds={User, Password}};
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
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun(Item, Acc) -> [io_lib:format("~p.~n", [Item]) | Acc] end
				 , "", [{couch_host, H, Port, U, P, AdminPort} | lists:keydelete(couch_host, 1, Config)])
		   ).

-spec(remove_ref/2 :: (Ref :: reference(), CH :: dict()) -> dict()).
remove_ref(Ref, CH) ->
    dict:filter(fun(_, {_, Ref1}) when Ref1 =:= Ref -> false;
		   (_, _) -> true end, CH).
