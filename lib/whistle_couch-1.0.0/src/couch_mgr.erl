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
-export([start_link/0, set_host/1, set_host/3, get_host/0, get_creds/0, get_url/0]).

%% System manipulation
-export([db_exists/1, db_info/1, db_create/1, db_compact/1, db_delete/1, db_replicate/1]).

%% Document manipulation
-export([save_doc/2, save_doc/3, save_docs/3, open_doc/2, open_doc/3, del_doc/2, lookup_doc_rev/2]).
-export([add_change_handler/2, rm_change_handler/2, load_doc_from_file/3, update_doc_from_file/3]).

%% attachments
-export([fetch_attachment/3, put_attachment/4, put_attachment/5, delete_attachment/3]).

%% Views
-export([get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-include_lib("whistle/include/whistle_types.hrl"). % get the whistle types
-include_lib("couchbeam/include/couchbeam.hrl").

-define(SERVER, ?MODULE). 
-define(STARTUP_FILE, [code:lib_dir(whistle_couch, priv), "/startup.config"]).
-define(DEFAULT_PORT, 5984).
-define(IBROWSE_OPTS, [{max_sessions, 1024}, {max_pipeline_size, 10}]).

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% Change handler {DBName :: string(), {Srv :: pid(), SrvRef :: reference()}
-record(state, {
          host = "" :: string()
	  ,connection = #server{} :: #server{}
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
	    io:format("badmatch ~p:~p: ~p~n", [_Type, Reason, erlang:get_stacktrace()]),
            {error, Reason};
 	_Type:Reason ->
	    io:format("excep ~p:~p: ~p~n", [_Type, Reason, erlang:get_stacktrace()]),
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
%% open a document given a docid returns not_found or the Document
%% @end
%%--------------------------------------------------------------------
-spec(open_doc/2 :: (DbName :: string(), DocId :: binary()) -> tuple(ok, json_object()) | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec(open_doc/3 :: (DbName :: string(), DocId :: binary(), Options :: proplist()) -> tuple(ok, json_object()) | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId, Options) when not is_binary(DocId) ->
    open_doc(DbName, whistle_util:to_binary(DocId), Options);
open_doc(DbName, DocId, Options) ->    
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:open_doc(Db, DocId, Options) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, Doc1}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% get the revision of a document (much faster than requesting the whole document)
%% @end
%%--------------------------------------------------------------------
-spec(lookup_doc_rev/2 :: (DbName :: string(), DocId :: binary()) -> tuple(error, term()) | tuple(ok, binary())).
lookup_doc_rev(DbName, DocId) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    case couchbeam:lookup_doc_rev(Db, DocId) of
		{error, _}=E -> E;
		{ok, Rev} ->
		    {ok, Rev}
	    end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec(save_doc/2 :: (DbName :: list(), Doc :: proplist() | json_object() | json_objects()) -> tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
save_doc(DbName, [{struct, [_|_]}=Doc]) ->
    save_doc(DbName, Doc, []);
save_doc(DbName, [{struct, _}|_]=Docs) ->
    save_docs(DbName, Docs, []);
save_doc(DbName, Doc) when is_list(Doc) ->
    save_doc(DbName, {struct, Doc}, []);
save_doc(DbName, Doc) ->
    save_doc(DbName, Doc, []).


-spec(save_doc/3 :: (DbName :: string(), Doc :: json_object(), Opts :: proplist()) -> tuple(ok, json_object()) | tuple(error, atom())).
save_doc(DbName, {struct, _}=Doc, Opts) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:save_doc(Db, Doc, Opts) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, Doc1}
            end
    end.

-spec(save_docs/3 :: (DbName :: string(), Docs :: json_objects(), Opts :: proplist()) -> tuple(ok, json_objects()) | tuple(error, atom())).
save_docs(DbName, Docs, Opts) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:save_docs(Db, Docs, Opts) of
                {error, _Error}=E -> E;
                {ok, Docs1} ->{ok, Docs1}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec(del_doc/2 :: (DbName :: list(), Doc :: proplist()) -> tuple(ok, term()) | tuple(error, atom())).
del_doc(DbName, Doc) ->
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db ->
	    case couchbeam:delete_doc(Db, Doc) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, Doc1}
            end
    end.

%%%===================================================================
%%% Attachment Functions
%%%===================================================================
-spec(fetch_attachment/3 :: (DbName :: string(), DocId :: binary(), AttachmentName :: binary()) -> tuple(ok, binary()) | tuple(error, term())).
fetch_attachment(DbName, DocId, AName) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:fetch_attachment(Db, DocId, AName)
    end.

%% Options = [ {'content_type', Type}, {'content_length', Len}, {'rev', Rev}] <- note atoms as keys in proplist
-spec(put_attachment/4 :: (DbName :: string(), DocId :: binary(), AttachmentName :: binary(), Contents :: binary()) -> tuple(ok, binary()) | tuple(error, term())).
put_attachment(DbName, DocId, AName, Contents) ->
    {ok, Rev} = ?MODULE:lookup_doc_rev(DbName, DocId),
    put_attachment(DbName, DocId, AName, Contents, [{rev, Rev}]).

-spec(put_attachment/5 :: (DbName :: string(), DocId :: binary(), AttachmentName :: binary(), Contents :: binary(), Options :: proplist()) -> tuple(ok, binary()) | tuple(error, term())).
put_attachment(DbName, DocId, AName, Contents, Options) ->
    case get_db(DbName) of
	{error, _} -> {error, db_not_reachable};
	Db ->
	    couchbeam:put_attachment(Db, DocId, AName, Contents, Options)
    end.

delete_attachment(DbName, DocId, AName) ->
    {ok, Rev} = ?MODULE:lookup_doc_rev(DbName, DocId),
    delete_attachment(DbName, DocId, AName, [{rev, Rev}]).
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
-spec(get_all_results/2 :: (DbName :: list(), DesignDoc :: tuple(string(), string())) -> tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

-spec(get_results/3 :: (DbName :: list(), DesignDoc :: tuple(string(), string()), ViewOptions :: proplist()) -> tuple(ok, json_object()) | tuple(ok, json_objects()) | tuple(error, atom())).
get_results(DbName, DesignDoc, ViewOptions) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
	    case get_view(Db, DesignDoc, ViewOptions) of
		{error, _Error}=E -> E;
		View ->
		    case couchbeam_view:fetch(View) of
			{ok, {struct, Prop}} ->
			    Rows = get_value(<<"rows">>, Prop, []),
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
    set_host(HostName, "", "").

set_host(HostName, UserName, Password) ->
    gen_server:call(?MODULE, {set_host, HostName, UserName, Password}, infinity).

get_host() ->
    gen_server:call(?MODULE, get_host).

get_creds() ->
    gen_server:call(?MODULE, {get_creds}).

get_conn() ->
    gen_server:call(?MODULE, {get_conn}).

get_db(DbName) ->
    Conn = gen_server:call(?MODULE, {get_conn}),
    open_db(whistle_util:to_list(DbName), Conn).

get_url() ->
    case {whistle_util:to_binary(get_host()), get_creds()} of 
        {<<"">>, _} -> 
            undefined;
        {H, {[], []}} ->
            <<"http://", H/binary, ":5984", $/>>;
        {H, {User, Pwd}} ->
            U = whistle_util:to_binary(User),
            P = whistle_util:to_binary(Pwd),
            <<"http://", U/binary, $:, P/binary, $@, H/binary, ":5984", $/>>
    end.

add_change_handler(DBName, DocID) ->
    gen_server:call(?MODULE, {add_change_handler, whistle_util:to_list(DBName), whistle_util:to_binary(DocID)}).

rm_change_handler(DBName, DocID) ->
    gen_server:call(?MODULE, {rm_change_handler, whistle_util:to_list(DBName), whistle_util:to_binary(DocID)}).

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
handle_call(get_host, _From, #state{host=H}=State) ->
    {reply, H, State};

handle_call({set_host, Host, User, Pass}, _From, #state{host=OldHost}=State) ->
    format_log(info, "WHISTLE_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    S = get_new_connection(Host, User, Pass),
    {reply, ok, State#state{host=Host, connection=S, change_handlers=dict:new(), creds={User,Pass}}};

handle_call({set_host, Host, User, Pass}, _From, State) ->
    format_log(info, "WHISTLE_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    S = get_new_connection(Host, User, Pass),
    {reply, ok, State#state{host=Host, connection=S, change_handlers=dict:new(), creds={User,Pass}}};

handle_call({get_conn}, _, #state{connection=S}=State) ->
    {reply, S, State};

handle_call({get_creds}, _, #state{creds=Cred}=State) ->
    {reply, Cred, State};

handle_call({add_change_handler, DBName, DocID}, {Pid, _Ref}, #state{change_handlers=CH, connection=S}=State) ->
    case dict:find(DBName, CH) of
	{ok, {Srv, _}} ->
	    logger:format_log(info, "COUCH_MGR(~p): Found CH(~p): Adding listener(~p) for doc ~p:~p~n", [self(), Srv, Pid, DBName, DocID]),
	    change_handler:add_listener(Srv, Pid, DocID),
	    {reply, ok, State};
	error ->
	    {ok, Srv} = change_mgr_sup:start_handler(open_db(whistle_util:to_list(DBName), S), []),
	    logger:format_log(info, "COUCH_MGR(~p): started CH(~p): Adding listener(~p) for doc ~p:~p~n", [self(), Srv, Pid, DBName, DocID]),
	    SrvRef = erlang:monitor(process, Srv),
	    change_handler:add_listener(Srv, Pid, DocID),
	    {reply, ok, State#state{change_handlers=dict:store(DBName, {Srv, SrvRef}, CH)}}
    end;

handle_call({rm_change_handler, DBName, DocID}, {Pid, _Ref}, #state{change_handlers=CH}=State) ->
    case dict:find(DBName, CH) of
	{Srv, _} -> change_handler:rm_listener(Srv, Pid, DocID);
	error -> ok
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    format_log(error, "WHISTLE_COUCH(~p): Failed call ~p with state ~p~n", [self(), _Request, State]),
    {reply, {error, unavailable}, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
    format_log(error, "WHISTLE_COUCH(~p): Srv ~p down after complete~n", [self(), Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info({'DOWN', Ref, process, Srv, {error,connection_closed}}, #state{change_handlers=CH}=State) ->
    format_log(error, "WHISTLE_COUCH(~p): Srv ~p down after conn closed~n", [self(), Srv]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{change_handlers=remove_ref(Ref, CH)}};
handle_info(_Info, State) ->
    format_log(error, "WHISTLE_COUCH(~p): Unexpected info ~p~n", [self(), _Info]),
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
-spec(get_new_connection/3 :: (Host :: string(), User :: string(), Pass :: string()) -> #server{}).
get_new_connection(Host, "", "") -> get_new_conn(Host, ?IBROWSE_OPTS);
get_new_connection(Host, User, Pass) -> get_new_conn(Host, [{basic_auth, {User, Pass}} | ?IBROWSE_OPTS]).

-spec(get_new_conn/2 :: (Host :: string(), Opts :: proplist()) -> #server{}).
get_new_conn(Host, Opts) ->
    Conn = couchbeam:server_connection(Host, ?DEFAULT_PORT, "", Opts),
    format_log(info, "WHISTLE_COUCH(~p): Host ~p Opts ~p has conn ~p~n", [self(), Host, Opts, Conn]),
    {ok, _Version} = couchbeam:server_info(Conn),
    format_log(info, "WHISTLE_COUCH(~p): Connected to ~p~n~p~n", [self(), Host, _Version]),
    spawn(fun() ->
		  case props:get_value(basic_auth, Opts) of
		      undefined -> save_config(Host);
		      {U, P} -> save_config(Host, U, P)
		  end
	  end),
    Conn.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% open_db, if DbName is known, returns the {#db{}, DBs}, else returns {#db{}, [{DbName, #db{}} | DBs]}
%% an error in opening the db will cause a {{error, Err}, DBs} to be returned
%% @end
%%--------------------------------------------------------------------
-spec(open_db/2 :: (DbName :: string(), Conn :: #server{}) -> tuple(error, db_not_reachable) | #db{}).
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
-spec(get_view/3 :: (Db :: #db{}, DesignDoc :: string() | tuple(string(), string()), ViewOptions :: list()) -> #view{} | tuple(error, view_not_found)).
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
	    {_, Host, User, Pass} = case lists:keyfind(couch_host, 1, Ts) of
					false ->
					    case lists:keyfind(default_couch_host, 1, Ts) of
						false -> {ok, net_adm:localhost(), "", ""};
						H -> H
					    end;
					H -> H
				    end,
	    #state{connection=get_new_connection(Host, User, Pass), host=Host, creds={User,Pass}};
	_ -> #state{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_startup_config/0 :: () -> tuple(ok, proplist()) | tuple(error, term())).
get_startup_config() ->
    file:consult(?STARTUP_FILE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(save_config/1 :: (H :: string()) -> no_return()).
save_config(H) ->
    save_config(H, "", "").

save_config(H, U, P) ->
    {ok, Config} = get_startup_config(),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun(Item, Acc) -> [io_lib:format("~p.~n", [Item]) | Acc] end
				 , "", [{couch_host, H, U, P} | lists:keydelete(couch_host, 1, Config)])
		   ).

-spec(remove_ref/2 :: (Ref :: reference(), CH :: dict()) -> dict()).
remove_ref(Ref, CH) ->
    dict:filter(fun(_, {_, Ref1}) when Ref1 =:= Ref -> false;
		   (_, _) -> true end, CH).
