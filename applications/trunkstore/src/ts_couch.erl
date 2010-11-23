%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage CouchDB connections
%%% @end
%%% Created : 16 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_couch).

-behaviour(gen_server).

%% API
-export([start_link/0, set_host/1]).

%% Document manipulation
-export([new_doc/0, add_to_doc/3, rm_from_doc/2, save_doc/2, open_doc/2, open_doc/3]).
-export([add_change_handler/2, rm_change_handler/1]).

%% Views
-export([get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE). 

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% DBs = [{DbName, #db{}}]
%% Views = [{ {#db{}, DesignDoc, ViewName}, #view{}}]
%% ViewOptions :: Proplist() -> See http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options
%% ChangeHandlers :: [ {DocID, ReqID, [Pid]}]
-record(state, {
	  connection = {} :: tuple(string(), tuple())
	  ,dbs = [] :: list(tuple(string(), tuple()))
	  ,views = [] :: list(tuple(tuple(tuple(), string(), string()), tuple()))
	  ,change_handlers = [] :: list(tuple(binary(), reference(), list(pid())))
	 }).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% set the host to connect to
set_host(HostName) ->
    gen_server:call(?MODULE, {set_host, HostName}).

%% create a new Document - a tuple with a proplist
new_doc() -> {[]}.

%% open a document given a docid
%% returns not_found or the Document
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

open_doc(DbName, DocId, Options) ->
    gen_server:call(?MODULE, {open_doc, whistle_util:to_list(DbName), whistle_util:to_binary(DocId), Options}, infinity).

%% add a K/V pair to a Document
-spec(add_to_doc/3 :: (Key :: binary(), Value :: term(), Doc :: proplist()) -> proplist()).
add_to_doc(Key, Value, Doc) ->
    {Doc1} = couchbeam_doc:extend(whistle_util:to_binary(Key), Value, {Doc}),
    Doc1.

%% remove a K from the Document
-spec(rm_from_doc/2 :: (Key :: binary(), Doc :: proplist()) -> proplist()).
rm_from_doc(Key, Doc) ->
    {Doc1} = couchbeam_doc:delete_value(whistle_util:to_binary(Key), {Doc}),
    Doc1.

%% save a Document to the DB
-spec(save_doc/2 :: (DbName :: list(), Doc :: proplist()) -> {ok, proplist()}).
save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, whistle_util:to_list(DbName), {Doc}}, infinity).

%% get the results of the view
%% {Total, Offset, Meta, Rows}
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

%% {Total, Offset, Meta, Rows}
get_results(DbName, DesignDoc, ViewOptions) ->
    gen_server:call(?MODULE, {get_results, whistle_util:to_list(DbName), DesignDoc, ViewOptions}, infinity).

add_change_handler(DBName, DocID) ->
    gen_server:call(?MODULE, {add_change_handler, whistle_util:to_list(DBName), whistle_util:to_binary(DocID)}).

rm_change_handler(DocID) ->
    gen_server:call(?MODULE, {rm_change_handler, whistle_util:to_binary(DocID)}).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call({get_results, DbName, DesignDoc, ViewOptions}, _From, #state{connection={_Host, Conn}, dbs=DBs, views=Vs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{{error, _Err}=E, _DBs} ->
	    {reply, E, State};
	{Db, DBs1} ->
	    case get_view(Db, DesignDoc, ViewOptions, Vs) of
		{{error, _Err}=E, _DBs} ->
		    {reply, E, State#state{dbs=DBs1}};
		{View, Vs1} ->
		    case couchbeam_view:fetch(View) of
			{ok, {Prop}} ->
			    {reply, get_value(<<"rows">>, Prop, []), State#state{dbs=DBs1, views=Vs1}};
			Error ->
			    {reply, Error, State#state{dbs=DBs1}}
		    end
	    end
    end;
handle_call({open_doc, DbName, DocId, Options}, _From, #state{connection={_Host, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{{error, _Err}=E, _DBs} ->
	    {reply, E, State};
	{Db, DBs1} ->
	    case couchbeam:open_doc(Db, DocId, Options) of
		{ok, {Doc}} ->
		    {reply, Doc, State#state{dbs=DBs1}};
		Other ->
		    format_log(error, "TS_COUCH(~p): Failed to find ~p: ~p~n", [self(), DocId, Other]),
		    {reply, {error, not_found}, State}
	    end
    end;
handle_call({save_doc, DbName, Doc}, _From, #state{connection={_Host, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{{error, _Err}=E, _DBs} ->
	    {reply, E, State};
	{Db, DBs1} ->
	    {ok, {Doc1}} = couchbeam:save_doc(Db, Doc),
	    {reply, {ok, Doc1}, State#state{dbs=DBs1}}
    end;
handle_call({set_host, Host}, _From, #state{connection={OldHost, _Conn}}=State) ->
    format_log(info, "TS_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[], change_handlers=[]}}
    end;
handle_call({set_host, Host}, _From, State) ->
    format_log(info, "TS_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[], change_handlers=[]}}
    end;
handle_call({add_change_handler, DBName, DocID}, {Pid, _Ref}, #state{connection={_H, Conn}, dbs=DBs, change_handlers=CH}=State) ->
    case lists:keyfind(DocID, 1, CH) of
	false ->
	    case get_db(DBName, Conn, DBs) of
		{{error, _Err}=E, _DBs} ->
		    {reply, E, State};
		{Db, _DBs1} ->
		    {ok, ReqID} = couchbeam:changes_wait(Db, self(), [{heartbeat, "true"}
								      ,{filter, <<"filter/by_doc">>}
								      ,{name, DocID}
								      %% don't include these last two params if you want a stream
								      %% of changes for the whole DB
								     ]),
		    format_log(info, "TS_COUCH(~p): Added handler for ~p(~p) ref ~p~n", [self(), DocID, Pid, ReqID]),
		    link(Pid),
		    {reply, ok, State#state{change_handlers=[{DocID, ReqID, [Pid]} | CH]}}
	    end;
	{DocID, ReqID, Pids} ->
	    case lists:member(Pid, Pids) of
		false ->
		    {reply, ok, State#state{change_handlers=[{DocID, ReqID, [Pid | Pids]} | lists:keydelete(DocID, 1, CH)]}};
		true ->
		    format_log(info, "TS_COUCH(~p): Found handler for ~p(~p)~n", [self(), DocID, Pid]),
		    {reply, {error, handler_exists}, State}
	    end
    end;
handle_call({rm_change_handler, DocID}, {From, _Ref}, #state{change_handlers=CH}=State) ->
    case lists:keyfind(DocID, 1, CH) of
	false ->
	    {reply, ok, State};
	{DocID, ReqID, Pids} when is_list(Pids) ->
	    CH1 = case lists:delete(From, Pids) of
		      [] ->
			  %% unreg from couchbeam
			  lists:keydelete(DocID, 1, CH);
		      Pids1 ->
			  [ {DocID, ReqID, Pids1} | lists:keydelete(DocID, 1, CH)]
		  end,
	    {reply, ok, State#state{change_handlers=CH1}}
    end;
handle_call(_Request, _From, State) ->
    format_log(error, "TS_COUCH(~p): Failed call ~p with state ~p~n", [self(), _Request, State]),
    {reply, {error, unhandled_call}, State}.

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
handle_info({_ReqID, done}, State) ->
    format_log(info, "TS_COUCH.wait(~p): DONE~n", [self()]),
    {noreply, State};
handle_info({_ReqID, {change, {Change}}}, #state{change_handlers=CH}=State) ->
    DocID = get_value(<<"id">>, Change),
    case lists:keyfind(DocID, 1, CH) of
	false ->
	    format_log(info, "TS_COUCH.wait(~p): ~p not found, skipping~n", [self(), DocID]),
	    {noreply, State};
	{DocID, _ReqID, Pids} ->
	    SendToPid = case get_value(<<"deleted">>, Change) of
			    true ->
				format_log(info, "TS_COUCH.wait(~p): ~p deleted~n", [self(), DocID]),
				{document_deleted, DocID}; % document deleted, no more looping
			    undefined ->
				format_log(info, "TS_COUCH.wait(~p): ~p change sending to ~p~n", [self(), DocID, Pids]),
				{document_changes, DocID, lists:map(fun({C}) -> C end, get_value(<<"changes">>, Change))}
			end,
	    lists:foreach(fun(Pid) -> Pid ! SendToPid end, Pids),
	    {noreply, State}
    end;
handle_info({_ReqID, {error, E}}, State) ->
    format_log(info, "TS_COUCH.wait(~p): ERROR ~p for reqid ~p~n", [self(), E, _ReqID]),
    {noreply, State};
handle_info({'DOWN', _MRefConn, process, _Pid, _Reason}, State) ->
    format_log(error, "TS_COUCH(~p): Pid(~p) went down: ~p~n", [self(), _Pid, _Reason]),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, #state{change_handlers=CH}=State) ->
    format_log(error, "TS_COUCH(~p): EXIT received for ~p with reason ~p~n", [self(), Pid, _Reason]),
    CH1 = lists:map(fun({_DocID, _ReqID, Pids}=T) -> setelement(3, T, lists:delete(Pid, Pids)) end, CH),
    {noreply, State#state{change_handlers=CH1}};
handle_info(_Info, State) ->
    format_log(error, "TS_COUCH(~p): Unexpected info ~p~n", [self(), _Info]),
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
-spec(get_new_connection/1 :: (Host :: string()) -> {string(), tuple()} | {error, term()}).
get_new_connection(Host) ->
    Conn = couchbeam:server_connection(Host, 5984, "", []),
    format_log(info, "TS_COUCH(~p): Host ~p has conn ~p~n", [self(), Host, Conn]),
    case couchbeam:server_info(Conn) of
	{ok, _Version} ->
	    format_log(info, "TS_COUCH(~p): Connected to ~p~n~p~n", [self(), Host, _Version]),
	    {Host, Conn};
	{error, _Error}=E ->
	    format_log(error, "TS_COUCH(~p): Unable to connect to ~p: ~p~n", [self(), Host, _Error]),
	    E
    end.

%% get_db, if DbName is known, returns the {#db{}, DBs}, else returns {#db{}, [{DbName, #db{}} | DBs]}
%% an error in opening the db will cause a {{error, Err}, DBs} to be returned
-spec(get_db/3 :: (DbName :: string(), Conn :: tuple(), DBs :: list()) -> tuple(tuple(), list())).
get_db(DbName, Conn, DBs) ->
    get_db(DbName, Conn, DBs, []).

-spec(get_db/4 :: (DbName :: string(), Conn :: tuple(), DBs :: list(), Options :: list()) -> tuple(tuple(), list())).
get_db(DbName, Conn, DBs, Options) ->
    case lists:keyfind(DbName, 1, DBs) of
	false ->
	    {ok, Db} = couchbeam:open_db(Conn, DbName, Options),
	    case couchbeam:db_info(Db) of
		{ok, _JSON} ->
		    {Db, [{DbName, Db} | DBs]};
		{error, _Error}=E ->
		    {E, DBs}
	    end;
	{DbName, Db} ->
	    {Db, DBs}
    end.

%% get_view, if Db/DesignDoc is known, return {#view{}, Views}, else returns {#view{}, [{{#db{}, DesignDoc, ViewOpts}, #view{}} | Views]}
-spec(get_view/4 :: (Db :: tuple(), DesignDoc :: string() | tuple(string(), string()), ViewOptions :: list(), Views :: list()) -> tuple(tuple(), list())).
get_view(Db, DesignDoc, ViewOptions, Views) ->
    case lists:keyfind({Db, DesignDoc, ViewOptions}, 1, Views) of
	{{Db, DesignDoc, ViewOptions}, View} ->
	    {View, Views};
	false ->
	    case couchbeam:view(Db, DesignDoc, ViewOptions) of
		{error, _Error}=E ->
		    {E, Views};
		{ok, View} ->
		    {View, [{{Db, DesignDoc, ViewOptions}, View} | Views]}
	    end
    end.
