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
%% Views = [{ {DesignDoc, ViewName}, #view{}}]
%% ViewOptions :: Proplist() -> See http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options
-record(state, {
	  connection = {} :: tuple(string(), tuple())
	  ,dbs = [] :: list(tuple(string(), tuple()))
	  ,views = [] :: list(tuple(tuple(string(), string()), tuple()))
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
    gen_server:call(?MODULE, {open_doc, to_list(DbName), to_binary(DocId), Options}, infinity).

%% add a K/V pair to a Document
add_to_doc(Key, Value, Doc) ->
    couchbeam_doc:extend(to_binary(Key), Value, Doc).

%% remove a K from the Document
rm_from_doc(Key, Doc) ->
    couchbeam_doc:delete_value(to_binary(Key), Doc).

%% save a Document to the DB
save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, to_list(DbName), Doc}, infinity).

%% get the results of the view
%% {Total, Offset, Meta, Rows}
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

%% {Total, Offset, Meta, Rows}
get_results(DbName, DesignDoc, ViewOptions) ->
    gen_server:call(?MODULE, {get_results, to_list(DbName), DesignDoc, ViewOptions}, infinity).

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
	    {ok, {Doc}} = couchbeam:open_doc(Db, DocId, Options),
	    {reply, Doc, State#state{dbs=DBs1}}
    end;
handle_call({save_doc, DbName, Doc}, _From, #state{connection={_Host, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{{error, _Err}=E, _DBs} ->
	    {reply, E, State};
	{Db, DBs1} ->
	    {reply, couchbeam:save_doc(Db, Doc), State#state{dbs=DBs1}}
    end;
handle_call({set_host, Host}, _From, #state{connection={OldHost, _Conn}}=State) ->
    format_log(info, "TS_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[]}}
    end;
handle_call({set_host, Host}, _From, State) ->
    format_log(info, "TS_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[]}}
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
handle_info({'DOWN', _MRefConn, process, _Pid, _Reason}, State) ->
    format_log(error, "TS_COUCH(~p): Pid(~p) went down: ~p~n", [self(), _Pid, _Reason]),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    format_log(error, "TS_COUCH(~p): EXIT received for ~p with reason ~p~n", [self(), _Pid, _Reason]),
    {noreply, State};
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

-spec(to_list/1 :: (X :: atom() | list() | binary()) -> list()).
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

-spec(to_binary/1 :: (X :: atom() | list() | binary()) -> binary()).
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.
