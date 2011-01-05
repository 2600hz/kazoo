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
-export([start_link/0, set_host/1, get_host/0]).

%% Document manipulation
-export([new_doc/0, add_to_doc/3, rm_from_doc/2, save_doc/2, open_doc/2, open_doc/3, del_doc/2]).
-export([add_change_handler/2, rm_change_handler/2]).

%% Views
-export([get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-include("../../src/whistle_api.hrl"). % get the proplists -type
-include_lib("couchbeam/include/couchbeam.hrl").

-define(SERVER, ?MODULE). 
-define(STARTUP_FILE, lists:concat([filename:dirname(filename:dirname(code:which(?MODULE))), "/priv/startup.config"])).

%% Host = IP Address or FQDN
%% Connection = {Host, #server{}}
%% DBs = [{DbName, #db{}}]
%% Views = [{ {#db{}, DesignDoc, ViewName}, #view{}}]
%% ViewOptions :: Proplist() -> See http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options
%% ChangeHandlers :: [{DBName, DocID}, ReqID, [Pid]]
-type change_handler_entry() :: tuple( tuple(string(), binary()), reference(), list(pid()) | []).
-type db_entry() :: tuple(string(), #db{}).
-type view_entry() :: tuple(tuple(#db{}, string(), string()), #view{}).
-record(state, {
	  connection = {} :: tuple(string(), #server{}) | {}
	  ,dbs = [] :: list(db_entry()) | []
	  ,views = [] :: list(view_entry()) | []
	  ,change_handlers = [] :: list(change_handler_entry()) | []
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
-spec(start_link/0 :: () -> tuple(ok, pid()) | ignore | tuple(error, term())).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% set the host to connect to
-spec(set_host/1 :: (HostName :: string()) -> ok | tuple(error, term())).
set_host(HostName) ->
    gen_server:call(?MODULE, {set_host, HostName}, infinity).

get_host() ->
    gen_server:call(?MODULE, get_host).

%% create a new Document - a tuple with a proplist
-spec(new_doc/0 :: () -> []).
new_doc() -> [].

%% open a document given a docid
%% returns not_found or the Document
-spec(open_doc/2 :: (DbName :: string(), DocId :: binary()) -> proplist() | not_found).
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec(open_doc/3 :: (DbName :: string(), DocId :: binary(), Options :: proplist()) -> proplist() | not_found).
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
-spec(save_doc/2 :: (DbName :: list(), Doc :: proplist()) -> tuple(ok, proplist()) | tuple(error, conflict)).
save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, whistle_util:to_list(DbName), {Doc}}, infinity).

-spec(del_doc/2 :: (DbName :: list(), Doc :: proplist()) -> tuple(ok | error, term())).
del_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {del_doc, whistle_util:to_list(DbName), {Doc}}, infinity).

%% get the results of the view
%% {Total, Offset, Meta, Rows}
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

%% {Total, Offset, Meta, Rows}
get_results(DbName, DesignDoc, ViewOptions) ->
    gen_server:call(?MODULE, {get_results, whistle_util:to_list(DbName), DesignDoc, ViewOptions}, infinity).

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
handle_call({get_results, DbName, DesignDoc, ViewOptions}, _From, #state{connection={_Host, Conn}, dbs=DBs, views=Vs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{error, db_not_reachable}=E ->
	    {reply, E, State};
	{Db, DBs1} ->
	    case get_view(Db, DesignDoc, ViewOptions, Vs) of
		{error, _Error}=E ->
		    {reply, E, State#state{dbs=DBs1}};
		{View, Vs1} ->
		    case couchbeam_view:fetch(View) of
			{ok, {Prop}} ->
			    {reply, get_value(<<"rows">>, Prop, []), State#state{dbs=DBs1, views=Vs1}};
			Error ->
			    {reply, {Error, fetch_failed}, State#state{dbs=DBs1}}
		    end
	    end
    end;
handle_call({open_doc, DbName, DocId, Options}, _From, #state{connection={_Host, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{error, db_not_reachable} = E ->
	    {reply, E, State};
	{Db, DBs1} ->
	    case couchbeam:open_doc(Db, DocId, Options) of
		{ok, {Doc}} ->
		    {reply, Doc, State#state{dbs=DBs1}};
		Other ->
		    format_log(error, "WHISTLE_COUCH(~p): Failed to find ~p: ~p~n", [self(), DocId, Other]),
		    {reply, {error, not_found}, State}
	    end
    end;

handle_call({save_doc, DbName, Doc}, _From, #state{connection={_Host, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{error, db_not_reachable}=E ->
	    {reply, E, State};
	{Db, DBs1} ->
	    case couchbeam:save_doc(Db, Doc) of
		{ok, {Doc1}} ->
		    {reply, {ok, Doc1}, State#state{dbs=DBs1}};
		{error, _E}=Err -> % conflict!
		    {reply, Err, State#state{dbs=DBs1}}
	    end
    end;

handle_call({del_doc, DbName, Doc}, _From, #state{connection={_, Conn}, dbs=DBs}=State) ->
    case get_db(DbName, Conn, DBs) of
	{error, db_not_reachable}=E ->
	    {reply, E, State};
	{Db, DBs1} ->
	    {reply, couchbeam:delete_doc(Db, Doc), State#state{dbs=DBs1}}
    end;

handle_call(get_host, _From, #state{connection={H,_}}=State) ->
    {reply, H, State};
handle_call({set_host, Host}, _From, #state{connection={OldHost, _}}=State) ->
    format_log(info, "WHISTLE_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[], change_handlers=[]}}
    end;
handle_call({set_host, Host}, _From, State) ->
    format_log(info, "WHISTLE_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC,  dbs=[], views=[], change_handlers=[]}}
    end;
handle_call({add_change_handler, DBName, <<>>}, {Pid, _Ref}, State) ->
    case start_change_handler(DBName, <<>>, Pid, State) of
	{ok, _R, State1} -> {reply, ok, State1};
	{error, E, State2} -> {reply, {error, E}, State2}
    end;
handle_call({add_change_handler, DBName, DocID}, {Pid, _Ref}, State) ->
    case start_change_handler(DBName, DocID, Pid, State) of
	{ok, _R, State1} -> {reply, ok, State1};
	{error, E, State2} -> {reply, {error, E}, State2}
    end;
handle_call({rm_change_handler, DBName, DocID}, {From, _Ref}, #state{change_handlers=CH}=State) ->
    case stop_change_handler({DBName, DocID}, From, CH) of
	{ok, CH1} -> {reply, ok, State#state{change_handlers=CH1}};
	{{error, _}=E, CH2} -> {reply, E, State#state{change_handlers=CH2}}
    end;
handle_call(Req, From, #state{connection={}}=State) ->
    format_log(info, "WHISTLE_COUCH(~p): No connection, trying localhost(~p)~n", [self(), net_adm:localhost()]),
    case get_new_connection(net_adm:localhost()) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    close_handlers(State#state.change_handlers),
	    handle_call(Req, From, State#state{connection=HC,  dbs=[], views=[], change_handlers=[]})
    end;
handle_call(_Request, _From, State) ->
    format_log(error, "WHISTLE_COUCH(~p): Failed call ~p with state ~p~n", [self(), _Request, State]),
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
handle_info({ReqID, done}, #state{change_handlers=CH}=State) ->
    format_log(info, "WHISTLE_COUCH.wait(~p): DONE change handler ref ~p~n", [self(), ReqID]),
    case lists:keyfind(ReqID, 2, CH) of
	false -> {noreply, State};
	{{_, DocID}=Key,_,Pids}=Item ->
	    TmpCH = [Item],
	    lists:foreach(fun(P) ->
				  stop_change_handler(Key, P, TmpCH),
				  P ! {change_handler_done, DocID}
			  end, Pids),
	    {noreply, State#state{change_handlers=lists:keydelete(ReqID, 2, CH)}}
    end;
handle_info({ReqID, {change, {Change}}}, #state{change_handlers=CH}=State) ->
    DocID = get_value(<<"id">>, Change),
    format_log(info, "WHISTLE_COUCH.wait(~p): keyfind res = ~p~n", [self(), lists:keyfind(ReqID, 2, CH)]),
    case lists:keyfind(ReqID, 2, CH) of
	false ->
	    format_log(info, "WHISTLE_COUCH.wait(~p): ~p not found, skipping~n", [self(), DocID]),
	    {noreply, State};
	{{_, DocID}, _, Pids} ->
	    notify_pids(Change, DocID, Pids),
	    {noreply, State};
	{{_, <<>>}, _, Pids} ->
	    notify_pids(Change, DocID, Pids),
	    {noreply, State}    
    end;
handle_info({ReqID, {error, connection_closed}}, #state{change_handlers=CH}=State) ->
    format_log(info, "WHISTLE_COUCH.wait(~p): connection closed for change handlers: reqid ~p~n", [self(), ReqID]),
    CH1 = lists:foldl(fun({{_, DocID}=Key, RID, Pids}=Item, Acc) when RID =:= ReqID ->
			      TmpCH = [Item],
			      lists:foreach(fun(P) ->
						    stop_change_handler(Key, P, TmpCH),
						    P ! {change_handler_down, DocID}
					    end, Pids),
			      Acc;
			 (C, Acc) -> [C | Acc]
		      end, [], CH),
    {noreply, State#state{change_handlers=CH1}};
handle_info({_ReqID, {error, E}}, State) ->
    format_log(info, "WHISTLE_COUCH.wait(~p): ERROR ~p for reqid ~p~n", [self(), E, _ReqID]),
    {noreply, State};
handle_info({'DOWN', _MRefConn, process, Pid, _Reason}, #state{change_handlers=CH}=State) ->
    format_log(error, "WHISTLE_COUCH(~p): Pid(~p) went down: ~p~n", [self(), Pid, _Reason]),
    CH1 = lists:foldl(fun({Key, _, _}=Item, Acc) ->
			      case stop_change_handler(Key, Pid, [Item]) of
				  {_, []} -> Acc;
				  {_, [Item1]} -> [ Item1 | Acc]
			      end
		    end, [], CH),
    {noreply, State#state{change_handlers=CH1}};
handle_info({'EXIT', Pid, _Reason}, #state{change_handlers=CH}=State) ->
    format_log(error, "WHISTLE_COUCH(~p): EXIT received for ~p with reason ~p~n", [self(), Pid, _Reason]),
    CH1 = lists:foldl(fun({Key, _, _}=Item, Acc) ->
			      case stop_change_handler(Key, Pid, [Item]) of
				  {_, []} -> Acc;
				  {_, [Item1]} -> [ Item1 | Acc]
			      end
		    end, [], CH),
    {noreply, State#state{change_handlers=CH1}};
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

-spec(close_handlers/1 :: (CHs :: list(change_handler_entry())) -> no_return()).
close_handlers(CHs) ->
    lists:foreach(fun({{_, DocID}, _, Pids}) ->
			  lists:foreach(fun(P) -> unlink(P), P ! {change_handler_down, DocID} end, Pids)
		  end, CHs).

-spec(stop_change_handler/3 :: (Key :: tuple(string(), binary()), Pid :: pid(), CH :: list(change_handler_entry())) -> tuple(ok, list(change_handler_entry())) | tuple(tuple(error, term()), list(change_handler_entry()))).
stop_change_handler({_,_}=Key, Pid, CH) ->
    unlink(Pid),
    case lists:keyfind(Key, 1, CH) of
	false -> {{error, doc_unmonitored}, CH};
	{Key, ReqID, Pids} when is_list(Pids) ->
	    Pids1 = lists:foldl(fun(P, AccPids) when P =:= Pid -> AccPids;
				   (P, AccPids) ->
					case erlang:is_process_alive(P) of
					    true -> [P | AccPids];
					    false -> AccPids
					end
				end, [], Pids),
	    case Pids1 of
		[] ->
		    {ok, lists:keydelete(Key, 1, CH)};
		_ ->
		    {ok, [ {Key, ReqID, Pids1} | lists:keydelete(Key, 1, CH)]}
	    end
    end.

-spec(start_change_handler/4 :: (DBName :: string(), DocID :: binary(), Pid :: pid(), State :: #state{}) -> tuple(ok, term(), #state{}) | tuple(error, term(), #state{})).
start_change_handler(DBName, <<>>, Pid, State) ->
    start_change_handler(DBName, <<>>, Pid, State, []);
start_change_handler(DBName, DocID, Pid, State) ->
    start_change_handler(DBName, DocID, Pid, State, [{filter, <<"filter/by_doc">>}, {name, DocID}]).

-spec(start_change_handler/5 :: (DBName :: string(), DocID :: binary(), Pid :: pid(), State :: #state{}, Opts :: proplist()) -> tuple(ok, reference(), #state{}) | tuple(error, term(), #state{})).
start_change_handler(DBName, DocID, Pid, #state{connection={_H, Conn}, dbs=DBs, change_handlers=CH}=State, Opts) ->
    case lists:keyfind({DBName, DocID}, 1, CH) of
	false ->
	    case get_db(DBName, Conn, DBs) of
		{error, db_not_reachable} ->
		    {error, db_not_reachable, State};
		{Db, _DBs1} ->
		    {ok, ReqID} = couchbeam:changes_wait(Db, self(), [{heartbeat, "true"} | Opts]),
		    format_log(info, "WHISTLE_COUCH(~p): Added handler for ~p(~p) ref ~p~n", [self(), DocID, Pid, ReqID]),
		    link(Pid),
		    {ok, ReqID, State#state{change_handlers=[{{DBName, DocID}, ReqID, [Pid]} | CH]}}
	    end;
	{{DBName, DocID}, ReqID, Pids} ->
	    case lists:member(Pid, Pids) of
		false ->
		    {ok, ReqID, State#state{change_handlers=[{{DBName, DocID}, ReqID, [Pid | Pids]} | lists:keydelete({DBName, DocID}, 1, CH)]}};
		true ->
		    format_log(info, "WHISTLE_COUCH(~p): Found handler for ~p(~p)~n", [self(), DocID, Pid]),
		    {error, handler_exists, State}
	    end
    end.

-spec(get_new_connection/1 :: (Host :: string()) -> tuple(string(), tuple()) | tuple(error, term())).
get_new_connection(Host) ->
    Conn = couchbeam:server_connection(Host, 5984, "", []),
    format_log(info, "WHISTLE_COUCH(~p): Host ~p has conn ~p~n", [self(), Host, Conn]),
    case couchbeam:server_info(Conn) of
	{ok, _Version} ->
	    format_log(info, "WHISTLE_COUCH(~p): Connected to ~p~n~p~n", [self(), Host, _Version]),
	    spawn(fun() -> save_config(Host) end),
	    {Host, Conn};
	{error, Err}=E ->
	    format_log(error, "WHISTLE_COUCH(~p): Unable to connect to ~p: ~p~n", [self(), Host, Err]),
	    E
    end.

%% get_db, if DbName is known, returns the {#db{}, DBs}, else returns {#db{}, [{DbName, #db{}} | DBs]}
%% an error in opening the db will cause a {{error, Err}, DBs} to be returned
-spec(get_db/3 :: (DbName :: string(), Conn :: #server{}, DBs :: list(db_entry()) | []) -> tuple(error, db_not_reachable) | tuple(#db{}, list(db_entry())) ).
get_db(DbName, Conn, DBs) ->
    get_db(DbName, Conn, DBs, []).

-spec(get_db/4 :: (DbName :: string(), Conn :: #server{}, DBs :: list(db_entry()) | [], Options :: list()) -> tuple(error, db_not_reachable) | tuple(#db{}, list(db_entry())) ).
get_db(DbName, Conn, DBs, Options) ->
    case lists:keyfind(DbName, 1, DBs) of
	false ->
	    {ok, Db} = couchbeam:open_or_create_db(Conn, DbName, Options),
	    case couchbeam:db_info(Db) of
		{ok, _JSON} ->
		    {Db, [{DbName, Db} | DBs]};
		{error, _Error} ->
		    {error, db_not_reachable}
	    end;
	{DbName, Db} ->
	    {Db, DBs}
    end.

%% get_view, if Db/DesignDoc is known, return {#view{}, Views}, else returns {#view{}, [{{#db{}, DesignDoc, ViewOpts}, #view{}} | Views]}
-spec(get_view/4 :: (Db :: #db{}, DesignDoc :: string() | tuple(string(), string()), ViewOptions :: list(), Views :: list(view_entry())) -> tuple(#view{}, list(view_entry())) | tuple(error, view_not_found)).
get_view(Db, DesignDoc, ViewOptions, Views) ->
    case lists:keyfind({Db, DesignDoc, ViewOptions}, 1, Views) of
	{{Db, DesignDoc, ViewOptions}, View} ->
	    {View, Views};
	false ->
	    case couchbeam:view(Db, DesignDoc, ViewOptions) of
		{error, _Error}=E -> E;
		{ok, View} ->
		    case couchbeam_view:first(View) of
			{error, not_found} -> {error, view_not_found};
			{error, _Error}=E -> format_log(error, "COUCH_MGR(~p): Failed view:first/1 ~p~n", [self(), _Error]), E;
			_ -> {View, [{{Db, DesignDoc, ViewOptions}, View} | Views]}
		    end
	    end
    end.

-spec(init_state/0 :: () -> #state{}).
init_state() ->
    case get_startup_config() of
	{ok, Ts} ->
	    Host = case props:get_value(couch_host, Ts, props:get_value(default_couch_host, Ts)) of
		       undefined -> net_adm:localhost();
		       "localhost" -> net_adm:localhost();
		       H -> H
		   end,
	    case get_new_connection(Host) of
		{error, _} -> #state{};
		{Host, _}=C -> #state{connection=C}
	    end;
	_ -> #state{}
    end.

%% notify_pids, sends change notifications to a list of PIDs, return void
-spec(notify_pids/3 :: (Change :: proplist(), DocID :: binary(), Pids :: list(pid())) -> ok).
notify_pids(Change, DocID, Pids) ->
    SendToPid = case get_value(<<"deleted">>, Change) of
        true ->
            format_log(info, "WHISTLE_COUCH.wait(~p): ~p deleted~n", [self(), DocID]),
            {document_deleted, DocID}; % document deleted, no more looping
        undefined ->
            format_log(info, "WHISTLE_COUCH.wait(~p): ~p change sending to ~p~n", [self(), DocID, Pids]),
            {document_changes, DocID, lists:map(fun({C}) -> C end, get_value(<<"changes">>, Change))}
        end,
    lists:foreach(fun(P) -> P ! SendToPid end, Pids).

-spec(get_startup_config/0 :: () -> tuple(ok, proplist()) | tuple(error, term())).
get_startup_config() ->
    file:consult(?STARTUP_FILE).

-spec(save_config/1 :: (H :: string()) -> no_return()).
save_config(H) ->
    {ok, Config} = get_startup_config(),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun({K,V}, Acc) ->
					 [io_lib:format("{~p, ~p}.~n", [K, V]) | Acc]
				 end, "", [{couch_host, H} | lists:keydelete(couch_host, 1, Config)])
		   ).
