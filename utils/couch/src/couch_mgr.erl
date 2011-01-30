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

%% System manipulation
-export([get_db/1]).

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
%% ChangeHandlers :: [{DBName, DocID}, ReqID, [Pid]]
-type change_handler_entry() :: tuple( tuple(string(), binary()), reference(), list(pid()) | []).
-record(state, {
	  connection = {} :: tuple(string(), #server{}) | {}
	  ,change_handlers = [] :: list(change_handler_entry()) | []
	 }).

%%%===================================================================
%%% Couch Functions
%%%===================================================================




%%%===================================================================
%%% Document Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% open a document given a docid returns not_found or the Document
%% @end
%%--------------------------------------------------------------------
-spec(open_doc/2 :: (DbName :: string(), DocId :: binary()) -> proplist() | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec(open_doc/3 :: (DbName :: string(), DocId :: binary(), Options :: proplist()) -> proplist() | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId, Options) when not is_binary(DocId) ->   
    open_doc(DbName, whistle_util:to_binary(DocId), Options);
open_doc(DbName, DocId, Options) ->    
    case get_db(DbName) of
        {error, db_not_reachable}=E ->
                E;
	Db ->
	    case couchbeam:open_doc(Db, DocId, Options) of
		{ok, {Doc}} ->
		    Doc;
		Other ->
		    format_log(error, "WHISTLE_COUCH(~p): Failed to find ~p: ~p~n", [self(), DocId, Other]),
		    {error, not_found}
	    end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% save document to the db
%% @end
%%--------------------------------------------------------------------
-spec(save_doc/2 :: (DbName :: list(), Doc :: proplist()) -> tuple(ok, proplist()) | tuple(error, conflict)).
save_doc(DbName, [{struct, [_|_]=Doc}]) ->
    save_doc(DbName, Doc);
save_doc(DbName, [{struct, _}|_]=Doc) ->
    io:format("Test 1 ~p~n", [Doc]),
    case get_db(DbName) of
	{error, db_not_reachable}=E ->
	    E;
	Db->
	    %% convert from mochijson encoding to couch
            Str = mochijson2:encode(Doc),
            couchbeam:save_docs(Db, couchbeam_util:json_decode(Str))
    end;
save_doc(DbName, Doc) ->
    case get_db(DbName) of
	{error, db_not_reachable}=E ->
	    E;
	Db->
            couchbeam:save_doc(Db, {Doc})
    end.
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove document from the db
%% @end
%%--------------------------------------------------------------------
-spec(del_doc/2 :: (DbName :: list(), Doc :: proplist()) -> tuple(ok | error, term())).
del_doc(DbName, Doc) ->
    case get_db(DbName) of
	{error, db_not_reachable}=E ->
	    E;
	Db ->
            Str = mochijson2:encode(Doc),
	    couchbeam:delete_doc(Db, couchbeam_util:json_decode(Str))
    end.

%%%===================================================================
%%% Document Helpers
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% create a new Document - a tuple with a proplist
%% @end
%%--------------------------------------------------------------------    
-spec(new_doc/0 :: () -> []).
new_doc() -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% add a K/V pair to a Document
%% @end
%%--------------------------------------------------------------------    
-spec(add_to_doc/3 :: (Key :: binary(), Value :: term(), Doc :: proplist()) -> proplist()).
add_to_doc(Key, Value, Doc) ->
    {Doc1} = couchbeam_doc:extend(whistle_util:to_binary(Key), Value, {Doc}),
    Doc1.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% remove a K from the Document
%% @end
%%--------------------------------------------------------------------    
-spec(rm_from_doc/2 :: (Key :: binary(), Doc :: proplist()) -> proplist()).
rm_from_doc(Key, Doc) ->
    {Doc1} = couchbeam_doc:delete_value(whistle_util:to_binary(Key), {Doc}),
    Doc1.

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
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

get_results(DbName, DesignDoc, ViewOptions) ->
    case get_db(DbName) of
	{error, db_not_reachable}=E ->
	    E;
	Db ->
	    case get_view(Db, DesignDoc, ViewOptions) of
		{error, _Error}=E ->
		    E;
		View ->
		    case couchbeam_view:fetch(View) of
			{ok, {Prop}} ->
			    get_value(<<"rows">>, Prop, []);
			Error ->
			    {Error, fetch_failed}
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
    gen_server:call(?MODULE, {set_host, HostName}, infinity).

get_host() ->
    gen_server:call(?MODULE, get_host).

get_db(DbName) ->
    Conn = gen_server:call(?MODULE, {get_db}),
    open_db(whistle_util:to_list(DbName), Conn).

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
handle_call(get_host, _From, #state{connection={H,_}}=State) ->
    {reply, H, State};
handle_call({set_host, Host}, _From, #state{connection={OldHost, _}}=State) ->
    format_log(info, "WHISTLE_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	HC ->
	    {reply, ok, State#state{connection=HC, change_handlers=[]}}
    end;
handle_call({set_host, Host}, _From, State) ->
    format_log(info, "WHISTLE_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    case get_new_connection(Host) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC, change_handlers=[]}}
    end;
handle_call({get_db}, _, #state{connection={_Host, Conn}}=State) ->
    {reply, Conn, State};
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
	    handle_call(Req, From, State#state{connection=HC, change_handlers=[]})
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(close_handlers/1 :: (CHs :: list(change_handler_entry())) -> no_return()).
close_handlers(CHs) ->
    lists:foreach(fun({{_, DocID}, _, Pids}) ->
			  lists:foreach(fun(P) -> unlink(P), P ! {change_handler_down, DocID} end, Pids)
		  end, CHs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(start_change_handler/4 :: (DBName :: string(), DocID :: binary(), Pid :: pid(), State :: #state{}) -> tuple(ok, term(), #state{}) | tuple(error, term(), #state{})).
start_change_handler(DBName, <<>>, Pid, State) ->
    start_change_handler(DBName, <<>>, Pid, State, []);
start_change_handler(DBName, DocID, Pid, State) ->
    start_change_handler(DBName, DocID, Pid, State, [{filter, <<"filter/by_doc">>}, {name, DocID}]).

-spec(start_change_handler/5 :: (DBName :: string(), DocID :: binary(), Pid :: pid(), State :: #state{}, Opts :: proplist()) -> tuple(ok, reference(), #state{}) | tuple(error, term(), #state{})).
start_change_handler(DBName, DocID, Pid, #state{connection={_H, Conn}, change_handlers=CH}=State, Opts) ->
    case lists:keyfind({DBName, DocID}, 1, CH) of
	false ->
	    case open_db(DBName, Conn) of
		{error, db_not_reachable} ->
		    {error, db_not_reachable, State};
		Db ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% open_db, if DbName is known, returns the {#db{}, DBs}, else returns {#db{}, [{DbName, #db{}} | DBs]}
%% an error in opening the db will cause a {{error, Err}, DBs} to be returned
%% @end
%%--------------------------------------------------------------------
-spec(open_db/2 :: (DbName :: string(), Conn :: #server{}) -> tuple(error, db_not_reachable) | #db{}).
open_db(DbName, Conn) ->
    {ok, Db} = couchbeam:open_or_create_db(Conn, DbName),
    case couchbeam:db_info(Db) of
	{ok, _JSON} -> Db;
	{error, _Error} -> {error, db_not_reachable}
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% notify_pids, sends change notifications to a list of PIDs, return void
%% @end
%%--------------------------------------------------------------------
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
    {ok, Config} = get_startup_config(),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun({K,V}, Acc) ->
					 [io_lib:format("{~p, ~p}.~n", [K, V]) | Acc]
				 end, "", [{couch_host, H} | lists:keydelete(couch_host, 1, Config)])
		   ).
