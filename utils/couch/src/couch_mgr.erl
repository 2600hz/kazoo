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
-export([start_link/0, set_host/1, set_host/3, get_host/0]).

%% System manipulation
-export([db_exists/1, db_info/1, db_create/1, db_compact/1, db_delete/1, db_replicate/1]).

%% Document manipulation
-export([save_doc/2, open_doc/2, open_doc/3, del_doc/2]).
-export([add_change_handler/2, rm_change_handler/2, load_doc_from_file/3, update_doc_from_file/3]).

%% Views
-export([get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-include("../../src/whistle_types.hrl"). % get the whistle types
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
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a file into couch as a document (not an attachement)
%% @end
%%--------------------------------------------------------------------
-spec(load_doc_from_file/3 :: (DB :: binary(), App :: atom(), File :: list() | binary()) -> tuple(ok, json_object()) | tuple(error, term())).
load_doc_from_file(DB, App, File) ->
    Path = lists:flatten([code:priv_dir(App), "/couchdb/", whistle_util:to_list(File)]),
    logger:format_log(info, "Read into ~p from CouchDB dir: ~p~n", [DB, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	?MODULE:save_doc(DB, mochijson2:decode(Bin)) %% if it crashes on the match, the catch will let us know
    catch
	_Type:Reason -> {error, Reason}
    end.

-spec(update_doc_from_file/3 :: (DB :: binary(), App :: atom(), File :: list() | binary()) -> tuple(ok, json_term()) | tuple(error, term())).
update_doc_from_file(DB, App, File) ->
    Path = lists:flatten([code:priv_dir(App), "/couchdb/", whistle_util:to_list(File)]),
    logger:format_log(info, "Read into ~p from CouchDB dir: ~p~n", [DB, Path]),
    try
	{ok, Bin} = file:read_file(Path),
	{struct, Prop} = mochijson2:decode(Bin),
	{ok, {struct, ExistingDoc}} = ?MODULE:open_doc(DB, props:get_value(<<"_id">>, Prop)),
	?MODULE:save_doc(DB, {struct, [{<<"_rev">>, props:get_value(<<"_rev">>, ExistingDoc)} | Prop]})
    catch
	_Type:Reason -> {error, Reason}
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
                {ok, Info} -> {ok, mochijson2:decode(couchbeam_util:json_encode(Info))}
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
%%  ,{<<"create_target">>, <<"true">>} % optional, creates the DB on target if non-existent
%%  ,{<<"continuous">>, <<"true">>} % optional, continuously update target from source
%%  ,{<<"cancel">>, <<"true">>} % optional, will cancel a replication (one-time or continuous)
%%  ,{<<"filter">>, <<"source_design_doc/source_filter_name">>} % optional, filter what documents are sent from source to target
%%  ,{<<"query_params">>, {struct, [{<<"key1">>, <<"value1">>}, {<<"key2">>, <<"value2">>}]} } % optional, send params to filter function
%%  ,{<<"doc_ids">>, [<<"source_doc_id_1">>, <<"source_doc_id_2">>]} % optional, if you only want specific docs, no need for a filter
%%  ,{<<"proxy">>, <<"http://some.proxy.server:12345">>} % optional, if you need to pass the replication via proxy to target
%% ].
%%
%% If authentication is needed at the source's end:
%% {<<"source">>, <<"http://user:password@some.couch.server:5984/source_db">>}
%%
%% @end
%%--------------------------------------------------------------------
-spec(db_replicate/1 :: (Prop :: tuple(struct, proplist()) | proplist()) -> tuple(ok, term()) | tuple(error, term())).
db_replicate(Prop) when is_list(Prop) ->
    db_replicate({struct, Prop});
db_replicate({struct, _}=MochiJson) ->
    case get_conn() of
	{} -> {error, server_not_reachable};
	Conn ->
	    couchbeam:replicate(Conn, couchbeam_util:json_decode(mochijson2:encode(MochiJson)))
    end.

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
-spec(open_doc/2 :: (DbName :: string(), DocId :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, []).

-spec(open_doc/3 :: (DbName :: string(), DocId :: binary(), Options :: proplist()) -> tuple(ok, json_term()) | tuple(error, not_found | db_not_reachable)).
open_doc(DbName, DocId, Options) when not is_binary(DocId) ->
    open_doc(DbName, whistle_util:to_binary(DocId), Options);
open_doc(DbName, DocId, Options) ->    
    case get_db(DbName) of
        {error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:open_doc(Db, DocId, Options) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, mochijson2:decode(couchbeam_util:json_encode(Doc1))}
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
    save_doc(DbName, Doc);
save_doc(DbName, [{struct, _}|_]=Doc) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db ->
            case couchbeam:save_docs(Db, couchbeam_util:json_decode(mochijson2:encode(Doc))) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, mochijson2:decode(couchbeam_util:json_encode(Doc1))}
            end
    end;
save_doc(DbName, Doc) when is_list(Doc) ->
    save_doc(DbName, {struct, Doc});
save_doc(DbName, {struct, _}=Doc) ->
    case get_db(DbName) of
	{error, _Error} -> {error, db_not_reachable};
	Db -> 
            case couchbeam:save_doc(Db, couchbeam_util:json_decode(mochijson2:encode(Doc))) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, mochijson2:decode(couchbeam_util:json_encode(Doc1))}
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
	    case couchbeam:delete_doc(Db, couchbeam_util:json_decode(mochijson2:encode(Doc))) of
                {error, _Error}=E -> E;
                {ok, Doc1} -> {ok, mochijson2:decode(couchbeam_util:json_encode(Doc1))}
            end
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
			{ok, {Prop}} ->
			    Rows = get_value(<<"rows">>, Prop, []),
                            {ok, mochijson2:decode(couchbeam_util:json_encode(Rows))};
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
set_host(HostName) ->
    set_host(HostName, "", "").

set_host(HostName, UserName, Password) ->
    gen_server:call(?MODULE, {set_host, HostName, UserName, Password}, infinity).

get_host() ->
    gen_server:call(?MODULE, get_host).

get_conn() ->
    gen_server:call(?MODULE, {get_conn}).

get_db(DbName) ->
    Conn = gen_server:call(?MODULE, {get_conn}),
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
handle_call({set_host, Host, User, Pass}, _From, #state{connection={OldHost, _}}=State) ->
    format_log(info, "WHISTLE_COUCH(~p): Updating host from ~p to ~p~n", [self(), OldHost, Host]),
    case get_new_connection(Host, User, Pass) of
	{error, _Error}=E ->
	    {reply, E, State};
	HC ->
	    {reply, ok, State#state{connection=HC, change_handlers=[]}}
    end;
handle_call({set_host, Host, User, Pass}, _From, State) ->
    format_log(info, "WHISTLE_COUCH(~p): Setting host for the first time to ~p~n", [self(), Host]),
    case get_new_connection(Host, User, Pass) of
	{error, _Error}=E ->
	    {reply, E, State};
	{_Host, _Conn}=HC ->
	    {reply, ok, State#state{connection=HC, change_handlers=[]}}
    end;
handle_call({get_conn}, _, #state{connection={_Host, Conn}}=State) ->
    {reply, Conn, State};
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
    format_log(info, "WHISTLE_COUCH(~p): No connection, trying localhost(~p) with no auth~n", [self(), net_adm:localhost()]),
    case get_new_connection(net_adm:localhost(), "", "") of
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
    %%format_log(info, "WHISTLE_COUCH.wait(~p): keyfind res = ~p~n", [self(), lists:keyfind(ReqID, 2, CH)]),
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
		    %%format_log(info, "WHISTLE_COUCH(~p): Found handler for ~p(~p)~n", [self(), DocID, Pid]),
		    {error, handler_exists, State}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_new_connection/3 :: (Host :: string(), User :: string(), Pass :: string()) -> tuple(string(), #server{}) | tuple(error, term())).
get_new_connection(Host, "", "") -> get_new_conn(Host, []);
get_new_connection(Host, User, Pass) -> get_new_conn(Host, [{basic_auth, {User, Pass}}]).

-spec(get_new_conn/2 :: (Host :: string(), Opts :: proplist()) -> tuple(string(), #server{}) | tuple(error, term())).
get_new_conn(Host, Opts) ->
    Conn = couchbeam:server_connection(Host, 5984, "", Opts),
    format_log(info, "WHISTLE_COUCH(~p): Host ~p Opts ~p has conn ~p~n", [self(), Host, Opts, Conn]),
    case couchbeam:server_info(Conn) of
	{ok, _Version} ->
	    format_log(info, "WHISTLE_COUCH(~p): Connected to ~p~n~p~n", [self(), Host, _Version]),
	    spawn(fun() ->
			  case props:get_value(basic_auth, Opts) of
			      undefined -> save_config(Host);
			      {U, P} -> save_config(Host, U, P)
			  end
		  end),
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
    case couchbeam:open_or_create_db(Conn, DbName) of
        {error, _Error}=E -> E;
        {ok, Db} ->
            case couchbeam:db_info(Db) of
                {error, _Error}=E -> E;
                {ok, _JSON} -> Db
            end
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
	    case get_new_connection(Host, User, Pass) of
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
            {document_changes, DocID, [ C || {C} <- get_value(<<"changes">>, Change)]}
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
    save_config(H, "", "").

save_config(H, U, P) ->
    {ok, Config} = get_startup_config(),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun(Item, Acc) -> [io_lib:format("~p.~n", [Item]) | Acc] end
				 , "", [{couch_host, H, U, P} | lists:keydelete(couch_host, 1, Config)])
		   ).
