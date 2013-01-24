%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_couch_connections).

-behaviour(gen_server).

-export([start_link/0]).
-export([add/1]).
-export([update/1]).
-export([wait_for_connection/0]).
-export([get_url/0
         ,get_host/0
         ,get_port/0
         ,get_creds/0
         ,get_conn/0
         ,test_conn/0
        ]).
-export([get_admin_url/0
         ,get_admin_port/0
         ,get_admin_conn/0
         ,test_admin_conn/0
        ]).
-export([get_node_cookie/0
         ,set_node_cookie/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([add_change_handler/1
         ,add_change_handler/2
         ,add_change_handler/3
         ,rm_change_handler/2
        ]).

-include_lib("whistle_couch/include/wh_couch.hrl").

-record(state, {cookie = change_me}).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update(#wh_couch_connection{}=Connection) ->
    gen_server:cast(?MODULE, {update_connection, Connection}).

add(#wh_couch_connection{}=Connection) ->
    gen_server:cast(?MODULE, {add_connection, Connection}).

wait_for_connection() ->    
    try test_conn() of
        _ -> ok
    catch
        error:{badmatch,'$end_of_table'} ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_connection()
    end.    

get_host() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,host = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Host], _} = ets:select(?MODULE, MatchSpec, 1),
    Host.

get_port() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,port = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Port], _} = ets:select(?MODULE, MatchSpec, 1),
    Port.

get_admin_port() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,port = '$3', _ = '_'}
                  ,['$1', '$2']
                  ,['$3']
                 }],
    {[AdminPort], _} = ets:select(?MODULE, MatchSpec, 1),
    AdminPort.

get_creds() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,username = '$3', password = '$4'
                                       ,_ = '_'}
                  ,['$1', {'not','$2'}]
                  ,[{{'$3', '$4'}}]
                 }],
    {[Creds], _} = ets:select(?MODULE, MatchSpec, 1),
    Creds.

get_conn() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,connection = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Connection], _} = ets:select(?MODULE, MatchSpec, 1),
    Connection.

get_admin_conn() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,connection = '$3', _ = '_'}
                  ,['$1', '$2']
                  ,['$3']
                 }],
    {[AdminConnection], _} = ets:select(?MODULE, MatchSpec, 1),
    AdminConnection.

-spec get_url/0 :: () -> ne_binary() | 'undefined'.
get_url() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,host = '$3', port = '$4'
                                       ,username = '$5', password = '$6'
                                       ,_ = '_'}
                  ,['$1', {'not','$2'}]
                  ,[{{'$3', '$4', '$5', '$6'}}]
                 }],
    {[{Host, Port, Username, Password}], _} = ets:select(?MODULE, MatchSpec, 1),
    get_url(Host, Port, Username, Password).

-spec get_admin_url/0 :: () -> ne_binary() | 'undefined'.
get_admin_url() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,host = '$3', port = '$4'
                                       ,username = '$5', password = '$6'
                                       ,_ = '_'}
                  ,['$1', '$2']
                  ,[{{'$3', '$4', '$5', '$6'}}]
                 }],
    {[{Host, Port, Username, Password}], _} = ets:select(?MODULE, MatchSpec, 1),
    get_url(Host, Port, Username, Password).

test_conn() ->
    couch_util:server_info(get_conn()).

test_admin_conn() ->
    couch_util:server_info(get_admin_conn()).

add_change_handler(DBName) ->
    lager:debug("add change handler for DB: ~s", [DBName]),
    gen_server:cast(?MODULE, {add_change_handler, wh_util:to_binary(DBName), <<>>, self()}).

add_change_handler(DBName, Pid) when is_pid(Pid) ->
    lager:debug("add change handler for DB: ~s", [DBName]),
    gen_server:cast(?MODULE, {add_change_handler, wh_util:to_binary(DBName), <<>>, Pid});
add_change_handler(DBName, DocID) ->
    lager:debug("add change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
    gen_server:cast(?MODULE, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), self()}).

add_change_handler(DBName, DocID, Pid) ->
    lager:debug("add change handler for Pid: ~p for DB: ~s and Doc: ~s", [Pid, DBName, DocID]),
    gen_server:cast(?MODULE, {add_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID), Pid}).

rm_change_handler(DBName, DocID) ->
    lager:debug("rm change handler for DB: ~s and Doc: ~s", [DBName, DocID]),
    gen_server:call(?MODULE, {rm_change_handler, wh_util:to_binary(DBName), wh_util:to_binary(DocID)}).

-spec get_node_cookie/0 :: () -> atom().
get_node_cookie() ->
    gen_server:call(?MODULE, node_cookie).

-spec set_node_cookie/1 :: (atom()) -> 'ok'.
set_node_cookie(Cookie) when is_atom(Cookie) ->
    gen_server:cast(?MODULE, {node_cookie, Cookie}).

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
init([]) ->
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    _ = ets:new(?MODULE, [ordered_set
                          ,{read_concurrency, true}
                          ,{keypos, #wh_couch_connection.id}
                          ,named_table
                         ]),
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
handle_call(node_cookie, _From, #state{cookie=Cookie}=State) ->
    {reply, Cookie, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast({add_connection, #wh_couch_connection{}=Connection}, State) ->
    maybe_start_new_connection(Connection),
    {noreply, State};
handle_cast({update_connection, #wh_couch_connection{}=Connection}, State) ->
    true = ets:insert(?MODULE, Connection),
    {noreply, State};    
handle_cast({node_cookie, Cookie}, State) ->
    {noreply, State#state{cookie=Cookie}};
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
handle_info(_Info, State) ->
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
    lager:debug("couch connections terminating: ~p", [_Reason]).

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
-spec maybe_start_new_connection/1 :: (#wh_couch_connection{}) -> 'ok'.
maybe_start_new_connection(Connection) ->
    _ = wh_couch_connection_sup:add(Connection),
    _ = ets:insert(?MODULE, Connection).

-spec get_url/4 :: (binary(), pos_integer(), string(), string()) -> api_binary().
get_url(<<>>, _, _, _) -> 'undefined';
get_url(H, P, [], []) ->
    list_to_binary(["http://", H, ":", wh_util:to_binary(P), "/"]);
get_url(H, P, User, Pwd) ->
    list_to_binary(["http://", wh_util:to_binary(User), ":", wh_util:to_binary(Pwd)
                    ,"@", H, ":", wh_util:to_binary(P), "/"
                   ]).

-spec maybe_start_change_handler/3 :: (ne_binary(), pid(), #state{}) -> #state{}.
maybe_start_change_handler(DbName, Pid, State) ->
    Conn = get_conn(),
    case couch_util:db_info(Conn, DbName) of
        {error, _} ->
            lager:notice("unable to fetch database info for ~s", [DbName]),
            State;
        {ok, JObj} ->
            UpdateSeq = wh_json:get_value(<<"update_seq">>, JObj),
            Db = couch_util:get_db(Conn, DbName)
%%            start_change_handler(Db, UpdateSeq, Pid, State)
    end.

%%-spec start_change_handler/4 :: (#db{}, list(), pid(), #state{}) -> #state{}.
%%start_change_handler(#db{name=DbName}=Db, [SeqNumber, Seq], Pid, #state{change_handlers=CH}=State) ->
%%    Since = <<"[", (wh_util:to_binary(SeqNumber))/binary, ",\"", Seq/binary, "\"]">>,
%%    case change_mgr_sup:start_handler(Db, [{since, wh_util:to_list(Since)}]) of
%%        {ok, Srv} ->
%%            lager:debug("started change handler(~p) ~s at sequence ~p", [Srv, DbName, SeqNumber]),
%%            SrvRef = erlang:monitor(process, Srv),
%%            change_handler:add_listener(Srv, Pid),
%%            State#state{change_handlers=dict:store(wh_util:to_binary(DbName), {Srv, SrvRef}, CH)};
%%        _Else ->
%%            lager:notice("failed to start ~s change handler: ~p", [DbName, _Else]),          
%%            State
%%    end.
    
