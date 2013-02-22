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
         ,get_server/0
         ,test_conn/0
        ]).
-export([get_admin_url/0
         ,get_admin_port/0
         ,get_admin_server/0
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
        ]).
-export([rm_change_handler/1
         ,rm_change_handler/2
         ,rm_change_handler/3
        ]).

-include_lib("wh_couch.hrl").

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec update(couch_connection()) -> 'ok'.
update(#wh_couch_connection{}=Connection) ->
    gen_server:cast(?MODULE, {update_connection, Connection}).

-spec add(couch_connection()) -> 'ok'.
add(#wh_couch_connection{}=Connection) ->
    gen_server:cast(?MODULE, {add_connection, Connection}).

-spec wait_for_connection() -> 'ok'.
wait_for_connection() ->    
    try test_conn() of
        _ -> ok
    catch
        error:{badmatch,'$end_of_table'} ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_connection()
    end.    

-spec get_host() -> string().
get_host() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,host = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Host], _} = ets:select(?MODULE, MatchSpec, 1),
    Host.

-spec get_port() -> integer().
get_port() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,port = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Port], _} = ets:select(?MODULE, MatchSpec, 1),
    Port.

-spec get_admin_port() -> integer().
get_admin_port() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,port = '$3', _ = '_'}
                  ,['$1', '$2']
                  ,['$3']
                 }],
    {[AdminPort], _} = ets:select(?MODULE, MatchSpec, 1),
    AdminPort.

-spec get_creds() -> {string(), string()}.
get_creds() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,username = '$3', password = '$4'
                                       ,_ = '_'}
                  ,['$1', {'not','$2'}]
                  ,[{{'$3', '$4'}}]
                 }],
    {[Creds], _} = ets:select(?MODULE, MatchSpec, 1),
    Creds.

-spec get_server() -> #server{}.
get_server() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,server = '$3', _ = '_'}
                  ,['$1', {'not','$2'}]
                  ,['$3']
                 }],
    {[Server], _} = ets:select(?MODULE, MatchSpec, 1),
    Server.

-spec get_admin_server() -> #server{}.
get_admin_server() ->
    MatchSpec = [{#wh_couch_connection{ready = '$1', admin = '$2'
                                       ,server = '$3', _ = '_'}
                  ,['$1', '$2']
                  ,['$3']
                 }],
    {[AdminServer], _} = ets:select(?MODULE, MatchSpec, 1),
    AdminServer.

-spec get_url() -> api_binary().
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

-spec get_admin_url() -> api_binary().
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

test_conn() -> couch_util:server_info(get_server()).

test_admin_conn() -> couch_util:server_info(get_admin_server()).

-spec add_change_handler(ne_binary()) -> 'ok'.
add_change_handler(DbName) -> add_change_handler(DbName, self()).

-spec add_change_handler(ne_binary(), pid() | binary()) -> 'ok'.
add_change_handler(DbName, Pid) when is_pid(Pid) ->
    add_change_handler(DbName, Pid, <<>>);
add_change_handler(DbName, DocId) ->
    add_change_handler(DbName, self(), DocId).

-spec add_change_handler(ne_binary(), pid(), binary()) -> 'ok'.
add_change_handler(DbName, Pid, DocId) ->
    lager:debug("add change listener ~p for ~s/~s", [Pid, DbName, DocId]),
    ServerName = wh_gen_changes:server_name(DbName),
    case catch wh_change_handler:add_listener(ServerName, Pid, DocId) of
        {'EXIT', {noproc, {gen_server, call, _}}} ->
            Db = couch_util:get_db(get_server(), DbName),
            {ok, _} = wh_change_handler_sup:start_handler(Db, []),
            add_change_handler(DbName, Pid, DocId);
        added -> ok;
        exists -> ok
    end.

-spec rm_change_handler(ne_binary()) -> 'ok'.
rm_change_handler(DbName) -> rm_change_handler(DbName, self()).

-spec rm_change_handler(ne_binary(), pid() | binary()) -> 'ok'.
rm_change_handler(DbName, Pid) when is_pid(Pid) ->
    rm_change_handler(DbName, Pid, <<>>);
rm_change_handler(DbName, DocId) ->
    rm_change_handler(DbName, self(), DocId).

-spec rm_change_handler(ne_binary(), pid(), binary()) -> 'ok'.
rm_change_handler(DbName, Pid, DocId) ->
    lager:debug("remove change listener ~p for ~s/~s", [Pid, DbName, DocId]),
    ServerName = wh_gen_changes:server_name(DbName),
    wh_change_handler:rm_listener(ServerName, Pid, DocId).

-spec get_node_cookie() -> atom().
get_node_cookie() ->
    Default = gen_server:call(?MODULE, node_cookie),
    try whapps_config:get(?CONFIG_CAT, <<"bigcouch_cookie">>, Default) of
        Cookie -> wh_util:to_atom(Cookie, true)
    catch
        _:_ -> wh_util:to_atom(Default, true)
    end.

-spec set_node_cookie(atom()) -> 'ok'.
set_node_cookie(Cookie) when is_atom(Cookie) ->
    _ = (catch whapps_config:set(?CONFIG_CAT, <<"bigcouch_cookie">>, wh_util:to_binary(Cookie))),
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
-spec maybe_start_new_connection(couch_connection()) -> any().
maybe_start_new_connection(Connection) ->
    _ = wh_couch_connection_sup:add(Connection),
    _ = ets:insert(?MODULE, Connection).

-spec get_url(binary(), pos_integer(), string(), string()) -> api_binary().
get_url(<<>>, _, _, _) -> 'undefined';
get_url(H, P, [], []) ->
    list_to_binary(["http://", H, ":", wh_util:to_binary(P), "/"]);
get_url(H, P, User, Pwd) ->
    list_to_binary(["http://", wh_util:to_binary(User), ":", wh_util:to_binary(Pwd)
                    ,"@", H, ":", wh_util:to_binary(P), "/"
                   ]).
