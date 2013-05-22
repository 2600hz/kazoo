%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_couch_bootstrap).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("wh_couch.hrl").

-define(SERVER, ?MODULE).
-record(state, {}).

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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
    put('callid', ?LOG_SYSTEM_ID),
    Config= get_config(),
    %% TODO: for the time being just maintain backward compatability
    wh_couch_connections:add(create_connection(Config)),
    wh_couch_connections:add(create_admin_connection(Config)),
    [AutoCmpt|_] = wh_config:get('bigcouch', 'compact_automatically', ['true']),
    CacheProps = [{'expires', 'infinity'}
                  ,{'origin', {'db', ?WH_CONFIG_DB, <<"whistle_couch">>}}
                 ],
    wh_cache:store_local(?WH_COUCH_CACHE, <<"compact_automatically">>, AutoCmpt, CacheProps),
    [Cookie|_] = wh_config:get_atom('bigcouch', 'cookie', ['monster']),
    wh_couch_connections:set_node_cookie(Cookie),
    lager:info("waiting for first bigcouch/haproxy connection...", []),
    wh_couch_connections:wait_for_connection(),
    {'ok', #state{}, 100}.

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
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
    {'noreply', State}.

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
handle_info('timeout', State) ->
%%    _ = wh_couch_sup:stop_bootstrap(),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("couch bootstrap terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_config() -> {'ok', wh_proplist()}.
get_config() ->
    [IP|_] = wh_config:get('bigcouch', 'ip', ["localhost"]),
    [Port|_] = wh_config:get_integer('bigcouch', 'port', [5984]),
    [Username|_]= wh_config:get_raw_string('bigcouch', 'username', [""]),
    [Pwd|_] = wh_config:get_raw_string('bigcouch', 'password', [""]),
    [AdminPort|_] = wh_config:get_integer('bigcouch', 'admin_port', [5986]),
    [{'default_couch_host', {IP, Port, Username, Pwd, AdminPort}}].

-spec create_connection(wh_proplist()) -> couch_connection().
create_connection(Props) ->
    Routines = [fun(C) ->
                        import_config(props:get_value('default_couch_host', Props), C)
                end
                ,fun(C) -> wh_couch_connection:set_admin('false', C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, #wh_couch_connection{id = 1}, Routines).

-spec create_admin_connection(wh_proplist()) -> couch_connection().
create_admin_connection(Props) ->
    Routines = [fun(C) ->
                        case props:get_value('default_couch_host', Props) of
                            {Host, _, User, Pass, AdminPort} ->
                                import_config({Host, AdminPort, User, Pass}, C);
                            {Host, _, User, Pass} ->
                                import_config({Host, User, Pass}, C);
                            Else ->
                                import_config(Else, C)
                        end
                end
                ,fun(C) -> wh_couch_connection:set_admin('true', C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, #wh_couch_connection{id = 2}, Routines).

-spec import_config('undefined' | tuple(), couch_connection()) -> couch_connection().
import_config({Host}, Connection) ->
    wh_couch_connection:config(Host, Connection);
import_config({Host, Port}, Connection) ->
    wh_couch_connection:config(Host, Port, Connection);
import_config({Host, User, Pass}, Connection) ->
    wh_couch_connection:config(Host, User, Pass, Connection);
import_config({Host, Port, User, Pass}, Connection) ->
    wh_couch_connection:config(Host, Port, User, Pass, Connection);
import_config({Host, Port, User, Pass, _}, Connection) ->
    wh_couch_connection:config(Host, Port, User, Pass, Connection);
import_config('undefined', Connection) -> Connection.
