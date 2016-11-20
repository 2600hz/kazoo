%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter DEfebvre
%%%-------------------------------------------------------------------
-module(blackhole_tracking).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_req/2
        ,add_socket/1
        ,remove_socket/1
        ,update_socket/1
        ,get_sockets/1
        ,get_socket/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("blackhole.hrl").

-type state() :: ets:tid() | atom().

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{?MODULE, [{<<"blackhole">>, <<"get_req">>}]}]).
-define(BINDINGS, [{'blackhole', ['federate']}]).

-define(BLACKHOLE_QUEUE_NAME, <<>>).
-define(BLACKHOLE_QUEUE_OPTIONS, []).
-define(BLACKHOLE_CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?BLACKHOLE_QUEUE_NAME}
                            ,{'queue_options', ?BLACKHOLE_QUEUE_OPTIONS}
                            ,{'consume_options', ?BLACKHOLE_CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = kapi_blackhole:get_req_v(ApiJObj),
    kz_util:put_callid(ApiJObj),

    Node = kz_json:get_binary_value(<<"Node">>, ApiJObj),
    RespData =
        handle_get_req_data(
          kz_json:get_value(<<"Account-ID">>, ApiJObj)
                           ,kz_json:get_value(<<"Socket-ID">>, ApiJObj)
                           ,Node
         ),
    case RespData of
        'ok' -> 'ok';
        RespData ->
            RespQ = kz_json:get_value(<<"Server-ID">>, ApiJObj),
            Resp = [{<<"Data">>, RespData}
                   ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, ApiJObj)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            lager:debug("sending reply ~p to ~s",[RespData, Node]),
            kapi_blackhole:publish_get_resp(RespQ, Resp)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_socket(bh_context:context()) -> 'ok'.
add_socket(Context) ->
    gen_server:cast(?SERVER, {'add_socket', Context}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_socket(bh_context:context()) -> 'ok'.
remove_socket(Context) ->
    gen_server:cast(?SERVER, {'remove_socket', Context}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_socket(bh_context:context()) -> 'ok'.
update_socket(Context) ->
    gen_server:cast(?SERVER, {'update_socket', Context}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_sockets(ne_binary()) -> [bh_context:context(), ...] | {'error', 'not_found'}.
get_sockets(AccountId) ->
    gen_server:call(?SERVER, {'get_sockets', AccountId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_socket(ne_binary()) -> {'ok', bh_context:context()} | {'error', 'not_found'}.
get_socket(Id) ->
    gen_server:call(?SERVER, {'get_socket', Id}).

%%%===================================================================
%%% gen_listener callbacks
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
-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    lager:debug("starting new ~s server", [?SERVER]),
    Tab = ets:new(?SERVER, ['set'
                           ,'protected'
                           ,'named_table'
                           ,{'keypos', #bh_context.websocket_session_id}
                           ]),
    blackhole_bindings:bind(<<"blackhole.session.open">>, ?MODULE, 'add_socket'),
    blackhole_bindings:bind(<<"blackhole.session.close">>, ?MODULE, 'remove_socket'),
    blackhole_bindings:bind(<<"blackhole.finish.*">>, ?MODULE, 'update_socket'),
    {'ok', Tab}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'get_sockets', AccountId}, _From, State) ->
    Pattern = #bh_context{auth_account_id=AccountId, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            Contexts -> Contexts
        end,
    {'reply', Result, State};
handle_call({'get_socket', Id}, _From, State) ->
    Pattern = #bh_context{websocket_session_id=Id, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            [Context] ->
                {'ok', Context}
        end,
    {'reply', Result, State};
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'add_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
handle_cast({'remove_socket', Context}, State) ->
    _ = ets:delete_object(State, Context),
    {noreply, State};
handle_cast({'update_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    blackhole_bindings:unbind(<<"blackhole.session.open">>, ?MODULE, 'add_socket'),
    blackhole_bindings:unbind(<<"blackhole.session.close">>, ?MODULE, 'remove_socket'),
    blackhole_bindings:unbind(<<"blackhole.finish.*">>, ?MODULE, 'update_socket'),
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_get_req_data(api_binary(), api_binary(), api_binary()) -> any().
handle_get_req_data('undefined', 'undefined', Node) ->
    lager:warning("received undefined blackhole get req", [Node]);
handle_get_req_data(AccountId, 'undefined', Node) ->
    lager:debug("received blackhole get for account:~s from ~s", [AccountId, Node]),
    case get_sockets(AccountId) of
        {'error', 'not_found'} ->
            lager:debug("no sockets found for ~s", [AccountId]),
            [];
        Contexts ->
            ToDelete = [<<"account_id">>, <<"auth_token">>, <<"req_id">>, <<"auth_account_id">>],
            [kz_json:delete_keys(ToDelete, bh_context:to_json(Context))
             || Context <- Contexts]
    end;
handle_get_req_data('undefined', SocketId, Node) ->
    lager:debug("received blackhole get for socket:~s from ~s", [SocketId, Node]),
    case get_socket(SocketId) of
        {'error', 'not_found'} ->
            lager:debug("socket ~s not found", [SocketId]);
        {'ok', Context} ->
            bh_context:to_json(Context)
    end.
