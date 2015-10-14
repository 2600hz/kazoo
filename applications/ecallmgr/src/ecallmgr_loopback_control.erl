-module(ecallmgr_loopback_control).

-behaviour(gen_server).

%% API functions
-export([start_link/4
         ,get_endpoints/1
         ,handle_call_command/2
         ,maybe_set_channel_id/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_event/2,
         terminate/2,
         code_change/3]).

-define(RESPONDERS, [{{?MODULE, 'handle_call_command'}
                      ,[{<<"call">>, <<"command">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call_id                         :: ne_binary()
                ,loopback_id                    :: ne_binary()
                ,resource_type = <<"audio">>    :: ne_binary()
                ,endpoint                       :: wh_json:object()
                ,self = self()                  :: pid()
                ,control_q                      :: api_binary()
                ,reply_to                       :: 'undefined' | {pid(), any()}
                ,call_vars = []                 :: wh_json:objects()
                ,channel_vars = []              :: wh_json:objects()
                ,endpoints                      :: wh_json:object()
                ,dial_method                    :: ne_binary()
               }).

-include("ecallmgr.hrl").

-type state() :: #state{}.
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(api_binary(), ne_binary(), ne_binary(), wh_json:object()) -> startlink_ret().
start_link('undefined', ResourceType, LoopbackId, Endpoint) ->
    CallId = <<"loopback-", (couch_mgr:get_uuid())/binary>>,
    start_link(CallId, ResourceType, LoopbackId, Endpoint);
start_link(<<"loopback-", _/binary>> = CallId, ResourceType, LoopbackId, Endpoint) ->
    Bindings = [{'call', [{'callid', CallId}]}
                ,{'dialplan', []}
                ,{'self', []}
               ],
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', Bindings}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ]
                            ,[CallId, LoopbackId, ResourceType, Endpoint]);
start_link(CallId, LoopbackId, ResourceType, Endpoints) ->
    start_link(<<"loopback-", CallId/binary, "-", (wh_util:rand_hex_binary(8))/binary>>, LoopbackId
               ,ResourceType, Endpoints).

-spec get_endpoints(pid()) -> {'ok', api_binary(), wh_json:objects()}.
get_endpoints(Pid) ->
    gen_server:call(Pid, 'get_endpoints').

-spec handle_call_command(wh_json:object(), wh_proplist()) -> any().
handle_call_command(JObj, Props) ->
    Pid = props:get_value('server', Props),
    LoopbackId = props:get_value('loopback_id', Props),
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    handle_call_command(App, Pid, LoopbackId, JObj).

-spec maybe_set_channel_id(ne_binary(), wh_proplist()) -> 'ok'.
maybe_set_channel_id(UUID, Props) ->
    case props:get_value(<<"variable_ecallmgr_Loopback-ID">>, Props) of
        'undefined' -> 'ok';
        LoopbackId ->
            set_channel_id(LoopbackId, UUID)
    end.


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
-spec init(ne_binaries()) -> {'ok', state()}.
init([CallId, LoopbackId, ResourceType, Endpoint]) ->
    wh_util:put_callid(CallId),
    {'ok', #state{call_id = CallId
                  ,endpoint = Endpoint
                  ,loopback_id = LoopbackId
                  ,resource_type = ResourceType
                 }}.

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
handle_call('get_endpoints', From, State) ->
    maybe_reply(State#state{reply_to = From});
handle_call(_Request, _From, State) ->
    {'noreply', State}.

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
handle_cast({'gen_listener', {'created_queue', CtrlQ}}, #state{call_id = CallId
                                                               ,endpoint = E
                                                               ,call_vars = CallVars
                                                               ,channel_vars = CCVs
                                                               ,resource_type = ResourceType
                                                              } = State) ->
    NewState = State#state{control_q = CtrlQ},
    EndpointCCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, E),
    Realm = wh_json:get_value(<<"To-Realm">>, E),
    Number = wh_json:get_value(<<"Route">>, E),
    RouteReq = [{<<"From">>, <<"unknown@unknown">>}
                ,{<<"To">>, <<"unknown@unknown">>}
                ,{<<"Request">>, <<Number/binary, "@", Realm/binary>>}
                ,{<<"Call-ID">>, CallId}
                ,{<<"Caller-ID-Number">>, <<"0">>}
                ,{<<"Caller-ID-Name">>, <<"Loopback">>}
                ,{<<"Resource-Type">>, ResourceType}
                ,{<<"Custom-Channel-Vars">>, EndpointCCVs}
                | wh_api:default_headers(<<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)
               ],
    {'ok', Resp} = wh_amqp_worker:call(RouteReq
                                       ,fun wapi_route:publish_req/1
                                       ,fun wapi_route:is_actionable_resp/1
                                       ,2000),
    RespCCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Resp, wh_json:new()),
    RespCallVars = wh_json:get_value(<<"Custom-Call-Vars">>, Resp, wh_json:new()),
    ServerId = wh_json:get_value(<<"Server-ID">>, Resp),
    RouteWin = [{<<"Call-ID">>, CallId}
                ,{<<"Custom-Channel-Vars">>, CCVs}
                ,{<<"Server-ID">>, ServerId}
                ,{<<"Control-Queue">>, CtrlQ}
                | wh_api:default_headers(<<"dialplan">>, <<"route_win">>
                                         ,?APP_NAME, ?APP_VERSION)],
    wh_amqp_worker:cast(RouteWin, fun(Payload) -> wapi_route:publish_win(ServerId, Payload) end),
    {'noreply', NewState#state{channel_vars = [RespCCVs, EndpointCCVs | CCVs]
                               ,call_vars = [RespCallVars | CallVars]
                              }};
handle_cast({'call_command', Data}, State) ->
    Endpoints = wh_json:get_value(<<"Endpoints">>, Data, []),
    DialMethod = wh_json:get_value(<<"Dial-Endpoint-Method">>, Data),
    NewState = State#state{endpoints = Endpoints, dial_method = DialMethod},
    maybe_reply(NewState);
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    {'noreply', State};
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
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec handle_event(Event, State) -> {reply, proplist()}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), state()) -> {'reply', proplist()}.
handle_event(_Event, #state{self = Self, loopback_id = LoopbackId} = _State) ->
    {'reply', [{'server', Self}, {'loopback_id', LoopbackId}]}.

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
    %% TODO we should send 'call ended' here
    'ok'.

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
-spec maybe_reply(state()) -> {'noreply', state()}.
maybe_reply(#state{reply_to = 'undefined'} = State) ->
    {'noreply', State};
maybe_reply(#state{endpoints = 'undefined'} = State) ->
    {'noreply', State};
maybe_reply(#state{reply_to = ReplyTo
                   ,endpoints = Endpoints
                   ,dial_method = DialMethod
                  } = State) ->
    gen_server:reply(ReplyTo, {'ok', DialMethod, Endpoints}),
    {'noreply', State}.

-spec handle_call_command(ne_binary(), pid(), ne_binary(), wh_json:object()) -> any().
handle_call_command(<<"bridge">>, Pid, _, JObj) ->
    gen_server:cast(Pid, {'call_command', JObj});
handle_call_command(_, _, LoopbackId, JObj) ->
    wh_cache:store_local(?ECALLMGR_CMDS_CACHE, LoopbackId, [JObj | stored_commands(LoopbackId)]).

-spec stored_commands(ne_binary()) -> wh_json:objects().
stored_commands(LoopbackId) ->
    case wh_cache:fetch_local(?ECALLMGR_CMDS_CACHE, LoopbackId) of
        {'error', 'not_found'} -> [];
        {'ok', Commands} -> Commands
    end.

-spec set_channel_id(ne_binary(), ne_binary()) -> 'ok'.
set_channel_id(LoopbackId, UUID) ->
    case wh_cache:fetch_local(?ECALLMGR_CMDS_CACHE, LoopbackId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Commands} ->
            wh_cache:store_local(?ECALLMGR_CMDS_CACHE, UUID, [wh_json:set_value(<<"Call-ID">>, UUID, Cmd) || Cmd <- lists:reverse(Commands)]),
            wh_cache:erase_local(?ECALLMGR_CMDS_CACHE, LoopbackId),
            'ok'
    end.
