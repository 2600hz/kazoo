%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(camper_offnet_handler).

-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
    ,handle_resource_response/2
]).

-export([add_request/2]).

-include("camper.hrl").

-record(state, {exten, stored_call, queue, n_try}).

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_originate_ready'}
                      ,[{<<"*">>, <<"*">>}]
%                      ,[{<<"dialplan">>, <<"originate_ready">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link([term()]) -> startlink_ret().
start_link(Args) ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], Args).

init([Exten, Call]) ->
    lager:info("Statred offnet listener, pid: ~p", [self()]),
    {'ok', #state{exten = Exten, stored_call = Call, queue = 'undefined', n_try = 0}}.

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
    lager:info("unhandled request from ~p: ~p", [_From, _Request]),
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
handle_cast({'gen_listener', {'created_queue', Q}}, #state{queue = 'undefined'} = S) ->
    gen_listener:cast(self(), 'originate_offnet'),
    {'noreply', S#state{queue = Q, n_try = 0}};
handle_cast({'srv', Srv}, S) ->
    lager:info("Got pid: ~p", [Srv]),
%    gen_listener:queue_name_async(Srv),
%    gen_listener:cast(Srv, 'originate_offnet'),
%    {'noreply', S#state{queue = gen_listener:queue_name(Srv), n_try = 0}};
    {'noreply', S};
%handle_cast({'queue_name', Q}, S) ->
%    lager:info("Got queue: ~s", [Q]),
%    gen_listener:cast(self(), 'originate_offnet'),
%    {'noreply', S#state{queue = Q, n_try = 0}};
handle_cast('originate_offnet', State) ->
    Exten = State#state.exten,
    Call = State#state.stored_call,
    Q = State#state.queue,
    lager:info("making originate request"),
    originate(Exten, Call, Q),
%    {_, Res} = timer:apply_after(timer:seconds(5), ?MODULE, originate, [Exten, Call]),
%    lager:info("Timer return: ~p", [Res]),
    {'noreply', State#state{n_try = 1 + State#state.n_try}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec add_offnet(ne_binary(), whapps_call:call()) -> 'ok'.
add_offnet(Exten, Call) ->
    lager:info("adding offnet request to ~s", [Exten]),
    camper_offnet_sup:new(Exten, Call).

originate(Exten, Call, Q) ->
    wapi_offnet_resource:publish_req(build_offnet_request(Exten, Call, Q)).
%    case wait_for_stepswitch() of
%        {<<"SUCCESS">>, _} ->
%            lager:info("completed successful offnet request");
%        {Cause, Code} ->
%            Self = self(),
%            lager:info("offnet request failed~nCall: ~p~n, Exten: ~s~n, Cause: ~s~n, Code: ~s~n", [Call, Exten, Cause, Code]),
%            timer:apply_after(timer:minutes(2), 'gen_server', 'cast', [Self, {'originate_offnet', NTry + 1}]);
%        Err -> lager:info("Unkonwn result: ~p", [Err])
%    end.

-spec build_offnet_request(wh_json:object(), whapps_call:call(), ne_binary()) -> wh_proplist().
build_offnet_request(Exten, Call, Q) ->
    lager:info("Building offnet request"),
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    MsgId = wh_util:rand_hex_binary(6),
    PresenceId = cf_attributes:presence_id(Call),
    AcctId = whapps_call:account_id(Call),
%    ToDid = whapps_call:request_user(Call),
    CallId = wh_util:rand_hex_binary(8),
    props:filter_undefined([{<<"Resource-Type">>, <<"originate">>}
        ,{<<"Application-Name">>, <<"park">>}
%        ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, Exten}])}
        ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
        ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
        ,{<<"Outbound-Caller-ID-Name">>, CIDName}
        ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
%        ,{<<"Endpoints">>, [wh_json:from_list([{<<"Invite-Format">>, <<"route">>}
%                                               ,{<<"Route">>, Exten}
%                                               ,{<<"To-DID">>, Exten}
%                                               ,{<<"To-User">>, Exten}
%                                               ,{<<"To-Realm">>, whapps_call:from_realm(Call)}
%                                              ])
%                           ]}
        ,{<<"Msg-ID">>, MsgId}
        ,{<<"Presence-ID">>, PresenceId}
        ,{<<"Account-ID">>, AcctId}
        ,{<<"Call-ID">>, CallId}
        ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
        ,{<<"Timeout">>, 10000}
        ,{<<"To-DID">>, Exten}
        ,{<<"From-URI-Realm">>, whapps_call:from_realm(Call)}
        | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
    ]).

-spec wait_for_stepswitch() -> {ne_binary(), api_binary()}.
wait_for_stepswitch() ->
    case whapps_call_command:receive_event(10000, 'true') of
        {'ok', JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    {wh_json:get_value(<<"Response-Message">>, JObj)
                        ,wh_json:get_value(<<"Response-Code">>, JObj)
                    };
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    lager:info("recv channel destroy"),
                    {wh_json:get_value(<<"Hangup-Cause">>, JObj)
                        ,wh_json:get_value(<<"Hangup-Code">>, JObj)
                    };
                _ -> wait_for_stepswitch()
            end;
        {'error', 'timeout'} = Err -> Err;
        _ -> wait_for_stepswitch()
    end.

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
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

handle_originate_ready(JObj, Props) ->
    lager:info("----~ngot:~nProps: ~p~n JObj: ~p~n----", [Props, JObj]),
    case {wh_json:get_value(<<"Event-Category">>, JObj)
          ,wh_json:get_value(<<"Event-Name">>, JObj)}
    of
        {<<"resource">>, <<"offnet_resp">>} ->
            ResResp = wh_json:get_value(<<"Resource-Response">>, JObj),
            case {wh_json:get_value(<<"Event-Category">>, ResResp)
                  ,wh_json:get_value(<<"Event-Name">>, ResResp)}
            of
                {<<"dialplan">>, <<"originate_ready">>} ->
                    Q = wh_json:get_value(<<"Server-ID">>, ResResp),
                    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, ResResp)}
                            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ResResp)}
                            | wh_api:default_headers(Q, <<"dialplan">>, <<"originate_execute">>, ?APP_NAME, ?APP_VERSION)
                    ],
                    wapi_dialplan:publish_originate_execute(Q, Prop);
                _ -> 'ok'
            end;
        _ -> 'ok'
    end,
    'ok'.


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
