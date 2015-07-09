%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%%  Read `tries`, 'try_interval' and 'stop_after' from app's config
%%%  document.
%%%
%%%  Ring to offnet number, parks it and bridge with reqester.
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
         ,handle_event/2
         ,terminate/2
         ,code_change/3
         ,handle_resource_response/2
        ]).

-export([add_request/1]).

-include("camper.hrl").

-record(state, {exten :: api_binary()
                ,stored_call :: whapps_call:call()
                ,queue :: api_binary()
                ,n_try :: non_neg_integer()
                ,max_tries :: non_neg_integer()
                ,try_after :: non_neg_integer()
                ,stop_timer :: 'undefined' | timer:tref()
                ,parked_call :: api_binary()
                ,offnet_ctl_q :: api_binary()
                ,moh :: api_binary()
               }).
-type state() :: #state{}.

-define(MK_CALL_BINDING(CALLID), [{'callid', CALLID}
                                  ,{'restrict_to', [<<"CHANNEL_DESTROY">>
                                                    ,<<"CHANNEL_ANSWER">>
                                                   ]}
                                 ]).

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_resource_response'}
                      ,[{<<"*">>, <<"*">>}]
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

-spec init([wh_json:object()]) -> {'ok', state()}.
init([JObj]) ->
    Exten = wh_json:get_value(<<"Number">>, JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    lager:info("Started offnet handler(~p) for request ~s->~s", [self(), whapps_call:from_user(Call), Exten]),

    MaxTriesSystem = whapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"tries">>, 10),
    MaxTries = wh_json:get_integer_value(<<"Tries">>, JObj, MaxTriesSystem),

    TryIntervalSystem = whapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"try_interval">>,3),
    TryInterval = timer:minutes(wh_json:get_value(<<"Try-Interval">>, JObj, TryIntervalSystem)),

    StopAfterSystem = whapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"stop_after">>, 31),
    StopAfter = timer:minutes(wh_json:get_integer_value(<<"Stop-After">>, JObj, StopAfterSystem)),

    {'ok', StopTimerRef} = timer:apply_after(StopAfter, 'gen_listener', 'cast', [self(), 'stop_campering']),

    Moh = case kz_account:fetch(whapps_call:account_id(Call)) of
              {'ok', JObj} ->
                  wh_media_util:media_path(
                    wh_json:get_value([<<"music_on_hold">>, <<"media_id">>], JObj)
                   );
              _ -> 'undefined'
          end,

    {'ok', #state{exten = Exten
                  ,stored_call = Call
                  ,queue = 'undefined'
                  ,n_try = 0
                  ,max_tries = MaxTries
                  ,stop_timer = StopTimerRef
                  ,try_after = TryInterval
                  ,moh = Moh
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
handle_call(_Request, _From, State) ->
    lager:debug("unhandled request from ~p: ~p", [_From, _Request]),
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
    gen_listener:cast(self(), 'count'),
    {'noreply', S#state{queue = Q}};
handle_cast('count', #state{n_try=NTry
                            ,max_tries=MaxTries
                           }=State) ->
    lager:debug("count"),

    case NTry < MaxTries of
        'true' ->
            lager:info("making originate request(~p/~p)", [NTry + 1, MaxTries]),
            gen_listener:cast(self(), 'originate_park'),
            {'noreply', State#state{n_try = 1 + NTry}};
        'false' ->
            {'stop', 'normal', State}
    end;
handle_cast('originate_park', #state{exten=Exten
                                     ,stored_call=Call
                                     ,queue=Q
                                    }=State) ->
    lager:debug("originate park"),
    originate_park(Exten, Call, Q),
    {'noreply', State};
handle_cast({'offnet_ctl_queue', CtrlQ}, State) ->
    {'noreply', State#state{offnet_ctl_q = CtrlQ}};
handle_cast('hangup_parked_call', #state{parked_call='undefined'}=State) ->
    {'noreply', State};
handle_cast('hangup_parked_call', #state{parked_call=ParkedCall
                                         ,queue=Queue
                                         ,offnet_ctl_q=CtrlQ
                                        }=State) ->
    lager:debug("hangup park"),

    Hangup = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Call-ID">>, ParkedCall}
              | wh_api:default_headers(Queue, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
             ],
    wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hangup)),
    {'noreply', State#state{parked_call = 'undefined'}};
handle_cast({'parked', <<_/binary>> = CallId}, #state{moh=MOH
                                                      ,queue=Queue
                                                      ,offnet_ctl_q=CtrlQ
                                                      ,stored_call=Call
                                                     }=State) ->
    Hold = [{<<"Application-Name">>, <<"hold">>}
            ,{<<"Insert-At">>, <<"now">>}
            ,{<<"Hold-Media">>, MOH}
            ,{<<"Call-ID">>, CallId}
            | wh_api:default_headers(Queue, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hold)),
    Req = build_bridge_request(CallId, Call, Queue),
    lager:debug("Publishing bridge request"),
    wapi_resource:publish_originate_req(Req),
    {'noreply', State#state{parked_call = CallId}};
handle_cast('wait', #state{try_after = Time} = State) ->
    lager:debug("wait before next try"),
    {'ok', _TimerRef} = timer:apply_after(Time, 'gen_listener', 'cast', [self(), 'count']),
    {'noreply', State};
handle_cast('stop_campering', #state{stop_timer = 'undefined'} = State) ->
    lager:debug("stopping"),
    {'stop', 'normal', State};
handle_cast('stop_campering', #state{stop_timer = Timer} = State) ->
    lager:debug("stopping"),
    _ = timer:cancel(Timer),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec add_request(wh_json:object()) -> 'ok'.
add_request(JObj) ->
    Exten = wh_json:get_value(<<"Number">>, JObj),
    lager:info("adding offnet request to ~s", [Exten]),
    camper_offnet_sup:new(JObj),
    'ok'.

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

handle_event(_JObj, _State) ->
    {'reply', []}.

-spec handle_resource_response(wh_json:object(), wh_proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case {wh_json:get_value(<<"Event-Category">>, JObj)
          ,wh_json:get_value(<<"Event-Name">>, JObj)
         }
    of
        {<<"resource">>, <<"offnet_resp">>} ->
            ResResp = wh_json:get_value(<<"Resource-Response">>, JObj),
            handle_originate_ready(ResResp, Props);
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            lager:debug("time to bridge"),
            gen_listener:cast(Srv, {'parked', CallId});
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            lager:debug("Got channel destroy, retrying..."),
            gen_listener:cast(Srv, 'wait');
        {<<"resource">>,<<"originate_resp">>} ->
            case {wh_json:get_value(<<"Application-Name">>, JObj)
                  ,wh_json:get_value(<<"Application-Response">>, JObj)
                 }
            of
                {<<"bridge">>, <<"SUCCESS">>} ->
                    lager:debug("Users bridged"),
                    gen_listener:cast(Srv, 'stop_campering');
                _Ev -> lager:info("Unhandled event: ~p", [_Ev])
            end;
        {<<"error">>,<<"originate_resp">>} ->
            gen_listener:cast(Srv, 'hangup_parked_call');
        _Ev -> lager:info("Unhandled event ~p", [_Ev])
    end.

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
-spec build_bridge_request(ne_binary(), whapps_call:call(), ne_binary()) -> wh_proplist().
build_bridge_request(ParkedCallId, Call, Q) ->
    CIDNumber = whapps_call:kvs_fetch('cf_capture_group', Call),
    MsgId = wh_util:rand_hex_binary(6),
    PresenceId = cf_attributes:presence_id(Call),
    AcctId = whapps_call:account_id(Call),
    {'ok', EP} = cf_endpoint:build(whapps_call:authorizing_id(Call)
                                   ,wh_json:from_list([{<<"can_call_self">>, 'true'}])
                                   ,Call
                                  ),
    props:filter_undefined([{<<"Resource-Type">>, <<"audio">>}
                            ,{<<"Application-Name">>, <<"bridge">>}
                            ,{<<"Existing-Call-ID">>, ParkedCallId}
                            ,{<<"Endpoints">>, EP}
                            ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
                            ,{<<"Originate-Immediate">>, 'false'}
                            ,{<<"Msg-ID">>, MsgId}
                            ,{<<"Presence-ID">>, PresenceId}
                            ,{<<"Account-ID">>, AcctId}
                            ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
                            ,{<<"Timeout">>, 10 * ?MILLISECONDS_IN_SECOND}
                            ,{<<"From-URI-Realm">>, whapps_call:from_realm(Call)}
                            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                           ]).

-spec originate_park(ne_binary(), whapps_call:call(), ne_binary()) -> 'ok'.
originate_park(<<_/binary>> = Exten, Call, <<_/binary>> = Q) ->
    wapi_offnet_resource:publish_req(build_offnet_request(Exten, Call, Q)).

-spec handle_originate_ready(wh_json:object(), wh_proplist()) -> 'ok'.
handle_originate_ready(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case {wh_json:get_value(<<"Event-Category">>, JObj)
          ,wh_json:get_value(<<"Event-Name">>, JObj)
         }
    of
        {<<"dialplan">>, <<"originate_ready">>} ->
            Q = wh_json:get_value(<<"Server-ID">>, JObj),
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
            Prop = [{<<"Call-ID">>, CallId}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(gen_listener:queue_name(Srv), ?APP_NAME, ?APP_VERSION)
                   ],
            gen_listener:cast(Srv, {'offnet_ctl_queue', CtrlQ}),
            gen_listener:add_binding(Srv, {'call', ?MK_CALL_BINDING(CallId)}),
            wapi_dialplan:publish_originate_execute(Q, Prop);
        _Ev -> lager:info("unkown event: ~p", [_Ev])
    end.

-spec build_offnet_request(ne_binary(), whapps_call:call(), ne_binary()) -> wh_proplist().
build_offnet_request(Exten, Call, Q) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    MsgId = wh_util:rand_hex_binary(6),
    PresenceId = cf_attributes:presence_id(Call),
    AcctId = whapps_call:account_id(Call),
    CallId = wh_util:rand_hex_binary(8),
    props:filter_undefined([{<<"Resource-Type">>, <<"originate">>}
                            ,{<<"Application-Name">>, <<"park">>}
                            ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
                            ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
                            ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                            ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
                            ,{<<"Msg-ID">>, MsgId}
                            ,{<<"Presence-ID">>, PresenceId}
                            ,{<<"Account-ID">>, AcctId}
                            ,{<<"Call-ID">>, CallId}
                            ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
                            ,{<<"Timeout">>, 10 * ?MILLISECONDS_IN_SECOND}
                            ,{<<"To-DID">>, Exten}
                            ,{<<"Format-From-URI">>, <<"true">>}
                            ,{<<"From-URI-Realm">>, whapps_call:from_realm(Call)}
                            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                           ]).
