%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Read `tries`, 'try_interval' and 'stop_after' from app's config
%%%  document.
%%%
%%%  Ring to offnet number, parks it and bridge with reqester.
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-define(SERVER, ?MODULE).

-record(state, {exten :: kz_term:api_binary()
               ,stored_call :: kapps_call:call()
               ,queue :: kz_term:api_binary()
               ,n_try :: non_neg_integer()
               ,max_tries :: non_neg_integer()
               ,try_after :: non_neg_integer()
               ,stop_timer :: 'undefined' | timer:tref()
               ,parked_call :: kz_term:api_binary()
               ,offnet_ctl_q :: kz_term:api_binary()
               ,moh :: kz_term:api_binary()
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

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(list()) -> kz_types:startlink_ret().
start_link(Args) ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], Args).

-spec init([kz_json:object()]) -> {'ok', state()}.
init([JObj]) ->
    Exten = kz_json:get_value(<<"Number">>, JObj),
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    lager:info("started offnet handler(~p) for request ~s->~s", [self(), kapps_call:from_user(Call), Exten]),

    MaxTriesSystem = kapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"tries">>, 10),
    MaxTries = kz_json:get_integer_value(<<"Tries">>, JObj, MaxTriesSystem),

    TryIntervalSystem = kapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"try_interval">>,3),
    TryInterval = timer:minutes(kz_json:get_value(<<"Try-Interval">>, JObj, TryIntervalSystem)),

    StopAfterSystem = kapps_config:get_integer(?CAMPER_CONFIG_CAT, <<"stop_after">>, 31),
    StopAfter = timer:minutes(kz_json:get_integer_value(<<"Stop-After">>, JObj, StopAfterSystem)),

    {'ok', StopTimerRef} = timer:apply_after(StopAfter, 'gen_listener', 'cast', [self(), 'stop_campering']),

    Moh = case kzd_accounts:fetch(kapps_call:account_id(Call)) of
              {'ok', JObj} ->
                  kz_media_util:media_path(
                    kz_json:get_value([<<"music_on_hold">>, <<"media_id">>], JObj)
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

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("unhandled request from ~p: ~p", [_From, _Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
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
              | kz_api:default_headers(Queue, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
             ],
    kapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hangup)),
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
            | kz_api:default_headers(Queue, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hold)),
    Req = build_bridge_request(CallId, Call, Queue),
    lager:debug("publishing bridge request"),
    kapi_resource:publish_originate_req(Req),
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

-spec add_request(kz_json:object()) -> 'ok'.
add_request(JObj) ->
    Exten = kz_json:get_value(<<"Number">>, JObj),
    lager:info("adding offnet request to ~s", [Exten]),
    _ = camper_offnet_sup:new(JObj),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec handle_resource_response(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case {kz_json:get_value(<<"Event-Category">>, JObj)
         ,kz_json:get_value(<<"Event-Name">>, JObj)
         }
    of
        {<<"resource">>, <<"offnet_resp">>} ->
            ResResp = kz_json:get_value(<<"Resource-Response">>, JObj),
            handle_originate_ready(ResResp, Props);
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            lager:debug("time to bridge"),
            gen_listener:cast(Srv, {'parked', CallId});
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            lager:debug("got channel destroy, retrying..."),
            gen_listener:cast(Srv, 'wait');
        {<<"resource">>,<<"originate_resp">>} ->
            case {kz_json:get_value(<<"Application-Name">>, JObj)
                 ,kz_json:get_value(<<"Application-Response">>, JObj)
                 }
            of
                {<<"bridge">>, <<"SUCCESS">>} ->
                    lager:debug("users bridged"),
                    gen_listener:cast(Srv, 'stop_campering');
                _Ev -> lager:info("unhandled event: ~p", [_Ev])
            end;
        {<<"error">>,<<"originate_resp">>} ->
            gen_listener:cast(Srv, 'hangup_parked_call');
        _Ev -> lager:info("unhandled event ~p", [_Ev])
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_bridge_request(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:proplist().
build_bridge_request(ParkedCallId, Call, Q) ->
    CIDNumber = kapps_call:kvs_fetch('cf_capture_group', Call),
    MsgId = kz_binary:rand_hex(6),
    PresenceId = kz_attributes:presence_id(Call),
    AcctId = kapps_call:account_id(Call),
    {'ok', EP} = kz_endpoint:build(kapps_call:authorizing_id(Call)
                                  ,kz_json:from_list([{<<"can_call_self">>, 'true'}])
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
                           ,{<<"Account-Realm">>, kapps_call:from_realm(Call)}
                           ,{<<"Timeout">>, 10 * ?MILLISECONDS_IN_SECOND}
                           ,{<<"From-URI-Realm">>, kapps_call:from_realm(Call)}
                            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                           ]).

-spec originate_park(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
originate_park(<<_/binary>> = Exten, Call, <<_/binary>> = Q) ->
    kapi_offnet_resource:publish_req(build_offnet_request(Exten, Call, Q)).

-spec handle_originate_ready(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_originate_ready(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case {kz_json:get_value(<<"Event-Category">>, JObj)
         ,kz_json:get_value(<<"Event-Name">>, JObj)
         }
    of
        {<<"dialplan">>, <<"originate_ready">>} ->
            Q = kz_json:get_value(<<"Server-ID">>, JObj),
            CallId = kz_json:get_value(<<"Call-ID">>, JObj),
            CtrlQ = kz_json:get_value(<<"Control-Queue">>, JObj),
            Prop = [{<<"Call-ID">>, CallId}
                   ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                    | kz_api:default_headers(gen_listener:queue_name(Srv), ?APP_NAME, ?APP_VERSION)
                   ],
            gen_listener:cast(Srv, {'offnet_ctl_queue', CtrlQ}),
            gen_listener:add_binding(Srv, {'call', ?MK_CALL_BINDING(CallId)}),
            kapi_dialplan:publish_originate_execute(Q, Prop);
        _Ev -> lager:info("unkown event: ~p", [_Ev])
    end.

-spec build_offnet_request(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:proplist().
build_offnet_request(Exten, Call, Q) ->
    {ECIDNum, ECIDName} = kz_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = kz_attributes:caller_id(<<"external">>, Call),
    MsgId = kz_binary:rand_hex(6),
    PresenceId = kz_attributes:presence_id(Call),
    AcctId = kapps_call:account_id(Call),
    CallId = kz_binary:rand_hex(8),
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
                           ,{<<"Account-Realm">>, kapps_call:from_realm(Call)}
                           ,{<<"Timeout">>, 10 * ?MILLISECONDS_IN_SECOND}
                           ,{<<"To-DID">>, Exten}
                           ,{<<"Format-From-URI">>, <<"true">>}
                           ,{<<"From-URI-Realm">>, kapps_call:from_realm(Call)}
                            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                           ]).
