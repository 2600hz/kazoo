%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_callback_listener).
-behaviour(gen_listener).

-export([start_link/1
        ,handle_resource_response/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("cccp.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_resource_response'},[{<<"*">>, <<"*">>}]}
                    ,{{'cccp_util', 'relay_amqp'}, [{<<"*">>, <<"*">>}]}
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(PROMPT_DELAY, kapps_config:get_integer(?CCCP_CONFIG_CAT, <<"prompt_delay">>, 1) * ?MILLISECONDS_IN_SECOND).

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(list()) -> kz_types:startlink_ret().
start_link(JObj) ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [JObj]).

-spec init([kz_json:object()]) -> {'ok', state()}.
init([JObj]) ->
    ALegName = kz_json:get_value(<<"a_leg_name">>, JObj),
    ALegNumber = kz_json:get_value(<<"a_leg_number">>, JObj),
    BLegNumber = kz_json:get_value(<<"b_leg_number">>, JObj),
    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    AuthorizingId = kz_json:get_value(<<"user_id">>, JObj),
    AuthDocId = kz_json:get_value(<<"id">>, JObj),
    MediaId = kz_json:get_value(<<"media_id">>, JObj),
    RetainCID = kz_json:get_binary_boolean(<<"retain_cid">>, JObj, <<"false">>),
    CallbackDelay = kz_json:get_value(<<"callback_delay">>, JObj),
    RealCallbackDelay =
        case is_integer(CallbackDelay) of
            'true' -> CallbackDelay * ?MILLISECONDS_IN_SECOND;
            'false' -> kapps_config:get_integer(?CCCP_CONFIG_CAT, <<"callback_delay">>, 3) * ?MILLISECONDS_IN_SECOND
        end,
    {'ok', #state{a_leg_name = ALegName
                 ,a_leg_number = ALegNumber
                 ,parked_call_id = 'undefined'
                 ,b_leg_number = BLegNumber
                 ,account_id = AccountId
                 ,authorizing_id = AuthorizingId
                 ,call = kapps_call:new()
                 ,queue = 'undefined'
                 ,auth_doc_id = AuthDocId
                 ,media_id = MediaId
                 ,retain_cid = RetainCID
                 ,callback_delay = RealCallbackDelay
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', Q}}, #state{queue='undefined'}=S) ->
    gen_listener:cast(self(), 'originate_park'),
    {'noreply', S#state{queue = Q}};
handle_cast('originate_park', State) ->
    originate_park(State),
    {'noreply', State};
handle_cast({'offnet_ctl_queue', CtrlQ}, State) ->
    {'noreply', State#state{offnet_ctl_q=CtrlQ}};
handle_cast({'call_update', CallUpdate}, State) ->
    {'noreply', State#state{call=CallUpdate}};
handle_cast({'call_id_update', NewCallId}, #state{call=Call}=State) ->
    NewCall = kapps_call:set_call_id(NewCallId, Call) ,
    {'noreply', State#state{parked_call_id = NewCallId, call = NewCall}};
handle_cast({'parked', CallId, ToDID}, State) ->
    _P = bridge_to_final_destination(CallId, ToDID, State),
    lager:debug("bridging to ~s (via ~s) in ~p", [ToDID, CallId, _P]),
    {'noreply', State#state{parked_call_id = CallId}};
handle_cast('stop_callback', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{call=Call
                          ,account_id=AccountId
                          ,b_leg_number=BLegNumber
                          ,auth_doc_id=AuthDocId
                          ,media_id=MediaId
                          }=_State
            ) ->
    {'reply', [{'call', Call}
              ,{'account_id', AccountId}
              ,{'b_leg_number', BLegNumber}
              ,{'auth_doc_id', AuthDocId}
              ,{'media_id', MediaId}
              ]
    }.

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
-spec originate_park(state()) -> 'ok'.
originate_park(#state{account_id=AccountId
                     ,parked_call_id=CallId
                     ,a_leg_number=ToDID
                     ,authorizing_id=AuthorizingId
                     ,queue=Q
                     ,callback_delay=CallbackDelay
                     }) ->
    _ = timer:sleep(CallbackDelay),
    case lists:member(ToDID, cccp_util:current_account_outbound_directions(AccountId)) of
        'false' ->
            Req = cccp_util:build_request(CallId, ToDID, AuthorizingId, Q, 'undefined', AccountId, <<"park">>, <<"false">>, <<>>, <<>>),
            kapi_resource:publish_originate_req(Req);
        'true' ->
            gen_listener:cast(self(), 'stop_callback')
    end.

-spec handle_resource_response(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case kz_util:get_event_type(JObj) of
        {<<"dialplan">>,<<"route_win">>} ->
            gen_listener:cast(Srv, {'call_update', kapps_call:from_route_win(JObj,call(Props))}),
            gen_listener:add_binding(Srv, {'call',[{'callid', CallId}]});
        {<<"call_event">>,<<"CHANNEL_REPLACED">>} ->
            gen_listener:rm_binding(Srv, {'call',[]}),
            NewCallId = kz_json:get_value(<<"Replaced-By">>, JObj),
            gen_listener:cast(Srv, {'call_id_update', NewCallId}),
            gen_listener:add_binding(Srv, {'call',[{'callid', NewCallId}]});
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            CallUpdate = kapps_call:kvs_store_proplist([{'consumer_pid', self()},{'auth_doc_id', props:get_value('auth_doc_id',Props)}]
                                                      ,kapps_call:from_route_req(JObj,call(Props))
                                                      ),
            gen_listener:cast(Srv, {'call_update', CallUpdate}),
            gen_listener:cast(Srv, {'parked', CallId, b_leg_number(props:set_value(call, CallUpdate,Props))});
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>} ->
            cccp_util:handle_disconnect(JObj, Props);
        {<<"resource">>,<<"originate_resp">>} ->
            handle_originate_response(JObj, Props);
        _ -> 'ok'
    end.

-spec handle_originate_response(kz_json:object(), gen_listener:callback_data()) -> 'ok'.
handle_originate_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case {kz_json:get_value(<<"Application-Name">>, JObj)
         ,kz_json:get_value(<<"Application-Response">>, JObj)
         }
    of
        {<<"bridge">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        _ -> 'ok'
    end.

-spec bridge_to_final_destination(kz_term:ne_binary(), kz_term:ne_binary(), state()) -> 'ok'.
bridge_to_final_destination(CallId, ToDID, #state{offnet_ctl_q=CtrlQ
                                                 ,account_id=AccountId
                                                 ,authorizing_id=AuthorizingId
                                                 ,auth_doc_id=AccountDocId
                                                 ,retain_cid=RetainCID
                                                 ,a_leg_name=ALegName
                                                 ,a_leg_number=ALegNumber
                                                 }) ->
    cccp_util:bridge(CallId, ToDID, AuthorizingId, CtrlQ, AccountId, RetainCID, ALegName, ALegNumber),
    case AccountDocId of
        'undefined' -> 'ok';
        _ -> cccp_util:store_last_dialed(ToDID, AccountDocId)
    end.

-spec b_leg_number(kz_term:proplist()) -> kz_term:ne_binary().
b_leg_number(Props) ->
    case props:get_value('b_leg_number', Props) of
        'undefined' ->
            Call = call(Props),
            _ = timer:sleep(?PROMPT_DELAY),
            {'num_to_dial', Number} = cccp_util:get_number(Call),
            Number;
        <<DocId:32/binary>> ->
            _ = maybe_make_announcement_to_a_leg(Props),
            maybe_handle_doc_id(DocId, Props);
        BLegNumber ->
            _ = maybe_make_announcement_to_a_leg(Props),
            BLegNumber
    end.

-spec maybe_make_announcement_to_a_leg(kz_term:proplist()) -> 'ok'.
maybe_make_announcement_to_a_leg(Props) ->
    case props:get_value('media_id', Props) of
        <<MediaId:32/binary>> ->
            Call = call(Props),
            MediaPath = kz_media_util:media_path(MediaId, kapps_call:account_id(Call)),
            _ = timer:sleep(?PROMPT_DELAY),
            _ = kapps_call_command:b_play(MediaPath, Call),
            'ok';
        _ -> 'ok'
    end.

-spec call(kz_term:proplist()) -> kapps_call:call().
call(Props) ->
    props:get_value('call', Props).

-spec maybe_handle_doc_id(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
maybe_handle_doc_id(DocId, Props) ->
    AccountDb = kzs_util:format_account_db(props:get_value('account_id', Props)),
    case kz_datamgr:open_cache_doc(AccountDb, DocId) of
        {'error', _} -> kapps_call_command:hangup(call(Props));
        {'ok', JObj} -> maybe_handle_doc(JObj, Props)
    end.

-spec maybe_handle_doc(kz_json:object(), kz_term:proplist()) -> 'ok'.
maybe_handle_doc(JObj, Props) ->
    Call = call(Props),
    case kz_doc:type(JObj) of
        <<"conference">> ->
            conf_discover(JObj, ensure_call(Call));
        _ ->
            kapps_call_command:hangup(Call)
    end.

-spec conf_discover(kz_json:object(), kapps_call:call()) -> 'ok'.
conf_discover(ConfDoc, Call) ->
    Command =
        props:filter_undefined(
          [{<<"Call">>, kapps_call:to_json(Call)}
          ,{<<"Conference-ID">>, kz_doc:id(ConfDoc)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ]),
    kapi_conference:publish_discovery_req(Command).

-spec ensure_call(kapps_call:call()) -> kapps_call:call().
ensure_call(Call) ->
    case kapps_call:switch_hostname(Call) of
        'undefined' -> switch_hostname_lookup(Call);
        _ -> Call
    end.

-spec switch_hostname_lookup(kapps_call:call()) -> kapps_call:call().
switch_hostname_lookup(Call) ->
    case kapps_call:switch_nodename(Call) of
        <<"freeswitch@", Hostname/binary>> ->
            kapps_call:set_switch_hostname(Hostname, Call);
        _ -> Call
    end.
