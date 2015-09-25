%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
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

-record(state, {customer_number :: ne_binary()
                ,account_id :: ne_binary()
                ,account_cid :: ne_binary()
                ,stored_call :: whapps_call:call()
                ,queue :: api_binary()
                ,parked_call_id :: ne_binary()
                ,offnet_ctl_q :: ne_binary()
                ,auth_doc_id :: ne_binary()
               }).

-type state() :: #state{}.

-define(MK_CALL_BINDING(CallId), [{'callid', CallId}
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
-spec start_link([any()]) -> startlink_ret().
start_link(JObj) ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [JObj]).

-spec init(wh_json:object()) -> {'ok', state()}.
init([JObj]) ->
    CustomerNumber = wh_json:get_value(<<"Number">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    OutboundCID = wh_json:get_value(<<"Outbound-Caller-ID-Number">>, JObj),
    AuthDocId = wh_json:get_value(<<"Auth-Doc-Id">>, JObj),

    {'ok', #state{customer_number = CustomerNumber
                  ,account_id = AccountId
                  ,account_cid = OutboundCID
                  ,stored_call = whapps_call:new()
                  ,queue = 'undefined'
                  ,auth_doc_id = AuthDocId
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
handle_cast({'gen_listener', {'created_queue', Q}}, #state{queue='undefined'}=S) ->
    gen_listener:cast(self(), 'originate_park'),
    {'noreply', S#state{queue = Q}};
handle_cast('originate_park', State) ->
    originate_park(State),
    {'noreply', State};
handle_cast({'offnet_ctl_queue', CtrlQ}, State) ->
    {'noreply', State#state{offnet_ctl_q=CtrlQ}};
handle_cast({'hangup_parked_call', _ErrMsg}, #state{parked_call_id='undefined'}=State) ->
    {'noreply', State};
handle_cast({'hangup_parked_call', _ErrMsg}, #state{parked_call_id=ParkedCallId
                                                    ,queue=Q
                                                    ,offnet_ctl_q=CtrlQ
                                                   }=State) ->
    hangup_parked_call(ParkedCallId, Q, CtrlQ),
    {'noreply', State#state{parked_call_id='undefined'}};
handle_cast({'set_auth_doc_id', CallId}, #state{auth_doc_id=AuthDocId}=State) ->
    {'ok', Call} = whapps_call:retrieve(CallId, ?APP_NAME),
    CallUpdate = whapps_call:kvs_store('auth_doc_id', AuthDocId, Call),
    whapps_call:cache(CallUpdate, ?APP_NAME),
    {'noreply', State};
handle_cast({'parked', CallId, ToDID}, State) ->
    _P = bridge_to_final_destination(CallId, ToDID, State),
    lager:debug("bridging to ~s (via ~s) in ~p", [ToDID, CallId, _P]),
    {'noreply', State#state{parked_call_id = CallId}};
handle_cast('stop_callback', State) ->
    {'stop', 'normal', State};
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
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec handle_resource_response(wh_json:object(), wh_proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    case wh_util:get_event_type(JObj) of
        {<<"resource">>, <<"offnet_resp">>} ->
            ResResp = wh_json:get_value(<<"Resource-Response">>, JObj),
            handle_originate_ready(ResResp, Props);
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            {'ok', Call} =  whapps_call:retrieve(CallId, ?APP_NAME),
            CallUpdate = whapps_call:kvs_store('consumer_pid', self(), Call),
            whapps_call:cache(CallUpdate, ?APP_NAME),
            gen_listener:add_binding(Srv, {'call',[{'callid', CallId}]}),
            gen_listener:add_responder(Srv, {'cccp_util', 'relay_amqp'}, [{<<"*">>, <<"*">>}]),
            {'num_to_dial', Number} = cccp_util:get_number(CallUpdate),
            gen_listener:cast(Srv, {'parked', CallId, Number});
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>} ->
            cccp_util:handle_disconnect(JObj, Props);
        {<<"resource">>,<<"originate_resp">>} ->
            handle_originate_response(JObj, Srv, CallId);
        {<<"error">>,<<"originate_resp">>} ->
            gen_listener:cast(Srv, {'hangup_parked_call', wh_json:get_value(<<"Error-Message">>, JObj)});
        _ -> 'ok'
    end.

-spec handle_originate_response(wh_json:object(), server_ref(), ne_binary()) -> 'ok'.
handle_originate_response(JObj, Srv, CallId) ->
    case {wh_json:get_value(<<"Application-Name">>, JObj)
          ,wh_json:get_value(<<"Application-Response">>, JObj)
         }
    of
        {<<"bridge">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"park">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, {'set_auth_doc_id', CallId});
        _ -> 'ok'
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

-spec originate_park(state()) -> 'ok'.
originate_park(State) ->
    wapi_offnet_resource:publish_req(create_request(State)).

-spec create_request(state()) -> wh_proplist().
create_request(#state{account_id=AccountId
                      ,customer_number=ToDID
                      ,account_cid=AccountCID
                      ,queue=Q
                     }) ->
    CCVs = [{<<"Account-ID">>, AccountId}],
    [{<<"Application-Name">>, <<"park">>}
     ,{<<"Resource-Type">>, <<"originate">>}
     ,{<<"Originate-Immediate">>, 'true'}
     ,{<<"To-DID">>, ToDID}
     ,{<<"Outbound-Caller-ID-Number">>, AccountCID}
     ,{<<"Progress-Timeout">>, 12}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
     ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
     | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
    ].

-spec handle_originate_ready(wh_json:object(), proplist()) -> 'ok'.
handle_originate_ready(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case wh_util:get_event_type(JObj) of
        {<<"dialplan">>, <<"originate_ready">>} ->
            Q = wh_json:get_value(<<"Server-ID">>, JObj),
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
            Call = whapps_call:set_control_queue(CtrlQ, whapps_call:from_route_req(JObj)),
            whapps_call:cache(Call, ?APP_NAME),
            Prop = [{<<"Call-ID">>, CallId}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(gen_listener:queue_name(Srv), ?APP_NAME, ?APP_VERSION)
                   ],
            gen_listener:cast(Srv, {'offnet_ctl_queue', CtrlQ}),
            gen_listener:add_binding(Srv, {'call', ?MK_CALL_BINDING(CallId)}),
            wapi_dialplan:publish_originate_execute(Q, Prop);
        _ -> 'ok'
    end.

-spec hangup_parked_call(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
hangup_parked_call(ParkedCallId, Q, CtrlQ) ->
    Hangup = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Call-ID">>, ParkedCallId}
              | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
             ],
    wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hangup)).

-spec bridge_to_final_destination(ne_binary(), ne_binary(), state()) -> pid().
bridge_to_final_destination(CallId, ToDID, #state{queue=Q
                                                  ,offnet_ctl_q=CtrlQ
                                                  ,account_id=AccountId
                                                  ,account_cid=AccountCID
                                                  ,auth_doc_id=AccountDocId
                                                 }) ->
    Req = cccp_util:build_bridge_request(CallId, ToDID, Q, CtrlQ, AccountId, AccountCID),
    wapi_offnet_resource:publish_req(Req),
    wh_util:spawn('cccp_util', 'store_last_dialed', [ToDID, AccountDocId]).
