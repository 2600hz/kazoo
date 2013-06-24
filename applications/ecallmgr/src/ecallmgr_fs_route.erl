%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).

-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([process_route_req/4]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: wh_proplist()
               }).

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
start_link(Node) -> start_link(Node, []).

start_link(Node, Options) -> gen_server:start_link(?MODULE, [Node, Options], []).

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
init([Node, Options]) ->
    put('callid', Node),
    lager:info("starting new fs route listener for ~s", [Node]),
    case freeswitch:bind(Node, 'dialplan') of
        'ok' -> {'ok', #state{node=Node, options=Options}};
        {'error', Reason} ->
            lager:critical("unable to establish route bindings: ~p", [Reason]),
            {'stop', Reason};
        'timeout' ->
            lager:critical("unable to establish route bindings: timeout", []),
            {'stop', 'timeout'}
    end.

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
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info({'fetch', _Section, _Something, _Key, _Value, ID, ['undefined' | _Data]}, #state{node=Node}=State) ->
    lager:debug("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p ID: ~s", [Node, _Section, _Something, _Key, _Value, ID]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, ID, 'dialplan', Resp),
    {'noreply', State};
handle_info({'fetch', 'dialplan', _Tag, _Key, _Value, FSID, [CallID | FSData]}, #state{node=Node}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData), props:get_value(<<"Caller-Context">>, FSData)} of
        {<<"REQUEST_PARAMS">>, ?WHISTLE_CONTEXT} ->
            %% TODO: move this to a supervisor somewhere
            spawn(?MODULE, 'process_route_req', [Node, FSID, CallID, FSData]),
            {'noreply', State, 'hibernate'};
        {_Other, _Context} ->
            lager:debug("ignoring event ~s in context ~s from ~s", [_Other, _Context, Node]),
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, FSID, 'dialplan', Resp),
            {'noreply', State, 'hibernate'}
    end;
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
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
terminate(_Reason, #state{node=Node}) ->
    lager:info("route listener for ~s terminating: ~p", [Node, _Reason]).

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
-spec process_route_req(atom(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
process_route_req(Node, FSID, CallId, Props) ->
    put('callid', CallId),
    lager:info("processing dialplan fetch request ~s (call ~s) from ~s", [FSID, CallId, Node]),
    case wh_util:is_true(props:get_value(<<"variable_recovered">>, Props)) of
        'false' -> search_for_route(Node, FSID, CallId, Props);
        'true' ->
            lager:debug("recovered channel already exists on ~s, park it", [Node]),
            RespJObj = wh_json:from_list([{<<"Routes">>, []}
                                          ,{<<"Method">>, <<"park">>}
                                         ]),
            reply_affirmative(Node, FSID, CallId, RespJObj, Props)
    end.

search_for_route(Node, FSID, CallId, Props) ->
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,route_req(CallId, FSID, Props, Node)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} -> lager:info("did not receive route response: ~p", [_R]);
        {'ok', RespJObj} ->
            'true' = wapi_route:resp_v(RespJObj),
            AuthzEnabled = wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, 'false')),
            case AuthzEnabled andalso wh_cache:wait_for_key_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)) of
                {'ok', 'false'} -> reply_forbidden(Node, FSID);
                _Else -> reply_affirmative(Node, FSID, CallId, RespJObj, Props)
            end
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), ne_binary()) -> 'ok'.
reply_forbidden(Node, FSID) ->
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"error">>}
                                                ,{<<"Route-Error-Code">>, <<"402">>}
                                                ,{<<"Route-Error-Message">>, <<"Payment Required">>}
                                               ]),
    lager:info("sending forbidden XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FSID, 'dialplan', iolist_to_binary(XML), 3000) of
        'ok' -> lager:debug("node ~s accepted our route unauthz", [Node]);
        {'error', _Reason} -> lager:debug("node ~s rejected our route unauthz, ~p", [Node, _Reason]);
        'timeout' -> lager:error("received no reply from node ~s, timeout", [Node])
    end.

-spec reply_affirmative(atom(), ne_binary(), ne_binary(), wh_json:object(), wh_proplist()) -> 'ok'.
reply_affirmative(Node, FSID, CallId, RespJObj, Props) ->
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml(RespJObj),
    ServerQ = wh_json:get_value(<<"Server-ID">>, RespJObj),
    lager:info("sending affirmative XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FSID, 'dialplan', iolist_to_binary(XML), 3000) of
        'ok' ->
            lager:debug("node ~s accepted our route (authzed), starting control and events", [Node]),
            CCVs = update_custom_channel_vars(Node, CallId, RespJObj, Props),
            start_control_and_events(Node, CallId, ServerQ, CCVs);
        {'error', _Reason} -> lager:debug("node ~s rejected our route response, ~p", [Node, _Reason]);
        'timeout' -> lager:error("received no reply from node ~s, timeout", [Node])
    end.

-spec update_custom_channel_vars(atom(), ne_binary(), wh_json:object(), wh_proplist()) ->
                                        wh_json:object().
update_custom_channel_vars(Node, CallId, JObj, Props) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    maybe_add_billing_ccv(CCVs, CallId, Node, Props).

-spec maybe_add_billing_ccv(wh_json:object(), ne_binary(), atom(), wh_proplist()) -> wh_json:object().
maybe_add_billing_ccv(CCVs, CallId, Node, Props) ->
    case props:get_value(?GET_CCV(<<"Billing-ID">>), Props) of
        'undefined' ->
            BillingId = wh_util:rand_hex_binary(16),
            lager:debug("created new billing id ~s for channel ~s", [BillingId, CallId]),
            _ = ecallmgr_util:send_cmd(Node, CallId, <<"export">>, ?SET_CCV(<<"Billing-ID">>, BillingId)),
            Vars = wh_json:set_value(<<"Billing-ID">>, BillingId, CCVs),
            set_custom_channel_vars(Vars, Node, CallId);
        _Else ->
            lager:debug("channel ~s already has billing id ~s", [CallId, _Else]),
            set_custom_channel_vars(CCVs, Node, CallId)
    end.

-spec set_custom_channel_vars(wh_json:object(), atom(), ne_binary()) -> wh_json:object().
set_custom_channel_vars(CCVs, Node, CallId) ->
    _ = ecallmgr_util:set(Node, CallId, wh_json:to_proplist(CCVs)),
    CCVs.

-spec start_control_and_events(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_control_and_events(Node, CallId, SendTo, CCVs) ->
    BillingId = wh_json:get_value(<<"Billing-ID">>, CCVs),
    {'ok', CtlPid} = ecallmgr_call_sup:start_control_process(Node, CallId, BillingId),
    {'ok', _EvtPid} = ecallmgr_call_sup:start_event_process(Node, CallId),
    CtlQ = ecallmgr_call_control:queue_name(CtlPid),
    CtlProp = [{<<"Msg-ID">>, CallId}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Control-Queue">>, CtlQ}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               | wh_api:default_headers(CtlQ, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
              ],
    send_control_queue(SendTo, CtlProp).

-spec send_control_queue(ne_binary(), wh_proplist()) -> 'ok'.
send_control_queue(SendTo, CtlProp) ->
    lager:debug("sending route_win to ~s", [SendTo]),
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,CtlProp
                        ,fun(P) -> wapi_route:publish_win(SendTo, P) end
                       ).

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), atom()) -> wh_proplist().
route_req(CallId, FSID, Props, Node) ->
    [{<<"Msg-ID">>, FSID}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, Props,
                                             props:get_value(<<"Caller-Caller-ID-Name">>, Props, <<"Unknown">>))}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, Props,
                                               props:get_value(<<"Caller-Caller-ID-Number">>, Props, <<"0000000000">>))}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"From-Network-Addr">>, props:get_value(<<"variable_sip_h_X-AUTH-IP">>, Props
                                                ,props:get_value(<<"variable_sip_received_ip">>, Props))}
     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
