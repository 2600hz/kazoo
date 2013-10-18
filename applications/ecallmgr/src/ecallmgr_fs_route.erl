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
    gen_server:cast(self(), 'bind_to_dialplan'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_dialplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'dialplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish route bindings: ~p", [Reason]),
            {'stop', Reason, State};
        'timeout' ->
            lager:critical("unable to establish route bindings: timeout", []),
            {'stop', 'timeout', State}
    end;
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
handle_info({'fetch', _Section, _Something, _Key, _Value, Id, ['undefined' | _Data]}, #state{node=Node}=State) ->
    lager:warning("fetch unknown section from ~s: ~p So: ~p, K: ~p V: ~p Id: ~s"
                  ,[Node, _Section, _Something, _Key, _Value, Id]),
    {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, Id, 'dialplan', Resp),
    {'noreply', State};
handle_info({'fetch', 'dialplan', _Tag, _Key, _Value, FSId, [CallId | FSData]}, #state{node=Node}=State) ->
    case {props:get_value(<<"Event-Name">>, FSData), props:get_value(<<"Caller-Context">>, FSData)} of
        {<<"REQUEST_PARAMS">>, _} ->
            %% TODO: move this to a supervisor somewhere
            lager:info("processing dialplan fetch request ~s (call ~s) from ~s", [FSId, CallId, Node]),
            spawn(?MODULE, 'process_route_req', [Node, FSId, CallId, FSData]),
            {'noreply', State, 'hibernate'};
        {_Other, _Context} ->
            lager:debug("ignoring event ~s in context ~s from ~s", [_Other, _Context, Node]),
            {'ok', Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, FSId, 'dialplan', Resp),
            {'noreply', State, 'hibernate'}
    end;
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
% terminate. It should be the opposite of Module:init/1 and do any
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
process_route_req(Node, FetchId, CallId, Props) ->
    put('callid', CallId),
    case wh_util:is_true(props:get_value(<<"variable_recovered">>, Props)) of
        'false' -> search_for_route(Node, FetchId, CallId, Props);
        'true' ->
            lager:debug("recovered channel already exists on ~s, park it", [Node]),
            JObj = wh_json:from_list([{<<"Routes">>, []}
                                      ,{<<"Method">>, <<"park">>}
                                      ,{<<"Context">>, hunt_context(Props)}
                                     ]),
            reply_affirmative(Node, FetchId, CallId, JObj)
    end.

search_for_route(Node, FetchId, CallId, Props) ->
    _ = spawn('ecallmgr_fs_authz', 'authorize', [Props, CallId, Node]),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,route_req(CallId, FetchId, Props, Node)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                  ,2500
                                 ),
    case ReqResp of
        {'error', _R} -> 
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', JObj} ->
            'true' = wapi_route:resp_v(JObj),
            J = wh_json:set_value(<<"Context">>, hunt_context(Props), JObj),
            maybe_wait_for_authz(J, Node, FetchId, CallId)
    end.

hunt_context(Props) ->
    props:get_value(<<"Hunt-Context">>, Props, ?WHISTLE_CONTEXT).

 maybe_wait_for_authz(JObj, Node, FetchId, CallId) ->
    case wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, 'false')) 
        andalso wh_json:get_value(<<"Method">>, JObj) =/= <<"error">>
    of
        'true' -> wait_for_authz(JObj, Node, FetchId, CallId);
        'false' -> reply_affirmative(Node, FetchId, CallId, JObj)
    end.

wait_for_authz(JObj, Node, FetchId, CallId) ->
    case wh_cache:wait_for_key_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)) of
        {'ok', {'true', AuthzCCVs}} ->
            _ = wh_cache:erase_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId)),
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),  
            J = wh_json:set_value(<<"Custom-Channel-Vars">>
                                  ,wh_json:merge_jobjs(CCVs, AuthzCCVs)
                                  ,JObj),
            reply_affirmative(Node, FetchId, CallId, J);
        _Else -> reply_forbidden(Node, FetchId)
    end.

%% Reply with a 402 for unauthzed calls
-spec reply_forbidden(atom(), ne_binary()) -> 'ok'.
reply_forbidden(Node, FetchId) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml([{<<"Method">>, <<"error">>}
                                                  ,{<<"Route-Error-Code">>, <<"403">>}
                                                  ,{<<"Route-Error-Message">>, <<"Incoming call barred">>}
                                                 ]),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, 'dialplan', iolist_to_binary(XML), 3000) of
        'ok' -> lager:info("node ~s accepted route response for request ~s", [Node, FetchId]);
        {'error', Reason} -> lager:debug("node ~s rejected our route unauthz: ~p", [Node, Reason])
    end.

-spec reply_affirmative(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
reply_affirmative(Node, FetchId, CallId, JObj) ->
    lager:info("received affirmative route response for request ~s", [FetchId]),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml(JObj),
    lager:debug("sending XML to ~s: ~s", [Node, XML]),
    case freeswitch:fetch_reply(Node, FetchId, 'dialplan', iolist_to_binary(XML), 3000) of
        {'error', _Reason} -> lager:debug("node ~s rejected our route response: ~p", [Node, _Reason]);
        'ok' ->
            lager:info("node ~s accepted route response for request ~s", [Node, FetchId]),
            maybe_start_call_handling(Node, FetchId, CallId, JObj)
    end.

-spec maybe_start_call_handling(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_start_call_handling(Node, FetchId, CallId, JObj) ->
    case wh_json:get_value(<<"Method">>, JObj) of
        <<"error">> -> 'ok';
        _Else -> start_call_handling(Node, FetchId, CallId, JObj)
    end.

-spec start_call_handling(atom(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_call_handling(Node, FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    _ = ecallmgr_call_sup:start_event_process(Node, CallId),
    _ = ecallmgr_call_sup:start_control_process(Node, CallId, FetchId, ServerQ, CCVs),
    ecallmgr_util:set(Node, CallId, wh_json:to_proplist(CCVs)).   

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), atom()) -> wh_proplist().
route_req(CallId, FetchId, Props, Node) ->
    CCVs = [{<<"Fetch-ID">>, FetchId}],
    [{<<"Msg-ID">>, FetchId}
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
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props, CCVs))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
