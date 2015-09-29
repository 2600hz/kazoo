%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is worker pool manager. It is used to create workers for a requests from cm_listener.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_pool_mgr).
-behaviour(gen_server).

-include("circlemaker.hrl").

-type authn_response() :: {'ok', 'aaa_mode_off'} | {'ok', tuple()}.

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,start_link/0
         ,send_authn_response/4
         ,send_accounting_response/4
         ,send_custom_response/4
         ,do_request/1
         ,maybe_session_timeout/2
         ,maybe_interim_update/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Makes an AAA request
%% @end
%%--------------------------------------------------------------------
-spec do_request(wh_json:object()) -> 'ok'.
do_request(Request) ->
    gen_server:cast(?MODULE, {'request', Request}).

%%--------------------------------------------------------------------
%% @doc
%% Handler for a response message for AuthN
%% @end
%%--------------------------------------------------------------------
-spec send_authn_response(pid(), authn_response(), wh_json:object(), pid()) -> 'ok'.
send_authn_response(SenderPid, Response, JObj, Self) ->
    lager:debug("Response ~p is prepared to send to worker ~p", [Response, Self]),
    gen_server:cast(SenderPid, {'response', Response, JObj, Self}).

%%--------------------------------------------------------------------
%% @doc
%% Handler for a custom response message
%% @end
%%--------------------------------------------------------------------
-spec send_custom_response(pid(), authn_response(), wh_json:object(), pid()) -> 'ok'.
send_custom_response(SenderPid, Response, JObj, Self) ->
    lager:debug("Response ~p is prepared to send to worker ~p", [Response, Self]),
    gen_server:cast(SenderPid, {'response', Response, JObj, Self}).

%%--------------------------------------------------------------------
%% @doc
%% Handler for a response message for Accounting
%% @end
%%--------------------------------------------------------------------
-spec send_accounting_response(pid(), authn_response(), wh_json:object(), pid()) -> 'ok'.
send_accounting_response(SenderPid, Response, JObj, Self) ->
    lager:debug("Response ~p is prepared to send to worker ~p", [Response, Self]),
    gen_server:cast(SenderPid, {'accounting_response', Response, JObj, Self}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', tuple()}.
init([]) ->
    lager:debug("Pool manager started"),
    {'ok', []}.

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
    {'noreply', State}.

% pattern for {ok,{{radius_request,0,accept,[{{attribute,{11000,5},string,"Is-Authorized",no},<<"true">>}],<<"zebra">>,<<93,69,40,2,251,41,84,90,169,220,196,35,151,33,26,168>>,false,<<>>},<<"caf3741e12264f786a676b35b853c5c3">>}}
extract_kv_from_attr_list({{_, _, _, Name, _}, Val}) -> {Name, Val};
% pattern for {ok,{{radius_request,0,accept,[{{attribute,{11000,5},string,"Is-Authorized",no},<<"true">>}],<<"zebra">>,<<93,69,40,2,251,41,84,90,169,220,196,35,151,33,26,168>>,false,<<>>},<<"caf3741e12264f786a676b35b853c5c3">>}}
extract_kv_from_attr_list({Name, Val}) -> {Name, Val}.

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
handle_cast({'request', JObj}, State) ->
    wh_util:put_callid(JObj),
    dist_workers(JObj),
    {'noreply', State};
handle_cast({'response', Response, JObj, Worker}, State) ->
    wh_util:put_callid(JObj),
    lager:debug("response message is ~p", [Response]),
    poolboy:checkin(?WORKER_POOL, Worker),
    {AaaResult, AttributeList, AccountId} = case Response of
                {'ok', {{'radius_request', _, 'accept', ParamList, _, _, _, _}, AuthzAccountId}} ->
                    AttrList = [extract_kv_from_attr_list(ParamItem) || ParamItem <- ParamList],
                    {<<"accept">>, AttrList, AuthzAccountId};
                {'ok', {'aaa_mode_on_and_no_servers', AuthzAccountId}} ->
                    {<<"accept">>, [], AuthzAccountId};
                {'ok', {{'radius_request', _, 'reject', ParamList, _, _, _, _}, AuthzAccountId}} ->
                    AttrList = [extract_kv_from_attr_list(ParamItem) || ParamItem <- ParamList],
                    {<<"reject">>, AttrList, AuthzAccountId};
                _ ->
                    {<<"reject">>, [], wh_json:get_value(<<"Account-ID">>, JObj)}
            end,
    lager:debug("AttributeList is: ~p", [AttributeList]),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>),
    AttributeList1 = case cm_util:determine_aaa_request_type(JObj) of
                         'authz' = RequestType ->
                             lager:debug("Operation is authz"),
                             lager:debug("Orig JObj on Response is ~p", [JObj]),
                             OrigAccountId = wh_json:get_value(<<"Account-ID">>, JObj),
                             maybe_session_timeout(AttributeList, OrigAccountId),
                             maybe_interim_update(AttributeList, OrigAccountId),
                             cm_util:maybe_translate_avps_into_kv(AttributeList, AaaDoc, RequestType);
                         'custom' = RequestType ->
                             lager:debug("Operation is custom"),
                             lager:debug("Orig JObj on Response is ~p", [JObj]),
                             cm_util:maybe_translate_avps_into_kv(AttributeList, AaaDoc, RequestType);
                         'authn' ->
                             lager:debug("Operation is authn"),
                             AttributeList
                     end,
    lager:debug("Resulted AttributeList1 is: ~p", [AttributeList1]),
    IsAuthorized = props:get_value(<<"Is-Authorized">>, AttributeList1, <<"false">>), % TODO: need to optimize
    case cm_util:determine_aaa_request_type(JObj) of
        'custom' ->
            CustomMsg = wh_json:get_value(<<"Custom-Msg">>, JObj),
            NewCustomMsg = wh_json:set_values(AttributeList1, CustomMsg),
            WrappedJObj = wh_json:set_value(<<"Custom-Msg">>, NewCustomMsg, JObj),
            JObj1 = wh_json:set_values([{<<"App-Name">>, ?APP_NAME}
                                        ,{<<"App-Version">>, ?APP_VERSION}]
                                        ,WrappedJObj),
            {EventCategory, EventName} = wapi_aaa:custom_resp_event_type(),
            JObj2 = wh_json:set_values([{<<"Event-Category">>, EventCategory}
                                        ,{<<"Event-Name">>, EventName}]
                                        ,JObj1),
            lager:debug("Resulted custom response is ~p", [JObj2]),
            Queue = wh_json:get_value(<<"Server-ID">>, JObj2),
            wapi_aaa:publish_custom_resp(Queue, JObj2);
        _ ->
            JObj3 = wh_json:set_values(AttributeList1, JObj),
            Password = wh_json:get_value(<<"User-Password">>, JObj3),
            JObj1 = wh_json:set_values([{<<"AAA-Result">>, AaaResult}
                                        ,{<<"Is-Authorized">>, IsAuthorized}
                                        ,{<<"Auth-Password">>, Password}
                                        ,{<<"App-Name">>, ?APP_NAME}
                                        ,{<<"App-Version">>, ?APP_VERSION}
                                        ,{[<<"Custom-Auth-Vars">>, <<"App-Name">>], ?APP_NAME}
                                        ,{[<<"Custom-Auth-Vars">>, <<"App-Version">>], ?APP_VERSION}]
                                        ,JObj3),
            case wh_json:get_value(<<"Event-Category">>, JObj) of
                <<"authz">> ->
                    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
                    JObj2 = wh_json:set_value(<<"Event-Name">>, <<"authz.broadcast.resp">>, JObj1),
                    lager:debug("Authz response prepared: ~p", [JObj2]),
                    wapi_authz:publish_authz_resp(Queue, JObj2);
                    % send response to the registrar_listener queue
                _ ->
                    Queue = wh_json:get_value(<<"Response-Queue">>, JObj),
                    JObj2 = wh_json:set_value(<<"Event-Name">>, <<"aaa_authn_resp">>, JObj1),
                    lager:debug("Other response prepared: ~p", [JObj2]),
                    wapi_aaa:publish_resp(Queue, JObj2)
            end
    end,
    {'noreply', State};
handle_cast({'accounting_response', Response, JObj, Worker}, State) ->
    wh_util:put_callid(JObj),
    lager:debug("accounting response message is ~p", [Response]),
    OtherLegCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    % cleanup
    IsChannelDestroy = whapps_util:get_event_type(JObj) =:= {<<"call_event">>, <<"CHANNEL_DESTROY">>},
    IsChannelDestroy andalso
        begin
            cm_util:ets_cleanup_other_and_orig_legs(OtherLegCallId, CallId),
            cm_util:cleanup_channel_type(CallId)
        end,
    % return worker to the pool
    poolboy:checkin(?WORKER_POOL, Worker),
    {'noreply', State};
handle_cast(Message, State) ->
    lager:debug("unexpected message is ~p", [Message]),
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
handle_info(Info, State) ->
    lager:debug("unexpected info message is ~p", [Info]),
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
terminate(Reason, _State) ->
    lager:debug("Circlemaker worker terminating: ~p", [Reason]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts new worker for processing a request
%% @end
%%--------------------------------------------------------------------
-spec dist_workers(wh_json:object()) -> any().
dist_workers(JObj) ->
    wh_util:put_callid(JObj),
    lager:debug("Trying to start new worker..."),
    case catch poolboy:checkout(?WORKER_POOL) of
        Worker when is_pid(Worker) ->
            lager:debug("Worker started sucessfully"),
            cm_worker:send_req(Worker, JObj);
        Else ->
            % TODO: need to send message to self for 'denied' response
            lager:error("Failed to start a worker. Reason is ~p", [Else])
    end.

-spec maybe_session_timeout(proplist(), ne_binary()) -> any().
maybe_session_timeout(AttributeList, AccountId) ->
    % because this auth operation is account-related, so we shouldn't use
    % account hierarchy to find suitable Session-Timeout value.
    case props:get_integer_value(<<"Session-Timeout">>, AttributeList) of
        'undefined' ->
            cm_util:clean_session_timeout(AccountId);
        SessionTimeout ->
            cm_util:put_session_timeout(SessionTimeout, AccountId)
    end.

-spec maybe_interim_update(proplist(), ne_binary()) -> any().
maybe_interim_update(AttributeList, AccountId) ->
    % because this auth operation is account-related, so we shouldn't use
    % account hierarchy to find suitable Acct-Interim-Interval value.
    case props:get_integer_value(<<"Acct-Interim-Interval">>, AttributeList) of
        'undefined' ->
            cm_util:clean_interim_update(AccountId);
        SessionTimeout ->
            cm_util:put_interim_update(SessionTimeout, AccountId)
    end.
