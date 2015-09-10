%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is listener for AMQP events
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_listener).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
         ,handle_authn_req/2
         ,handle_authz_req/2
         ,handle_accounting_req/2]).
-export([handle_hangup_by_session_timeout/1, handle_interim_update/1, find_channel/2, find_channel/3]).

-include("circlemaker.hrl").
-include_lib("rabbitmq_client/include/amqp_client.hrl").

-record(state, {}).

-define(RESPONDERS, [{{?MODULE, 'handle_authn_req'}, [wapi_aaa:req_event_type()]}
                     ,{{?MODULE, 'handle_authz_req'}, [{<<"authz">>, <<"authz_req">>}]}
                     ,{{?MODULE, 'handle_accounting_req'}, [{<<"call_event">>, <<"*">>}]}
                    ]).
-define(BINDINGS, [{'aaa', []}
                   ,{'authz', []}
                   ,{'self', []}
                   ,{'call', [{'restrict_to', ['CHANNEL_CREATE', 'CHANNEL_DESTROY']}]}
                  ]).
-define(QUEUE_NAME, <<"circlemaker_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(SERVER, ?MODULE).

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
                                                          ,{'responders', ?RESPONDERS}
                                                          ,{'queue_name', ?QUEUE_NAME}
                                                          ,{'queue_options', ?QUEUE_OPTIONS}
                                                          ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle AuthN requests from another process.
%% @end
%%--------------------------------------------------------------------
-spec handle_authn_req(wh_json:object(), wh_proplist()) -> any().
handle_authn_req(JObj, _Props) ->
    wh_util:put_callid(JObj),
    lager:debug("cm_listener handled authn request ~p", [JObj]),
    'true' = wapi_aaa:req_v(JObj),
    cm_pool_mgr:do_request(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle AuthZ requests from another process.
%% @end
%%--------------------------------------------------------------------
-spec handle_authz_req(wh_json:object(), wh_proplist()) -> any().
handle_authz_req(JObj, _Props) ->
    wh_util:put_callid(JObj),
    'true' = wapi_authz:authz_req_v(JObj),
    lager:debug("cm_listener handled authz request ~p", [JObj]),
    case wh_json:get_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Disabled">>], JObj) of
        <<"true">> ->
            lager:debug("Authz disabled, no processing"),
            'ok';
        _ ->
            case wh_json:get_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Granted">>], JObj) of
                <<"true">> ->
                    lager:debug("Authz granted. Request bypassed."),
                    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
                    JObj1 = wh_json:set_values([{<<"Event-Name">>, <<"authz.broadcast.resp">>}
                        ,{<<"Is-Authorized">>, <<"true">>}]
                        ,JObj),
                    wapi_authz:publish_authz_resp(Queue, JObj1);
                _ ->
                    maybe_processing_authz(JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle AuthZ requests from another process.
%% @end
%%--------------------------------------------------------------------
-spec handle_accounting_req(wh_json:object(), wh_proplist()) -> any().
handle_accounting_req(JObj, _Props) ->
    wh_util:put_callid(JObj),
    % TODO: Add validation
    % 'true' = wapi_aaa:accounting_req_v(JObj),
    lager:debug("cm_listener handled accounting request ~p", [JObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    % check that is inbound outer leg which has no Account-ID
    IsInboundOuterLegOnChannelCreate = whapps_util:get_event_type(JObj) =:= {<<"call_event">>, <<"CHANNEL_CREATE">>}
        andalso (wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Resource-ID">>], JObj) =:= 'undefined')
        andalso (wh_json:get_value(<<"Call-Direction">>, JObj) =:= <<"inbound">>),
    IsInboundOuterLegOnChannelDestroy = whapps_util:get_event_type(JObj) =:= {<<"call_event">>, <<"CHANNEL_DESTROY">>},
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' when IsInboundOuterLegOnChannelCreate ->
            lager:debug("Trying to make 'start' accounting operation for outer inbound leg. The operation should be delayed."),
            % store delayed accounting call
            ets:insert(?ETS_DELAY_ACCOUNTING, {CallId, JObj});
        'undefined' when IsInboundOuterLegOnChannelDestroy ->
            % this situation can be if outer inbound leg wasn't authorized so ETS doesn't any account
            % as result we shouldn't do 'stop' accounting operation
            case ets:lookup(?ETS_DELAY_ACCOUNTING, CallId) of
                [] ->
                    lager:debug("Nothing to clean");
                [{CallId, LostJObj}] ->
                    % outbound channel wasn't created, so there is no channel creation was for CallId
                    % so we should bypass this accounting "stop" request
                    lager:debug("Cleanup of the delayed operation for outer inbound leg: ~p", [LostJObj]),
                    ets:delete(?ETS_DELAY_ACCOUNTING, CallId)
            end;
        AccountId ->
            {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
            AccountName = wh_json:get_value(<<"name">>, AccountDoc),
            {'ok', AaaDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>),
            NasAddress = wh_json:get_value(<<"nas_address">>, AaaDoc),
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_CREATE">>} ->
                    OtherLegCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
                    % check if we have delayed accounting leg for this call
                    case ets:lookup(?ETS_DELAY_ACCOUNTING, OtherLegCallId) of
                        [] -> 'ok';
                        [{OtherLegCallId, JObjDelayed}] ->
                            lager:debug("Found corresponding delayed operation for outer inbound leg accounting. The operation should be retrieved and executed."),
                            ets:delete(?ETS_DELAY_ACCOUNTING, OtherLegCallId),
                            OriginatorType = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Originator-Type">>], JObj),
                            JObjDelayed1 = wh_json:set_values([{[<<"Custom-Channel-Vars">>, <<"Account-ID">>], AccountId}
                                                               ,{[<<"Custom-Channel-Vars">>, <<"Account-Name">>], AccountName}
                                                               ,{[<<"Custom-Channel-Vars">>, <<"Originator-Type">>], OriginatorType}]
                                                               ,JObjDelayed),
                            % delayed leg info was found
                            CallIdDelayed = wh_json:get_value(<<"Call-ID">>, JObjDelayed1),
                            maybe_start_interim_update_timer(AccountId, CallIdDelayed),
                            JObjDelayed2 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Start">>}
                                                               ,{<<"Acct-Delay-Time">>, 0}
                                                               ,{<<"NAS-IP-Address">>, NasAddress}
                                                              ], JObjDelayed1),
                            % send delayed accounting start
                            lager:debug("Delayed operation for outer inbound leg is ~p", [JObjDelayed2]),
                            cm_pool_mgr:do_request(JObjDelayed2)
                    end,
                    BridgeId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], JObj),
                    maybe_start_session_timer(AccountId, BridgeId),
                    maybe_start_interim_update_timer(AccountId, CallId),
                    JObj1 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Start">>}
                                                ,{<<"Acct-Delay-Time">>, 0}
                                                ,{<<"NAS-IP-Address">>, NasAddress}
                                                ,{[<<"Custom-Channel-Vars">>, <<"Account-Name">>], AccountName}
                                               ], JObj),
                    % send accounting start
                    lager:debug("Sending accounting start operation: ~p", [JObj1]),
                    cm_pool_mgr:do_request(JObj1);
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    case ets:lookup(?ETS_DELAY_ACCOUNTING, CallId) of
                        [] ->
                            BridgeId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], JObj),
                            maybe_cancel_session_timer(AccountId, BridgeId),
                            maybe_cancel_interim_update_timer(AccountId, CallId),
                            JObj1 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Stop">>}
                                                        ,{<<"Acct-Delay-Time">>, 0}
                                                        ,{<<"NAS-IP-Address">>, NasAddress}
                                                        ,{[<<"Custom-Channel-Vars">>, <<"Account-Name">>], AccountName}
                                                       ], JObj),
                            % send accounting stop
                            lager:debug("Sending accounting stop operation: ~p", [JObj1]),
                            cm_pool_mgr:do_request(JObj1);
                        [{CallId, LostJObj}] ->
                            % outbound channel wasn't created, so there is no channel creation was for CallId
                            % so we should bypass this accounting "stop" request
                            lager:debug("Cleanup of the delayed operation for outer inbound leg: ~p", [LostJObj]),
                            ets:delete(?ETS_DELAY_ACCOUNTING, CallId)
                    end;
                {<<"call_event">>, <<"channel_fs_status_resp">>} ->
                    % Interim-Update
                    JObj1 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Interim-Update">>}
                                                ,{<<"Acct-Delay-Time">>, 0}
                                                ,{<<"NAS-IP-Address">>, NasAddress}
                                               ], JObj),
                    % send accounting stop
                    lager:debug("Sending Interim-Update as accounting operation: ~p", [JObj1]),
                    cm_pool_mgr:do_request(JObj1)
            end
    end.

maybe_start_session_timer(AccountId, CallId) ->
    case cm_util:get_session_timeout(AccountId) of
        'undefined' -> 'ok';
        SessionTimeout ->
            lager:debug("Applying session timeout timer for Call ID ~p of the account ~p", [CallId, AccountId]),
            {'ok', TRef} = timer:apply_after(SessionTimeout * ?MILLISECONDS_IN_SECOND, ?MODULE, 'handle_hangup_by_session_timeout', [CallId]),
            ets:insert(?ETS_SESSION_TIMEOUT, {CallId, AccountId, TRef})
    end.

handle_hangup_by_session_timeout(CallId) ->
    wh_util:put_callid(CallId),
    lager:debug("Applying call hangup because of session timeout for Call ID ~p", [CallId]),
    cm_util:hangup_call(CallId),
    ets:delete(?ETS_SESSION_TIMEOUT, CallId).

maybe_cancel_session_timer(AccountId, CallId) ->
    case ets:lookup(?ETS_SESSION_TIMEOUT, CallId) of
        [] -> 'ok';
        [{CallId, AccountId, TRef}] ->
            lager:debug("Cancelling session timeout for CallID ~p of the account ~p", [CallId, AccountId]),
            timer:cancel(TRef),
            ets:delete(?ETS_SESSION_TIMEOUT, CallId)
    end.

maybe_start_interim_update_timer(AccountId, CallId) ->
    case cm_util:get_interim_update(AccountId) of
        'undefined' -> 'ok';
        Interval ->
            lager:debug("Applying interim update timer for CallID ~p of the account ~p", [CallId, AccountId]),
            gen_listener:cast(?SERVER, {'interim_update', CallId, Interval, AccountId})
    end.

handle_interim_update(CallId) ->
    wh_util:put_callid(CallId),
    case whapps_call_command:fs_channel_status(CallId) of
        {'ok', ChannelStatus} ->
            % emulation of Accounting Request operation
            lager:debug("Sending Interim Update"),
            handle_accounting_req(ChannelStatus, []);
        {'error', Error} ->
            lager:debug("Error on sending Interim Update: ~p", [Error])
    end.

-spec find_channel(ne_binary(), wh_json:objects()) -> api_object().
find_channel(_CallId, []) ->
    [];
find_channel(CallId, StatusJObjs) ->
    find_channel(CallId, StatusJObjs, []).

-spec find_channel(ne_binary(), wh_json:objects(), wh_json:objects()) -> api_object().
find_channel(_CallId, [], Acc) -> Acc;
find_channel(CallId, [StatusJObj|JObjs], Acc) ->
    lager:debug("Next checked StatusJObj is ~p", [StatusJObj]),
    Channel = wh_json:get_value([<<"Channels">>, CallId], StatusJObj),
    case wh_json:get_value(<<"Call-ID">>, Channel) of
        CallId -> find_channel(CallId, JObjs, [Channel | Acc]);
        _AnotherCallId -> find_channel(CallId, JObjs, Acc)
    end.

maybe_cancel_interim_update_timer(AccountId, CallId) ->
    case ets:lookup(?ETS_INTERIM_UPDATE, CallId) of
        [] -> 'ok';
        [{CallId, AccountId, TRef}] ->
            lager:debug("Cancelling interim update timeout for CallID ~p of the account ~p", [CallId, AccountId]),
            timer:cancel(TRef),
            ets:delete(?ETS_INTERIM_UPDATE, CallId)
    end.

maybe_processing_authz(JObj) ->
    % add neccessary set of fields
    case wapi_authz:maybe_determine_account_id(wapi_authz:from_jobj(JObj)) of
        {'ok', AccountId} ->
            lager:debug("Account ID found. Value is ~p", [AccountId]),
            {'ok', AaaDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>),
            JObj1 = case {wh_json:get_value(<<"User-Name">>, JObj), wh_json:get_value(<<"User-Password">>, JObj)} of
                        {'undefined', 'undefined'} ->
                            lager:debug("No User-Name and User-Password AVPs in request. "
                            "Insert it from account fields authz_username and authz_password"),
                            Username = wh_json:get_value(<<"authz_username">>, AaaDoc),
                            Password = wh_json:get_value(<<"authz_password">>, AaaDoc),
                            wh_json:set_values([{<<"Auth-User">>, Username}
                                ,{<<"Auth-Password">>, Password}
                                ,{<<"User-Name">>, Username}
                                ,{<<"User-Password">>, Password}
                                ,{<<"Account-ID">>, AccountId}
                            ], JObj);
                        {Username, Password} ->
                            lager:debug("The User-Name and User-Password AVPs were found in request"),
                            wh_json:set_values([{<<"Auth-User">>, Username}
                                ,{<<"Auth-Password">>, Password}
                                ,{<<"User-Name">>, Username}
                                ,{<<"User-Password">>, Password}
                                ,{<<"Account-ID">>, AccountId}
                            ], JObj)
                    end,
            {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
            AccountName = wh_json:get_value(<<"name">>, AccountDoc),
            JObj2 = wh_json:set_value([<<"Custom-Auth-Vars">>, <<"Account-Name">>], AccountName, JObj1),
            cm_pool_mgr:do_request(JObj2);
        {'error', Error} ->
            lager:debug("Account ID not found. Error is ~p", [Error]),
            Queue = wh_json:get_value(<<"Response-Queue">>, JObj),
            JObj1 = wh_json:set_value(<<"Is-Authorized">>, <<"false">>, JObj),
            wapi_authz:publish_authz_resp(Queue, JObj1)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    % TODO: rework using ets_mgr
    ets:new(?ETS_SESSION_TIMEOUT, ['named_table', {'keypos', 1}, 'public']),
    ets:new(?ETS_INTERIM_UPDATE, ['named_table', {'keypos', 1}, 'public']),
    ets:new(?ETS_DELAY_ACCOUNTING, ['named_table', {'keypos', 1}, 'public']),
    {'ok', #state{}}.

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
handle_cast({'interim_update', CallId, Interval, AccountId}, State) ->
    {'ok', TRef} = timer:apply_interval(Interval * ?MILLISECONDS_IN_SECOND, ?MODULE, 'handle_interim_update', [CallId]),
    ets:insert(?ETS_INTERIM_UPDATE, {CallId, AccountId, TRef}),
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
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
terminate(_Reason, _State) ->
    ets:delete(?ETS_DELAY_ACCOUNTING),
    ets:delete(?ETS_SESSION_TIMEOUT),
    ets:delete(?ETS_INTERIM_UPDATE),
    lager:debug("listener terminating: ~p", [_Reason]).

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
