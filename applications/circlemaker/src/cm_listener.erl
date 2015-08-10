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
-export([handle_hangup_by_session_timeout/1]).

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

-define(ETS_SESSION_TIMEOUT, 'cm_session_timeout').

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
    % TODO: Add validation
    % 'true' = wapi_aaa:accounting_req_v(JObj),
    lager:debug("cm_listener handled accounting request ~p", [JObj]),
    AccountId = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
                    'undefined' ->
                        % Additional check for FMC numbers
                        Request = wh_json:get_value(<<"Request">>, JObj),
                        [Num|_] = binary:split(Request, <<"@">>),
                        {'ok', FMCJObjs} = couch_mgr:get_all_results(?WH_FMC_DB, <<"fmc_devices/crossbar_listing">>),
                        FMCValues = [wh_json:get_value(<<"value">>, FMCJObj) || FMCJObj <- FMCJObjs],
                        ResultedFMCValues = [FMCValue || FMCValue <- FMCValues
                            ,wnm_util:normalize_number(wh_json:get_value(<<"a_number">>, FMCValue))
                                =:= wnm_util:normalize_number(Num)],
                        FMCHeader = whapps_config:get(<<"fmc">>, <<"x_fmc_header">>),
                        IsFMCCall = wh_json:get_value([<<"Custom-SIP-Headers">>, FMCHeader], JObj) =/= 'undefined',
                        case length(ResultedFMCValues) > 0 andalso IsFMCCall of
                            'true' ->
                                [ResultedFMCValue|_] = ResultedFMCValues,
                                wh_json:get_value(<<"account_id">>, ResultedFMCValue);
                            'false' ->
                                'undefined'
                        end;
                    AccId ->
                        AccId
                end,
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>),
    NasAddress = wh_json:get_value(<<"nas_address">>, AaaDoc),
    case whapps_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_CREATE">>} ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            maybe_start_session_timer(AccountId, CallId),
            JObj1 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Start">>}
                                        ,{<<"Acct-Delay-Time">>, 0}
                                        ,{<<"NAS-IP-Address">>, NasAddress}]
                                        ,JObj),
            cm_pool_mgr:do_request(JObj1);
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            maybe_cancel_session_timer(AccountId, CallId),
            JObj1 = wh_json:set_values([{<<"Acct-Status-Type">>, <<"Stop">>}
                                        ,{<<"Acct-Delay-Time">>, 0}
                                        ,{<<"NAS-IP-Address">>, NasAddress}]
                                        ,JObj),
            cm_pool_mgr:do_request(JObj1)
    end.

maybe_start_session_timer(AccountId, CallId) ->
    case cm_util:get_session_timeout(AccountId) of
        'undefined' -> 'ok';
        SessionTimeout ->
            {'ok', TRef} = timer:apply_after(SessionTimeout, ?MODULE, 'handle_hangup_by_session_timeout', [CallId]),
            ets:insert(?ETS_SESSION_TIMEOUT, {CallId, AccountId, TRef})
    end.

handle_hangup_by_session_timeout(CallId) ->
    whapps_call_command:hangup(CallId),
    ets:delete(?ETS_SESSION_TIMEOUT, CallId).

maybe_cancel_session_timer(AccountId, CallId) ->
    case ets:lookup(?ETS_SESSION_TIMEOUT, CallId) of
        [] -> 'ok';
        [{CallId, AccountId, TRef}] ->
            timer:cancel(TRef),
            ets:delete(?ETS_SESSION_TIMEOUT, CallId)
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
            {'ok', AccountDoc} = couch_mgr:open_cache_doc(<<"account">>, AccountId),
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
    ets:new(?ETS_SESSION_TIMEOUT, [named_table, {keypos, 1}, protected]),
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
    ets:delete(?ETS_SESSION_TIMEOUT),
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
