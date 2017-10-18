%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Handle updating devices and emails about voicemails
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(notify_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("notify.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{'notify_voicemail_to_email', [{<<"notification">>, <<"voicemail_new">>}]}
                    ,{'notify_voicemail_full', [{<<"notification">>, <<"voicemail_full">>}]}
                    ,{'notify_fax_inbound_to_email', [{<<"notification">>, <<"inbound_fax">>}]}
                    ,{'notify_fax_outbound_to_email', [{<<"notification">>, <<"outbound_fax">>}]}
                    ,{'notify_fax_inbound_error_to_email', [{<<"notification">>, <<"inbound_fax_error">>}]}
                    ,{'notify_fax_outbound_error_to_email', [{<<"notification">>, <<"outbound_fax_error">>}]}
                    ,{'notify_deregister', [{<<"notification">>, <<"deregister">>}]}
                    ,{'notify_password_recovery', [{<<"notification">>, <<"password_recovery">>}]}
                    ,{'notify_new_account', [{<<"notification">>, <<"new_account">>}]}
                    ,{'notify_cnam_request', [{<<"notification">>, <<"cnam_request">>}]}
                    ,{'notify_port_request', [{<<"notification">>, <<"port_request">>}]}
                    ,{'notify_port_cancel', [{<<"notification">>, <<"port_cancel">>}]}
                    ,{'notify_ported', [{<<"notification">>, <<"ported">>}]}
                    ,{'notify_low_balance', [{<<"notification">>, <<"low_balance">>}]}
                    ,{'notify_transaction', [{<<"notification">>, <<"transaction">>}]}
                    ,{'notify_system_alert', [{<<"notification">>, <<"system_alert">>}]}
                    ,{'notify_topup', [{<<"notification">>, <<"topup">>}]}
                    ,{'notify_low_balance', [{<<"notification">>, <<"low_balance">>}]}
                    ]).

-define(RESTRICT_TO, ['new_voicemail'
                     ,'voicemail_full'
                     ,'inbound_fax'
                     ,'inbound_fax_error'
                     ,'outbound_fax'
                     ,'outbound_fax_error'
                     ,'deregister'
                     ,'password_recovery'
                     ,'new_account'
                     ,'cnam_requests'
                     ,'port_request'
                     ,'port_cancel'
                     ,'low_balance'
                     ,'transaction'
                     ,'system_alerts'
                     ,'first_occurrence'
                     ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?RESTRICT_TO} | ?FEDERATE_BINDING(?NOTIFY_CONFIG_CAT)]}
                  ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"notify_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-record(state, {}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lager:debug("starting new notify server"),
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(JObj, _State) ->
    case should_handle(JObj)
        orelse should_handle_port(JObj)
    of
        'false' -> 'ignore';
        'true' ->
            lager:debug("handling notification for ~p", [kz_util:get_event_type(JObj)]),
            {'reply', []}
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("notify server ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec should_handle_port(kz_json:object()) -> boolean().
should_handle_port(JObj) ->
    case kz_util:get_event_type(JObj) of
        {<<"notification">>, <<"port_request">>} ->
            kz_json:get_value(<<"Port-Request-ID">>, JObj) =:= 'undefined';
        {<<"notification">>, <<"ported">>} ->
            kz_json:get_value(<<"Port-Request-ID">>, JObj) =:= 'undefined';
        _Else -> 'false'
    end.

-spec should_handle(kz_json:object()) -> boolean().
should_handle(JObj) ->
    Account = kz_json:get_first_defined([<<"Account-ID">>, <<"Account-DB">>], JObj),

    Config = kz_account:get_inherited_value(Account
                                           ,fun kz_account:notification_preference/1
                                           ,kapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT, <<"notification_app">>, <<"teletype">>)
                                           ),

    lager:debug("notification configuration is: ~p", [Config]),
    Config =:= ?APP_NAME.
