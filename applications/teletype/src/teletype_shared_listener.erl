%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_shared_listener).
-behaviour(gen_listener).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(RESPONDERS, [{{'teletype_voicemail_to_email', 'handle_new_voicemail'}
                     ,[{<<"notification">>, <<"voicemail_new">>}]
                     }
                    ,{{'teletype_voicemail_full', 'handle_full_voicemail'}
                     ,[{<<"notification">>, <<"voicemail_full">>}]
                     }
                    ,{{'teletype_fax_inbound_to_email', 'handle_fax_inbound'}
                     ,[{<<"notification">>, <<"inbound_fax">>}]
                     }
                    ,{{'teletype_fax_inbound_error_to_email', 'handle_fax_inbound_error'}
                     ,[{<<"notification">>, <<"inbound_fax_error">>}]
                     }
                    ,{{'teletype_fax_outbound_to_email', 'handle_fax_outbound'}
                     ,[{<<"notification">>, <<"outbound_fax">>}]
                     }
                    ,{{'teletype_fax_outbound_error_to_email', 'handle_fax_outbound_error'}
                     ,[{<<"notification">>, <<"outbound_fax_error">>}]
                     }
                    ,{{'teletype_new_account', 'handle_new_account'}
                     ,[{<<"notification">>, <<"new_account">>}]
                     }
                    ,{{'teletype_new_user', 'handle_req'}
                     ,[{<<"notification">>, <<"new_user">>}]
                     }
                    ,{'teletype_template_skel'
                     ,[{<<"notification">>, <<"skel">>}]
                     }
                    ,{{'teletype_customer_update', 'handle_req'}
                     ,[{<<"notification">>, <<"customer_update">>}]
                     }
                    ,{{'teletype_deregister', 'handle_deregister'}
                     ,[{<<"notification">>, <<"deregister">>}]
                     }
                    ,{{'teletype_transaction', 'handle_transaction'}
                     ,[{<<"notification">>, <<"transaction">>}]
                     }
                    ,{{'teletype_password_recovery', 'handle_password_recovery'}
                     ,[{<<"notification">>, <<"password_recovery">>}]
                     }
                    ,{{'teletype_system_alert', 'handle_system_alert'}
                     ,[{<<"notification">>, <<"system_alert">>}]
                     }
                    ,{{'teletype_topup', 'handle_topup'}
                     ,[{<<"notification">>, <<"topup">>}]
                     }
                    ,{{'teletype_cnam_request', 'handle_cnam_request'}
                     ,[{<<"notification">>, <<"cnam_request">>}]
                     }
                    ,{{'teletype_low_balance', 'handle_low_balance'}
                     ,[{<<"notification">>, <<"low_balance">>}]
                     }
                    ,{{'teletype_first_occurrence', 'first_occurrence'}
                     ,[{<<"notification">>, <<"first_occurrence">>}]
                     }
                    ,{'teletype_port_unconfirmed'
                     ,[{<<"notification">>, <<"port_unconfirmed">>}]
                     }
                    ,{'teletype_port_request'
                     ,[{<<"notification">>, <<"port_request">>}]
                     }
                    ,{'teletype_port_request_admin'
                     ,[{<<"notification">>, <<"port_request">>}]
                     }
                    ,{'teletype_port_pending'
                     ,[{<<"notification">>, <<"port_pending">>}]
                     }
                    ,{'teletype_port_scheduled'
                     ,[{<<"notification">>, <<"port_scheduled">>}]
                     }
                    ,{'teletype_port_rejected'
                     ,[{<<"notification">>, <<"port_rejected">>}]
                     }
                    ,{'teletype_port_cancel'
                     ,[{<<"notification">>, <<"port_cancel">>}]
                     }
                    ,{'teletype_ported'
                     ,[{<<"notification">>, <<"ported">>}]
                     }
                    ,{'teletype_port_comment'
                     ,[{<<"notification">>, <<"port_comment">>}]
                     }
                    ,{{'teletype_webhook_disabled', 'handle_webhook_disabled'}
                     ,[{<<"notification">>, <<"webhook_disabled">>}]
                     }
                    ,{'teletype_denied_emergency_bridge'
                     ,[{<<"notification">>, <<"denied_emergency_bridge">>}]
                     }
                    ,{'teletype_service_added'
                     ,[{<<"notification">>, <<"service_added">>}]
                     }
                    ]).

-define(RESTRICT_TO, ['cnam_requests'
                     ,'denied_emergency_bridge'
                     ,'deregister'
                     ,'inbound_fax'
                     ,'inbound_fax_error'
                     ,'low_balance'
                     ,'new_account'
                     ,'new_user'
                     ,'new_voicemail'
                     ,'outbound_fax'
                     ,'outbound_fax_error'
                     ,'password_recovery'
                     ,'ported'
                     ,'port_cancel'
                     ,'port_comment'
                     ,'port_pending'
                     ,'port_rejected'
                     ,'port_request'
                     ,'port_scheduled'
                     ,'port_unconfirmed'
                     ,'system_alerts'
                     ,'topup'
                     ,'transaction'
                     ,'voicemail_full'
                     ,'webhook_disabled'
                     ,'customer_update'
                     ,'service_added'
                     ,'first_occurrence'
                      %%,'skel'
                     ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?RESTRICT_TO} | ?FEDERATE_BINDING(?NOTIFY_CONFIG_CAT)]}
                  ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"teletype_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

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
    {'noreply', State}.

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
handle_cast({'gen_listener', {'created_queue', _QueueName}}, State) ->
    kz_util:put_callid(?MODULE),
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    kz_util:put_callid(?MODULE),
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'EXIT', _Pid, 'normal'}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(JObj, _State) ->
    case teletype_util:should_handle_notification(JObj) of
        'false' -> 'ignore';
        'true' -> {'reply', []}
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
    lager:debug("listener terminating: ~p", [_Reason]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
