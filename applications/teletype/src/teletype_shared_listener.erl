%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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

-record(state, {}).

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
                     ,{{'teletype_deregister', 'handle_deregister'}
                       ,[{<<"notification">>, <<"deregister">>}]
                      }
                     ,{{'teletype_password_recovery', 'handle_password_recovery'}
                       ,[{<<"notification">>, <<"password_recovery">>}]
                      }
                     ,{{'teletype_system_alert', 'handle_system_alert'}
                       ,[{<<"notification">>, <<"system_alert">>}]
                      }
                     ,{{'teletype_cnam_request', 'handle_cnam_request'}
                       ,[{<<"notification">>, <<"cnam_request">>}]
                      }
                    ]).
%% -define(RESPONDERS, []}

%%                      ,{'teletype_fax_outbound_to_email', [{<<"notification">>, <<"outbound_fax">>}]}

%%                      ,{'teletype_fax_outbound_error_to_email', [{<<"notification">>, <<"outbound_fax_error">>}]}


%%                      ,{'teletype_new_account', [{<<"notification">>, <<"new_account">>}]}
%%                      ,{'teletype_port_request', [{<<"notification">>, <<"port_request">>}]}
%%                      ,{'teletype_port_cancel', [{<<"notification">>, <<"port_cancel">>}]}
%%                      ,{'teletype_ported', [{<<"notification">>, <<"ported">>}]}
%%                      ,{'teletype_low_balance', [{<<"notification">>, <<"low_balance">>}]}
%%                      ,{'teletype_transaction', [{<<"notification">>, <<"transaction">>}]}
%%                      ,{'teletype_topup', [{<<"notification">>, <<"topup">>}]}

%%                     ]).

-define(RESTRICT_TO, ['new_voicemail'
                      ,'voicemail_full'
                      ,'inbound_fax'
                      ,'outbound_fax'
                      ,'new_account'
                      ,'new_user'
                      ,'inbound_fax_error'
                      ,'outbound_fax_error'
                      ,'deregister'
                      ,'pwd_recovery'
                      ,'cnam_requests'
                      %% ,'port_request'
                      %% ,'port_cancel'
                      %% ,'low_balance'
                      %% ,'transaction'
                      ,'system_alerts'
                      %%,'skel'
                     ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?RESTRICT_TO}]}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"teletype_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
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
handle_event(JObj, _State) ->
    case should_handle(JObj) of
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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec should_handle(wh_json:object()) -> boolean().
should_handle(JObj) ->
    should_handle(JObj, wh_json:is_true(<<"Preview">>, JObj, 'false')).

should_handle(_JObj, 'true') -> 'true';
should_handle(JObj, 'false') ->
    case wh_json:get_first_defined([<<"Account-ID">>, <<"Account-DB">>], JObj) of
        'undefined' -> should_handle_system();
        Account -> should_handle_account(Account)
    end.

-spec should_handle_system() -> boolean().
should_handle_system() ->
    whapps_config:get_value(?NOTIFY_CONFIG_CAT
                            ,<<"notification_app">>
                            ,?APP_NAME
                           ) =:= ?APP_NAME.

-spec should_handle_account(ne_binary()) -> boolean().
should_handle_account(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', AccountJObj} ->
            kz_account:notification_preference(AccountJObj) =:= ?APP_NAME;
        {'error', _E} -> 'true'
    end.
