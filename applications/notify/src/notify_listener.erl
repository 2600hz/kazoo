  %%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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

-define(RESPONDERS, [{'notify_voicemail_to_email', [{<<"notification">>, <<"new_voicemail">>}]}
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
                    ]).

-define(RESTRICT_TO, ['new_voicemail'
                      ,'voicemail_full'
                      ,'inbound_fax'
                      ,'inbound_fax_error'
                      ,'outbound_fax'
                      ,'outbound_fax_error'
                      ,'deregister'
                      ,'pwd_recovery'
                      ,'new_account'
                      ,'cnam_requests'
                      ,'port_request'
                      ,'port_cancel'
                      ,'low_balance'
                      ,'transaction'
                      ,'system_alerts'
                     ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?RESTRICT_TO}]}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"notify_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-record(state, {}).

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
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
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
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),
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
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
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
    lager:debug("notify server ~p termination", [_Reason]).

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
