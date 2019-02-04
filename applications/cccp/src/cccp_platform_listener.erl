%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_platform_listener).
-behaviour(gen_listener).

-export([start_link/1
        ,handle_answer/2
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

-define(SERVER, ?MODULE).
-define(RESPONDERS, [{{'cccp_util', 'relay_amqp'},[{<<"call_event">>, <<"*">>}]}
                    ,{{'cccp_util', 'handle_disconnect'},[{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}]}
                    ,{{?MODULE, 'handle_answer'}, [{<<"call_event">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kapps_call:call()) -> kz_types:startlink_ret().
start_link(Call) ->
    CallId = kapps_call:call_id(Call),
    Bindings = [{'call', [{'callid', CallId}]}
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'bindings', Bindings}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kapps_call:call()]) -> {'ok', state()}.
init([Call]) ->
    CallUpdate = kapps_call:kvs_store('server_pid', self(), Call),
    {'ok', #state{call=CallUpdate}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'created_queue',Queue}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=kapps_call:set_controller_queue(Queue, Call)}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{call=Call}=State) ->
    kapps_call_command:answer(Call),
    {'noreply', State};
handle_cast({'call_update', CallUpdate}, State) ->
    {'noreply', State#state{call=CallUpdate}};
handle_cast('stop_platform_listener', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{call=Call}) ->
    {'reply', [{'call', Call}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_answer(kz_json:object(), kz_term:proplist()) -> ok.
handle_answer(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            CallUpdate = kapps_call:kvs_store('consumer_pid', self(), props:get_value('call', Props)),
            gen_listener:cast(Srv, {'call_update', CallUpdate}),
            process_call(CallUpdate);
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(Srv, 'stop_platform_listener');
        _ -> 'ok'
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_call(kapps_call:call()) -> 'ok'.
process_call(Call) ->
    CID = knm_converters:normalize(kapps_call:caller_id_number(Call)),
    case cccp_util:authorize(CID, <<"cccps/cid_listing">>) of
        {'ok', AuthJObj} ->
            dial(AuthJObj, Call);
        _ ->
            pin_collect(Call)
    end.

-spec dial(kz_json:object(), kapps_call:call()) -> 'ok'.
dial(JObj, Call) ->
    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    UserId = kz_json:get_value(<<"user_id">>, JObj),
    MaxConcurentCallsPerUser = kz_json:get_integer_value(<<"max_concurent_calls_per_user">>, JObj, 1),
    case (cccp_util:count_user_legs(UserId, AccountId) >= MaxConcurentCallsPerUser * 2) of
        'true' ->
            kapps_call_command:b_prompt(<<"cf-move-too_many_channels">>, Call),
            kapps_call_command:hangup(Call);
        'false' ->
            AccountId = kz_json:get_value(<<"account_id">>, JObj),
            UserId = kz_json:get_value(<<"user_id">>, JObj),
            AuthDocId = kz_json:get_value(<<"id">>, JObj),
            RetainCID = kz_json:get_binary_boolean(<<"retain_cid">>, JObj, <<"false">>),
            CallUpdate = kapps_call:kvs_store('auth_doc_id', AuthDocId, Call),
            gen_listener:cast(kapps_call:kvs_fetch('server_pid', CallUpdate), {'call_update', CallUpdate}),
            {'num_to_dial', ToDID} = cccp_util:get_number(CallUpdate),
            CallId = kapps_call:call_id(CallUpdate),
            CtrlQ = kapps_call:control_queue(CallUpdate),
            CallerName = knm_converters:normalize(kapps_call:caller_id_name(Call)),
            CallerNumber = knm_converters:normalize(kapps_call:caller_id_number(Call)),
            cccp_util:bridge(CallId, ToDID, UserId, CtrlQ, AccountId, RetainCID, CallerName, CallerNumber),
            cccp_util:store_last_dialed(ToDID, AuthDocId)
    end.

-spec pin_collect(kapps_call:call()) -> 'ok'.
pin_collect(Call) ->
    pin_collect(Call, 3).
pin_collect(Call, 0) ->
    kapps_call_command:hangup(Call);
pin_collect(Call, Retries) ->
    case kapps_call_command:b_prompt_and_collect_digits(9, 12, <<"disa-enter_pin">>, 3, Call) of
        {'ok', <<>>} ->
            kapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            pin_collect(Call, Retries - 1);
        {'ok', EnteredPin} ->
            handle_entered_pin(Call, Retries, EnteredPin);
        _ ->
            lager:info("no pin entered."),
            kapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            pin_collect(Call, Retries - 1)
    end.

-spec handle_entered_pin(kapps_call:call(), integer(), kz_term:ne_binary()) -> 'ok'.
handle_entered_pin(Call, Retries, EnteredPin) ->
    case cccp_util:authorize(EnteredPin, <<"cccps/pin_listing">>) of
        {'ok', AuthJObj} ->
            dial(AuthJObj, Call);
        _ ->
            lager:info("wrong Pin entered."),
            kapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            pin_collect(Call, Retries - 1)
    end.
