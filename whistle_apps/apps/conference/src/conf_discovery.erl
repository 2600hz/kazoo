%%%============================================================================
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%% This module is responsible for the second stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(conf_discovery).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("conference.hrl").

-define(SERVER, ?MODULE).
-define(LIST_BY_NUMBER, {<<"conference">>, <<"listing_by_number">>}).

-record(state, {amqp_q = <<>> :: binary()}).
-record(search, {conf_id
                 ,account_id
                 ,account_db
                 ,moderator = <<"false">>
                 ,pins = []
                 ,amqp_q
                 ,ctrl_q
                 ,call_id
                 ,loop_count = 1
                 ,prompts=#prompts{}
                }).

%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% @public
% @doc
% Starts the server
%
% @end
%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS
%%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% @private
% @doc
% Initializes the server
%
% @end
%------------------------------------------------------------------------------
init([]) ->
    ?LOG_SYS("starting new conference discovery process"),
    {ok, #state{}, 0}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
handle_info(timeout, #state{amqp_q = <<>>}=State) ->
    try
	{ok, Q} = start_amqp(),
	{noreply, State#state{amqp_q=Q}}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  io:format("Discovery RX: ~p~n", [JObj]),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
terminate( _Reason, _State) ->
    ?LOG_SYS("conference discovery ~p termination", [_Reason]),
    ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    try
	{'basic.qos_ok'} = amqp_util:basic_qos(1),
        _ = amqp_util:conference_exchange(),
        Q = amqp_util:new_conference_queue(discovery),
        amqp_util:bind_q_to_conference(Q, discovery),
	amqp_util:basic_consume(Q, [{exclusive, false}]),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), State :: #state{}) -> no_return()).
process_req({<<"conference">>, <<"discovery">>}, JObj, _) ->
    S1 = #search{conf_id = wh_json:get_value(<<"Conference-ID">>, JObj)
                 ,account_id = wh_json:get_value(<<"Account-ID">>, JObj)
                 ,amqp_q = Q = amqp_util:new_queue()
                 ,call_id = CallId = wh_json:get_value(<<"Call-ID">>, JObj)
                 ,ctrl_q = wh_json:get_value(<<"Control-Queue">>, JObj)
                 ,moderator = wh_json:get_value(<<"Moderator">>, JObj, <<"false">>)
                },

    put(callid, CallId),
    ?LOG_START("recieved discovery request for conference"),

    amqp_util:bind_q_to_callevt(Q, CallId),
    amqp_util:basic_consume(Q),

    {ok, _} = play_greeting(S1),

    {ok, S2} = validate_conference_id(S1#search{loop_count=1}),
    {ok, S3} = validate_conference_pin(S2#search{loop_count=1}),

    try send_add_caller(S3) catch _:_ -> send_add_caller(S3) end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
send_add_caller(#search{conf_id=ConfId, account_id=AccountId, call_id=CallId, ctrl_q=CtrlQ, moderator=Moderator, amqp_q=Q}) ->

    ?LOG("attempting to handoff call control to conference service"),

    amqp_util:bind_q_to_conference(Q, events, ConfId),
    amqp_util:register_return_handler(),

    Caller = [{<<"Conference-ID">>, ConfId}
              ,{<<"Account-ID">>, AccountId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Control-Queue">>, CtrlQ}
              ,{<<"Moderator">>, Moderator}
               | whistle_api:default_headers(Q, <<"conference">>, <<"add_caller">>, ?APP_NAME, ?APP_VERSION)
              ],
    Payload = whistle_util:to_binary(mochijson2:encode({struct, Caller})),

    amqp_util:conference_publish(Payload, service, ConfId, [{immediate, true}]),
    wait_for_handoff(AccountId, ConfId, Caller).

wait_for_handoff(AccountId, ConfId, Caller) ->
    AddCallerPayload = whistle_util:to_binary(mochijson2:encode({struct, Caller})),
    receive
        {#'basic.return'{}, #amqp_msg{payload=AddCallerPayload}} ->
            ?LOG("no conference service is running, starting new"),
            {ok, Conference} =
                couch_mgr:open_doc(whapps_util:get_db_name(AccountId, encoded), ConfId),
            {ok, _} = conf_service_sup:start_service(ConfId, Conference, Caller),
            ?LOG_END("new conference service has accepted call");
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload=Payload}} ->
            JObj = mochijson2:decode(Payload),
            try
                {<<"conference">>, <<"added_caller">>} = whapps_util:get_event_type(JObj),
                true = wh_json:get_value(<<"Call-ID">>, Caller) =:= wh_json:get_value(<<"Call-ID">>, JObj),
                ?LOG_END("existing conference service has accepted call")
            catch
                _:_ ->
                    wait_for_handoff(AccountId, ConfId, Caller)
            end
    after
        2500 ->
            ?LOG_END("conference discovery timed out while tring to handoff control")
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
play_greeting(#search{prompts=Prompts}=Search) ->
    play(Prompts#prompts.greeting, Search).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
validate_conference_id(#search{prompts=Prompts, loop_count=Loop}=Search) when Loop > 4 ->
    ?LOG_END("caller has failed to provide a valid conference number to many times"),
    play(Prompts#prompts.to_many_attempts, Search),
    {error, to_many_attempts};
validate_conference_id(#search{conf_id=undefined, prompts=Prompts, account_id=AccountId, loop_count=Loop}=Search) ->
    ?LOG("requesting conference number from caller"),
    {ok, ConfNum} = play_and_collect_digits(Prompts#prompts.request_id, Search),
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, [{<<"key">>, ConfNum}]) of
        {ok, [JObj]} ->
            ConfId = wh_json:get_value(<<"id">>, JObj),
            Mod = case wh_json:is_true([<<"value">>, <<"moderator">>], JObj, false) of
                      true ->
                          ?LOG("identified conference number ~s as a moderator for conference ~s", [ConfNum, ConfId]),
                          <<"true">>;
                      false ->
                          ?LOG("identified conference number ~s as a member for conference ~s", [ConfNum, ConfId]),
                          <<"false">>
                  end,
            {ok, Search#search{conf_id=ConfId
                               ,moderator=Mod
                               ,pins=wh_json:get_value([<<"value">>, <<"pins">>], JObj, [])
                              }
            };
        _ ->
            play(Prompts#prompts.incorrect_id, Search),
            validate_conference_id(Search#search{conf_id=undefined, loop_count=Loop + 1})
    end;
validate_conference_id(#search{conf_id=ConfId, account_id=AccountId, loop_count=Loop}=Search) ->
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:open_doc(Db, ConfId) of
        {ok, _} ->
            ?LOG("validated provided conference id, ~s", [ConfId]),
            {ok, Search};
        {error, _} ->
            ?LOG("provided with invalid conference id ~s, degrading to request", [ConfId]),
            validate_conference_id(Search#search{conf_id=undefined, loop_count=Loop + 1})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
validate_conference_pin(#search{pins=[]}=Search) ->
    {ok, Search};
validate_conference_pin(#search{prompts=Prompts, loop_count=Loop}=Search) when Loop > 4 ->
    ?LOG_END("caller has failed to provide the correct pin to many times"),
    play(Prompts#prompts.to_many_attempts, Search),
    {error, to_many_attempts};
validate_conference_pin(#search{prompts=Prompts, pins=Pins, loop_count=Loop}=Search) ->
    ?LOG("requesting conference pin from caller"),
    {ok, Pin} = play_and_collect_digits(Prompts#prompts.request_pin, Search),
    case [P || P <- Pins, whistle_util:to_binary(P) =:= Pin] of
        [_] -> {ok, Search};
        [] ->
            play(Prompts#prompts.incorrect_pin, Search),
            validate_conference_pin(Search#search{loop_count=Loop + 1})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
play(undefined, _) ->
    {ok, no_media};
play(Media, #search{call_id=CallId, amqp_q=Q, ctrl_q=CtrlQ}) ->
    Command = [{<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Terminators">>, []}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:play_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),
    wait_for_command(<<"play">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
play_and_collect_digits(Media, #search{call_id=CallId, amqp_q=Q, ctrl_q=CtrlQ}) ->
    Command = [{<<"Application-Name">>, <<"play_and_collect_digits">>}
               ,{<<"Minimum-Digits">>, <<"2">>}
               ,{<<"Maximum-Digits">>, <<"9">>}
               ,{<<"Terminators">>, [<<"#">>]}
               ,{<<"Timeout">>, <<"15000">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Media-Tries">>, <<"1">>}
               ,{<<"Failed-Media-Name">>, <<"silence_stream://250">>}
               ,{<<"Digits-Regex">>, <<"\\d+">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:play_collect_digits_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),
    wait_for_command(<<"play_and_collect_digits">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
wait_for_command(Command) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}} ->
            JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                { _, _, <<"error">> } ->
                    {error, execution_failure};
                { Command, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
                    {ok, wh_json:get_value(<<"Application-Response">>, JObj, <<>>)};
		_ ->
                    wait_for_command(Command)
            end;
        _ ->
            wait_for_command(Command)
    end.
