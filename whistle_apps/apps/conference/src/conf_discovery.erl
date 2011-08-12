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

-define(VIEW_FILE, <<"views/conference.json">>).
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
    spawn(fun() -> whapps_util:revise_whapp_views_in_accounts(conference) end),
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
            {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
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
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};

handle_info({#'basic.deliver'{}, #amqp_msg{props = Props, payload = Payload}}, State) when
      Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
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
-spec start_amqp/0 :: () -> tuple(ok, binary()).
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
-spec process_req/3 :: (MsgType, JObj, State) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object(),
      State :: #state{}.
process_req({<<"conference">>, <<"discovery">>}, JObj, _) ->
    %% TODO: If I had more time this additional Q is possibly not necessary, or at least pooled...
    S1 = #search{conf_id = wh_json:get_value(<<"Conference-ID">>, JObj)
                 ,account_id = wh_json:get_value(<<"Account-ID">>, JObj)
                 ,amqp_q = Q = amqp_util:new_queue()
                 ,call_id = CallId = wh_json:get_value(<<"Call-ID">>, JObj)
                 ,ctrl_q = CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj)
                 ,moderator = wh_json:get_value(<<"Moderator">>, JObj, <<"false">>)
                },

    put(callid, CallId),
    ?LOG_START("received discovery request for conference"),

    %% Bind to call events for collecting conference numbers, pins, and playing prompts
    amqp_util:bind_q_to_callevt(Q, CallId),
    amqp_util:basic_consume(Q),

    Command = [{<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:answer_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),

    {ok, _} = play_greeting(S1),

    %% Ensure that a provided conference ID is valid, or if there is none ask the
    %% caller for the conference number and look it up in the db.  Either way pins
    %% is loaded with the pin numbers for the conference (based on the callers role)
    {ok, S2} = validate_conference_id(S1#search{loop_count=1}),

    %% If the role of this caller has pin numbers defined then request it from the
    %% caller and confirm it is valid.
    {ok, S3} = validate_conference_pin(S2#search{loop_count=1}),

    %% hmmm, avert your eyes....
    %% when building a conference service if two calls are executing at the same
    %% time the initial request to add a caller will be returned, both will try
    %% to create new services, however one will fail because the queue is exclusive.
    %% when that happens the one with the failing service just needs to re-send the
    %% add caller request... Quick and dirty...
    try send_add_caller(S3) catch _:_ -> send_add_caller(S3) end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_add_caller/1 :: (Search) -> no_return() when
      Search :: #search{}.
send_add_caller(#search{conf_id=ConfId, account_id=AccountId, call_id=CallId, ctrl_q=CtrlQ, moderator=Moderator, amqp_q=Q}) ->
    ?LOG("attempting to handoff call control to conference service"),

    %% Bind to conference events for this confid so we will know if an existing service accepts the
    %% add caller request (since it has to produce this event anyway lets just have it do double duty)
    amqp_util:bind_q_to_conference(Q, events, ConfId),

    %% However, if there is no service running there will be no consumers when we publish our add caller
    %% message, so register our self as a return handler so we will be notified.
    amqp_util:register_return_handler(),

    Caller = [{<<"Conference-ID">>, ConfId}
              ,{<<"Account-ID">>, AccountId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Control-Queue">>, CtrlQ}
              ,{<<"Moderator">>, Moderator}
               | wh_api:default_headers(Q, <<"conference">>, <<"add_caller">>, ?APP_NAME, ?APP_VERSION)
              ],
    %% TODO: Add this to the wh_api once finalized
    Payload = wh_util:to_binary(mochijson2:encode({struct, Caller})),
    amqp_util:conference_publish(Payload, service, ConfId, [{immediate, true}]),

    case wait_for_handoff(AccountId, ConfId, Caller) of
        {ok, added_caller} ->
            %% TODO: notify requestor that we have succeeded!
            ?LOG_END("conference service has accepted call");
        {error, _} ->
            %% TODO: notify requestor that we failed... sorry
            ?LOG_END("conference discovery could not handoff control")
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait in a receive loop to see what happens to our add caller request.
%%
%% If we recieve a returned message whose payload is exactly the same as our
%% add caller request (remeber we will be recieving ALL returned messages) then
%% no service exsits. Try to start the service but it will not be able to start
%% if we are in a race condition.  However, that means some other discovery
%% server has JUST started it, the try..catch on send_add_caller will repeat
%% the request if it crashes here.
%%
%% If we recieve a AMQP message that is a conference added caller event for
%% our call-id we know a service accept it. Since we will be recieving all
%% kinds of events, and added callers for other discovery services throw
%% if it isnt ours, then catch that and go back to waiting for a response.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_handoff/3 :: (AccountId, ConfId, Caller) -> tuple(error, atom()) | tuple(ok, added_caller) when
      AccountId :: binary(),
      ConfId :: binary(),
      Caller :: proplist().
wait_for_handoff(AccountId, ConfId, Caller) ->
    AddCallerPayload = wh_util:to_binary(mochijson2:encode({struct, Caller})),
    receive
        {#'basic.return'{}, #amqp_msg{payload=AddCallerPayload}} ->
            ?LOG("no conference service is running, starting new"),
            %% TODO: If I had more time this doc should come from validate_conference_id...
            {ok, Conference} =
                couch_mgr:open_doc(whapps_util:get_db_name(AccountId, encoded), ConfId),
            {ok, _} = conf_service_sup:start_service(ConfId, Conference, {struct, Caller}),
            wait_for_handoff(AccountId, ConfId, Caller);
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload=Payload}} ->
            JObj = mochijson2:decode(Payload),
            try
                {<<"conference">>, <<"added_participant">>} = whapps_util:get_event_type(JObj),
                true = wh_json:get_value(<<"Call-ID">>, Caller) =:= wh_json:get_value(<<"Call-ID">>, JObj),
                {ok, added_caller}
            catch
                _:_ ->
                    wait_for_handoff(AccountId, ConfId, Caller)
            end
    after
        2500 ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays the greeting prompt (if defined) for this accounts conference
%% service.
%% @end
%%--------------------------------------------------------------------
-spec play_greeting/1 :: (Search) -> tuple(error, atom()) | tuple(ok, binary()) when
      Search :: #search{}.
play_greeting(#search{prompts=Prompts}=Search) ->
    play(Prompts#prompts.greeting, Search).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the discovery request has a conference id already on it find out
%% if that is valid. If so we are good, if not forget about it and
%% ask the caller for a conference number.
%%
%% If the discovery request did not specify a conference id then ask
%% the caller for a conference number and figure out the id from that.
%% They will have three chances to do so.
%%
%% Either way, the result of this function is to return a with a validated
%% conference id and the pins property of the record loaded with any pin
%% numbers defined for the type of conference participant (member or
%% moderator)
%% @end
%%--------------------------------------------------------------------
-spec validate_conference_id/1 :: (Search) -> tuple(error, to_many_attempts) | tuple(ok, #search{}) when
      Search :: #search{}.
validate_conference_id(#search{prompts=Prompts, loop_count=Loop}=Search) when Loop > 4 ->
    ?LOG_END("caller has failed to provide a valid conference number to many times"),
    {ok, _} =  play(Prompts#prompts.to_many_attempts, Search),
    {error, to_many_attempts};
validate_conference_id(#search{conf_id=undefined, prompts=Prompts, account_id=AccountId, loop_count=Loop}=Search) ->
    ?LOG("requesting conference number from caller"),
    {ok, ConfNum} = play_and_collect_digits(Prompts#prompts.request_id, Search),
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, [{<<"key">>, ConfNum}]) of
        {ok, [JObj]} ->
            ConfId = wh_json:get_value(<<"id">>, JObj),
            Mod = case wh_json:get_value([<<"value">>, <<"moderator">>], JObj, <<"unknown">>) of
                      <<"unknown">> ->
                          ?LOG("identified conference number ~s as conference ~s, dont know participant type yet", [ConfNum, ConfId]),
                          <<"unknown">>;
                      Moderator ->
                          ?LOG("identified conference number ~s as conference ~s", [ConfNum, ConfId]),
                          wh_util:to_binary(wh_util:is_true(Moderator))
                  end,
            {ok, Search#search{conf_id=ConfId
                               ,moderator=Mod
                               ,pins={wh_json:get_value([<<"value">>, <<"pins">>, <<"moderator">>], JObj, [])
                                      ,wh_json:get_value([<<"value">>, <<"pins">>, <<"member">>], JObj, [])
                                     }
                              }
            };
        _ ->
            ?LOG("could not find conference number ~s", [ConfNum]),
            {ok, _} = play(Prompts#prompts.incorrect_id, Search),
            validate_conference_id(Search#search{conf_id=undefined, loop_count=Loop + 1})
    end;
validate_conference_id(#search{conf_id=ConfId, account_id=AccountId, loop_count=Loop}=Search) ->
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:open_doc(Db, ConfId) of
        {ok, JObj} ->
            ?LOG("validated provided conference id, ~s", [ConfId]),
            {ok, Search#search{moderator = <<"unknown">>
                               ,pins={wh_json:get_value([<<"moderator">>, <<"pins">>], JObj, [])
                                      ,wh_json:get_value([<<"member">>, <<"pins">>], JObj, [])
                                     }
                              }};
        {error, _} ->
            ?LOG("provided with invalid conference id ~s, degrading to request", [ConfId]),
            validate_conference_id(Search#search{conf_id=undefined, loop_count=Loop + 1})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the search record has pin numbers defined ask the caller to enter
%% theirs then validate that it matches one of the pins in the list.
%% Callers have three chances to enter a pin before we give up.
%% @end
%%--------------------------------------------------------------------
-spec validate_conference_pin/1 :: (Search) -> tuple(error, to_many_attempts) | tuple(ok, #search{}) when
      Search :: #search{}.
validate_conference_pin(#search{moderator = <<"true">>, pins={[], _}}=Search) ->
    %% moderator has no pins for entry
    {ok, Search};
validate_conference_pin(#search{moderator = <<"false">>, pins={_, []}}=Search) ->
    %% member has no pins for entry
    {ok, Search};
validate_conference_pin(#search{prompts=Prompts, loop_count=Loop}=Search) when Loop > 4 ->
    ?LOG_END("caller has failed to provide the correct pin to many times"),
    {ok, _} = play(Prompts#prompts.to_many_attempts, Search),
    {error, to_many_attempts};
validate_conference_pin(#search{moderator = <<"true">>, pins={Pins, _}, prompts=Prompts, loop_count=Loop}=Search) ->
    ?LOG("requesting conference moderator pin from caller"),
    {ok, Pin} = play_and_collect_digits(Prompts#prompts.request_pin, Search),
    case lists:member(Pin, Pins) of
        true ->
            ?LOG("caller entered a valid moderator pin"),
            {ok, Search};
        false ->
            ?LOG("caller entered an invalid pin"),
            {ok, _} = play(Prompts#prompts.incorrect_pin, Search),
            validate_conference_pin(Search#search{loop_count=Loop + 1})
    end;
validate_conference_pin(#search{moderator = <<"false">>, pins={_, Pins}, prompts=Prompts, loop_count=Loop}=Search) ->
    ?LOG("requesting conference member pin from caller"),
    {ok, Pin} = play_and_collect_digits(Prompts#prompts.request_pin, Search),
    case lists:member(Pin, Pins) of
        true ->
            ?LOG("caller entered a valid member pin"),
            {ok, Search};
        false ->
            ?LOG("caller entered an invalid pin"),
            {ok, _} = play(Prompts#prompts.incorrect_pin, Search),
            validate_conference_pin(Search#search{loop_count=Loop + 1})
    end;
validate_conference_pin(#search{pins={ModeratorPins, MemberPins}, prompts=Prompts, loop_count=Loop}=Search) ->
    ?LOG("requesting conference pin from caller"),
    {ok, Pin} = play_and_collect_digits(Prompts#prompts.request_pin, Search),
    case {lists:member(Pin, ModeratorPins), lists:member(Pin, MemberPins)} of
        {true, _} ->
            ?LOG("caller entered a pin belonging to a moderator"),
            {ok, Search#search{moderator = <<"true">>}};
        {_, true} ->
            ?LOG("caller entered a pin belonging to a member"),
            {ok, Search#search{moderator = <<"false">>}};
        {false, false} ->
            ?LOG("caller entered an invalid pin"),
            {ok, _} = play(Prompts#prompts.incorrect_pin, Search),
            validate_conference_pin(Search#search{loop_count=Loop + 1})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays media the caller that we are searching for the conference for
%% unless that media is undefined (as in an account didnt want a greeting).
%% @end
%%--------------------------------------------------------------------
-spec play/2 :: (Media, Search) -> tuple(error, atom()) | tuple(ok, binary()) when
      Media :: binary(),
      Search :: #search{}.
play(undefined, _) ->
    {ok, no_media};
play(Media, #search{call_id=CallId, amqp_q=Q, ctrl_q=CtrlQ}) ->
    Command = [{<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Terminators">>, []}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:play_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),
    wait_for_command(<<"play">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays media then collect digits from the caller that we are searching
%% for the conference for
%% @end
%%--------------------------------------------------------------------
-spec play_and_collect_digits/2 :: (Media, Search) -> tuple(error, atom()) | tuple(ok, binary()) when
      Media :: binary(),
      Search :: #search{}.
play_and_collect_digits(Media, #search{call_id=CallId, amqp_q=Q, ctrl_q=CtrlQ}) ->
    Command = [{<<"Application-Name">>, <<"play_and_collect_digits">>}
               ,{<<"Minimum-Digits">>, <<"2">>}
               ,{<<"Maximum-Digits">>, <<"9">>}
               ,{<<"Terminators">>, [<<"#">>]}
               ,{<<"Timeout">>, <<"15000">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Media-Tries">>, <<"1">>}
               ,{<<"Failed-Media-Name">>, <<"silence_stream://50">>}
               ,{<<"Digits-Regex">>, <<"\\d+">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:play_collect_digits_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),
    wait_for_command(<<"play_and_collect_digits">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for a command to complete, error out, or the caller hangs up
%% @end
%%--------------------------------------------------------------------
-spec wait_for_command/1 :: (Command) -> tuple(error, atom()) | tuple(ok, binary()) when
      Command :: binary().
wait_for_command(Command) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}} ->
            JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Application-Name">>, JObj) } of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, _ } ->
                    ?LOG("channel was unbridged while waiting for ~s", [Command]),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    ?LOG("channel was hungup while waiting for ~s", [Command]),
                    {error, channel_hungup};
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while waiting for ~s", [Command]),
                    {error, execution_failure};
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Command } ->
                    {ok, wh_json:get_value(<<"Application-Response">>, JObj, <<>>)};
		_ ->
                    wait_for_command(Command)
            end;
        _ ->
            wait_for_command(Command)
    end.
