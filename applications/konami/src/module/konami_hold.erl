%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Put the call on hold
%%% Data = {
%%%   "moh":"media_id"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(konami_hold).

-behaviour(gen_server).

-export([handle/2
         ,number_builder/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("konami.hrl").

-record(state, {call :: whapps_call:call()
                ,requesting_leg :: ne_binary()
                ,hold_leg :: ne_binary()}).
-type state() :: #state{}.

-define(HOLD_CALL_EVENTS, [<<"CHANNEL_BRIDGE">> ,<<"CHANNEL_DESTROY">>]).
-define(HOLD_TIMEOUT, 3600 * 1000).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call:put_callid(Call),
    MOH = wh_json:get_value(<<"moh">>, Data),

    RequestingLeg = wh_json:get_value(<<"dtmf_leg">>, Data),
    HoldLeg = hold_leg(Call, RequestingLeg),

    lager:debug("first, we need to receive call events for our two legs"),
    konami_event_listener:add_call_binding(RequestingLeg, ?HOLD_CALL_EVENTS),
    konami_event_listener:add_call_binding(HoldLeg, ?HOLD_CALL_EVENTS),

    lager:debug("then unbridging the call"),
    whapps_call_command:unbridge(Call),

    HoldCommand = whapps_call_command:hold_command(MOH, HoldLeg),
    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
      ,Call
     ),
    lager:debug("leg ~s is putting ~s on hold", [RequestingLeg, HoldLeg]),

    try gen_server:enter_loop(?MODULE, [], #state{call = Call
                                                  ,requesting_leg = RequestingLeg
                                                  ,hold_leg = HoldLeg}, ?HOLD_TIMEOUT)
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            wh_util:log_stacktrace(ST)
    end.

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'hold' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'hold'? ", "~d"),

    K = [<<"numbers">>, wh_util:to_binary(Number)],

    case number_builder_check(wh_json:get_value(K, DefaultJObj)) of
        'undefined' -> wh_json:delete_key(K, DefaultJObj);
        NumberJObj -> wh_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    number_builder_moh(wh_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [wh_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(wh_json:object(), string()) -> api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_moh(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_moh(wh_json:object()) -> wh_json:object().
number_builder_moh(NumberJObj) ->
    {'ok', [MOH]} = io:fread("Any custom music on hold to play ('n' to leave as default MOH, 'h' for help)? ", "~s"),
    metaflow_jobj(NumberJObj, MOH).

-spec metaflow_jobj(wh_json:object(), string()) -> wh_json:object().
metaflow_jobj(NumberJObj, "h") ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    number_builder_moh(NumberJObj);
metaflow_jobj(NumberJObj, MOH) ->
    wh_json:set_values([{<<"module">>, <<"hold">>}
                        ,{<<"data">>, moh_data(MOH)}
                       ], NumberJObj).

-spec moh_data(string()) -> wh_json:object().
moh_data("n") ->
    wh_json:new();
moh_data(MOH) ->
    wh_json:from_list([{<<"moh">>, wh_util:to_binary(MOH)}]).

%% ------------------------------------------------------------------
%% gen_server callback functions
%% ------------------------------------------------------------------
init(Args) ->
    {'ok', Args}.

handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.

handle_info({'amqp_msg', JObj}, State) ->
    handle_call_event(wh_json:get_first_defined([<<"Call-ID">>
                                                 ,<<"Outbound-Call-ID">>
                                                ], JObj)
                      ,wh_json:get_value(<<"Event-Name">>, JObj)
                      ,State);
handle_info('timeout', #state{call = Call
                              ,requesting_leg = RequestingLeg
                              ,hold_leg = HoldLeg} = State) ->
    hangup_leg(Call, RequestingLeg),
    hangup_leg(Call, HoldLeg),
    lager:debug("timeout, hangup both ~s and ~s", [RequestingLeg, HoldLeg]),
    {'stop', 'timeout', State}.

-spec handle_call_event(ne_binary(), ne_binary(), state()) ->
    {'noreply', state(), ?HOLD_TIMEOUT} | {'stop', term(), state()}.
handle_call_event(CallId, <<"CHANNEL_BRIDGE">>
                  ,#state{requesting_leg = CallId
                          ,hold_leg = HoldLeg} = State) ->
    lager:debug("~s and ~s are reconnected", [CallId, HoldLeg]),
    {'stop', 'normal', State};
handle_call_event(CallId, <<"CHANNEL_BRIDGE">>
                  ,#state{requesting_leg = RequestingLeg
                          ,hold_leg = CallId} = State) ->
    lager:debug("~s and ~s are reconnected", [RequestingLeg, CallId]),
    {'stop', 'normal', State};
handle_call_event(CallId, <<"CHANNEL_DESTROY">>
                  ,#state{call = Call
                          ,requesting_leg = CallId
                          ,hold_leg = HoldLeg} = State) ->
    hangup_leg(Call, HoldLeg),
    lager:debug("leg ~s are destroy, handup ~s too", [CallId, HoldLeg]),
    {'stop', 'normal', State};
handle_call_event(CallId, <<"CHANNEL_DESTROY">>
                  ,#state{call = Call
                          ,requesting_leg = RequestingLeg
                          ,hold_leg = CallId} = State) ->
    hangup_leg(Call, RequestingLeg),
    lager:debug("leg ~s are destroy, handup ~s too", [CallId, RequestingLeg]),
    {'stop', 'normal', State};
handle_call_event(_, _, State) ->
    lager:debug("ignore other event"),
    {'noreply', State, ?HOLD_TIMEOUT}.

terminate(_Reason, #state{requesting_leg = RequestingLeg
                        ,hold_leg = HoldLeg}) ->
    konami_event_listener:rm_call_binding(RequestingLeg, ?HOLD_CALL_EVENTS),
    konami_event_listener:rm_call_binding(HoldLeg, ?HOLD_CALL_EVENTS),
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%% ------------------------------------------------------------------
%% internal helper functions
%% ------------------------------------------------------------------
-spec hold_leg(whapps_call:call(), ne_binary()) -> ne_binary().
hold_leg(Call, RequestingLeg) ->
    case whapps_call:call_id(Call) of
        RequestingLeg -> whapps_call:other_leg_call_id(Call);
        HoldLeg -> HoldLeg
    end.

-spec hangup_leg(whapps_call:call(), ne_binary()) -> 'ok'.
hangup_leg(Call, CallId) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    whapps_call_command:send_command(Command, Call).
