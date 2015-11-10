%%%-------------------------------------------------------------------
%%% File    : j5_balacne_crawler.erl
%%% Description : Jonny5 module for disconnect calls when account
%%% balance drops below zero
%%%-------------------------------------------------------------------
-module(j5_balcraw_worker).

-export([start/0]).

-export([maybe_disconnect_account/1]).
-export([disconnect_account/1]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERACCOUNT_DELAY, 10).

-spec start() -> no_return().
start() ->
    case j5_channels:accounts() of
        [] -> exit("no accounts");
        Accounts ->
            lager:debug("check ~p account(s) for zero balance", [length(Accounts)]),
            crawler(Accounts)
    end.

-spec crawler(api_binaries()) -> no_return().
crawler([]) -> exit("work done");
crawler([Account|Accounts]) ->
    maybe_disconnect_account(Account),
    Delay = whapps_config:get_integer(?APP_NAME, <<"balance_crawler_interaccount_delay_ms">>, ?DEFAULT_INTERACCOUNT_DELAY),
    timer:sleep(Delay),
    crawler(Accounts).

-spec maybe_disconnect_account(ne_binary()) -> 'ok'.
maybe_disconnect_account(AccountId) ->
    Limits = j5_limits:get(AccountId),
    DisconnectActiveCalls = j5_limits:disconnect_active_calls(Limits),
    case wh_util:is_true(DisconnectActiveCalls) of
        'false' -> 'ok';
        'true' ->
            case j5_per_minute:maybe_credit_available(0, Limits, 'true') of
                'true' -> 'ok';
                'false' -> disconnect_account(AccountId)
            end
    end.

-spec disconnect_account(ne_binary()) -> 'ok'.
disconnect_account(AccountId) ->
    case j5_channels:per_minute(AccountId) of
        Count when Count > 0 ->
            lager:debug("account ~p has ~p per-minute calls, disconnect them",[AccountId, Count]),
            maybe_disconnect_channels(j5_channels:account(AccountId));
        _ ->
            lager:debug("account ~p doesn't have any per-minute call",[AccountId]),
            'ok'
    end.

-spec maybe_disconnect_channels(j5_channels:channels()) -> 'ok'.
maybe_disconnect_channels([]) -> 'ok';
maybe_disconnect_channels([Channel|Channels]) ->
    Props = j5_channels:to_props(Channel),
    case props:get_binary_value(<<"Account-Billing">>, Props) =:= <<"per_minute">>
         orelse props:get_binary_value(<<"Reseller-Billing">>, Props) =:= <<"per_minute">>
    of
        'false' -> maybe_disconnect_channels(Channels);
        'true' ->
            disconnect_channel(Props),
            maybe_disconnect_channels(Channels)
    end.

-spec disconnect_channel(wh_proplist()) -> 'ok'.
disconnect_channel(Props) ->
    CallId =  props:get_ne_binary_value(<<"Call-ID">>, Props),
    HangupDelay = case whapps_config:get_is_true(?APP_NAME, <<"balance_crawler_delayed_hangup">>, true) of
                      'true' -> get_hangup_delay(Props);
                      'false' -> 0
                  end,
    lager:debug("call id ~p, account billing ~p, reseller billing ~p",[CallId, props:get_binary_value(<<"Account-Billing">>, Props), props:get_binary_value(<<"Reseller-Billing">>, Props)]),
    try_disconnect_call(CallId, HangupDelay).

-spec get_hangup_delay(wh_proplist()) -> integer().
get_hangup_delay(Props) ->
    case props:get_integer_value(<<"Answered-Timestamp">>, Props) of
        'undefined' -> 0;
        AnsweredTimestamp ->
            CallTime = wh_util:current_tstamp() - AnsweredTimestamp,
            RateMinimum = props:get_integer_value(<<"Rate-Minimum">>, Props),
            RateIncrement = props:get_integer_value(<<"Rate-Increment">>, Props),
            MaxCallTime = ((CallTime div RateIncrement) + 1) * RateIncrement,
            case CallTime < RateMinimum of
                'true' -> RateMinimum - CallTime - 1;
                'false' when MaxCallTime > CallTime -> MaxCallTime - CallTime - 1;
                'false' -> 0
            end
    end.

-spec send_hangup_req(ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"hangup">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1).

-spec try_disconnect_call(ne_binary(), integer()) -> 'ok'.
try_disconnect_call(CallId, HangupDelay) when is_integer(HangupDelay) andalso HangupDelay > 0 ->
    lager:debug("disconnect call ~p delayed to ~p seconds",[CallId, HangupDelay]),
    _ = spawn(fun() ->
                      timer:sleep(HangupDelay * ?MILLISECONDS_IN_SECOND),
                      send_hangup_req(CallId)
              end
             ),
    'ok';
try_disconnect_call(CallId, _HangupDelay) -> send_hangup_req(CallId).

