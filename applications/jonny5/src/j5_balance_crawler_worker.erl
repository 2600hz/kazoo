%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, VoIP, INC
%%% @doc
%%% Jonny5 module (workrt) for disconnect calls when account
%%% balance drops below zero
%%% @end
%%% @contributors
%%%     Dinkor (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(j5_balance_crawler_worker).

-export([start/0]).

-export([maybe_disconnect_account/1]).
-export([disconnect_account/1]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(INTERACCOUNT_DELAY_MS, kapps_config:get_integer(?APP_NAME, <<"balance_crawler_interaccount_delay_ms">>, 10)).
-define(DELAYED_HANGUP, kapps_config:get_is_true(?APP_NAME, <<"balance_crawler_delayed_hangup">>, 'true')).

-spec start() -> no_return().
start() ->
    case j5_channels:accounts() of
        [] -> exit('no_accounts');
        Accounts ->
            lager:debug("check ~p account(s) for a balance of zero", [length(Accounts)]),
            crawler(Accounts)
    end.

-spec crawler(api_binaries()) -> no_return().
crawler([]) -> exit('work_done');
crawler([Account|Accounts]) ->
    maybe_disconnect_account(Account),
    timer:sleep(?INTERACCOUNT_DELAY_MS),
    crawler(Accounts).

-spec maybe_disconnect_account(ne_binary()) -> 'ok'.
maybe_disconnect_account(AccountId) ->
    Limits = j5_limits:get(AccountId),
    DisconnectActiveCalls = j5_limits:disconnect_active_calls(Limits),
    case kz_util:is_true(DisconnectActiveCalls) of
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
            lager:debug("account ~p doesn't have any per-minute call",[AccountId])
    end.

-spec maybe_disconnect_channels(j5_channels:channels()) -> 'ok'.
maybe_disconnect_channels([]) -> 'ok';
maybe_disconnect_channels([Channel|Channels]) ->
    Props = j5_channels:to_props(Channel),
    case props:get_binary_value(<<"Account-Billing">>, Props) == <<"per_minute">>
         orelse props:get_binary_value(<<"Reseller-Billing">>, Props) == <<"per_minute">>
    of
        'false' -> maybe_disconnect_channels(Channels);
        'true' ->
            disconnect_channel(Props),
            maybe_disconnect_channels(Channels)
    end.

-spec disconnect_channel(kz_proplist()) -> 'ok'.
disconnect_channel(Props) ->
    CallId =  props:get_ne_binary_value(<<"Call-ID">>, Props),
    HangupDelay = get_hangup_delay(Props),
    lager:debug("call id ~p, account billing ~p, reseller billing ~p (delayed hangup:~s seconds)",
                [CallId
                 ,props:get_binary_value(<<"Account-Billing">>, Props)
                 ,props:get_binary_value(<<"Reseller-Billing">>, Props)
                 ,HangupDelay
                ]),
    try_disconnect_call(CallId, HangupDelay).

-spec get_hangup_delay(kz_proplist()) -> integer().
get_hangup_delay(Props) ->
    case ?DELAYED_HANGUP andalso props:get_integer_value(<<"Answered-Timestamp">>, Props) of
        'false' -> 0;
        'undefined' -> 0;
        AnsweredTimestamp ->
            CallTime = kz_util:current_tstamp() - AnsweredTimestamp,
            RateMinimum = props:get_integer_value(<<"Rate-Minimum">>, Props),
            RateIncrement = props:get_integer_value(<<"Rate-Increment">>, Props),
            MaxCallTime = ((CallTime div RateIncrement) + 1) * RateIncrement,
            case CallTime < RateMinimum of
                'true' -> RateMinimum - CallTime - 1;
                'false' when MaxCallTime > CallTime -> MaxCallTime - CallTime - 1;
                'false' -> 0
            end
    end.

-spec try_disconnect_call(ne_binary(), integer()) -> 'ok'.
try_disconnect_call(CallId, HangupDelay) when is_integer(HangupDelay) andalso HangupDelay > 0 ->
    _ = kz_util:spawn(fun() ->
                      timer:sleep(HangupDelay * ?MILLISECONDS_IN_SECOND),
                      konami_util:send_hangup_req(CallId)
              end
             ),
    'ok';
try_disconnect_call(CallId, _HangupDelay) -> konami_util:send_hangup_req(CallId).
