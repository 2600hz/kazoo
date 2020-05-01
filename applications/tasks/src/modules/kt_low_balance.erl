%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Check and notify if account's balance is below notification threshold.
%%% @author Harenson Henao
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_low_balance).

-export([init/0]).

%% Triggerables
-export([maybe_test_for_low_balance/2]).

-include("tasks.hrl").

-define(CATEGORY, "account_crawler").
-define(SHOULD_CRAWL_FOR_LOW_BALANCE
       ,kapps_config:get_is_true(?CONFIG_CAT
                                ,<<"should_crawl_for_low_balance">>
                                ,'true'
                                )
       ).

-define(LOW_BALANCE_REPEAT
       ,kapps_config:get_integer(?CONFIG_CAT
                                ,<<"low_balance_repeat_s">>
                                ,1 * ?SECONDS_IN_DAY
                                )
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok' | {'error', 'exists'}.
init() ->
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY>>
                           ,?MODULE
                           ,'maybe_test_for_low_balance'
                           ).

%% Triggerables
-spec maybe_test_for_low_balance(kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
maybe_test_for_low_balance(AccountId, AccountJObj) ->
    case ?SHOULD_CRAWL_FOR_LOW_BALANCE of
        'false' -> 'ok';
        'true' -> test_for_low_balance(AccountId, AccountJObj, 3)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec test_for_low_balance(kz_term:ne_binary(), kzd_accounts:doc(), 0..3) -> 'ok'.
test_for_low_balance(_AccountId, _AccountJObj, 0) ->
    lager:debug("max try to get account ~s current balance", [_AccountId]);
test_for_low_balance(AccountId, AccountJObj, Loop) ->
    case kz_currency:available_units(AccountId) of
        {'error', 'timeout'} ->
            test_for_low_balance(AccountId, AccountJObj, Loop - 1);
        {'error', _R} -> 'ok';
        {'ok', AvailableUnits} ->
            maybe_topup_account(AccountJObj, AvailableUnits)
    end.

-spec maybe_topup_account(kzd_accounts:doc(), kz_currency:units()) -> 'ok'.
maybe_topup_account(AccountJObj, AvailableUnits) ->
    AccountId = kz_doc:id(AccountJObj),
    lager:info("checking topup for account ~s with balance $~w"
              ,[AccountId, kz_currency:units_to_dollars(AvailableUnits)]),
    case kz_services_topup:maybe_topup(AccountId, AvailableUnits) of
        {'ok', _Transaction, _Ledger} ->
            lager:info("topup successful for ~s", [AccountId]);
        {'error', _Error} ->
            lager:debug("topup failed for ~s: ~p", [AccountId, _Error]),
            maybe_notify_for_low_balance(AccountJObj, AvailableUnits)
    end.

-spec maybe_notify_for_low_balance(kzd_accounts:doc(), kz_currency:units()) -> 'ok'.
maybe_notify_for_low_balance(AccountJObj, AvailableUnits) ->
    AccountId = kz_doc:id(AccountJObj),
    Threshold = kzd_accounts:low_balance_threshold(AccountJObj),
    lager:info("checking if account ~s balance $~w is below notification threshold $~w"
              ,[AccountId, kz_currency:units_to_dollars(AvailableUnits), Threshold]),
    case is_balance_below_notify_threshold(AvailableUnits, Threshold) of
        'false' -> maybe_reset_low_balance_sent(AccountJObj);
        'true' -> maybe_low_balance_notify(AccountJObj, AvailableUnits)
    end.

-spec is_balance_below_notify_threshold(kz_currency:units(), number()) -> boolean().
is_balance_below_notify_threshold(AvailableUnits, Threshold) ->
    AvailableUnits =< kz_currency:dollars_to_units(Threshold).

-spec maybe_reset_low_balance_sent(kzd_accounts:doc()) -> 'ok'.
maybe_reset_low_balance_sent(AccountJObj) ->
    case kzd_accounts:low_balance_sent(AccountJObj)
        orelse kzd_accounts:low_balance_tstamp(AccountJObj) =/= 'undefined'
    of
        'true' -> reset_low_balance_sent(AccountJObj);
        'false' -> 'ok'
    end.

-spec reset_low_balance_sent(kzd_accounts:doc()) ->  'ok'.
reset_low_balance_sent(AccountJObj0) ->
    lager:debug("resetting low balance sent"),

    Updates = [{kzd_accounts:path_low_balance_sent(), 'false'}
              ,{kzd_accounts:path_low_balance_tstamp(), 'null'}
              ],

    {'ok', _} = kzd_accounts:update(kz_doc:id(AccountJObj0), Updates),
    'ok'.

-spec maybe_low_balance_notify(kzd_accounts:doc(), kz_currency:units()) -> 'ok'.
maybe_low_balance_notify(AccountJObj, AvailableUnits) ->
    case kzd_accounts:low_balance_enabled_exists(AccountJObj) of
        'false' ->
            lager:debug("low balance notification enabled key not present, using deprecated check"),
            maybe_low_balance_notify_deprecated(AccountJObj, AvailableUnits);
        'true' ->
            maybe_low_balance_notify(AccountJObj, AvailableUnits, kzd_accounts:low_balance_enabled(AccountJObj))
    end.

-spec maybe_low_balance_notify(kzd_accounts:doc(), kz_currency:units(), boolean()) -> 'ok'.
maybe_low_balance_notify(_AccountJObj, _AvailableUnits, 'false') ->
    lager:debug("low balance notification disabled");
maybe_low_balance_notify(AccountJObj, AvailableUnits, 'true') ->
    lager:debug("low balance notification enabled"),
    case kzd_accounts:low_balance_tstamp(AccountJObj) of
        LowBalanceSent when is_number(LowBalanceSent) ->
            Cycle = ?LOW_BALANCE_REPEAT,
            Diff = kz_time:now_s() - LowBalanceSent,
            case Diff >= Cycle of
                'true' -> notify_of_low_balance(AccountJObj, AvailableUnits);
                'false' ->
                    lager:debug("low balance alert sent ~w seconds ago, repeats every ~w", [Diff, Cycle])
            end;
        _Else -> notify_of_low_balance(AccountJObj, AvailableUnits)
    end.

-spec maybe_low_balance_notify_deprecated(kzd_accounts:doc(), kz_currency:units()) -> 'ok'.
maybe_low_balance_notify_deprecated(AccountJObj, AvailableUnits) ->
    case kzd_accounts:low_balance_sent(AccountJObj) of
        'true' -> lager:debug("low balance alert already sent");
        'false' -> notify_of_low_balance(AccountJObj, AvailableUnits)
    end.

-spec notify_of_low_balance(kzd_accounts:doc(), kz_currency:units()) -> 'ok'.
notify_of_low_balance(AccountJObj, AvailableUnits) ->
    AccountId = kz_doc:id(AccountJObj),
    lager:debug("sending low balance alert for account ~s with balance ~w"
               ,[AccountId, AvailableUnits]),

    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Current-Balance">>, AvailableUnits}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_low_balance/1),

    update_account_low_balance_sent(AccountJObj).

-spec update_account_low_balance_sent(kzd_accounts:doc()) -> 'ok'.
update_account_low_balance_sent(AccountJObj0) ->
    Updates = [{kzd_accounts:path_low_balance_sent(), 'true'}
              ,{kzd_accounts:path_low_balance_tstamp(), kz_time:now_s()}
              ],
    {'ok', _} = kzd_accounts:update(kz_doc:id(AccountJObj0), Updates),
    'ok'.
