%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(jonny5_maintenance).

-export([flush/0]).
-export([flush_channels/0]).
-export([authz_summary/0
        ,authz_summary/1
        ]).
-export([authz_details/1]).
-export([limits_summary/0
        ,limits_summary/1
        ]).
-export([limits_details/1]).

-include("jonny5.hrl").

-spec flush() -> 'ok'.
flush() -> kz_cache:flush_local(?CACHE_NAME).

-spec flush_channels() -> 'ok'.
flush_channels() -> j5_channels:flush().

-spec authz_summary() -> 'no_return'.
authz_summary() ->
    case j5_channels:accounts() of
        [] ->
            io:format("no channels found~n", []),
            'no_return';
        Accounts ->
            print_authz_summary_header(),
            authz_summary(Accounts)
    end.

print_authz_summary_header() ->
    io:format("+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+~n"),
    io:format("| Account ID                       | Calls | Resource Calls | Allotments | Inbound Trunks | Outbound Trunks | Per Minute |~n"),
    io:format("+==================================+=======+================+============+================+=================+============+~n").

-spec authz_summary(kz_term:ne_binaries()) -> 'no_return'.
authz_summary([]) -> 'no_return';
authz_summary([AccountId|AccountIds]) ->
    io:format("| ~-32s | ~-5w | ~-14w | ~-10w | ~-14w | ~-15w | ~-10w |~n"
             ,[AccountId
              ,j5_channels:total_calls(AccountId)
              ,j5_channels:resource_consuming(AccountId)
              ,j5_channels:allotments(AccountId)
              ,j5_channels:inbound_flat_rate(AccountId)
              ,j5_channels:outbound_flat_rate(AccountId)
              ,j5_channels:per_minute(AccountId)
              ]),
    io:format("+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+~n"),
    authz_summary(AccountIds);
authz_summary(<<_/binary>> = AccountId) ->
    print_authz_summary_header(),
    authz_summary([AccountId]).

-spec authz_details(j5_channels:channels() | kz_term:ne_binary()) -> 'no_return'.
authz_details([]) ->
    io:format("~n", []),
    'no_return';
authz_details([Channel|Channels]) ->
    io:format("~n", []),
    Props = j5_channels:to_props(Channel),
    Timestamp = kz_time:now_s(),
    pretty_print_field(<<"Call ID">>, props:get_value(<<"Call-ID">>, Props)),
    pretty_print_field(<<"Other Leg Call ID">>, props:get_value(<<"Other-Leg-Call-ID">>, Props)),
    pretty_print_field(<<"Direction">>, props:get_value(<<"Direction">>, Props)),
    pretty_print_field(<<"Per-Minute Cost">>, authz_details_cost(Props, Timestamp)),
    pretty_print_field(<<"Account ID">>, props:get_value(<<"Account-ID">>, Props)),
    pretty_print_field(<<"Account Billing">>, props:get_value(<<"Account-Billing">>, Props)),
    pretty_print_field(<<"Reseller ID">>, props:get_value(<<"Reseller-ID">>, Props)),
    pretty_print_field(<<"Reseller Billing">>, props:get_value(<<"Reseller-Billing">>, Props)),
    pretty_print_field(<<"Soft-limit">>, props:get_value(<<"Soft-Limit">>, Props)),
    pretty_print_field(<<"Duration">>, authz_details_duration(<<"Timestamp">>, Props, Timestamp)),
    pretty_print_field(<<"Answered Duration">>, authz_details_duration(<<"Answered-Timestamp">>, Props, Timestamp)),
    pretty_print_field(<<"Rate Name">>, props:get_value(<<"Rate-Name">>, Props)),
    pretty_print_field(<<"Rate ID">>, props:get_value(<<"Rate-ID">>, Props)),
    pretty_print_field(<<"Rate">>, props:get_value(<<"Rate">>, Props)),
    pretty_print_field(<<"Rate Increment">>, props:get_value(<<"Rate-Increment">>, Props)),
    pretty_print_field(<<"Rate Minimum">>, props:get_value(<<"Rate-Minimum">>, Props)),
    pretty_print_field(<<"Rate No Charge Time">>, props:get_value(<<"Rate-NoCharge-Time">>, Props)),
    pretty_print_field(<<"Discount Percentage">>, props:get_value(<<"Discount-Percentage">>, Props)),
    pretty_print_field(<<"Surcharge">>, props:get_value(<<"Surcharge">>, Props)),
    pretty_print_field(<<"Base Cost">>, props:get_value(<<"Base-Cost">>, Props)),
    authz_details(Channels);
authz_details(AccountId) ->
    authz_details(j5_channels:account(AccountId)).

-spec authz_details_cost(kz_term:proplist(), non_neg_integer()) -> non_neg_integer().
authz_details_cost(Props, Timestamp) ->
    case props:get_integer_value(<<"Answered-Timestamp">>, Props) of
        'undefined' -> 0;
        Answered ->
            BillingSeconds = Timestamp - Answered,
            JObj = kz_json:from_list([{<<"Billing-Seconds">>, BillingSeconds} | Props]),
            kz_currency:units_to_dollars(kapps_call_util:call_cost(JObj))
    end.

-spec authz_details_duration(kz_term:ne_binary(), kz_term:proplist(), non_neg_integer()) -> iolist().
authz_details_duration(Key, Props, Timestamp) ->
    case props:get_integer_value(Key, Props) of
        'undefined' -> "0s";
        Created ->
            [kz_term:to_list(Timestamp - Created), "s"]
    end.

-spec limits_summary() -> 'no_return'.
limits_summary() ->
    case j5_limits:cached() of
        [] ->
            io:format("no limits found~n", []),
            'no_return';
        Limits ->
            limit_summary_header(),
            limits_summary(Limits)
    end.

-spec limits_summary([j5_limits:limits()] | kz_term:ne_binary()) -> 'no_return'.
limits_summary([]) -> 'no_return';
limits_summary([Limit|Limits]) ->
    case j5_limits:enabled(Limit) of
        'false' ->
            io:format("| ~-32s ", [j5_limits:account_id(Limit)]),
            io:format("|   -   ", []),
            io:format("|       -        ", []),
            io:format("|     -      ", []),
            io:format("|  -  ", []),
            io:format("|  -  ", []),
            io:format("|   -  ", []),
            io:format("|   -   ", []),
            io:format("|     -      ", []),
            io:format("|      -      |~n", []);
        'true' ->
            io:format("| ~-32s | ~-5w | ~-14w | ~-10w | ~-3w | ~-3w | ~-4w | ~-5w | ~-10s | ~-11s |~n"
                     ,[j5_limits:account_id(Limit)
                      ,j5_limits:calls(Limit)
                      ,j5_limits:resource_consuming_calls(Limit)
                      ,length(kz_json:get_keys(j5_limits:allotments(Limit)))
                      ,j5_limits:inbound_trunks(Limit)
                      ,j5_limits:outbound_trunks(Limit)
                      ,j5_limits:twoway_trunks(Limit)
                      ,j5_limits:burst_trunks(Limit)
                      ,limits_summary_prepay(Limit)
                      ,limits_summary_postpay(Limit)
                      ])
    end,
    io:format("+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+~n"),
    limits_summary(Limits);
limits_summary(AccountId) ->
    Limits = [j5_limits:fetch(AccountId)],
    limit_summary_header(),
    limits_summary(Limits).

-spec limits_summary_prepay(j5_limits:limits()) -> string().
limits_summary_prepay(Limit) ->
    case j5_limits:allow_prepay(Limit) of
        'false' -> "disabled";
        'true' ->
            current_balance(j5_limits:account_id(Limit))
    end.

-spec limits_summary_postpay(j5_limits:limits()) -> string().
limits_summary_postpay(Limit) ->
    case j5_limits:allow_postpay(Limit) of
        'false' -> "disabled";
        'true' ->
            kz_term:to_list(
              kz_currency:units_to_dollars(
                j5_limits:max_postpay(Limit)
               )
             )

    end.

-spec limit_summary_header() -> 'ok'.
limit_summary_header() ->
    io:format("+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+~n"),
    io:format("| Account ID                       | Calls | Resource Calls | Allotments |           Trunks         | Per Minute | Max Postpay |~n"),
    io:format("|                                  |       |                |            |  In | Out | Both | Burst |            |             |~n"),
    io:format("+==================================+=======+================+============+==========================+============+=============+~n").

-spec limits_details(atom() | string() | kz_term:ne_binary()) -> 'no_return'.
limits_details(Account) when not is_binary(Account) ->
    limits_details(kz_term:to_binary(Account));
limits_details(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Props = j5_limits:to_props(j5_limits:get(AccountId)),
    io:format("Account Info:~n", []),
    pretty_print_field("  Account ID", props:get_value('account_id', Props)),
    pretty_print_field("  Account DB", props:get_value('account_db', Props)),
    pretty_print_field("  Current Balance", current_balance(AccountId)),
    io:format("Configuration:~n", []),
    pretty_print_field("  Enabled", props:get_value('enabled', Props)),
    pretty_print_field("  Prepay Allowed", props:get_value('allow_prepay', Props)),
    pretty_print_field("  Postpay Allowed", props:get_value('allow_postpay', Props)),
    pretty_print_field("  Max Postpay Amount", kz_currency:units_to_dollars(props:get_value('max_postpay_amount', Props))),
    pretty_print_field("  Reserve Amount", kz_currency:units_to_dollars(props:get_value('reserve_amount', Props))),
    pretty_print_field("  Soft Limit Inbound", props:get_value('soft_limit_inbound', Props)),
    pretty_print_field("  Soft Limit Outbound", props:get_value('soft_limit_outbound', Props)),
    io:format("Limits:~n", []),
    pretty_print_field("  Calls Hard Limit", props:get_value('calls', Props)),
    pretty_print_field("  Resources Hard Limit", props:get_value('resource_consuming_calls', Props)),
    pretty_print_field("  Inbound Trunks", props:get_value('inbound_trunks', Props)),
    pretty_print_field("  Bundled Inbound Trunks", props:get_value('bundled_inbound_trunks', Props)),
    pretty_print_field("  Outbound Trunks", props:get_value('outbound_trunks', Props)),
    pretty_print_field("  Bundled Outbound Trunks", props:get_value('bundled_outbound_trunks', Props)),
    pretty_print_field("  Twoway Trunks", props:get_value('twoway_trunks', Props)),
    pretty_print_field("  Bundled Twoway Trunks", props:get_value('bundled_twoway_trunks', Props)),
    io:format("Allotments:~n", []),
    limits_details_allotments(props:get_value('allotments', Props)),
    'no_return'.

-spec limits_details_allotments(kz_json:object()) -> 'ok'.
limits_details_allotments(JObj) ->
    case kz_json:get_keys(JObj) of
        [] -> io:format("  -none-~n", []);
        Keys -> limits_details_allotments(Keys, JObj)
    end.

-spec limits_details_allotments(kz_term:ne_binaries(), kz_json:object()) -> 'ok'.
limits_details_allotments([], _) -> 'ok';
limits_details_allotments([Key|Keys], JObj) ->
    io:format("~n", []),
    pretty_print_field("  Name", kz_json:get_value([Key, <<"name">>], JObj, Key)),
    pretty_print_field("  Amount", kz_json:get_value([Key, <<"amount">>], JObj, 0)),
    pretty_print_field("  Cycle", kz_json:get_value([Key, <<"cycle">>], JObj, <<"monthly">>)),
    limits_details_allotments(Keys, JObj).

-spec pretty_print_field(kz_term:text(), any()) -> 'ok'.
pretty_print_field(_, 'undefined') -> 'ok';
pretty_print_field(Name, Value) when is_number(Value) ->
    io:format("~-25s: ~w~n", [Name, Value]);
pretty_print_field(Name, Value) ->
    io:format("~-25s: ~s~n", [Name, Value]).

-spec current_balance(kz_term:ne_binary()) -> iolist().
current_balance(AccountId) ->
    case kz_currency:available_dollars(AccountId) of
        {'ok', Dollars} -> kz_term:to_list(Dollars);
        {'error', _R} -> kz_term:to_list(io_lib:format("not known at the moment: ~p", [_R]))
    end.
