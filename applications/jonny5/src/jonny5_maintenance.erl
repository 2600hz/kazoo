%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(jonny5_maintenance).

-export([flush/0]).
-export([remove_callid/1]).
-export([authz_summary/0]).
-export([authz_details/1]).
-export([limits_summary/0
         ,limits_summary/1
        ]).
-export([limits_details/1]).

-include("jonny5.hrl").

-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?JONNY5_CACHE).

-spec remove_callid(ne_binary()) -> 'ok'.
remove_callid(CallId) ->
    j5_channels:remove(CallId).

-spec authz_summary() -> 'no_return'.
authz_summary() ->
    case j5_channels:accounts() of
        [] ->
            io:format("no channels found~n", []),
            'no_return';
        Accounts ->
            io:format("+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+~n"),
            io:format("| Account ID                       | Calls | Resource Calls | Allotments | Inbound Trunks | Outbound Trunks | Per Minute |~n"),
            io:format("+==================================+=======+================+============+================+=================+============+~n"),
            authz_summary(Accounts)
    end.

-spec authz_summary(ne_binaries()) -> 'no_return'.
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
    authz_summary(AccountIds).

-spec authz_details(j5_channels:channels() | ne_binary()) -> 'no_return'.
authz_details([]) ->
    io:format("~n", []),
    'no_return';
authz_details([Channel|Channels]) ->
    io:format("~n", []),
    Props = j5_channels:to_props(Channel),
    Timestamp = wh_util:current_tstamp(),
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
    pretty_print_field(<<"Discount Percentage">>, props:get_value(<<"Discount-Percentage">>, Props)),
    pretty_print_field(<<"Surcharge">>, props:get_value(<<"Surcharge">>, Props)),
    pretty_print_field(<<"Base Cost">>, props:get_value(<<"Base-Cost">>, Props)),
    authz_details(Channels);
authz_details(AccountId) ->
    authz_details(j5_channels:account(AccountId)).

-spec authz_details_cost(wh_proplist(), non_neg_integer()) -> non_neg_integer().
authz_details_cost(Props, Timestamp) ->
    case props:get_integer_value(<<"Answered-Timestamp">>, Props) of
        'undefined' -> 0;
        Answered ->
            BillingSeconds = Timestamp - Answered,
            JObj = wh_json:from_list([{<<"Billing-Seconds">>, BillingSeconds} | Props]),
            wht_util:units_to_dollars(wht_util:call_cost(JObj))
    end.

-spec authz_details_duration(ne_binary(), wh_proplist(), non_neg_integer()) -> iolist().
authz_details_duration(Key, Props, Timestamp) ->
    case props:get_integer_value(Key, Props) of
        'undefined' -> "0s";
        Created ->
            [wh_util:to_list(Timestamp - Created), "s"]
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

-spec limits_summary([j5_limits:limits(),...] | [] | ne_binary()) -> 'no_return'.
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
                        ,length(wh_json:get_keys(j5_limits:allotments(Limit)))
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
            AccountId = j5_limits:account_id(Limit),
            wh_util:to_list(
              wht_util:units_to_dollars(
                wht_util:current_balance(AccountId)
               )
             )
    end.

-spec limits_summary_postpay(j5_limits:limits()) -> string().
limits_summary_postpay(Limit) ->
    case j5_limits:allow_postpay(Limit) of
        'false' -> "disabled";
        'true' ->
            wh_util:to_list(
              wht_util:units_to_dollars(
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

-spec limits_details(atom() | string() | ne_binary()) -> 'no_return'.
limits_details(Account) when not is_binary(Account) ->
    limits_details(wh_util:to_binary(Account));
limits_details(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Props = j5_limits:to_props(j5_limits:get(AccountId)),
    io:format("Account Info:~n", []),
    pretty_print_field("  Account ID", props:get_value('account_id', Props)),
    pretty_print_field("  Account DB", props:get_value('account_db', Props)),
    pretty_print_field("  Current Balance", wht_util:units_to_dollars(wht_util:current_balance(AccountId))),
    io:format("Configuration:~n", []),
    pretty_print_field("  Enabled", props:get_value('enabled', Props)),
    pretty_print_field("  Prepay Allowed", props:get_value('allow_prepay', Props)),
    pretty_print_field("  Postpay Allowed", props:get_value('allow_postpay', Props)),
    pretty_print_field("  Max Postpay Amount", wht_util:units_to_dollars(props:get_value('max_postpay_amount', Props))),
    pretty_print_field("  Reserve Amount", wht_util:units_to_dollars(props:get_value('reserve_amount', Props))),
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

-spec limits_details_allotments(wh_json:object()) -> 'ok'.
limits_details_allotments(JObj) ->
    case wh_json:get_keys(JObj) of
        [] -> io:format("  -none-~n", []);
        Keys -> limits_details_allotments(Keys, JObj)
    end.

-spec limits_details_allotments(ne_binaries(), wh_json:object()) -> 'ok'.
limits_details_allotments([], _) -> 'ok';
limits_details_allotments([Key|Keys], JObj) ->
    io:format("~n", []),
    pretty_print_field("  Name", wh_json:get_value([Key, <<"name">>], JObj, Key)),
    pretty_print_field("  Amount", wh_json:get_value([Key, <<"amount">>], JObj, 0)),
    pretty_print_field("  Cycle", wh_json:get_value([Key, <<"cycle">>], JObj, <<"monthly">>)),
    limits_details_allotments(Keys, JObj).

-spec pretty_print_field(text(), any()) -> 'ok'.
pretty_print_field(_, 'undefined') -> 'ok';
pretty_print_field(Name, Value) when is_number(Value) ->
    io:format("~-25s: ~w~n", [Name, Value]);
pretty_print_field(Name, Value) ->
    io:format("~-25s: ~s~n", [Name, Value]).
