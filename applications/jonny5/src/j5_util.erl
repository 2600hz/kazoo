%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_util).

-export([remove_call_charges/2]).
-export([send_system_alert/1]).

-include("jonny5.hrl").

-spec remove_call_charges(kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
remove_call_charges('undefined', _) -> 'ok';
remove_call_charges(_, 'undefined') -> 'ok';
remove_call_charges(AccountId, CallId) ->
    case kz_transactions:call_charges(AccountId, CallId, 'false') of
        [] -> 'ok';
        Transactions ->
            _ = kz_transactions:remove(Transactions),
            'ok'
    end.

-spec send_system_alert(j5_request:request()) -> any().
send_system_alert(Request) ->
    AccountId = j5_request:account_id(Request),
    ResellerId = j5_request:reseller_id(Request),
    Routines = [fun(P) ->
                        [{<<"Request">>, j5_request:number(Request)}
                        ,{<<"Call-ID">>, j5_request:call_id(Request)}
                        ,{<<"Other-Leg-Call-ID">>, j5_request:other_leg_call_id(Request)}
                        ,{<<"Call-Direction">>, j5_request:call_direction(Request)}
                        ,{<<"To">>, j5_request:to(Request)}
                        ,{<<"From">>, j5_request:from(Request)}
                        ,{<<"Classification">>, j5_request:classification(Request)}
                        ,{<<"Account-ID">>, AccountId}
                        ,{<<"Account-Billing">>, j5_request:account_billing(Request)}
                        ,{<<"Reseller-ID">>, ResellerId}
                        ,{<<"Reseller-Billing">>, j5_request:reseller_billing(Request)}
                        ,{<<"Soft-Limit">>, kz_term:to_binary(j5_request:soft_limit(Request))}
                         | P
                        ]
                end
               ,fun(P) -> add_limit_details(AccountId, <<"Account">>, P) end
               ,fun(P) -> add_limit_details(ResellerId, <<"Reseller">>, P) end
               ],
    kz_notify:detailed_alert("blocked ~s to ~s / Account ~s / Reseller ~s"
                            ,[j5_request:from(Request)
                             ,j5_request:number(Request)
                             ,get_account_name(AccountId)
                             ,get_account_name(ResellerId)
                             ]
                            ,lists:foldr(fun(F, P) -> F(P) end, [], Routines)
                            ).

-spec add_limit_details(kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_limit_details('undefined', _, Props) -> Props;
add_limit_details(Account, Prefix, Props) ->
    AccountId = kz_util:format_account_id(Account),
    Limits = j5_limits:get(AccountId),
    [{<<Prefix/binary, "-Enforce-Limits">>, kz_term:to_binary(j5_limits:enabled(Limits))}
    ,{<<Prefix/binary, "-Calls">>, kz_term:to_binary(calls(AccountId, Limits))}
    ,{<<Prefix/binary, "-Resource-Calls">>, kz_term:to_binary(resource(AccountId, Limits))}
    ,{<<Prefix/binary, "-Inbound-Trunks">>, kz_term:to_binary(inbound_trunks(AccountId, Limits))}
    ,{<<Prefix/binary, "-Outbound-Trunks">>, kz_term:to_binary(outbound_trunks(AccountId, Limits))}
    ,{<<Prefix/binary, "-Twoway-Trunks">>, kz_term:to_binary(j5_limits:twoway_trunks(Limits))}
    ,{<<Prefix/binary, "-Burst-Trunks">>, kz_term:to_binary(j5_limits:burst_trunks(Limits))}
    ,{<<Prefix/binary, "-Allow-Prepay">>, kz_term:to_binary(j5_limits:allow_prepay(Limits))}
    ,{<<Prefix/binary, "-Balance">>, kz_term:to_binary(current_balance(AccountId))}
    ,{<<Prefix/binary, "-Allow-Postpay">>, kz_term:to_binary(j5_limits:allow_postpay(Limits))}
    ,{<<Prefix/binary, "-Max-Postpay">>, kz_term:to_binary(max_postpay(Limits))}
     | Props
    ].

resource(AccountId, Limits) ->
    Consuming = j5_channels:resource_consuming(AccountId),
    ConsumingCalls = j5_limits:resource_consuming_calls(Limits),
    io_lib:format("~w/~w", [Consuming, ConsumingCalls]).

calls(AccountId, Limits) ->
    TotalCalls = j5_channels:total_calls(AccountId),
    io_lib:format("~w/~w", [TotalCalls, j5_limits:calls(Limits)]).

inbound_trunks(AccountId, Limits) ->
    FlatRate = j5_channels:inbound_flat_rate(AccountId),
    io_lib:format("~w/~w", [FlatRate, j5_limits:inbound_trunks(Limits)]).

outbound_trunks(AccountId, Limits) ->
    FlatRate = j5_channels:outbound_flat_rate(AccountId),
    io_lib:format("~w/~w", [FlatRate, j5_limits:outbound_trunks(Limits)]).

max_postpay(Limits) ->
    wht_util:units_to_dollars(j5_limits:max_postpay(Limits)).

-spec get_account_name(kz_term:api_binary()) -> kz_term:ne_binary().
get_account_name('undefined') -> <<"unknown">>;
get_account_name(Account) ->
    case kzd_accounts:fetch_name(Account) of
        undefined -> kz_util:format_account_id(Account);
        Name -> Name
    end.

-spec current_balance(kz_term:ne_binary()) -> kz_term:ne_binary().
current_balance(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> wht_util:units_to_dollars(Balance);
        {'error', _} -> <<"not known at the moment">>
    end.
