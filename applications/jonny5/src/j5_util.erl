%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([remove_call_charges/2]).
-export([send_system_alert/1]).

-include_lib("jonny5.hrl").

-spec remove_call_charges(api_binary(), api_binary()) -> 'ok'.
remove_call_charges('undefined', _) -> 'ok';
remove_call_charges(_, 'undefined') -> 'ok';
remove_call_charges(AccountId, CallId) ->
    case wh_transactions:call_charges(AccountId, CallId, 'false') of
        [] -> 'ok';
        Transactions ->
            _ = wh_transactions:remove(Transactions),
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
                         ,{<<"Soft-Limit">>, wh_util:to_binary(j5_request:soft_limit(Request))}
                         |P
                        ]
                end
                ,fun(P) -> add_limit_details(AccountId, <<"Account">>, P) end
                ,fun(P) -> add_limit_details(ResellerId, <<"Reseller">>, P) end
               ],
    wh_notify:system_alert("blocked ~s to ~s / Account ~s / Reseller ~s"
                           ,[j5_request:from(Request)
                             ,j5_request:number(Request)
                             ,get_account_name(AccountId)
                             ,get_account_name(ResellerId)
                            ]
                           ,lists:foldr(fun(F, P) -> F(P) end, [], Routines)).

-spec add_limit_details(api_binary(), ne_binary(), wh_proplist()) -> wh_proplist().
add_limit_details('undefined', _, Props) -> Props;
add_limit_details(Account, Prefix, Props) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Limits = j5_limits:get(AccountId),
    [{<<Prefix/binary, "-Enforce-Limits">>, wh_util:to_binary(j5_limits:enabled(Limits))}
     ,{<<Prefix/binary, "-Calls">>
          ,wh_util:to_binary(
             io_lib:format("~w/~w"
                           ,[j5_channels:total_calls(AccountId)
                             ,j5_limits:calls(Limits)
                            ])
            )}
     ,{<<Prefix/binary, "-Resource-Calls">>
           ,wh_util:to_binary(
              io_lib:format("~w/~w"
                            ,[j5_channels:resource_consuming(AccountId)
                              ,j5_limits:resource_consuming_calls(Limits)
                             ])
             )}
     ,{<<Prefix/binary, "-Inbound-Trunks">>
          ,wh_util:to_binary(
             io_lib:format("~w/~w"
                           ,[j5_channels:inbound_flat_rate(AccountId)
                             ,j5_limits:inbound_trunks(Limits)
                            ])
            )}
     ,{<<Prefix/binary, "-Outbound-Trunks">>
           ,wh_util:to_binary(
              io_lib:format("~w/~w"
                            ,[j5_channels:outbound_flat_rate(AccountId)
                              ,j5_limits:outbound_trunks(Limits)
                             ])
             )}
     ,{<<Prefix/binary, "-Twoway-Trunks">>, wh_util:to_binary(j5_limits:twoway_trunks(Limits))}
     ,{<<Prefix/binary, "-Burst-Trunks">>, wh_util:to_binary(j5_limits:burst_trunks(Limits))}
     ,{<<Prefix/binary, "-Allow-Prepay">>, wh_util:to_binary(j5_limits:allow_prepay(Limits))}
     ,{<<Prefix/binary, "-Balance">>
           ,wh_util:to_binary(
              wht_util:units_to_dollars(
                wht_util:current_balance(AccountId)
               )
             )}
     ,{<<Prefix/binary, "-Allow-Postpay">>, wh_util:to_binary(j5_limits:allow_postpay(Limits))}
     ,{<<Prefix/binary, "-Max-Postpay">>
           ,wh_util:to_binary(
              wht_util:units_to_dollars(
                j5_limits:max_postpay(Limits)
               )
             )}
     | Props
    ].

-spec get_account_name(api_binary()) -> ne_binary().
get_account_name('undefined') -> <<"unknown">>;
get_account_name(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'error', _} -> AccountId;
        {'ok', JObj} -> wh_json:get_ne_value(<<"name">>, JObj, AccountId)
    end.




