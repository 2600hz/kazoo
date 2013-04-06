%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_credit).

-export([is_available/2]).
-export([reauthorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec is_available(#limits{}, wh_json:json_object()) -> boolean().
is_available(#limits{account_id=AccountId}=Limits, JObj) ->
    Balance = wht_util:current_balance(AccountId),
    prepay_is_available(Limits, Balance, JObj)
        orelse postpay_is_available(Limits, Balance, JObj).

-spec reauthorize(#limits{}, wh_json:json_object()) -> 'ok'.
reauthorize(#limits{reserve_amount=ReserveAmount}=Limits, JObj) ->
    case ReserveAmount - wht_util:call_cost(JObj) > 0 of
        'true' ->
            lager:info("call has not exceeded the reserved amount yet", []),
            j5_reauthz_req:send_allow_resp(JObj);
        'false' ->
            maybe_debit_next_minute(Limits, JObj)
    end.

-spec reconcile_cdr(ne_binary(), wh_json:json_object()) -> 'ok'.
reconcile_cdr(Account, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Units = wh_transactions:call_charges(Account, CallId)
        - (-1 * wht_util:call_cost(JObj)),
    case Units > 0 of
        'true' -> create_debit_transaction(<<"end">>, Units, Account, JObj);
        'false' -> create_credit_transaction(<<"end">>, Units, Account, JObj)
    end.

-spec prepay_is_available(#limits{}, integer(), wh_json:json_object()) -> boolean().
prepay_is_available(#limits{allow_prepay='false'}, _, _) ->
    'false';
prepay_is_available(#limits{allow_prepay='true', reserve_amount=ReserveAmount
                            ,account_id=LedgerId}, Balance, JObj) ->
    case (Balance - ReserveAmount) > 0 of
        'false' -> 'false';
        'true' ->
            create_debit_transaction(<<"reservation">>, ReserveAmount, LedgerId, JObj),
            'true'
    end.

-spec postpay_is_available(#limits{}, integer(), wh_json:json_object()) -> boolean().
postpay_is_available(#limits{allow_postpay='false'}, _, _) ->
    'false';
postpay_is_available(#limits{allow_postpay='true', max_postpay_amount=MaxPostpay
                             ,reserve_amount=ReserveAmount, account_id=LedgerId}
                     ,Balance, JObj) ->
    case (Balance - ReserveAmount) > MaxPostpay of
        'false' -> 'false';
        'true' ->
            create_debit_transaction(<<"reservation">>, ReserveAmount, LedgerId, JObj),
            'true'
    end.

-spec maybe_debit_next_minute(#limits{}, wh_json:json_object()) -> boolean().
maybe_debit_next_minute(#limits{account_id=AccountId}=Limits, JObj) ->
    Amount = wht_util:per_minute_cost(JObj),
    case is_credit_still_available(wht_util:current_balance(AccountId) - Amount, Limits) of
        'false' ->
            lager:debug("account does not have the required credit to continue this call", []),
            j5_reauthz_req:send_deny_resp(JObj);
        'true' ->
            lager:debug("account has the required credit to continue this call for another minute", []),
            Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
            create_debit_transaction(wh_util:to_binary(Timestamp)
                                     ,Amount
                                     ,AccountId
                                     ,JObj),
            j5_reauthz_req:send_allow_resp(JObj)
    end.

-spec is_credit_still_available(integer(), #limits{}) -> boolean().
is_credit_still_available(Balance, #limits{allow_prepay='true'}) when Balance > 0 ->
    'true';
is_credit_still_available(Balance, #limits{allow_postpay='true', max_postpay_amount=MaxPostpay}) ->
    Balance > MaxPostpay;
is_credit_still_available(_, _) ->
    'false'.

create_debit_transaction(Event, Units, LedgerId, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Routines = [fun(T) ->
                        AccountId = get_authz_account_id(JObj),
                        case AccountId =:= LedgerId of
                            'true' ->
                               wh_transaction:set_reason(<<"per_minute_call">>, T);
                            'false' ->
                                T1 = wh_transaction:set_reason(<<"sub_account_per_minute_call">>, T),
                                wh_transaction:set_sub_account_id(AccountId, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_event(Event, T) end
                ,fun(T) -> wh_transaction:set_call_id(CallId, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"per minute call">>, T)
                 end
                ,fun(T) ->
                         case Event of
                             <<"end">> ->
                                 wh_transaction:set_metadata(set_metadata(JObj), T);
                             _ ->
                                 T
                         end
                 end
               ],
    T = lists:foldl(fun(F, T) -> F(T) end, wh_transaction:debit(LedgerId, Units), Routines),
    wh_transaction:save(T).

create_credit_transaction(Event, Units, LedgerId, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Routines = [fun(T) ->
                        AccountId = get_authz_account_id(JObj),
                        case AccountId =:= LedgerId of
                            'true' ->
                               wh_transaction:set_reason(<<"per_minute_call">>, T);
                            'false' ->
                                T1 = wh_transaction:set_reason(<<"sub_account_per_minute_call">>, T),
                                wh_transaction:set_sub_account_id(AccountId, T1)
                        end
                end
                ,fun(T) -> wh_transaction:set_event(Event, T) end
                ,fun(T) -> wh_transaction:set_call_id(CallId, T) end
                ,fun(T) ->
                         wh_transaction:set_description(<<"per minute call">>, T)
                 end
                ,fun(T) ->
                         case Event of
                             <<"end">> ->
                                 wh_transaction:set_metadata(set_metadata(JObj), T);
                             _ ->
                                 T
                         end
                 end
               ],
    T = lists:foldl(fun(F, T) -> F(T) end, wh_transaction:credit(LedgerId, Units), Routines),
    wh_transaction:save(T).

set_metadata(JObj) ->
    wh_json:set_values([{<<"direction">>, wh_json:get_value(<<"Call-Direction">>, JObj, 'undefined')}
                        ,{<<"duration">>, wh_json:get_value(<<"Billing-Seconds">>, JObj, 'undefined')}
                        ,{<<"account_id">>, wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, 'undefined')}
                        ,{<<"to">>, wh_json:get_value(<<"Callee-ID-Number">>, JObj, 'undefined')}
                        ,{<<"from">>, wh_json:get_value(<<"Caller-ID-Number">>, JObj, 'undefined')}
                       ]
                        ,wh_json:new()).

get_authz_account_id(JObj) ->
    wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

postpay_limits() ->
    #limits{allow_postpay='true'
            ,allow_prepay='false'
            ,max_postpay_amount=-10000
            ,reserve_amount=5000
           }.

prepay_limits() ->
    #limits{allow_postpay='false'
            ,allow_prepay='true'
            ,max_postpay_amount=-10000
            ,reserve_amount=5000
           }.

request_jobj() ->
    Props = [{<<"Auth-Account-ID">>, <<"000000000000000000000000000000000">>}
             ,{<<"Call-ID">>, <<"000000000000000000000000000000000">>}
            ],
    wh_json:from_list(Props).

prepay_is_available_test() ->
    ?assertEqual('false', prepay_is_available(prepay_limits(), 0, request_jobj())),
    ?assertEqual('true', prepay_is_available(prepay_limits(), 10000, request_jobj())),
    ?assertEqual('false', prepay_is_available(prepay_limits(), -1000, request_jobj())),
    ?assertEqual('false', prepay_is_available(postpay_limits(), 0, request_jobj())),
    ok.

postpay_is_available_test() ->
    ?assertEqual('true', postpay_is_available(postpay_limits(), 0, request_jobj())),
    ?assertEqual('true', postpay_is_available(postpay_limits(), 10000, request_jobj())),
    ?assertEqual('false', postpay_is_available(postpay_limits(), -10000, request_jobj())),
    ?assertEqual('false', postpay_is_available(prepay_limits(), 0, request_jobj())),
    ok.

-endif.
