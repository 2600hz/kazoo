%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_credit).

-export([is_available/2]).

-include("jonny5.hrl").

-spec is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Balance = j5_util:current_balance(AccountId),
    case prepay_is_available(Limits, Balance, JObj) of
        false -> postpay_is_available(Limits, Balance, JObj);
        true -> true
    end.             
             
-spec prepay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
prepay_is_available(#limits{allow_prepay=false}, _, _) ->
    false;
prepay_is_available(#limits{allow_prepay=true, reserve_amount=ReserveAmount}=Limits, Balance, JObj) ->
    case (Balance - ReserveAmount) > 0 of
        false -> false;             
        true ->
            j5_util:start_per_minute(ReserveAmount, Limits, JObj),
            true
    end.

-spec postpay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
postpay_is_available(#limits{allow_postpay=false}, _, _) ->
    false;
postpay_is_available(#limits{max_postpay_amount=MaxPostpay}=Limits, Balance, JObj) when MaxPostpay > 0 ->
    postpay_is_available(Limits#limits{max_postpay_amount=MaxPostpay*-1}, Balance, JObj);
postpay_is_available(#limits{allow_postpay=true, max_postpay_amount=MaxPostpay
                             ,reserve_amount=ReserveAmount}=Limits, Balance, JObj) ->
    case (Balance - ReserveAmount) > MaxPostpay of
        false -> false;             
        true -> 
            j5_util:start_per_minute(ReserveAmount, Limits, JObj),
            true
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

postpay_limits() ->
    #limits{allow_postpay=true
            ,allow_prepay=false
            ,max_postpay_amount=10000
            ,reserve_amount=5000
           }.

prepay_limits() ->
    #limits{allow_postpay=false
            ,allow_prepay=true
            ,max_postpay_amount=10000
            ,reserve_amount=5000
           }.

request_jobj() ->
    Props = [{<<"Account-ID">>, <<"000000000000000000000000000000000">>}
             ,{<<"Call-ID">>, <<"000000000000000000000000000000000">>}
            ],
    wh_json:from_list(Props).

is_available_test() ->
    put(j5_test_balance, 0),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, 10000),
    ?assertEqual(true, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, -1000),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, -10000),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(false, is_available(postpay_limits(), request_jobj())),
    ok.

prepay_is_available_test() ->
    ?assertEqual(false, prepay_is_available(prepay_limits(), 0, request_jobj())),
    ?assertEqual(true, prepay_is_available(prepay_limits(), 10000, request_jobj())),
    ?assertEqual(false, prepay_is_available(prepay_limits(), -1000, request_jobj())),
    ?assertEqual(false, prepay_is_available(postpay_limits(), 0, request_jobj())),
    ok.

postpay_is_available_test() ->
    ?assertEqual(true, postpay_is_available(postpay_limits(), 0, request_jobj())),
    ?assertEqual(true, postpay_is_available(postpay_limits(), 10000, request_jobj())),
    ?assertEqual(false, postpay_is_available(postpay_limits(), -10000, request_jobj())),
    ?assertEqual(false, postpay_is_available(prepay_limits(), 0, request_jobj())),
    ok.

-endif.
