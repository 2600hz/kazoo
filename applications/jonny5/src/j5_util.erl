%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([send_system_alert/3]).
-export([remove_call_charges/2]).

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

-spec send_system_alert(ne_binary(), wh_json:object(), #limits{}) -> pid().
-ifdef(TEST).
send_system_alert(_Reason, _JObj, _Limits) -> spawn(fun() -> ok end).
-else.
send_system_alert(Reason, JObj, Limits) ->
    spawn(fun() ->
                  AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
                  Routines = [fun(J) -> wh_json:set_value(<<"Request">>, wh_json:get_value(<<"Request">>, JObj), J) end
                              ,fun(J) -> wh_json:set_value(<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj), J) end
                              ,fun(J) -> wh_json:set_value(<<"Node">>, wh_json:get_value(<<"Node">>, JObj), J) end
                              ,fun(J) -> wh_json:set_value(<<"Inbound-Only-Limit">>, wh_util:to_binary(Limits#limits.inbound_trunks), J) end 
                              ,fun(J) -> wh_json:set_value(<<"Twoway-Limit">>, wh_util:to_binary(Limits#limits.twoway_trunks), J) end        
                              ,fun(J) -> wh_json:set_value(<<"Resource-Limit">>, wh_util:to_binary(Limits#limits.resource_consuming_calls), J) end
                              ,fun(J) -> wh_json:set_value(<<"Call-Limit">>, wh_util:to_binary(Limits#limits.calls), J) end
                              ,fun(J) -> wh_json:set_value(<<"Allow-Prepay">>, wh_util:to_binary(Limits#limits.allow_prepay), J) end
                              ,fun(J) -> wh_json:set_value(<<"Allow-Postpay">>, wh_util:to_binary(Limits#limits.allow_postpay), J) end
                              ,fun(J) ->
                                       MaxPostPay = wht_util:units_to_dollars(Limits#limits.max_postpay_amount),
                                       wh_json:set_value(<<"Max-Postpay">>, <<"$", (wh_util:to_binary(MaxPostPay))/binary>>, J)
                               end
                              ,fun(J) ->
                                       ReserveAmount = wht_util:units_to_dollars(Limits#limits.reserve_amount),
                                       wh_json:set_value(<<"Reserve-Amount">>, <<"$", (wh_util:to_binary(ReserveAmount))/binary>>, J)
                               end
                              ,fun(J) ->
                                       Balance = wht_util:units_to_dollars(wht_util:current_balance(AccountId)),
                                       wh_json:set_value(<<"Available-Balance">>, <<"$", (wh_util:to_binary(Balance))/binary>>, J) 
                               end
                             ],
                  Details = lists:foldl(fun(F, A) -> F(A) end, wh_json:get_value(<<"Usage">>, JObj, wh_json:new()), Routines),
                  CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, JObj, <<>>),
                  CallerIdNumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj, <<>>),
                  [To, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
                  case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
                      {error, _} ->
                          wh_notify:system_alert("blocked undefined / ~s ~s to ~s / Account ~s / ~s", [CallerIdName, CallerIdNumber, To, AccountId, Reason], wh_json:to_proplist(Details));
                      {ok, Account} ->
                          AccountName = wh_json:get_value(<<"name">>, Account, <<>>), 
                          wh_notify:system_alert("blocked ~s / ~s ~s to ~s / Account ~s / ~s", [AccountName, CallerIdName, CallerIdNumber, To, AccountId, Reason], wh_json:to_proplist(Details))
                  end
          end).
-endif.
