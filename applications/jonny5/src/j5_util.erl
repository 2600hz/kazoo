%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([get_limits/1]).
-export([send_system_alert/3]).
-export([remove_call_charges/2]).

-include_lib("jonny5.hrl").

-define(LIMITS_KEY(AccountId), {limits, AccountId}).

-spec get_limits(ne_binary()) -> #limits{}.
get_limits(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {'ok', Limits} -> Limits;
        {'error', 'not_found'} ->
            JObj = get_limit_jobj(AccountDb),
            DefaultUsePrepay = whapps_config:get_is_true(<<"jonny5">>, <<"default_use_prepay">>, true),
            DefaultPostpay = whapps_config:get_is_true(<<"jonny5">>, <<"default_allow_postpay">>, false),
            DefaultMaxPostpay = whapps_config:get_float(<<"jonny5">>, <<"default_max_postpay_amount">>, 0.0),
            DefaultReserve = whapps_config:get_float(<<"jonny5">>, <<"default_reserve_amount">>, ?DEFAULT_RATE),
            Limits = #limits{account_id = AccountId
                             ,account_db = AccountDb
                             ,enabled = wh_json:is_true(<<"pvt_enabled">>, JObj, true)
                             ,twoway_trunks = get_limit(<<"twoway_trunks">>, JObj)
                             ,inbound_trunks = get_limit(<<"inbound_trunks">>, JObj)
                             ,resource_consuming_calls = get_limit(<<"resource_consuming_calls">>, JObj)
                             ,calls = get_limit(<<"calls">>, JObj)
                             ,allow_prepay = wh_json:is_true(<<"allow_prepay">>, JObj, DefaultUsePrepay)
                             ,allow_postpay = wh_json:is_true(<<"pvt_allow_postpay">>, JObj, DefaultPostpay)
                             ,max_postpay_amount = wht_util:dollars_to_units(abs(wh_json:get_float_value(<<"pvt_max_postpay_amount">>, JObj, DefaultMaxPostpay))) * -1
                             ,reserve_amount = wht_util:dollars_to_units(wh_json:get_float_value(<<"pvt_reserve_amount">>, JObj, DefaultReserve))
                             ,allotments = wh_json:get_value(<<"pvt_allotments">>, JObj, wh_json:new())
                             ,soft_limit_inbound = wh_json:is_true(<<"pvt_soft_limit_inbound">>, JObj)
                             ,soft_limit_outbound = wh_json:is_true(<<"pvt_soft_limit_outbound">>, JObj)
                            },
            CacheProps = [{'origin', {'db', AccountDb, <<"limits">>}}],
            wh_cache:store_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId), Limits, CacheProps),
            Limits
    end.

-spec get_limit(ne_binary(), wh_json:object()) -> integer().
get_limit(Key, JObj) ->
    DefaultValue = whapps_config:get_integer(<<"jonny5">>, <<"default_", Key/binary>>, -1),
    PublicValue =  wh_json:get_integer_value(Key, JObj, DefaultValue),
    case wh_json:get_integer_value(<<"pvt_", Key/binary>>, JObj) of
        'undefined' -> PublicValue;
        -1 -> -1;
        PrivateValue when PrivateValue < PublicValue -> PrivateValue;
        _Else -> PublicValue
    end.

-spec get_limit_jobj(ne_binary()) -> wh_json:object().
get_limit_jobj(AccountDb) ->
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {'ok', J} -> J;
        {'error', 'not_found'} ->
            lager:debug("failed to open limits doc in account db ~s", [AccountDb]),
            create_init_limits(AccountDb)
    end.

-spec create_init_limits(ne_binary()) -> wh_json:object().
create_init_limits(AccountDb) ->
    TStamp = wh_util:current_tstamp(),
    JObj = wh_json:from_list([{<<"_id">>, <<"limits">>}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_account_id">>, wh_util:format_account_id(AccountDb, raw)}
                              ,{<<"pvt_type">>, <<"limits">>}
                              ,{<<"pvt_created">>, TStamp}
                              ,{<<"pvt_modified">>, TStamp}
                              ,{<<"pvt_vsn">>, 1}
                             ]),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'ok', J} ->
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            J;
         {'error', _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            wh_json:new()
    end.

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
