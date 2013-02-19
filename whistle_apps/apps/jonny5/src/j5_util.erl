%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([get_limits/1]).
-export([current_balance/1]).
-export([write_to_ledger/5]).
-export([get_session_id/1]).
-export([send_system_alert/3]).

-include_lib("jonny5.hrl").

-define(LIMITS_KEY(AccountId), {limits, AccountId}).

-spec get_limits(ne_binary()) -> #limits{}.
get_limits(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {ok, Limits} -> Limits;
        {error, not_found} ->
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
                             ,max_postpay_amount = wapi_money:dollars_to_units(abs(wh_json:get_float_value(<<"pvt_max_postpay_amount">>, JObj, DefaultMaxPostpay))) * -1
                             ,reserve_amount = wapi_money:dollars_to_units(wh_json:get_float_value(<<"pvt_reserve_amount">>, JObj, DefaultReserve))
                             ,allotments = wh_json:get_value(<<"pvt_allotments">>, JObj, wh_json:new())
                             ,soft_limit_inbound = wh_json:is_true(<<"pvt_soft_limit_inbound">>, JObj)
                             ,soft_limit_outbound = wh_json:is_true(<<"pvt_soft_limit_outbound">>, JObj)
                            },
            CacheProps = [{origin, {db, AccountDb, <<"limits">>}}],
            wh_cache:store_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId), Limits, CacheProps),
            Limits
    end.

-spec get_limit(ne_binary(), wh_json:json_object()) -> integer().
get_limit(Key, JObj) ->
    DefaultValue = whapps_config:get_integer(<<"jonny5">>, <<"default_", Key/binary>>, -1),
    PublicValue =  wh_json:get_integer_value(Key, JObj, DefaultValue),
    case wh_json:get_integer_value(<<"pvt_", Key/binary>>, JObj) of
        undefined -> PublicValue;
        -1 -> -1;
        PrivateValue when PrivateValue < PublicValue -> PrivateValue;
        _Else -> PublicValue
    end.

-spec get_limit_jobj(ne_binary()) -> wh_json:json_object().
get_limit_jobj(AccountDb) ->
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {ok, J} -> J;
        {error, not_found} ->
            lager:debug("failed to open limits doc in account db ~s", [AccountDb]),
            create_init_limits(AccountDb)
    end.

-spec create_init_limits(ne_binary()) -> wh_json:json_object().
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
        {ok, J} -> 
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            J;
         {error, _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            wh_json:new()
    end.

-spec write_to_ledger(ne_binary(), float() | integer(), wh_json:json_object(), ne_binary(), debit | credit) -> wh_jobj_return().
-ifdef(TEST).
write_to_ledger(_Suffix, _Props, _Units, _Limits, _JObj) -> {ok, wh_json:new()}.
-else.
write_to_ledger(Suffix, Props, Units, #limits{account_id=LedgerId, account_db=LedgerDb}, JObj) ->
    Timestamp = get_timestamp(JObj),
    SessionId = get_session_id(JObj),
    Id = <<SessionId/binary, "-", (wh_util:to_binary(Suffix))/binary>>,
    case props:get_value(<<"pvt_type">>, Props) of
        <<"credit">> ->
            lager:debug("credit ~s $~w for session ~s: ~s"
                        ,[LedgerId, abs(wapi_money:units_to_dollars(Units)), SessionId, props:get_value(<<"reason">>, Props, <<"no_reason">>)]);
        <<"debit">> ->
            lager:debug("debit ~s $~w for session ~s: ~s"
                        ,[LedgerId, abs(wapi_money:units_to_dollars(Units)), SessionId, props:get_value(<<"reason">>, Props, <<"no_reason">>)]);
        <<"credit_allotment">> ->
            lager:debug("credit allotment ~s ~wsec for session ~s: ~s"
                        ,[LedgerId, Units, SessionId, props:get_value(<<"reason">>, Props, <<"no_reason">>)]);
        <<"debit_allotment">> ->
            lager:debug("debit allotment ~s ~wsec for session ~s: ~s"
                        ,[LedgerId, Units, SessionId, props:get_value(<<"reason">>, Props, <<"no_reason">>)])
    end,
    case props:get_value(<<"pvt_type">>, Props) of
        <<"credit">> ->
            Tr = wh_transaction:credit(abs(Units), per_minute_sub_account),
            Tr1 = wh_transaction:set_call_id(get_call_id(JObj), Tr),
            Tr2 = wh_transaction:set_pvt_account_id(LedgerId, Tr1),
            Tr3 = wh_transaction:set_sub_account_id(get_account_id(JObj), Tr2),
            Tr4 = wh_transaction:set_description(Suffix, Tr3),
            case  wh_transaction:save(Tr4) of
                {ok, Res} ->
                    {ok, wh_transaction:to_json(Res)};
                {error, Res, _E} ->
                    {error, wh_transaction:to_json(Res)}
            end;
        <<"debit">> ->
            Tr = wh_transaction:debit(abs(Units), per_minute_sub_account),
            Tr1 = wh_transaction:set_call_id(get_call_id(JObj), Tr),
            Tr2 = wh_transaction:set_pvt_account_id(LedgerId, Tr1),
            Tr3 = wh_transaction:set_sub_account_id(get_account_id(JObj), Tr2),
            Tr4 = wh_transaction:set_description(Suffix, Tr3),
            case  wh_transaction:save(Tr4) of
                {ok, Res} ->
                    {ok, wh_transaction:to_json(Res)};
                {error, Res, _E} ->
                    {error, wh_transaction:to_json(Res)}
            end;
        _ ->
            Entry = wh_json:from_list([{<<"_id">>, Id}
                                       ,{<<"session_id">>, SessionId}
                                       ,{<<"account_id">>, get_account_id(JObj)}
                                       ,{<<"call_id">>, get_call_id(JObj)}
                                       ,{<<"amount">>, abs(Units)}
                                       ,{<<"pvt_account_id">>, LedgerId}
                                       ,{<<"pvt_account_db">>, LedgerDb}
                                       ,{<<"pvt_created">>, Timestamp}
                                       ,{<<"pvt_modified">>, Timestamp}
                                       ,{<<"pvt_vsn">>, 1}
                                       ,{<<"pvt_whapp">>, ?APP_NAME}
                                       | Props
                                      ]),
            couch_mgr:save_doc(LedgerDb, Entry)
    end. 


get_timestamp(JObj) ->
    case wh_json:get_integer_value(<<"Timestamp">>, JObj) of
        undefined -> wh_json:get_integer_value(<<"timestamp">>, JObj, wh_util:current_tstamp());
        Timestamp -> Timestamp
    end.

get_account_id(JObj) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        undefined ->
            wh_json:get_value([<<"custom_channel_vars">>, <<"account_id">>], JObj);
        AccountId -> AccountId
    end.
-endif.

get_call_id(JObj) ->
    case wh_json:get_value(<<"Call-ID">>, JObj) of
        undefined -> wh_json:get_value(<<"call_id">>, JObj);
        CallId-> CallId
    end.

-spec current_balance(ne_binary()) -> integer().
-ifdef(TEST).
current_balance(_Ledger) -> get(j5_test_balance).
-else.
current_balance(Ledger) ->
    wh_transaction:get_current_balance(Ledger).
-endif.

-spec get_session_id(wh_json:json_object()) -> ne_binary().
get_session_id(JObj) ->
    wh_util:to_hex_binary(crypto:md5(get_call_id(JObj))).

-spec send_system_alert(ne_binary(), wh_json:json_object(), #limits{}) -> pid().
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
                                       MaxPostPay = wapi_money:units_to_dollars(Limits#limits.max_postpay_amount),
                                       wh_json:set_value(<<"Max-Postpay">>, <<"$", (wh_util:to_binary(MaxPostPay))/binary>>, J)
                               end
                              ,fun(J) -> 
                                       ReserveAmount = wapi_money:units_to_dollars(Limits#limits.reserve_amount),
                                       wh_json:set_value(<<"Reserve-Amount">>, <<"$", (wh_util:to_binary(ReserveAmount))/binary>>, J)
                               end
                              ,fun(J) -> 
                                       Balance = wapi_money:units_to_dollars(j5_util:current_balance(AccountId)),
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
