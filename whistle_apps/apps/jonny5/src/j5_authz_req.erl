%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    true = wapi_authz:req_v(JObj),
    wh_util:put_callid(JObj),

    io:format("~p~n", [JObj]),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Limits = j5_util:get_limits(AccountId),

    Routines = [fun(_) -> 
                        case calls_at_limit(Limits, JObj) of
                            false -> {ok, under_limit}; 
                            true -> 
                                send_system_alert(<<"call limit">>, JObj, Limits, AccountId),
                                {error, call_limit}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, _}) ->
                         case resource_consumption_at_limit(Limits, JObj) of
                             false -> {ok, under_limit}; 
                             true -> 
                                 send_system_alert(<<"max resource consumption limit">>, JObj, Limits, AccountId),
                                 {error, resource_consumption_limit}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, _}) ->
                         case eligible_for_flat_rate(JObj) 
                             andalso (not trunks_at_limit(Limits, JObj)) 
                         of
                             true -> {ok, flat_rate}; 
                             false -> 
                                 lager:debug("inellegable for flat rate trunks", []),
                                 {error, trunk_limit}
                         end
                 end
                ,fun({error, trunk_limit}) -> 
                         case credit_is_available(Limits, JObj) of
                             true -> {ok, per_minute};
                             false -> 
                                 send_system_alert(<<"no flat rate or credit">>, JObj, Limits, AccountId),
                                 {error, trunk_limit}
                         end;
                    (Else) -> Else
                 end
               ],
    send_resp(JObj, props:get_value(queue, Props), lists:foldl(fun(F, A) -> F(A) end, ok, Routines)).

-spec eligible_for_flat_rate/1 :: (wh_json:json_object()) -> boolean().
eligible_for_flat_rate(JObj) ->
    [Num, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
    Number = wnm_util:to_e164(Num),
    TrunkWhitelist = whapps_config:get(<<"jonny5">>, <<"flat_rate_whitelist">>, <<"^\\+?1\\d{10}$">>),
    %% Example blacklist "^\\+?1(900|800)\\d{7}$"
    TrunkBlacklist = whapps_config:get(<<"jonny5">>, <<"flat_rate_blacklist">>),
    (wh_util:is_empty(TrunkWhitelist) orelse re:run(Number, TrunkWhitelist) =/= nomatch)
        andalso 
          (wh_util:is_empty(TrunkBlacklist) orelse re:run(Number, TrunkBlacklist) =:= nomatch).

-spec send_system_alert/4 :: (ne_binary(), wh_json:json_object(), #limits{}, ne_binary()) -> pid().
send_system_alert(Reason, JObj, Limits, AccountId) ->
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

-spec calls_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
calls_at_limit(#limits{calls=-1}, _) ->
    false;
calls_at_limit(#limits{calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Calls">>], JObj, 0),
    Resources - ConsumedResources =< 0.    

-spec resource_consumption_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
resource_consumption_at_limit(#limits{resource_consuming_calls=-1}, _) ->
    false;
resource_consumption_at_limit(#limits{resource_consuming_calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Resource-Consuming-Calls">>], JObj, 0),
    Resources - ConsumedResources =< 0.

-spec trunks_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
trunks_at_limit(Limits, JObj) ->
    RemainingInbound = consume_inbound_limits(Limits, get_inbound_resources(JObj)),
    consume_twoway_limits(Limits, RemainingInbound + get_outbound_resources(JObj)).    

-spec get_inbound_resources/1 :: (wh_json:json_object()) -> integer().
get_inbound_resources(JObj) ->
    CurrentUsage = wh_json:get_integer_value([<<"Usage">>, <<"Inbound-Flat-Rate">>], JObj, 0),
    case wh_json:get_value(<<"Call-Direction">>, JObj) of
        <<"inbound">> -> CurrentUsage + 1;
        _Else -> CurrentUsage
    end.

-spec get_outbound_resources/1 :: (wh_json:json_object()) -> integer().
get_outbound_resources(JObj) ->
    CurrentUsage = wh_json:get_integer_value([<<"Usage">>, <<"Outbound-Flat-Rate">>], JObj, 0),
    case wh_json:get_value(<<"Call-Direction">>, JObj) of
        <<"outbound">> -> CurrentUsage + 1;
        _Else -> CurrentUsage
    end.            

-spec credit_is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
credit_is_available(Limits, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Balance = j5_util:current_balance(AccountId),
    case prepay_is_available(Limits, Balance, JObj) of
        true -> true;
        false -> postpay_is_available(Limits, Balance, JObj)
    end.             
             
-spec prepay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
prepay_is_available(#limits{allow_prepay=false}, _, _) ->
    false;
prepay_is_available(#limits{allow_prepay=true, reserve_amount=ReserveAmount}, Balance,JObj) ->
    case (Balance - ReserveAmount) > 0 of
        false -> false;             
        true -> 
            AccountId = wh_json:get_value(<<"Account-ID">>, JObj),            
            j5_util:write_debit_to_ledger(<<"start">>, ReserveAmount, JObj, AccountId),
            SessionId = j5_util:get_session_id(JObj),
            lager:debug("reserving $~w for prepay session ~s from ~s", [wapi_money:units_to_dollars(ReserveAmount), SessionId, AccountId]),
            true
    end.

-spec postpay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
postpay_is_available(#limits{allow_postpay=false}, _, _) ->
    false;
postpay_is_available(#limits{allow_postpay=true, max_postpay_amount=MaxPostpay
                             ,reserve_amount=ReserveAmount}, Balance, JObj) ->
    case (Balance - ReserveAmount) > MaxPostpay of
        false -> false;             
        true -> 
            AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
            j5_util:write_debit_to_ledger(<<"start">>, ReserveAmount, JObj, AccountId),
            SessionId = j5_util:get_session_id(JObj),
            lager:debug("reserving $~w for postpay session ~s from ~s", [wapi_money:units_to_dollars(ReserveAmount), SessionId, AccountId]),
            true
    end.

-spec consume_inbound_limits/2 :: (#limits{}, wh_json:json_object()) -> integer().
consume_inbound_limits(#limits{inbound_trunks=-1}, _) ->
    lager:debug("account has unlimited inbound trunks", []),
    0;
consume_inbound_limits(_, 0) -> 
    lager:debug("account is not consuming any inbound resources", []),
    0;
consume_inbound_limits(#limits{inbound_trunks=0}, Resources) ->
    lager:debug("account has no inbound only trunks", []),
    Resources;
consume_inbound_limits(#limits{inbound_trunks=Trunks}, Resources) ->
    lager:debug("account has ~p inbound only trunks and is using ~p inbound resources", [Trunks, Resources]),    
    case Resources - Trunks of
        Count when Count > 0 -> 
            lager:debug("remaining ~p channels unaccounted for by inbound only trunks", [Count]),
            Count;
        _Else -> 
            lager:debug("all inbound channels are accounted for by inbound only trunks", []),
            0
    end.

-spec consume_twoway_limits/2 :: (#limits{}, wh_json:json_object()) -> boolean().
consume_twoway_limits(#limits{twoway_trunks=-1}, _) ->
    lager:debug("account has unlimited twoway trunks", []),
    false;
consume_twoway_limits(_, 0) -> 
    lager:debug("account is not consuming any two-way resources", []),
    false;
consume_twoway_limits(#limits{twoway_trunks=0}, _) -> 
    lager:debug("account has no two-way trunks", []),
    true;
consume_twoway_limits(#limits{twoway_trunks=Trunks}, Resources) ->
    case Resources - Trunks of
        Count when Count > 0 -> 
            lager:debug("remaining ~p two-way channels are unaccounted for by trunks", [Count]),  
            true;
        _Else ->
            lager:debug("all remaining channels are accounted for by two-way trunks", []),
            false
    end.

-spec send_resp/3 :: (wh_json:json_object(),  ne_binary(), {'ok', 'credit' | 'flatrate'} | {'error', _}) -> 'ok'.
send_resp(JObj, Q, {error, _R}) ->
    lager:debug("call is unauthorize due to ~s", [_R]),
    Resp = [{<<"Is-Authorized">>, <<"false">>}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
send_resp(JObj, Q, {ok, Type}) ->
    lager:debug("call is authorized as ~s", [Type]),
    Resp = [{<<"Is-Authorized">>, <<"true">>}
            ,{<<"Type">>, wh_util:to_binary(Type)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
