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
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Limits = j5_util:get_limits(AccountId),
    Routines = [fun(_) -> 
                        case calls_at_limit(Limits, JObj) of
                            false -> {ok, under_limit}; 
                            true -> {error, call_limit}
                        end
                end
                ,fun({error, _}=E) -> E;
                    ({ok, _}) ->
                         case resource_consumption_at_limit(Limits, JObj) of
                             false -> {ok, under_limit}; 
                             true -> {error, resource_consumption_limit}
                         end
                 end
                ,fun({error, _}=E) -> E;
                    ({ok, _}) ->
                         case trunks_at_limit(Limits, JObj) of
                             false -> {ok, flat_rate}; 
                             true -> {error, trunk_limit}
                         end
                 end
                ,fun({error, trunk_limit}) -> 
                         case credit_is_available(Limits, JObj) of
                             true -> {ok, per_minute};
                             false -> {error, trunk_limit}
                         end;
                    (Else) -> Else
                 end
               ],
    send_resp(JObj, props:get_value(queue, Props), lists:foldl(fun(F, A) -> F(A) end, ok, Routines)).

-spec calls_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
calls_at_limit(#limits{calls=-1}, _) ->
    false;
calls_at_limit(#limits{calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Calls">>], JObj, 0),
    Resources - ConsumedResources < 0.    

-spec resource_consumption_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
resource_consumption_at_limit(#limits{resource_consuming_calls=-1}, _) ->
    false;
resource_consumption_at_limit(#limits{resource_consuming_calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Resource-Consuming-Calls">>], JObj, 0),
    Resources - ConsumedResources < 0.

-spec trunks_at_limit/2 :: (#limits{}, wh_json:json_object()) -> boolean().
trunks_at_limit(Limits, JObj) ->
    InboundResources = wh_json:get_integer_value([<<"Usage">>, <<"Inbound-Flat-Rate">>], JObj, 0),
    RemainingInbound = consume_inbound_limits(Limits, InboundResources),
    OutboundResources = wh_json:get_integer_value([<<"Usage">>, <<"Outbound-Flat-Rate">>], JObj, 0),
    consume_twoway_limits(Limits, RemainingInbound + OutboundResources) < 0.    

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
            j5_util:write_debit_to_ledger(<<"start">>, ReserveAmount, JObj),
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
            j5_util:write_debit_to_ledger(<<"start">>, ReserveAmount, JObj),
            true
    end.

-spec consume_inbound_limits/2 :: (#limits{}, wh_json:json_object()) -> integer().
consume_inbound_limits(#limits{inbound_trunks=-1}, _) ->
    0;
consume_inbound_limits(#limits{inbound_trunks=Trunks}, Resources) ->
    case Trunks - Resources of
        Count when Count >= 0 -> 0;
        Count -> abs(Count)
    end.

-spec consume_twoway_limits/2 :: (#limits{}, wh_json:json_object()) -> integer().
consume_twoway_limits(#limits{twoway_trunks=-1}, _) ->
    0;
consume_twoway_limits(#limits{twoway_trunks=Trunks}, Resources) ->
    case Trunks - Resources of
        Count when Count >= 0 -> 0;
        Count -> Count
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
