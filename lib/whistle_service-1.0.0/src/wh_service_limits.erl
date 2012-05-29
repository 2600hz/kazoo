%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_limits).

-export([update/1, update/2]).

-include("wh_service.hrl").

-spec update/1 :: (wh_json:json_object()) -> wh_resellers:resellers().
-spec update/2 :: (wh_json:json_object(), wh_resellers:resellers()) -> wh_resellers:resellers().

update(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    {ok, Resellers} = wh_resellers:fetch(AccountId),
    update(JObj, Resellers).

update(JObj, Resellers) ->
    Limits = wh_json:get_keys(wh_json:public_fields(JObj)),
    lists:foldr(fun(Limit, R) ->
                        Quantity = wh_json:get_value(Limit, JObj, 0), 
                        case Quantity =< 0 of
                            true -> wh_resellers:update_quantity(<<"limits">>, Limit, 0, R);
                            false -> wh_resellers:update_quantity(<<"limits">>, Limit, Quantity, R)
                        end
                end, Resellers, Limits).
