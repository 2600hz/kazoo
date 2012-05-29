%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_numbers).

-export([update/1]).

-spec update/1 :: (wh_json:json_object()) -> ok.
update(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    {ok, Resellers} = wh_resellers:fetch(AccountId),
    lists:foldr(fun(PhoneNumber, R) ->
                        update_number(R, JObj, PhoneNumber)
                end, Resellers, wh_json:get_keys(wh_json:public_fields(JObj))).

update_number(Resellers, JObj, PhoneNumber) ->
    case wh_json:is_true([PhoneNumber, <<"local_number">>], JObj) of
        true -> Resellers;
        false ->
            Routines = [fun(R) ->
                                wh_resellers:increment_quantity(<<"phone_numbers">>, PhoneNumber, R)
                        end
                        ,fun(R) ->
                                 Number = wh_json:get_value(PhoneNumber, JObj, wh_json:new()),
                                 Services = [Service
                                             || Service <- wh_json:get_keys(Number)
                                                    ,wh_json:is_true(Service, Number)
                                            ],
                                 update_number_services(R, Services)
                         end
                       ],
            lists:foldr(fun(F, R) -> F(R) end, Resellers, Routines)
    end.

update_number_services(Resellers, Services) ->    
    lists:foldr(fun(Service, R) ->
                        wh_resellers:increment_quantity(<<"number_services">>, Service, R)
                end, Resellers, Services).

