%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_numbers).

-export([update/1, update/2]).

-include("wh_service.hrl").

-spec update/1 :: (wh_json:json_object()) -> 'ok' | {'error', _}.
-spec update/2 :: (wh_json:json_object(), wh_resellers:resellers()) -> wh_resellers:resellers().

update(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case wh_resellers:fetch(AccountId) of
        {ok, []} -> ok;
        {ok, Resellers} ->
            Updates = update(JObj, Resellers),
            wh_resellers:commit_changes(Updates)
    end.

update(JObj, Resellers) ->
    R1 = wh_resellers:reset_category_addons(<<"phone_numbers">>, Resellers),
    R2 = wh_resellers:reset_category_addons(<<"number_services">>, R1),
    lists:foldr(fun(PhoneNumber, R) ->
                        update_number(JObj, PhoneNumber, R)
                end, R2, wh_json:get_keys(wh_json:public_fields(JObj))).


-spec update_number/3 :: (wh_json:json_object(), ne_binary(), wh_resellers:resellers()) ->  wh_resellers:resellers().
update_number(JObj, PhoneNumber, Resellers) ->
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
                                 update_number_services(Services, R)
                         end
                       ],
            lists:foldr(fun(F, R) -> F(R) end, Resellers , Routines)
    end.

-spec update_number_services/2 :: ([ne_binary(),...] | [], wh_resellers:resellers()) ->  wh_resellers:resellers().
update_number_services(Services, Resellers) ->    
    lists:foldr(fun(Service, R) ->
                        wh_resellers:increment_quantity(<<"number_services">>, Service, R)
                end, Resellers, Services).

