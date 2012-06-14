%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_numbers).

-export([activate_phone_number/2]).
-export([activate_feature/2]).
-export([update/2]).

-include("wh_service.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec activate_phone_number/2 :: (ne_binary(), wh_resellers:resellers()) -> wh_resellers:resellers().
activate_phone_number(Number, Resellers) ->
    wh_resellers:process_activation_charges(<<"phone_numbers">>, Number, Resellers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec activate_feature/2 :: (ne_binary(), wh_resellers:resellers()) -> wh_resellers:resellers().
activate_feature(Feature, Resellers) ->
    wh_resellers:process_activation_charges(<<"number_services">>, Feature, Resellers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the account phone_number document reconcile the service
%% charges
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (wh_json:json_object(), wh_resellers:resellers()) -> wh_resellers:resellers().
update(JObj, Resellers) ->
    Routines = [fun(R) -> wh_resellers:reset_category_addons(<<"phone_numbers">>, R) end
                ,fun(R) -> wh_resellers:reset_category_addons(<<"number_services">>, R) end
                ,fun(R) -> 
                         lists:foldr(fun(PhoneNumber, R1) ->
                                             update_number(JObj, PhoneNumber, R1)
                                     end, R, wh_json:get_keys(wh_json:public_fields(JObj)))
                 end
               ],
    lists:foldl(fun(F, R) -> F(R) end, Resellers, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_number/3 :: (wh_json:json_object(), ne_binary(), wh_resellers:resellers()) ->  wh_resellers:resellers().
update_number(JObj, PhoneNumber, Resellers) ->
    case wh_json:is_true([PhoneNumber, <<"local_number">>], JObj) of
        true -> Resellers;
        false ->
            Routines = [fun(R) -> wh_resellers:increment_quantity(<<"phone_numbers">>, PhoneNumber, R) end
                        ,fun(R) ->
                                 Services = wh_json:get_value([PhoneNumber, <<"features">>], JObj, []),
                                 update_number_services(Services, R)
                         end
                       ],
            lists:foldl(fun(F, R) -> F(R) end, Resellers , Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_number_services/2 :: ([ne_binary(),...] | [], wh_resellers:resellers()) ->  wh_resellers:resellers().
update_number_services(Services, Resellers) ->    
    lists:foldr(fun(Service, R) ->
                        wh_resellers:increment_quantity(<<"number_services">>, Service, R)
                end, Resellers, Services).

