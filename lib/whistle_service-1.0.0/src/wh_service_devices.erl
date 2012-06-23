%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_devices).

-export([update/2, update/3]).
-export([activate_device_type/2]).

-include("wh_service.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec activate_device_type/2 :: (ne_binary(), wh_resellers:resellers()) -> wh_resellers:resellers().
activate_device_type(DeviceType, Resellers) ->
    wh_resellers:process_activation_charges(<<"devices">>, DeviceType, Resellers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the account limit document reconcile the service
%% charges
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: ([] | [ne_binary(),...], wh_resellers:resellers()) -> wh_resellers:resellers().
-spec update/3 :: ([] | [ne_binary(),...], wh_resellers:resellers(), 'created' | 'deleted' | 'updated') -> wh_resellers:resellers().

update(DeviceTypes, Resellers) ->
    update(DeviceTypes, Resellers, updated).

update(DeviceTypes, Resellers, Action) ->
    Routines = [fun(R) -> wh_resellers:reset_category_addons(<<"devices">>, R) end
                ,fun(R) ->
                         lists:foldr(fun(DeviceType, R1) ->
                                             wh_resellers:increment_quantity(<<"devices">>, DeviceType, R1)
                                     end, R, DeviceTypes)
                 end
                ,fun(R) ->
                         case Action of
                             created -> wh_resellers:set_created_flag(<<"device">>, R);
                             deleted -> wh_resellers:set_deleted_flag(<<"device">>, R);
                             updated -> R
                         end
                 end 
               ],
    lists:foldl(fun(F, R) -> F(R) end, Resellers, Routines).
