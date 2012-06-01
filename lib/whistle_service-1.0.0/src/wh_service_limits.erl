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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the account limit document reconcile the service
%% charges
%% @end
%%--------------------------------------------------------------------
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
    R1 = wh_resellers:reset_category_addons(<<"limits">>, Resellers),
    Limits = wh_json:get_keys(wh_json:public_fields(JObj)),
    lists:foldr(fun(Limit, R) ->
                        Quantity = wh_json:get_value(Limit, JObj, 0), 
                        case Quantity =< 0 of
                            true -> wh_resellers:update_quantity(<<"limits">>, Limit, 0, R);
                            false -> wh_resellers:update_quantity(<<"limits">>, Limit, Quantity, R)
                        end
                end, R1, Limits).
