%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% VCCS Telecom
%%%-------------------------------------------------------------------
-module(wh_service_limits).

-export([reconcile/1, reconcile/2]).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable to get current limits in service: ~p", [_R]),
            Services;
        {'ok', JObj} -> reconcile(Services, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services(), wh_json:object()) -> wh_services:services().
reconcile(Services, JObj) ->
    Routines = [
        fun(S) ->
            Quantity = wh_json:get_integer_value(<<"twoway_trunks">>, JObj, 0),
            wh_services:update(<<"limits">>, <<"twoway_trunks">>, Quantity, S)
        end
        ,fun(S) ->
            Quantity = wh_json:get_integer_value(<<"inbound_trunks">>, JObj, 0),
            wh_services:update(<<"limits">>, <<"inbound_trunks">>, Quantity, S)
         end
        ,fun(S) ->
            Quantity = wh_json:get_integer_value(<<"outbound_trunks">>, JObj, 0),
            wh_services:update(<<"limits">>, <<"outbound_trunks">>, Quantity, S)
         end
    ],
    lists:foldl(fun(F, S) -> F(S) end, wh_services:reset_category(<<"limits">>, Services), Routines).
