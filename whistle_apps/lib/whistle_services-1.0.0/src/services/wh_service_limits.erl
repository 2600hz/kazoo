%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_limits).

-export([reconcile/1]).

-include_lib("whistle_services/src/whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/1 :: (wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable to get current limits in service: ~p", [_R]),
            Services;
        {'ok', JObj} ->
            Routines = [fun(S) ->
                                Quantity = wh_json:get_integer_value(<<"twoway_trunks">>, JObj, 0),
                                wh_services:update(<<"limits">>, <<"twoway_trunks">>, Quantity, S)
                        end
                        ,fun(S) ->
                                 Quantity = wh_json:get_integer_value(<<"inbound_trunks">>, JObj, 0),
                                 wh_services:update(<<"limits">>, <<"inbound_trunks">>, Quantity, S)
                         end
                       ],
            lists:foldl(fun(F, S) -> F(S) end, wh_services:reset_category(<<"limits">>, Services), Routines)
    end.
