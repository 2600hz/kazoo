%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_ips).

-export([reconcile/1]).

-include("../whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/1 :: (wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    case kz_ips:assigned(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current dedicated ips in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            S = wh_services:reset_category(<<"ips">>, Services),
            wh_services:update(<<"ips">>, <<"dedicated">>, length(JObjs), S)
    end.
