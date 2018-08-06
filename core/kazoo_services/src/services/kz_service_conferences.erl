%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_conferences).
-behaviour(kz_gen_service).

-export([reconcile/1]).
-export([reconcile/2]).

-include("services.hrl").

-define(SERVICE_CATEGORY, <<"conferences">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                  ,'group'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"services/conferences">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current conferences in service: ~p", [_R]),
            Services;
        {'ok', []} -> kz_services:reset_category(?SERVICE_CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = kz_json:get_value(<<"key">>, JObj),
                                Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),
                                kz_services:update(?SERVICE_CATEGORY, Item, Quantity, S)
                        end, kz_services:reset_category(?SERVICE_CATEGORY, Services), JObjs)
    end.

-spec reconcile(kz_services:services(), kz_json:object()) -> kz_services:services().
reconcile(Services, JObj) ->
    Routines = [fun reconcile_faxbox/2],
    lists:foldl(fun(F, S) ->
                        F(S, JObj)
                end
               ,reconcile(Services)
               ,Routines
               ).

-spec reconcile_faxbox(kz_services:services(), kz_json:object()) -> kz_services:services().
reconcile_faxbox(Services, JObj) ->
    case kz_doc:type(JObj) =:= <<"conference">> of
        'false' -> Services;
        'true' ->
            Quantity = kz_services:updated_quantity(?SERVICE_CATEGORY, <<"conference">>, Services),
            kz_services:update(?SERVICE_CATEGORY, <<"conference">>, Quantity+1, Services)
    end.
