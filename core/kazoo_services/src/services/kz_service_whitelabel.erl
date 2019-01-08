%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_whitelabel).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).

-define(WHITELABEL, <<"whitelabel">>).
-define(CATEGORY, <<"branding">>).
-define(ITEM, <<"whitelabel">>).
-define(DESIGN_DOC, <<"whitelabel/crossbar_listing">>).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results_count(AccountDb, ?DESIGN_DOC, []) of
        {'error', _R} ->
            lager:debug("unable to get current whitelabel docs: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', Count} ->
            kz_services:update(?CATEGORY
                              ,?ITEM
                              ,Count
                              ,kz_services:reset_category(?CATEGORY, Services)
                              )
    end.

-spec reconcile(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
reconcile(Services0, ?ITEM=Item) ->
    Services1 = reconcile(Services0),
    Quantity = kz_services:updated_quantity(?CATEGORY, Item, Services1),
    kz_services:update(?CATEGORY, Item, Quantity + 1, Services1).
