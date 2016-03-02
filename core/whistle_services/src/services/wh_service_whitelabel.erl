%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_service_whitelabel).

-export([reconcile/1, reconcile/2]).

-define(WHITELABEL, <<"whitelabel">>).
-define(CATEGORY, <<"branding">>).
-define(ITEM, <<"whitelabel">>).
-define(DESIGN_DOC, <<"whitelabel/crossbar_listing">>).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), ne_binary()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results_count(AccountDb, ?DESIGN_DOC, []) of
        {'error', _R} ->
            lager:debug("unable to get current whitelabel docs: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', Count} ->
            wh_services:update(
                ?CATEGORY
                ,?ITEM
                ,Count
                ,wh_services:reset_category(?CATEGORY, Services)
            )
    end.

reconcile(Services0, ?ITEM=Item) ->
    Services1 = reconcile(Services0),
    Quantity = wh_services:updated_quantity(?CATEGORY, Item, Services1),
    wh_services:update(?CATEGORY, Item, Quantity + 1, Services1).
