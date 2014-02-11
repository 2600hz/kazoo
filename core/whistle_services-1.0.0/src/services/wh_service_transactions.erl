%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_transactions).

-export([current_billing_period/2]).
-export([current_billing_period/3]).
-export([reconcile/1]).

-include("../whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec current_billing_period(ne_binary(), atom()) ->
                                    wh_json:objects() | atom().
current_billing_period(AccountId, 'subscriptions') ->
    wh_bookkeeper_braintree:subscriptions(AccountId).

-spec current_billing_period(ne_binary(), atom(), {ne_binary(), ne_binary()}) ->
                                    wh_json:objects() | atom().
current_billing_period(AccountId, 'transactions', {Min, Max}) ->
    wh_bookkeeper_braintree:transactions(AccountId, Min, Max).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    Services.
