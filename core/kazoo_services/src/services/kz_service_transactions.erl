%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_transactions).

-export([current_billing_period/2]).
-export([current_billing_period/3]).
-export([reconcile/1]).

-include("kazoo_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec current_billing_period(ne_binary(), atom()) ->
                                    kz_json:objects() | atom().
current_billing_period(AccountId, 'subscriptions') ->
    kz_bookkeeper_braintree:subscriptions(AccountId).

-spec current_billing_period(ne_binary(), atom(), {gregorian_seconds(), gregorian_seconds()}) ->
                                    {'error', 'not_found'} |
                                    {'error', 'unknown_error'} |
                                    {'ok', kz_transaction:transactions()}.
current_billing_period(AccountId, 'transactions', {Min, Max}) ->
    kz_bookkeeper_braintree:transactions(AccountId, Min, Max).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    Services.
