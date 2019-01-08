%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_transactions).
-behaviour(kz_gen_service).

-export([current_billing_period/2]).
-export([current_billing_period/3]).
-export([reconcile/1]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec current_billing_period(kz_term:ne_binary(), atom()) ->
                                    kz_json:objects() | atom().
current_billing_period(AccountId, 'subscriptions') ->
    kz_bookkeeper_braintree:subscriptions(AccountId).

-spec current_billing_period(kz_term:ne_binary(), atom(), {kz_time:gregorian_seconds(), kz_time:gregorian_seconds()}) ->
                                    {'error', 'not_found'} |
                                    {'error', 'unknown_error'} |
                                    {'ok', kz_transaction:transactions()}.
current_billing_period(AccountId, 'transactions', {Min, Max}) ->
    kz_bookkeeper_braintree:transactions(AccountId, Min, Max).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    Services.
