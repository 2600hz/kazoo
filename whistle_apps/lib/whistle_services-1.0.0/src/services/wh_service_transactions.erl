%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_transactions).

-export([current_billing_period/2]).
-export([current_billing_period/3]).

-include("../whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec current_billing_period(ne_binary(), atom()) -> [wh_json:object(), ...] | atom().
current_billing_period(AccountId, 'subscriptions') ->
    wh_bookkeeper_braintree:subscriptions(AccountId).

-spec current_billing_period(ne_binary(), atom(), tuple()) -> [wh_json:object(), ...] | atom().
current_billing_period(AccountId, 'transactions', {Min, Max}) ->
    wh_bookkeeper_braintree:transactions(AccountId, Min, Max).
