%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_maintenance).

-export([hooks_configured/0, hooks_configured/1]).

hooks_configured() ->
    webhooks_listener:hooks_configured(),
    'ok'.

hooks_configured(AccountId) ->
    webhooks_listener:hooks_configured(AccountId),
    'ok'.
