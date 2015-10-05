%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_handlers).

-export([handle_config_change/2]).

-include("ts.hrl").

-spec handle_config_change(wh_json:object(), _) -> _.
handle_config_change(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    lager:info("trunkstore doc change detected: ~p", [JObj]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    trunkstore_maintenance:flush(AccountId).
