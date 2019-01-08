%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(trunkstore_handlers).

-export([handle_config_change/2]).

-include("ts.hrl").

-spec handle_config_change(kz_json:object(), any()) -> any().
handle_config_change(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    lager:info("trunkstore doc change detected: ~p", [JObj]),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    trunkstore_maintenance:flush(AccountId).
