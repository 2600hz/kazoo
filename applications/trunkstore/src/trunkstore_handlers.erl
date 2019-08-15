%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
