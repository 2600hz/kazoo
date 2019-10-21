%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_delivery_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_sms:delivery_v(JObj),
    _ = kz_log:put_callid(JObj),
    maybe_update_doc(JObj).

-spec maybe_update_doc(kz_json:object()) -> any().
maybe_update_doc(JObj) ->
    DeliveryCode = kz_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = kz_json:get_value(<<"Status">>, JObj),
    Value = case {Status, DeliveryCode} of
                {_ , <<"200">> } -> <<"delivered">>;
                {_ , <<"202">> } -> <<"accepted">>;
                {<<"Success">>, _ } -> <<"completed">>;
                _Else -> <<"pending">>
            end,
    update_doc(JObj, Value).

-spec update_doc(kz_json:object(), kz_term:ne_binary()) -> any().
update_doc(JObj, Value) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, CCVs),
    case kazoo_modb:open_doc(AccountId, CallId) of
        {'ok', Doc} ->
            UpdatedDoc = kz_json:set_value(<<"pvt_status">>, Value, Doc),
            kazoo_modb:save_doc(AccountId, UpdatedDoc);
        {'error', _E} ->
            lager:debug("error reading doc ~s from modb in account ~s",[CallId, AccountId])
    end.
