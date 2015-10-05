%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_delivery_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_sms:delivery_v(JObj),
    _ = wh_util:put_callid(JObj),
    maybe_update_doc(JObj).

-spec maybe_update_doc(wh_json:object()) -> _.
maybe_update_doc(JObj) ->
    DeliveryCode = wh_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = wh_json:get_value(<<"Status">>, JObj),
    Value = case {Status, DeliveryCode} of
                {_ , <<"200">> } -> <<"delivered">>;
                {_ , <<"202">> } -> <<"accepted">>;
                {<<"Success">>, _ } -> <<"completed">>;
                _Else -> <<"pending">>
            end,
    update_doc(JObj, Value).

-spec update_doc(wh_json:object(), ne_binary()) -> _.
update_doc(JObj, Value) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, CCVs),
    case kazoo_modb:open_doc(AccountId, CallId) of
        {'ok', Doc} ->
            UpdatedDoc = wh_json:set_value(<<"pvt_status">>, Value, Doc),
            kazoo_modb:save_doc(AccountId, UpdatedDoc);
        {'error', _E} ->
            lager:debug("error reading doc ~s from modb in account ~s",[CallId, AccountId])
    end.
