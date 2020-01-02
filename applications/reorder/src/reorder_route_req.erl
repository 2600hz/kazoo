%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc handler for route requests, responds if reorder match
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(reorder_route_req).

-include("reorder.hrl").

-export([handle_req/2]).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    case kz_term:is_empty(AccountId) of
        'false' -> 'ok';
        'true' ->
            lager:debug("received route request with no account-id"),
            ControllerQ = props:get_value('queue', Props),
            maybe_known_number(ControllerQ, JObj)
    end.

-spec maybe_known_number(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_known_number(ControllerQ, JObj) ->
    Number = get_dest_number(JObj),
    case knm_number:lookup_account(Number) of
        {'ok', _, _} -> choose_response(ControllerQ, JObj, 'false', <<"known_number">>);
        {'error', _R} ->
            lager:debug("~s is not associated with any account, ~p", [Number, _R]),
            Reconcilable = knm_converters:is_reconcilable(Number),
            choose_response(ControllerQ, JObj, Reconcilable, <<"unknown_number">>)
    end.

-spec choose_response(kz_term:ne_binary(), kz_json:object(), boolean(), kz_term:ne_binary()) -> 'ok'.
choose_response(ControllerQ, JObj, Reconcilable, Type) ->
    case kapps_config:get_ne_binary(?CONFIG_CAT, [Type, <<"action">>], <<"respond">>) of
        <<"respond">> -> send_response(JObj, ControllerQ, Reconcilable, Type);
        <<"transfer">> -> maybe_send_transfer(JObj, ControllerQ, Reconcilable, Type);
        <<"bridge">> -> maybe_send_bridge(JObj, ControllerQ, Reconcilable, Type)
    end.

-spec send_response(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary()) -> 'ok'.
send_response(JObj, ControllerQ, Reconcilable, <<"unknown_number">> = Type) ->
    Code = kapps_config:get_binary(?APP_NAME, [Type, <<"response_code">>], <<"604">>),
    Message = kapps_config:get_binary(?APP_NAME, [Type, <<"response_message">>], <<"Nope Nope Nope">>),
    send_response(JObj, ControllerQ, Reconcilable, Code, Message);
send_response(JObj, ControllerQ, Reconcilable, <<"known_number">> = Type) ->
    Code = kapps_config:get_binary(?APP_NAME, [Type, <<"response_code">>], <<"686">>),
    Message = kapps_config:get_binary(?APP_NAME, [Type, <<"response_message">>], <<"PICNIC">>),
    send_response(JObj, ControllerQ, Reconcilable, Code, Message).

-spec send_response(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
send_response(JObj, ControllerQ, Reconcilable, Code, Message) ->
    lager:debug("sending response: ~s ~s", [Code, Message]),
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Method">>, <<"error">>}
           ,{<<"Route-Error-Code">>, Code}
           ,{<<"Route-Error-Message">>, Message}
           ,{<<"Defer-Response">>, (not Reconcilable)}
            | kz_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec maybe_send_transfer(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary()) -> 'ok'.
maybe_send_transfer(JObj, ControllerQ, Reconcilable, Type) ->
    case kapps_config:get_ne_binary(?CONFIG_CAT, [Type, <<"transfer_target">>]) of
        'undefined' -> send_response(JObj, ControllerQ, Reconcilable, Type);
        Number -> send_transfer(JObj, ControllerQ, Reconcilable, Number)
    end.

-spec send_transfer(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary()) -> 'ok'.
send_transfer(JObj, ControllerQ, Reconcilable, Number) ->
    lager:debug("sending transfer to ~s", [Number]),
    Route = kz_json:from_list([{<<"Invite-Format">>, <<"loopback">>}
                              ,{<<"Route">>, Number}
                              ,{<<"To-DID">>, Number}
                              ]),
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Method">>, <<"bridge">>}
           ,{<<"Routes">>, [Route]}
           ,{<<"Defer-Response">>, (not Reconcilable)}
            | kz_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec maybe_send_bridge(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary()) -> 'ok'.
maybe_send_bridge(JObj, ControllerQ, Reconcilable, Type) ->
    AccountId = kapps_config:get_ne_binary(?CONFIG_CAT, [Type, <<"bridge_account_id">>]),
    EndpointId = kapps_config:get_ne_binary(?CONFIG_CAT, [Type, <<"bridge_endpoint_id">>]),
    case kz_term:is_empty(AccountId)
        orelse kz_term:is_empty(EndpointId)
    of
        'true' -> send_response(JObj, ControllerQ, Reconcilable, Type);
        'false' ->
            Routines = [{fun kapps_call:set_account_id/2, AccountId}],
            Call = kapps_call:exec(Routines, kapps_call:from_route_req(JObj)),
            Endpoint = kz_endpoint:build(EndpointId, Call),
            send_bridge(JObj, ControllerQ, Reconcilable, Type, Endpoint)
    end.

-spec send_bridge(kz_json:object(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kz_term:jobjs_return()) -> 'ok'.
send_bridge(JObj, ControllerQ, Reconcilable, _Type, {'ok', Routes}) ->
    lager:debug("sending bridge to endpoint", []),
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Method">>, <<"bridge">>}
           ,{<<"Routes">>, Routes}
           ,{<<"Defer-Response">>, (not Reconcilable)}
            | kz_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp);
send_bridge(JObj, ControllerQ, Type, Reconcilable, _) ->
    send_response(JObj, ControllerQ, Type, Reconcilable).

-spec get_dest_number(kz_json:object()) -> kz_term:ne_binary().
get_dest_number(JObj) ->
    {User, _} = kapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case kapps_config:get_is_true(?CONFIG_CAT, <<"assume_inbound_e164">>, false) of
        'true' ->
            Number = assume_e164(User),
            lager:debug("assuming number is e164, normalizing to ~s", [Number]),
            Number;
        _ ->
            Number = knm_converters:normalize(User),
            lager:debug("converted number to e164: ~s", [Number]),
            Number
    end.

-spec assume_e164(kz_term:ne_binary()) -> kz_term:ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.
