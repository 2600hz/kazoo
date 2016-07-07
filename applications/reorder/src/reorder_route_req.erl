%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, 2600Hz
%%% @doc
%%% handler for route requests, responds if reorder match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(reorder_route_req).

-include("reorder.hrl").

-export([handle_req/2]).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    case kz_util:is_empty(AccountId) of
        'false' -> 'ok';
        'true' ->
            lager:debug("received route request with no account-id"),
            ControllerQ = props:get_value('queue', Props),
            maybe_known_number(ControllerQ, JObj)
    end.

-spec maybe_known_number(ne_binary(), kz_json:object()) -> 'ok'.
maybe_known_number(ControllerQ, JObj) ->
    Number = get_dest_number(JObj),
    case knm_number:lookup_account(Number) of
        {'ok', _, _} -> send_known_number_response(JObj, ControllerQ);
        {'error', _R} ->
            lager:debug("~s is not associated with any account, ~p", [Number, _R]),
            send_unknown_number_response(JObj, ControllerQ)
    end.

-spec get_dest_number(kz_json:object()) -> ne_binary().
get_dest_number(JObj) ->
    {User, _} = kapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case kapps_config:get_is_true(?CONFIG_CAT, <<"assume_inbound_e164">>) of
        'true' ->
            Number = assume_e164(User),
            lager:debug("assuming number is e164, normalizing to ~s", [Number]),
            Number;
        _ ->
            Number = knm_converters:normalize(User),
            lager:debug("converted number to e164: ~s", [Number]),
            Number
    end.

-spec assume_e164(ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.

-spec send_known_number_response(kz_json:object(), ne_binary()) -> 'ok'.
send_known_number_response(JObj, Q) ->
    ErrorCode = kapps_config:get_binary(?APP_NAME, <<"known-error-code">>, <<"686">>),
    ErrorMsg = kapps_config:get_binary(?APP_NAME, <<"known-error-message">>, <<"PEBCAK">>),
    lager:debug("sending known number response: ~s ~s", [ErrorCode, ErrorMsg]),
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Method">>, <<"error">>}
           ,{<<"Route-Error-Code">>, ErrorCode}
           ,{<<"Route-Error-Message">>, ErrorMsg}
           ,{<<"Defer-Response">>, <<"true">>}
            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_unknown_number_response(kz_json:object(), ne_binary()) -> 'ok'.
send_unknown_number_response(JObj, Q) ->
    ErrorCode = kapps_config:get_binary(?APP_NAME, <<"unknown-error-code">>, <<"604">>),
    ErrorMsg = kapps_config:get_binary(?APP_NAME, <<"unknown-error-message">>, <<"Nope Nope Nope">>),
    lager:debug("sending unknown number response: ~s ~s", [ErrorCode, ErrorMsg]),
    Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
           ,{<<"Method">>, <<"error">>}
           ,{<<"Route-Error-Code">>, ErrorCode}
           ,{<<"Route-Error-Message">>, ErrorMsg}
           ,{<<"Defer-Response">>, <<"true">>}
            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).
