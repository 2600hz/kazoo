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
-module(doodle_outbound_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_sms:outbound_v(JObj),
    _ = kz_log:put_callid(JObj),
    maybe_route(JObj).

-spec maybe_route(kz_json:object()) -> any().
maybe_route(JObj) ->
    Funs = [fun account_has_sms/1
           ,fun trusted_application/1
           ,fun number/1
           ,fun exchange/1
           ],
    Route = kzd_sms:route_id(JObj, <<"default">>),
    route(kz_maps:exec(Funs, #{payload => JObj, route => Route})).

-spec route(map()) -> any().
route(#{payload := Payload}) ->
    kapps_sms_command:send_amqp_sms(Payload, ?OUTBOUND_POOL);
route(_Map) ->
    lager:debug_unsafe("not routing sms ~p", [_Map]).

account_has_sms(#{payload := JObj} = Map) ->
    %% TODO should this verify services ?
    AccountId = kzd_sms:account_id(JObj),
    case kzd_accounts:fetch(AccountId, 'accounts') of
        {'ok', Account} ->
            Map#{account_id => AccountId, enabled => kz_json:is_true(<<"pvt_outbound_sms">>, Account)};
        _ -> Map#{account_id => AccountId, enabled => 'false'}
    end.

trusted_application(#{payload := JObj} = Map) ->
    AppId = kzd_sms:route_id(JObj),
    case kz_json:get_ne_binary_value([<<"default">>, <<"outbound">>, <<"trusted_apps">>, AppId], config()) of
        'undefined' -> Map;
        RouteId -> Map#{enabled => 'true', route => RouteId, payload => kzd_sms:set_route_id(JObj, RouteId)}
    end.

exchange(#{payload := JObj} = Map) ->
    Exchange = kz_json:get_ne_binary_value(?OUTBOUND_EXCHANGE_ARG(<<"name">>), config(), kz_binary:rand_hex(16)),
    Map#{payload => kz_json:set_value(<<"Exchange-ID">>, Exchange, JObj)}.

number(#{enabled := 'false'} = Map) -> Map;
number(#{payload := JObj} = Map) ->
    case knm_phone_number:fetch(kzd_sms:caller_id_number(JObj)) of
        {'ok', Num} ->
            case route_from_number(Num) of
                'undefined' -> Map#{number => Num};
                RouteId -> Map#{number => Num, route => RouteId, payload => kzd_sms:set_route_id(JObj, RouteId)}
            end;
        _ -> Map
    end.

route_from_number(Num) ->
    Mod = knm_phone_number:module_name(Num),
    kz_json:get_ne_binary_value([<<"default">>, <<"outbound">>, <<"knm">>, Mod], config()).

config() ->
    case kapps_config:get_category(?APP_NAME) of
        {'ok', JObj} -> JObj;
        _ -> kz_json:new()
    end.
