%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_util).

-export([listen_on_other_leg/2]).
-export([send_hangup_req/1]).
-export([send_break_req/1]).
-export([maybe_start_metaflow/2]).

-include("konami.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-spec listen_on_other_leg(kapps_call:call(), kz_term:ne_binaries()) -> 'ok'.
listen_on_other_leg(Call, Events) ->
    API = [{<<"Application-Name">>, <<"noop">>}
          ,{<<"B-Leg-Events">>, Events}
          ,{<<"Insert-At">>, <<"now">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    kapps_call_command:send_command(API, Call).

-spec send_hangup_req(kz_term:ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"hangup">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

-spec send_break_req(kz_term:ne_binary()) -> 'ok'.
send_break_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"break">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to break ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

-spec maybe_start_metaflow(kapps_call:call(), kz_json:object()) -> 'ok'.
maybe_start_metaflow(Call, Endpoint) ->
    case not is_sms(Call)
        andalso kz_json:get_first_defined([<<"metaflows">>, <<"Metaflows">>], Endpoint)
    of
        'false' -> 'ok';
        'undefined' -> 'ok';
        ?EMPTY_JSON_OBJECT -> 'ok';
        Metaflow ->
            Id = kz_json:get_first_defined([<<"_id">>, <<"Endpoint-ID">>], Endpoint),
            API = props:filter_undefined(
                    [{<<"Endpoint-ID">>, Id}
                    ,{<<"Account-ID">>, kapps_call:account_id(Call)}
                    ,{<<"Call">>, kapps_call:to_json(Call)}
                    ,{<<"Numbers">>, kzd_metaflows:numbers(Metaflow)}
                    ,{<<"Patterns">>, kzd_metaflows:patterns(Metaflow)}
                    ,{<<"Binding-Digit">>, kzd_metaflows:binding_digit(Metaflow)}
                    ,{<<"Digit-Timeout">>, kzd_metaflows:digit_timeout(Metaflow)}
                    ,{<<"Listen-On">>, kzd_metaflows:listen_on(Metaflow, <<"self">>)}
                     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ]),
            lager:debug("sending metaflow for endpoint: ~s: ~s"
                       ,[Id, kzd_metaflows:listen_on(Metaflow, <<"self">>)]
                       ),
            kapps_util:amqp_pool_send(API, fun kapi_metaflow:publish_binding/1)
    end.

-spec is_sms(kapps_call:call()) -> boolean().
is_sms(Call) ->
    kapps_call:resource_type(Call) =:= <<"sms">>.

