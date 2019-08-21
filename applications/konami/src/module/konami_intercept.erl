%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc data:
%%%   target_type: "user" or "device"
%%%   target_id: Device-ID or User-ID where Call-ID will be bridged (intercepted)
%%%
%%% @author Peter Defebvre
%%% @author Sergey Korobkov
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_intercept).

-export([handle/2]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()} |
                    {'stop', kapps_call:call()}.
handle(Data, Call) ->
    case send_originate_req(get_originate_req(Data, Call), Call) of
        {'ok', Result} ->
            maybe_update_metaflow(Data, Call, Result);
        {'error', _E} ->
            lager:debug("failed to originate: ~p", [_E]),
            {'stop', Call};
        {'timeout', Result} ->
            lager:debug("timed out, here's what we got: ~p", [Result]),
            maybe_update_metaflow(Data, Call, Result)
    end.

-spec maybe_update_metaflow(kz_json:object(), kapps_call:call(), kz_json:objects()) ->
                                   {'stop', kapps_call:call()}.
maybe_update_metaflow(Data, Call, Results) ->
    case [Result || Result <- Results, is_resp(Result)] of
        [] ->
            lager:debug("no useful responses"),
            {'stop', Call};
        [Resp|_] ->
            CallId = kz_json:get_value(<<"Call-ID">>, Resp),
            maybe_update_metaflow(Data, Call, Results, CallId)
    end.

-spec maybe_update_metaflow(kz_json:object(), kapps_call:call(), kz_json:objects(), kz_term:api_binary()) ->
                                   {'stop', kapps_call:call()}.
maybe_update_metaflow(Data, Call, Results, CallId) ->
    case [Result || Result <- Results, is_originate_uuid(Result, CallId)] of
        [] ->
            lager:debug("no matching originate_uuid"),
            {'stop', Call};
        [OriginateUUID|_] ->
            ControlQueue = kz_json:get_value(<<"Outbound-Call-Control-Queue">>, OriginateUUID),
            lager:debug("should use ~s for control of ~s", [ControlQueue, CallId]),
            maybe_update_metaflow_control(Data, Call, CallId, ControlQueue, source_leg_of_dtmf(Data, Call))
    end.

-spec maybe_update_metaflow_control(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), 'a' | 'b') ->
                                           {'stop', kapps_call:call()}.
maybe_update_metaflow_control(_Data, Call, CallId, ControlQueue, 'a') ->
    lager:debug("update ~s to ~s with ctl ~s", [kapps_call:call_id(Call), CallId, ControlQueue]),

    konami_code_statem:transfer_to(kapps_call:set_control_queue(ControlQueue
                                                               ,kapps_call:set_call_id(CallId, Call)
                                                               )
                                  ,'a'
                                  ),

    {'stop', Call};
maybe_update_metaflow_control(_Data, Call, CallId, _ControlQueue, 'b') ->
    lager:debug("update ~s to ~s with ctl ~s", [kapps_call:other_leg_call_id(Call), CallId, _ControlQueue]),

    konami_code_statem:transfer_to(kapps_call:set_other_leg_call_id(CallId, Call)
                                  ,'b'
                                  ),

    {'stop', Call}.

-spec source_leg_of_dtmf(kz_term:ne_binary() | kz_json:object(), kapps_call:call()) -> 'a' | 'b'.
source_leg_of_dtmf(<<_/binary>> = SourceDTMF, Call) ->
    case kapps_call:call_id(Call) =:= SourceDTMF of
        'true' -> 'a';
        'false' -> 'b'
    end;
source_leg_of_dtmf(Data, Call) ->
    source_leg_of_dtmf(kz_json:get_value(<<"dtmf_leg">>, Data), Call).

-spec get_originate_req(kz_json:object(), kapps_call:call()) -> kz_term:proplist().
get_originate_req(Data, Call) ->
    SourceOfDTMF = kz_json:get_value(<<"dtmf_leg">>, Data),
    TargetType = kz_json:get_value(<<"target_type">>, Data),
    TargetId = kz_json:get_value(<<"target_id">>, Data),
    UnbridgedOnly = kz_json:is_true(<<"unbridged_only">>, Data, 'true'),

    Params = kz_json:set_values([{<<"source">>, <<?MODULE_STRING>>}
                                ,{<<"can_call_self">>, 'true'}
                                ]
                               ,Data
                               ),

    SourceDeviceId = find_device_id_for_leg(SourceOfDTMF),
    Endpoints = build_endpoints(TargetType, TargetId, SourceDeviceId, Params, Call),
    build_originate(Endpoints, SourceOfDTMF, UnbridgedOnly, Call).

-spec build_endpoints(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_json:objects().
build_endpoints(<<"device">>, Id, Id, _Params, _Call) ->
    lager:debug("skipping ~s since it same source device id", [Id]),
    [];
build_endpoints(<<"device">>, DeviceId, _SourceDeviceId, Params, Call) ->
    lager:debug("building endpoints for ~s", [DeviceId]),
    case kz_endpoint:build(DeviceId, Params, Call) of
        {'ok', Endpoint} -> Endpoint;
        _Else -> []
    end;
build_endpoints(<<"user">>, OwnerId, SourceDeviceId, Params, Call) ->
    lager:debug("building endpoints for user ~s", [OwnerId]),
    lists:foldr(fun(EndpointId, Acc) when EndpointId =:= SourceDeviceId ->
                        lager:debug("skipping ~s since it matches device id", [EndpointId]),
                        Acc;
                   (EndpointId, Acc) ->
                        lager:debug("building endpoint ~s", [EndpointId]),
                        case kz_endpoint:build(EndpointId, Params, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            _Else -> Acc
                        end
                end
               ,[]
               ,kz_attributes:owned_by(OwnerId, <<"device">>, Call)
               ).

-spec build_originate(kz_json:objects(), kz_term:ne_binary(), boolean(), kapps_call:call()) -> kz_term:proplist().
build_originate([], _CallId, _UnbridgedOnly, _Call) -> [];
build_originate(Endpoints, CallId, UnbridgedOnly, Call) ->
    lager:debug("targeting ~s for intercept", [CallId]),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
      ,{<<"Endpoints">>, Endpoints}
      ,{<<"Existing-Call-ID">>, CallId}
      ,{<<"Intercept-Unbridged-Only">>, UnbridgedOnly}
      ,{<<"Outbound-Call-ID">>, <<(kz_binary:rand_hex(18))/binary, "-intercept">>}
      ,{<<"Outbound-Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
      ,{<<"Outbound-Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
      ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
      ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}

      ,{<<"Outbound-Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
      ,{<<"Outbound-Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
      ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
      ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}

       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]
     ).

-spec send_originate_req(kz_term:proplist(), kapps_call:call()) ->
                                {'ok', kz_json:objects()} |
                                {'timeout', kz_json:objects()} |
                                {'error', any()}.
send_originate_req([], _Call) ->
    lager:debug("no origination proprs, skipping"),
    {'error', 'no_endpoints'};
send_originate_req(OriginateProps, _Call) ->
    kz_amqp_worker:call_collect(OriginateProps
                               ,fun kapi_resource:publish_originate_req/1
                               ,fun is_resp/1
                               ,20 * ?MILLISECONDS_IN_SECOND
                               ).

-spec is_resp(kz_json:objects() | kz_json:object()) -> boolean().
is_resp([JObj|_]) ->
    is_resp(JObj);
is_resp(JObj) ->
    kapi_resource:originate_resp_v(JObj).

-spec is_originate_uuid(kz_json:object(), kz_term:api_binary()) -> boolean().
is_originate_uuid(JObj, CallId) ->
    kapi_resource:originate_uuid_v(JObj)
        andalso (CallId =:= 'undefined'
                 orelse CallId =:= kz_json:get_value(<<"Outbound-Call-ID">>, JObj)
                ).

-spec find_device_id_for_leg(kz_term:ne_binary()) -> kz_term:api_binary().
find_device_id_for_leg(CallId) ->
    case kz_amqp_worker:call([{<<"Fields">>, [<<"Authorizing-ID">>]}
                             ,{<<"Call-ID">>, CallId}
                              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]
                            ,fun kapi_call:publish_query_channels_req/1
                            ,fun kapi_call:query_channels_resp_v/1
                            )
    of
        {'ok', RespJObj} ->
            kz_json:get_value([<<"Channels">>, CallId, <<"Authorizing-ID">>], RespJObj);
        {'error', _E} ->
            lager:debug("failed to query for ~s: ~p", [CallId, _E]),
            'undefined'
    end.
