%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% data:
%%%   owner_id: User-ID to fetch devices for
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(konami_move).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()} |
                    {'stop', whapps_call:call()}.
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

-spec maybe_update_metaflow(wh_json:object(), whapps_call:call(), wh_json:objects()) ->
                                   {'stop', whapps_call:call()}.
maybe_update_metaflow(Data, Call, Results) ->
    case [Result || Result <- Results, is_resp(Result)] of
        [] ->
            lager:debug("no useful responses"),
            {'stop', Call};
        [Resp|_] ->
            CallId = wh_json:get_value(<<"Call-ID">>, Resp),
            maybe_update_metaflow(Data, Call, Results, CallId)
    end.

-spec maybe_update_metaflow(wh_json:object(), whapps_call:call(), wh_json:objects(), api_binary()) ->
                                   {'stop', whapps_call:call()}.
maybe_update_metaflow(Data, Call, Results, CallId) ->
    case [Result || Result <- Results, is_originate_uuid(Result, CallId)] of
        [] ->
            lager:debug("no matching originate_uuid"),
            {'stop', Call};
        [OriginateUUID|_] ->
            ControlQueue = wh_json:get_value(<<"Outbound-Call-Control-Queue">>, OriginateUUID),
            lager:debug("should use ~s for control of ~s", [ControlQueue, CallId]),
            maybe_update_metaflow_control(Data, Call, CallId, ControlQueue, source_leg_of_dtmf(Data, Call))
    end.

-spec maybe_update_metaflow_control(wh_json:object(), whapps_call:call(), ne_binary(), ne_binary(), 'a' | 'b') ->
                                           {'stop', whapps_call:call()}.
maybe_update_metaflow_control(_Data, Call, CallId, ControlQueue, 'a') ->
    lager:debug("update ~s to ~s with ctl ~s", [whapps_call:call_id(Call), CallId, ControlQueue]),

    konami_code_fsm:transfer_to(
      whapps_call:set_control_queue(ControlQueue
                                    ,whapps_call:set_call_id(CallId, Call)
                                   )
      ,'a'
     ),

    {'stop', Call};
maybe_update_metaflow_control(_Data, Call, CallId, _ControlQueue, 'b') ->
    lager:debug("update ~s to ~s with ctl ~s", [whapps_call:other_leg_call_id(Call), CallId, _ControlQueue]),

    konami_code_fsm:transfer_to(
      whapps_call:set_other_leg_call_id(CallId, Call)
      ,'b'
     ),

    {'stop', Call}.

-spec source_leg_of_dtmf(ne_binary() | wh_json:object(), whapps_call:call()) -> 'a' | 'b'.
source_leg_of_dtmf(<<_/binary>> = SourceDTMF, Call) ->
    case whapps_call:call_id(Call) =:= SourceDTMF of
        'true' -> 'a';
        'false' -> 'b'
    end;
source_leg_of_dtmf(Data, Call) ->
    source_leg_of_dtmf(wh_json:get_value(<<"dtmf_leg">>, Data), Call).

-spec get_originate_req(wh_json:object(), whapps_call:call()) -> wh_proplist().
get_originate_req(Data, Call) ->
    SourceOfDTMF = wh_json:get_value(<<"dtmf_leg">>, Data),

    Params = wh_json:set_values([{<<"source">>, ?MODULE}
                                 ,{<<"can_call_self">>, 'true'}
                                ], Data),

    {SourceDeviceId, TargetCallId} =
        case source_leg_of_dtmf(SourceOfDTMF, Call) of
            'a' -> {whapps_call:authorizing_id(Call), whapps_call:other_leg_call_id(Call)};
            'b' -> {find_device_id_for_leg(SourceOfDTMF), whapps_call:call_id(Call)}
        end,

    DeviceOwnerId = cf_attributes:owner_id(SourceDeviceId, Call),
    OwnerId = wh_json:get_value(<<"owner_id">>, Data, DeviceOwnerId),
    Endpoints = build_endpoints(SourceDeviceId, OwnerId, Params, Call),
    build_originate(Endpoints, TargetCallId, Call).

-spec build_endpoints(ne_binary(), ne_binary(), wh_json:object(), whapps_call:call()) -> wh_json:objects().
build_endpoints(DeviceId, OwnerId, Params, Call) ->
    lager:debug("building endpoints against ~s(~s)", [DeviceId, OwnerId]),
    lists:foldr(
      fun(EndpointId, Acc) when EndpointId =:= DeviceId ->
              lager:debug("skipping ~s since it matches device id", [EndpointId]),
              Acc;
         (EndpointId, Acc) ->
              lager:debug("building endpoint ~s", [EndpointId]),
              case cf_endpoint:build(EndpointId, Params, Call) of
                  {'ok', Endpoint} -> Endpoint ++ Acc;
                  _Else -> Acc
              end
      end
      ,[]
      ,cf_attributes:owned_by(OwnerId, <<"device">>, Call)
     ).

-spec build_originate(wh_json:objects(), ne_binary(), whapps_call:call()) -> wh_proplist().
build_originate([], _CallId, _Call) -> [];
build_originate(Endpoints, CallId, Call) ->
    lager:debug("targeting ~s for intercept", [CallId]),
    props:filter_undefined(
        [{<<"Application-Name">>, <<"bridge">>}
         ,{<<"Endpoints">>, Endpoints}
         ,{<<"Existing-Call-ID">>, CallId}
         ,{<<"Intercept-Unbridged-Only">>, 'false'}
         ,{<<"Outbound-Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
         ,{<<"Outbound-Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
         ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
         ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}

         ,{<<"Outbound-Callee-ID-Name">>, whapps_call:callee_id_name(Call)}
         ,{<<"Outbound-Callee-ID-Number">>, whapps_call:callee_id_number(Call)}
         ,{<<"Callee-ID-Name">>, whapps_call:callee_id_name(Call)}
         ,{<<"Callee-ID-Number">>, whapps_call:callee_id_number(Call)}

         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ]
    ).

-spec send_originate_req(wh_proplist(), whapps_call:call()) ->
                                {'ok', wh_json:objects()} |
                                {'timeout', wh_json:objects()} |
                                {'error', any()}.
send_originate_req([], _Call) ->
    lager:debug("no origination proprs, skipping"),
    {'error', 'no_endpoints'};
send_originate_req(OriginateProps, _Call) ->
    whapps_util:amqp_pool_collect(OriginateProps
                                  ,fun wapi_resource:publish_originate_req/1
                                  ,fun is_resp/1
                                  ,20 * ?MILLISECONDS_IN_SECOND
                                 ).

-spec is_resp(wh_json:objects() | wh_json:object()) -> boolean().
is_resp([JObj|_]) ->
    is_resp(JObj);
is_resp(JObj) ->
    wapi_resource:originate_resp_v(JObj).

-spec is_originate_uuid(wh_json:object(), api_binary()) -> boolean().
is_originate_uuid(JObj, CallId) ->
    wapi_resource:originate_uuid_v(JObj)
        andalso (CallId =:= 'undefined'
                 orelse CallId =:= wh_json:get_value(<<"Outbound-Call-ID">>, JObj)
                ).

-spec find_device_id_for_leg(ne_binary()) -> api_binary().
find_device_id_for_leg(CallId) ->
    case whapps_util:amqp_pool_request([{<<"Fields">>, [<<"Authorizing-ID">>]}
                                        ,{<<"Call-ID">>, CallId}
                                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                       ]
                                       ,fun wapi_call:publish_query_channels_req/1
                                       ,fun wapi_call:query_channels_resp_v/1
                                      )
    of
        {'ok', RespJObj} ->
            wh_json:get_value([<<"Channels">>, CallId, <<"Authorizing-ID">>], RespJObj);
        {'error', _E} ->
            lager:debug("failed to query for ~s: ~p", [CallId, _E]),
            'undefined'
    end.
