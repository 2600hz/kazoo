%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% data:
%%%   owner_id: User-ID to fetch devices for
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(konami_move).

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

-spec maybe_update_metaflow(kz_json:object(), kapps_call:call(), kz_json:objects(), api_binary()) ->
                                   {'stop', kapps_call:call()}.
maybe_update_metaflow(Data, Call, Results, CallId) ->
    case [Result || Result <- Results, is_originate_uuid(Result, CallId)] of
        [] ->
            lager:debug("no matching originate_uuid"),
            {'stop', Call};
        [OriginateUUID|_] ->
            ControlQueue = kapps_call:control_queue(kapps_call:from_originate_uuid(JObj, Call)),
            lager:debug("should use ~p for control of ~s", [ControlQueue, CallId]),
            maybe_update_metaflow_control(Data, Call, CallId, ControlQueue, source_leg_of_dtmf(Data, Call))
    end.

-spec maybe_update_metaflow_control(kz_json:object(), kapps_call:call(), ne_binary(), kapps_call:ctrl_queue(), 'a' | 'b') ->
                                           {'stop', kapps_call:call()}.
maybe_update_metaflow_control(_Data, Call, CallId, ControlQueue, 'a') ->
    lager:debug("update ~s to ~s with ctl ~s", [kapps_call:call_id(Call), CallId, ControlQueue]),

    konami_code_statem:transfer_to(
      kapps_call:set_control_queue(ControlQueue
                                  ,kapps_call:set_call_id(CallId, Call)
                                  )
                                  ,'a'
     ),

    {'stop', Call};
maybe_update_metaflow_control(_Data, Call, CallId, _ControlQueue, 'b') ->
    lager:debug("update ~s to ~s with ctl ~s", [kapps_call:other_leg_call_id(Call), CallId, _ControlQueue]),

    konami_code_statem:transfer_to(
      kapps_call:set_other_leg_call_id(CallId, Call)
                                  ,'b'
     ),

    {'stop', Call}.

-spec source_leg_of_dtmf(ne_binary() | kz_json:object(), kapps_call:call()) -> 'a' | 'b'.
source_leg_of_dtmf(<<_/binary>> = SourceDTMF, Call) ->
    case kapps_call:call_id(Call) =:= SourceDTMF of
        'true' -> 'a';
        'false' -> 'b'
    end;
source_leg_of_dtmf(Data, Call) ->
    source_leg_of_dtmf(kz_json:get_value(<<"dtmf_leg">>, Data), Call).

-spec get_originate_req(kz_json:object(), kapps_call:call()) -> kz_proplist().
get_originate_req(Data, Call) ->
    SourceOfDTMF = kz_json:get_value(<<"dtmf_leg">>, Data),

    Params = kz_json:set_values([{<<"source">>, ?MODULE}
                                ,{<<"can_call_self">>, 'true'}
                                ], Data),

    {SourceDeviceId, TargetCallId} =
        case source_leg_of_dtmf(SourceOfDTMF, Call) of
            'a' -> {kapps_call:authorizing_id(Call), kapps_call:other_leg_call_id(Call)};
            'b' -> {find_device_id_for_leg(SourceOfDTMF), kapps_call:call_id(Call)}
        end,

    DeviceOwnerId = kz_attributes:owner_id(SourceDeviceId, Call),
    OwnerId = kz_json:get_value(<<"owner_id">>, Data, DeviceOwnerId),
    Endpoints = build_endpoints(SourceDeviceId, OwnerId, Params, Call),
    build_originate(Endpoints, TargetCallId, Call).

-spec build_endpoints(ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) -> kz_json:objects().
build_endpoints(DeviceId, OwnerId, Params, Call) ->
    lager:debug("building endpoints against ~s(~s)", [DeviceId, OwnerId]),
    lists:foldr(
      fun(EndpointId, Acc) when EndpointId =:= DeviceId ->
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

-spec build_originate(kz_json:objects(), ne_binary(), kapps_call:call()) -> kz_proplist().
build_originate([], _CallId, _Call) -> [];
build_originate(Endpoints, CallId, Call) ->
    lager:debug("targeting ~s for intercept", [CallId]),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
      ,{<<"Endpoints">>, Endpoints}
      ,{<<"Existing-Call-ID">>, CallId}
      ,{<<"Intercept-Unbridged-Only">>, 'false'}
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

-spec send_originate_req(kz_proplist(), kapps_call:call()) ->
                                {'ok', kz_json:objects()} |
                                {'timeout', kz_json:objects()} |
                                {'error', any()}.
send_originate_req([], _Call) ->
    lager:debug("no origination proprs, skipping"),
    {'error', 'no_endpoints'};
send_originate_req(OriginateProps, _Call) ->
    kapps_util:amqp_pool_collect(OriginateProps
                                ,fun kapi_resource:publish_originate_req/1
                                ,fun is_resp/1
                                ,20 * ?MILLISECONDS_IN_SECOND
                                ).

-spec is_resp(kz_json:objects() | kz_json:object()) -> boolean().
is_resp([JObj|_]) ->
    is_resp(JObj);
is_resp(JObj) ->
    kapi_resource:originate_resp_v(JObj).

-spec is_originate_uuid(kz_json:object(), api_binary()) -> boolean().
is_originate_uuid(JObj, CallId) ->
    kapi_resource:originate_uuid_v(JObj)
        andalso (CallId =:= 'undefined'
                 orelse CallId =:= kz_json:get_value(<<"Outbound-Call-ID">>, JObj)
                ).

-spec find_device_id_for_leg(ne_binary()) -> api_binary().
find_device_id_for_leg(CallId) ->
    case kapps_util:amqp_pool_request([{<<"Fields">>, [<<"Authorizing-ID">>]}
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
