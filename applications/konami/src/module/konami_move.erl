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
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    Req = get_originate_req(Data, Call),
    Resp = send_originate_req(Req, Call),
    lager:debug("resp to orig: ~p", [Resp]),
    {'continue', Call}.

-spec get_originate_req(wh_json:object(), whapps_call:call()) -> wh_json:objects().
get_originate_req(Data, Call) ->
    SourceOfDTMF = wh_json:get_value(<<"dtmf_leg">>, Data),

    Params = wh_json:set_values([{<<"source">>, ?MODULE}
                                 ,{<<"can_call_self">>, 'true'}
                                ], Data),

    OwnerId = wh_json:get_value(<<"owner_id">>, Data),
    DeviceId = case whapps_call:call_id(Call) =:= SourceOfDTMF of
                   'true' -> whapps_call:authorizing_id(Call);
                   'false' -> find_device_id_for_leg(SourceOfDTMF)
               end,

    DeviceIds = cf_attributes:owned_by(OwnerId, <<"device">>, Call),
    Endpoints = build_endpoints(DeviceId, OwnerId, Params, Call),
    case lists:member(DeviceId, DeviceIds) of
        'true' ->
            lager:debug("filter a-leg devices, intercept b-leg"),
            build_originate(Endpoints, whapps_call:other_leg_call_id(Call), Call);
        'false' ->
            lager:debug("filter b-leg devices, intercept a-leg"),
            build_originate(Endpoints, whapps_call:call_id(Call), Call)
    end.

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
build_originate(Endpoints, CallId, _Call) ->
    props:filter_undefined(
        [{<<"Application-Name">>, <<"bridge">>}
         ,{<<"Endpoints">>, Endpoints}
         ,{<<"Existing-Call-ID">>, CallId}
         ,{<<"Intercept-Unbridged-Only">>, 'false'}
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
    whapps_util:amqp_pool_collect(OriginateProps, fun wapi_resource:publish_originate_req/1, fun is_resp/1, 20000).

-spec is_resp(wh_json:objects()) -> {'ok', iolist()} |
                                    {'error', string()}.
is_resp([JObj|_]) ->
    wapi_resource:originate_resp_v(JObj).

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
