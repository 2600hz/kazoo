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
    Params = wh_json:set_values([{<<"source">>, ?MODULE}
                                 ,{<<"can_call_self">>, 'true'}
                                ], Data),
    OwnerId = wh_json:get_value(<<"owner_id">>, Data),
    DeviceId = whapps_call:authorizing_id(Call),
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
    lists:foldr(
        fun(EndpointId, Acc) when EndpointId =:= DeviceId ->
            Acc;
        (EndpointId, Acc) ->
            case cf_endpoint:build(EndpointId, Params, Call) of
                {'ok', Endpoint} -> Endpoint ++ Acc;
                _Else -> Acc
            end
        end
        ,[]
        ,cf_attributes:owned_by(OwnerId, <<"device">>, Call)
    ).

-spec build_originate(wh_json:objects(), ne_binary(), whapps_call:call()) -> wh_proplist().
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
send_originate_req(OriginateProps, _Call) ->
    whapps_util:amqp_pool_collect(OriginateProps, fun wapi_resource:publish_originate_req/1, fun is_resp/1, 20000).

-spec is_resp(wh_json:objects()) -> {'ok', iolist()} |
                                    {'error', string()}.
is_resp([JObj|_]) ->
    wapi_resource:originate_resp_v(JObj).

