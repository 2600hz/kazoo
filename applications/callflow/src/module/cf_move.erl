%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_move).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Endpoints = get_endpoints(Data, Call),
    send_originate_req(Endpoints, Call),
    cf_exe:continue(Call).


get_endpoints(Data, Call) ->
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    OwnerId = whapps_call:owner_id(Call),
    DeviceId = whapps_call:authorizing_id(Call),
    lists:foldr(
        fun(EndpointId, Acc) when EndpointId =:= DeviceId ->
            Acc;
        (EndpointId, Acc) ->
            case cf_endpoint:build(EndpointId, Params, Call) of
                {'ok', Endpoint} -> Endpoint ++ Acc;
                _Else ->
                    lager:error("~p", [_Else]),
                    Acc
            end
        end
        ,[]
        ,cf_attributes:owned_by(OwnerId, <<"device">>, Call)
    ).

send_originate_req(Endpoints, Call) ->
    Originate = build_originate(Endpoints, Call),
    io:format("cf_move.erl:MARKER:50 ~p~n", [Originate]),
    'ok' = wapi_resource:publish_originate_req(Originate).

build_originate(Endpoints, Call) ->
    Fs = whapps_call:switch_nodename(Call),
    props:filter_undefined(
        [{<<"Application-Name">>, <<"bridge">>}
         ,{<<"Endpoints">>, Endpoints}
         ,{<<"Existing-Call-ID">>, whapps_call:call_id(Call)}
         ,{<<"Intercept-Unbridged-Only">>, <<"false">>}
         | wh_api:default_headers(Fs, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
        ]).




