%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_device).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_endpoints(Data, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the device"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to device: ~s"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints(wh_json:object(), whapps_call:call()) ->
                                 cf_api_bridge_return().
bridge_to_endpoints(Data, Call) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    case cf_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
            IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
            _ = maybe_start_metaflows(Call, Endpoints),
            whapps_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, IgnoreEarlyMedia, Call)
    end.

maybe_start_metaflows(Call, Endpoints) ->
    [maybe_start_metaflow(Call, Endpoint) || Endpoint <- Endpoints],
    'ok'.
maybe_start_metaflow(Call, Endpoint) ->
    case wh_json:get_value(<<"Metaflows">>, Endpoint) of
        'undefined' -> 'ok';
        JObj ->
            API = props:filter_undefined(
                    [{<<"Endpoint-ID">>, wh_json:get_value(<<"Endpoint-ID">>, Endpoint)}
                     ,{<<"Call">>, whapps_call:to_json(Call)}
                     ,{<<"Numbers">>, wh_json:get_value(<<"numbers">>, JObj)}
                     ,{<<"Patterns">>, wh_json:get_value(<<"patterns">>, JObj)}
                     ,{<<"Binding-Digit">>, wh_json:get_value(<<"binding_digit">>, JObj)}
                     ,{<<"Digit-Timeout">>, wh_json:get_value(<<"digit_timeout">>, JObj)}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ]),
            lager:debug("sending metaflow for endpoint: ~s", [wh_json:get_value(<<"Endpoint-ID">>, Endpoint)]),
            whapps_util:amqp_pool_send(API, fun wapi_dialplan:publish_metaflow/1)
    end.
