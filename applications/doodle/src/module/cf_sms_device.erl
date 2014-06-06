%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_sms_device).

-include("../doodle.hrl").

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
            doodle_exe:continue(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to device: ~s"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            doodle:continue(Call)
    end.

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> doodle_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints(wh_json:object(), whapps_call:call()) ->
                                 any().
bridge_to_endpoints(Data, Call) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    case cf_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            whapps_sms_command:b_send_sms(Endpoints, Call)
    end.
