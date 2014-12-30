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
handle(Data, Call1) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    Call = doodle_util:set_callee_id(EndpointId, Call1),
    case bridge_to_endpoint(EndpointId, Data, Call) of
        {'ok', JObj} -> handle_result(JObj, Call);
        {'error', _} = Reason -> maybe_handle_bridge_failure(Reason, Call)
    end.

-spec handle_result(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_result(JObj, Call) ->
    Status = doodle_util:sms_status(JObj),
    Call1 = doodle_util:set_flow_status(Status, Call),
    handle_result_status(Call1, Status).

-spec handle_result_status(whapps_call:call(), ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_exe:stop(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:continue(Call).

-spec maybe_handle_bridge_failure({'error', _}, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case doodle_util:handle_bridge_failure(Reason, Call) of
        'not_found' ->
            doodle_exe:stop(doodle_util:set_flow_status(<<"pending">>, Call));
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoint(ne_binary(), wh_json:object(), whapps_call:call()) ->
                                {'error', atom() | wh_json:object()} |
                                {'fail', ne_binary() | wh_json:object()} |
                                {'ok', wh_json:object()}.
bridge_to_endpoint(EndpointId, Data, Call) ->
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    case cf_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            whapps_sms_command:b_send_sms(Endpoints, Call)
    end.
