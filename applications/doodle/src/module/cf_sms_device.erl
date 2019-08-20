%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_sms_device).

-include("doodle.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call1) ->
    EndpointId = kz_doc:id(Data),
    Call2 = kapps_call:kvs_store(<<"target_device_id">>, EndpointId, Call1),
    case build_endpoint(EndpointId, Data, doodle_util:set_callee_id(EndpointId, Call2)) of
        {'error', 'do_not_disturb'} = Reason ->
            maybe_handle_bridge_failure(Reason, Call1);
        {'error', Reason} ->
            doodle_exe:continue(doodle_util:set_flow_error(<<"error">>, kz_term:to_binary(Reason), Call1));
        {Endpoints, Call} ->
            case kapps_sms_command:b_send_sms(Endpoints, Call) of
                {'ok', JObj} -> handle_result(JObj, Call);
                {'error', _} = Reason -> maybe_handle_bridge_failure(Reason, Call)
            end
    end.

-spec handle_result(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_result(JObj, Call) ->
    Status = doodle_util:sms_status(JObj),
    Call1 = doodle_util:set_flow_status(Status, Call),
    handle_result_status(Call1, Status).

-spec handle_result_status(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:stop(Call).

-spec maybe_handle_bridge_failure({'error', any()}, kapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure({_ , R}=Reason, Call) ->
    case doodle_util:handle_bridge_failure(Reason, Call) of
        'not_found' ->
            doodle_util:maybe_reschedule_sms(
              doodle_util:set_flow_status(<<"pending">>, kz_term:to_binary(R), Call));
        'ok' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempts to build the endpoints to reach this device
%% @end
%%------------------------------------------------------------------------------
-spec build_endpoint(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
                            {'error', atom() | kz_json:object()} |
                            {'fail', kz_term:ne_binary() | kz_json:object()} |
                            {kz_json:objects(), kapps_call:call()}.
build_endpoint(EndpointId, Data, Call) ->
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),
    case kz_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} -> maybe_note_owner(Endpoints, Call)
    end.

-spec maybe_note_owner(kz_json:objects(), kapps_call:call()) ->
                              {kz_json:objects(), kapps_call:call()}.
maybe_note_owner([Endpoint]=Endpoints, Call) ->
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], Endpoint) of
        'undefined' -> {Endpoints, Call};
        OwnerId ->
            {Endpoints, kapps_call:kvs_store(<<"target_owner_id">>, OwnerId, Call)}
    end.
