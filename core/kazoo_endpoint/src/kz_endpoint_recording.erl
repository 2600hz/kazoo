%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoint_recording).

-export([maybe_record_inbound/3
        ,maybe_record_outbound/3
        ]).

-include("kazoo_endpoint.hrl").

-define(ENDPOINT_INBOUND_RECORDING(Network), [<<"call_recording">>, <<"endpoint">>, <<"inbound">>, Network]).
-define(ENDPOINT_OUTBOUND_RECORDING(Network), [<<"call_recording">>, <<"endpoint">>, <<"outbound">>, Network]).
-define(ENDPOINT_OUTBOUND_RECORDING_LABEL(Network), <<"outbound to ", Network/binary, " from endpoint">>).

%% @doc should recording be started on call TO the endpoint
-spec maybe_record_inbound(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
                                  {'true', {kz_json:path(), kz_json:object()}} | 'false'.
maybe_record_inbound(FromNetwork, Endpoint, Call) ->
    maybe_record_inbound(FromNetwork, Endpoint, Call, kz_json:get_json_value(?ENDPOINT_INBOUND_RECORDING(FromNetwork), Endpoint)).

-spec maybe_record_inbound(kz_term:ne_binary(), kz_json:object(), kapps_call:call(), kz_term:api_object()) ->
                                  {'true', {kz_json:path(), kz_json:object()}} | 'false'.
maybe_record_inbound(_FromNetwork, _Endpoint, _Call, 'undefined') -> 'false';
maybe_record_inbound(FromNetwork, Endpoint, Call, Data) ->
    case kz_json:is_true(<<"enabled">>, Data) of
        'false' -> 'false';
        'true' ->
            Values = [{<<"origin">>, <<"inbound from ", FromNetwork/binary, " to endpoint">>}
                     ,{<<"endpoint_id">>, kz_doc:id(Endpoint)}
                     ],
            App = kapps_call_recording:record_call_command(kz_json:set_values(Values, Data), Call),
            lager:info("setting endpoint ~s to record on answer", [kz_doc:id(Endpoint)]),
            {'true', {[<<"Execute-On-Answer">>, <<"Record-Endpoint">>], App}}
    end.

%% @doc maybe start recording on call made FROM the endpoint
-spec maybe_record_outbound(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
                                   {'true', kapps_call:call()} | 'false'.
maybe_record_outbound(ToNetwork, Endpoint, Call) ->
    maybe_record_outbound(ToNetwork, Endpoint, Call, kz_json:get_json_value(?ENDPOINT_OUTBOUND_RECORDING(ToNetwork), Endpoint)).

-spec maybe_record_outbound(kz_term:ne_binary(), kz_json:object(), kapps_call:call(), kz_term:api_object()) ->
                                   {'true', kapps_call:call()} | 'false'.
maybe_record_outbound(_ToNetwork, _Endpoint, _Call, 'undefined') -> 'false';
maybe_record_outbound(ToNetwork, _Endpoint, Call, Data) ->
    case kz_json:is_true(<<"enabled">>, Data) of
        'false' -> 'false';
        'true' ->
            LabeledData = kz_json:set_value(<<"origin">>, ?ENDPOINT_OUTBOUND_RECORDING_LABEL(ToNetwork), Data),
            {'true'
            ,kapps_call:start_recording(LabeledData, kapps_call:kvs_store('recording_follow_transfer', 'false', Call))
            }
    end.
