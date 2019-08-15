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
-module(cf_device).

-behaviour(gen_cf_action).

-include_lib("callflow/src/callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_endpoints(Data, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the device"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} when is_atom(_R) ->
            lager:info("failed to build endpoint from device: ~p", [_R]),
            cf_exe:continue(Call);
        {'error', _R} ->
            lager:info("error bridging to device: ~s"
                      ,[kz_json:get_ne_binary_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec maybe_handle_bridge_failure(any(), kapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempts to bridge to the endpoints created to reach this device
%% @end
%%------------------------------------------------------------------------------
-spec bridge_to_endpoints(kz_json:object(), kapps_call:call()) ->
                                 cf_api_bridge_return().
bridge_to_endpoints(Data, Call) ->
    EndpointId = kz_json:get_ne_binary_value(<<"id">>, Data),
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),

    case kz_endpoint:build(EndpointId, Params, Call) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            FailOnSingleReject = kz_json:is_true(<<"fail_on_single_reject">>, Data, 'undefined'),
            Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
            IgnoreEarlyMedia = kz_endpoints:ignore_early_media(Endpoints),
            Strategy = kz_json:get_ne_binary_value(<<"dial_strategy">>, Data, <<"simultaneous">>),
            CustomSIPHeaders = kz_json:get_ne_json_value(<<"custom_sip_headers">>, Data),

            kapps_call_command:b_bridge(Endpoints
                                       ,Timeout
                                       ,Strategy
                                       ,IgnoreEarlyMedia
                                       ,'undefined' % Ringback
                                       ,CustomSIPHeaders
                                       ,<<"false">> % IgnoreForward
                                       ,FailOnSingleReject
                                       ,Call
                                       )
    end.
