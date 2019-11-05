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
-module(cf_user).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UserId = kz_json:get_ne_binary_value(<<"id">>, Data),
    Endpoints = get_endpoints(UserId, Data, Call),

    maybe_bridge(Data, Call, Endpoints).

-spec maybe_bridge(kz_json:object(), kapps_call:call(), kz_json:objects()) -> 'ok'.
maybe_bridge(_Data, Call, []) ->
    lager:notice("user ~s has no endpoints"
                ,[kz_json:get_ne_binary_value(<<"id">>, _Data)]
                ),
    cf_exe:continue(Call);
maybe_bridge(Data, Call, Endpoints) ->
    bridge(Data, Call, Endpoints).

-spec bridge(kz_json:object(), kapps_call:call(), kz_json:objects()) -> 'ok'.
bridge(Data, Call, Endpoints) ->
    FailOnSingleReject = kz_json:is_true(<<"fail_on_single_reject">>, Data, kapps_call:custom_channel_var(<<"Require-Fail-On-Single-Reject">>, Call)),
    Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = kz_json:get_ne_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    IgnoreEarlyMedia = Strategy =:= <<"simultaneous">>
        orelse kz_endpoints:ignore_early_media(Endpoints),
    CustomSIPHeaders = kz_json:get_ne_json_value(<<"custom_sip_headers">>, Data),

    lager:info("attempting ~b user devices with strategy ~s", [length(Endpoints), Strategy]),

    case kapps_call_command:b_bridge(Endpoints
                                    ,Timeout
                                    ,Strategy
                                    ,IgnoreEarlyMedia
                                    ,'undefined' % Ringback
                                    ,CustomSIPHeaders
                                    ,<<"false">> % IgnoreForward
                                    ,FailOnSingleReject
                                    ,Call
                                    )
    of
        {'ok', _} ->
            lager:info("completed successful bridge to user"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to user: ~p", [_R]),
            cf_exe:continue(Call)
    end.

-spec maybe_handle_bridge_failure(any(), kapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%------------------------------------------------------------------------------
-spec get_endpoints(kz_term:api_binary(), kz_json:object(), kapps_call:call()) ->
                           kz_json:objects().
get_endpoints('undefined', _, _) -> [];
get_endpoints(UserId, Data, Call) ->
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),
    kz_endpoints:by_owner_id(UserId, Params, Call).
