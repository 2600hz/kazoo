%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_user).

-include("doodle.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_im:im()) -> 'ok'.
handle(Data, Im) ->
    EndpointId = kz_doc:id(Data),
    case get_endpoints(EndpointId, Data, Im) of
        {'ok', []} -> tf_exe:stop(Im, 'no_endpoints');
        {'ok', Endpoints} -> send_sms(Endpoints, Data, Im);
        {'error', Reason} -> tf_exe:stop(Im, Reason)
    end.

-spec send_sms(kz_json:objects(), kz_json:object(), kapps_im:im()) -> 'ok'.
send_sms(Endpoints, Data, Im) ->
    Strategy = kz_json:get_ne_binary_value(<<"sms_strategy">>, Data, <<"single">>),
    case kapps_im_command:send_sms(Endpoints, Strategy, Im) of
        {'ok', JObj} -> tf_exe:stop(Im, tf_util:delivery_status(JObj));
        {'error', Reason} -> tf_exe:stop(Im, Reason)
    end.

%%------------------------------------------------------------------------------
%% @doc Builds the Endpoints for the user
%% @end
%%------------------------------------------------------------------------------
-spec get_endpoints(kz_term:ne_binary(), kz_json:object(), kapps_im:im()) ->
          {'error', any()} |
          {'ok', kz_json:objects()}.
get_endpoints(EndpointId, Data, Im) ->
    Params = kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Data),
    case kz_endpoint:get(EndpointId, kapps_im:account_id(Im)) of
        {'ok', Endpoint} -> tf_util:build_im_endpoint(Endpoint, Params, Im);
        {'error', _}=E -> E
    end.
