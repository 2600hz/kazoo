%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_directory).

-export([lookup/2, lookup/3]).

-include("kazoo_directory.hrl").

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
lookup(EndpointId, AccountId) ->
    lookup(EndpointId, AccountId, []).

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
lookup(EndpointId, AccountId, Options) ->
    case kz_directory_endpoint:profile(EndpointId, AccountId, Options) of
        {'ok', _Endpoint} = OK -> OK;
        _Error -> kz_directory_resource:profile(EndpointId, AccountId, Options)
    end.

