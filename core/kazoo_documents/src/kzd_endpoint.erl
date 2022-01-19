%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_endpoint).

-export([id/1]).
-export([type/1]).
-export([account_id/1]).

-type endpoint() :: kz_json:object().
-export_type([endpoint/0]).

-spec id(endpoint()) -> kz_term:ne_binary().
id(Endpoint) ->
    kz_json:get_ne_binary_value(<<"Endpoint-ID">>, Endpoint).

-spec type(endpoint()) -> kz_term:ne_binary().
type(Endpoint) ->
    kz_json:get_ne_binary_value(<<"Endpoint-Type">>, Endpoint).

-spec account_id(endpoint()) -> kz_term:api_ne_binary().
account_id(Endpoint) ->
    kz_json:get_ne_binary_value(<<"Endpoint-Account-ID">>, Endpoint).
