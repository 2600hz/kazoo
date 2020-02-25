%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_limits).

-export([init/0
        ,limits/2
        ]).

-include("blackhole.hrl").

-spec init() -> 'ok'.
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.limits.*">>, ?MODULE, 'limits'),
    'ok'.

-spec limits(bh_context:context(), kz_json:object()) -> bh_context:context().
limits(Context, RequestJObj) ->
    AuthAccountId = bh_context:auth_account_id(Context),
    maybe_limit(Context, AuthAccountId, kz_json:get_json_value(<<"data">>, RequestJObj)).

%% TODO: limits requests based on:
maybe_limit(Context, _AuthAccountId, 'undefined') ->
    %% no data typically means connection establishment
    Context;
maybe_limit(Context, _AuthAccountId, _Data) ->
    %% data means limit actions like subscribe
    Context.
