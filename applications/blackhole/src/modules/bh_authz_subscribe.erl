%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Subscribe authz module
%%% This is a simple authz mechanism, it checks if a subscribe
%%% action is allowed by looking at auth_account_id tree
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_authz_subscribe).

-export([init/0
        ,authorize_account/2
        ]).

-include("blackhole.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.authorize.*">>, ?MODULE, 'authorize_account'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize_account(bh_context:context(), map()) -> bh_context:context().
authorize_account(Context, #{account_id := <<"*">>}) ->
    case bh_context:is_superduper_admin(Context) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"unauthorized wildcard account id">>)
    end;
authorize_account(Context, #{account_id := AccountId}) ->
    AuthAccount = bh_context:auth_account_id(Context),
    maybe_authorize_account(Context, AuthAccount, AccountId).

-spec maybe_authorize_account(bh_context:context(), binary(), binary()) -> bh_context:context().
maybe_authorize_account(Context, 'undefined', _AccountId) ->
    lager:warning("auth_account_id is not set, maybe you need to start bh_token_auth ?"),
    bh_context:add_error(Context, <<"auth_account_id not set">>);

maybe_authorize_account(Context, AuthAccountId, AccountId) ->
    case kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId, 'true') of
        'true'  -> Context;
        'false' -> bh_context:add_error(Context, <<"unauthorized account id">>)
    end.
