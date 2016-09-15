%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Subscribe authz module
%%%
%%% This is a simple authz mechanism, it checks if a subscribe
%%% action is allowed by looking at auth_account_id tree
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(bh_authz_subscribe).

-export([init/0
        ,authorize_account/2
        ]).

-include("blackhole.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.authorize.*">>, ?MODULE, 'authorize_account').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_account(bh_context:context(), map()) -> bh_context:context().
authorize_account(Context, #{account_id := <<"*">>}) ->
    case bh_context:is_superduper_admin(Context) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"unauthorized wildcard account id">>)
    end;
authorize_account(Context, #{account_id := AccountId}) ->
    case kz_util:is_in_account_hierarchy(bh_context:auth_account_id(Context), AccountId, 'true') of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"unauthorized account id">>)
    end.
