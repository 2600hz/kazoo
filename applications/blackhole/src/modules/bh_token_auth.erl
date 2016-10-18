%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_token_auth).

-export([init/0
        ,authenticate/2
        ]).

-include("blackhole.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.authenticate.*">>, ?MODULE, 'authenticate'),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(bh_context:context(), kz_json:object()) -> bh_context:context().
authenticate(Context, _Payload) ->
    Token = bh_context:auth_token(Context),
    lager:debug("trying to authenticate with token: ~s", [Token]),
    case kz_auth:validate_token(Token) of
        {'ok', JObj} ->
            lager:debug("token auth is valid, authenticating : ~p", [JObj]),
            AccountId = kz_json:get_ne_value(<<"account_id">>, JObj),
            bh_context:set_auth_account_id(Context, AccountId);
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            bh_context:add_error(Context, <<"failed to authenticate token ", Token/binary>>)
    end.
