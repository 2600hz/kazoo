%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Token auth module
%%% This is a simple auth mechanism, once the user has acquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Ben Wann
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_token_auth).

-export([init/0
        ,authenticate/2
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
    _ = blackhole_bindings:bind(<<"blackhole.authenticate.*">>, ?MODULE, 'authenticate'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(bh_context:context(), kz_json:object()) -> bh_context:context().
authenticate(Context, _Payload) ->
    auth_token(Context, bh_context:auth_token(Context)).

-spec auth_token(bh_context:context(), kz_term:api_binary()) -> bh_context:context().
auth_token(Context, 'undefined') ->
    lager:debug("no token included"),
    bh_context:add_error(Context, <<"authentication token required">>);
auth_token(Context, Token)
  when is_binary(Token)->
    lager:debug("trying to authenticate with token: ~s", [Token]),
    case kz_auth:validate_token(Token) of
        {'ok', JObj} ->
            lager:info("token auth is valid, authenticating : ~p", [JObj]),
            AccountId = kz_json:get_ne_value(<<"account_id">>, JObj),
            bh_context:set_auth_account_id(Context, AccountId);
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            bh_context:add_error(Context, <<"failed to authenticate token ", Token/binary>>)
    end;
auth_token(Context, _Token) ->
    lager:warning("token is not of required type, , ~p", [_Token]),
    bh_context:add_error(Context, <<"invalid authentication token format">>).
