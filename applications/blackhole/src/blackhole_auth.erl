%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Roman Galeev
%%%-------------------------------------------------------------------
-module(blackhole_auth).
-include("blackhole.hrl").
-export([check_auth/3]).

-spec check_auth(ne_binary(), kz_json:object(), bh_context:context()) -> #bh_context{}.
check_auth(<<"authorize">>, JMsg, Context=#bh_context{}) ->
    Token = kz_json:get_value(<<"token">>, JMsg),
    AuthAccountId = get_account_id(Token),
    lager:debug("auth_token:~p found, auth_account_id:~p", [Token, AuthAccountId]),
    Context#bh_context{auth_account_id=AuthAccountId, auth_token=Token};
check_auth(_, _JMsg, #bh_context{auth_token = <<>>}) ->
    erlang:error('not_authorized');
check_auth(_, _JMsg, Context) ->
    Context.

-spec get_account_id(ne_binary()) -> ne_binary().
get_account_id(AuthToken) ->
    {'ok', JObj} = kz_datamgr:open_doc(?KZ_TOKEN_DB, AuthToken),
    kz_json:get_ne_value(<<"account_id">>, JObj).
