%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_mobile_manager).

-export([delete_account/1]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"mobile_manager">>).

-spec delete_account(cb_context:context()) -> 'ok'.
delete_account(Context) ->
    case req_uri([<<"accounts">>, cb_context:account_id(Context)]) of
        'undefined' ->
            lager:debug("ignore request mobile_manger url is not set");
        UrlString ->
            lager:debug("mobile_manager delete via ~s", [UrlString]),

            Headers = req_headers(cb_context:auth_token(Context)),

            Resp = kz_http:delete(UrlString, Headers),
            handle_resp(Resp)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_resp(kz_http:ret()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}) ->
    lager:debug("mobile_manager success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("mobile_manager error ~p. ~s", [Code,Resp]);
handle_resp(_Error) ->
    lager:error("mobile_manager fatal error ~p", [_Error]).


-spec req_uri(kz_term:ne_binaries()) -> kz_term:api_list().
req_uri(ExplodedPath) ->
    case kapps_config:get_binary(?MOD_CONFIG_CAT, <<"url">>) of
        'undefined' -> 'undefined';
        Url ->
            Uri = kz_http_util:uri(Url, ExplodedPath),
            kz_term:to_list(Uri)
    end.


-spec req_headers(kz_term:ne_binary()) -> kz_term:proplist().
req_headers(AuthToken) ->
    props:filter_undefined(
      [{"content-type", "application/json"}
      ,{"x-auth-token", kz_term:to_list(AuthToken)}
      ,{"user-agent", kz_term:to_list(erlang:node())}
      ]).
