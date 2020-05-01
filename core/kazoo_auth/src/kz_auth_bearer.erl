%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_bearer).

%%==============================================================================
%% API functions
%%==============================================================================

-export([create/1
        ,fetch/1
        ]).

-include("kazoo_auth.hrl").

-spec create(kz_term:proplist()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'algorithm_not_supported'} |
          kz_datamgr:data_error().
create(Claims) ->
    case kz_auth:create_token(Claims) of
        {'ok', Token} -> save_token(Token);
        Else -> Else
    end.

-spec fetch(kz_term:ne_binary()) -> kz_term:api_ne_binary().
fetch(Bearer) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, Bearer) of
        {'ok', JObj} -> kz_json:get_ne_binary_value(<<"Token">>, JObj);
        Else -> Else
    end.

-spec save_token(binary()) ->
          {'ok', kz_term:ne_binary()} |
          kz_datamgr:data_error().
save_token(Token) ->
    JObj = kz_json:from_list([{<<"Token">>, Token}]),
    Doc = kz_doc:update_pvt_parameters(JObj, ?KZ_AUTH_DB, [{'type', <<"bearer-token">>}]),
    case kz_datamgr:save_doc(?KZ_AUTH_DB, Doc) of
        {'ok', Created} -> {'ok', kz_doc:id(Created)};
        Else -> Else
    end.
