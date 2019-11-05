%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_oauth_service).

-include("kazoo_oauth.hrl").

-export([service_token/2]).


-spec service_token(kz_term:api_binary() | oauth_service_app(), kz_term:api_binary()) -> kz_term:api_object().
service_token('undefined', _) ->
    'undefined';
service_token(_, 'undefined') ->
    'undefined';
service_token(AppId, Scopes) when is_binary(AppId) ->
    lager:debug("getting service app"),
    case kazoo_oauth_util:get_oauth_service_app(AppId) of
        {'ok', App } -> service_token(App, Scopes);
        {'error', Error} ->
            lager:debug("service token ~p",[Error]),'undefined'
    end;
service_token(#oauth_service_app{private_key=_PrivateKey
                                ,provider=#oauth_provider{auth_url=URL}
                                }=ServiceApp, Scopes) ->
    Assertion = kazoo_oauth_util:jwt(ServiceApp, Scopes),
    GrantType = kz_term:to_list(kz_http_util:urlencode(?OAUTH_GRANT_TYPE)),
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}
              ,{"User-Agent", "Kazoo"}
              ],
    Fields = [{"grant_type", GrantType}
             ,{"assertion", kz_term:to_list(kz_http_util:urlencode(Assertion))}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
    case kz_http:post(kz_term:to_list(URL), Headers, Body) of
        {'ok', 200, _RespHeaders, RespXML} ->
            kz_json:decode(RespXML);
        _Else ->
            lager:debug("unable to request service token: ~p", [_Else]),
            'undefined'
    end.
