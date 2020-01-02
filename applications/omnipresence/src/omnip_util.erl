%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(omnip_util).

-export([extract_user/1
        ,normalize_variables/1
        ,are_valid_uris/1, is_valid_uri/1
        ]).

-export([request_probe/2, request_probe/3]).

-include("omnipresence.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-spec extract_user(kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()}.
extract_user(User) ->
    [#uri{scheme=Proto, user=Username, domain=Realm}] = kzsip_uri:uris(User),
    {kz_term:to_binary(Proto), <<Username/binary, "@", Realm/binary>>, [Username, Realm]}.

-spec normalize_variables(kz_term:proplist()) -> kz_term:proplist().
normalize_variables(Props) ->
    [{kz_json:normalize_key(K), V} || {K, V} <- Props].

-spec are_valid_uris(kz_term:ne_binaries()) -> boolean().
are_valid_uris(L) ->
    lists:all(fun is_valid_uri/1, L).

-spec is_valid_uri(kz_term:ne_binary()) -> boolean().
is_valid_uri(Uri) ->
    case binary:split(Uri, <<"@">>) of
        [_User, _Host] -> 'true';
        _ -> 'false'
    end.

-spec request_probe(binary(), binary()) -> 'ok'.
request_probe(Package, User) ->
    case binary:split(User, <<"@">>, ['global']) of
        [Username, Realm | _] -> request_probe(Package, Username, Realm);
        _Other -> lager:warning("request probe for ~p failed", [_Other])
    end.

-spec request_probe(binary(), binary(), binary()) -> 'ok'.
request_probe(<<"message-summary">>, Username, Realm) ->
    API = [{<<"Username">>, Username}
          ,{<<"Realm">>, Realm}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_presence:publish_mwi_query/1);
request_probe(Package, Username, Realm) ->
    API = [{<<"Event-Package">>, Package}
          ,{<<"Username">>, Username}
          ,{<<"Realm">>, Realm}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_presence:publish_probe/1).
