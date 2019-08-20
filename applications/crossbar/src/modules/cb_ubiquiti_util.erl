%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Ubiquiti SSO Utilities
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_ubiquiti_util).

-export([create_api_token/1, create_api_token/2
        ,make_api_token/4
        ,split_api_token/1
        ]).

-ifdef(TEST).
-export([auth_hash/4
        ,encode_timestamp/1
        ]).
-endif.

-include("crossbar.hrl").
-define(U_CONFIG_CAT, <<"crossbar.ubiquiti">>).

-define(VERSION, <<"1">>).
-define(EXPIRES, kapps_config:get_integer(?U_CONFIG_CAT, <<"api_token_expires_s">>, 1800)).
-define(SECRET, kapps_config:get_ne_binary(?U_CONFIG_CAT, <<"api_secret">>)).
-define(SALT_LENGTH, kapps_config:get_integer(?U_CONFIG_CAT, <<"salt_length">>, 20)).

-spec create_api_token(kz_term:ne_binary()) -> kz_term:ne_binary().
create_api_token(ProviderId) ->
    create_api_token(ProviderId, ?SECRET).

-spec create_api_token(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
create_api_token(ProviderId, <<_/binary>> = Secret) ->
    Salt = kz_binary:rand_hex(?SALT_LENGTH),
    ExpireTime = kz_time:current_unix_tstamp() + ?EXPIRES,
    make_api_token(ProviderId, ExpireTime, Salt, Secret);
create_api_token(_ProviderId, 'undefined') ->
    throw({'error', 'no_api_secret'}).

-spec make_api_token(kz_term:ne_binary(), integer(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
make_api_token(ProviderId, Timestamp, Salt, Secret) ->
    TimestampHex = encode_timestamp(Timestamp),
    kz_binary:join([?VERSION % Version
                   ,Salt
                   ,TimestampHex
                   ,auth_hash(ProviderId, TimestampHex, Salt, Secret)
                   ]
                  ,<<":">>
                  ).

-spec encode_timestamp(integer()) -> kz_term:ne_binary().
encode_timestamp(Timestamp) when is_integer(Timestamp) ->
    kz_term:to_lower_binary(integer_to_list(Timestamp, 16)).

-spec decode_timestamp(kz_term:ne_binary()) -> integer().
decode_timestamp(<<_/binary>> = TimestampHex) ->
    list_to_integer(binary_to_list(TimestampHex), 16).

-spec auth_hash(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
auth_hash(ProviderId, TimestampHex, Salt, Secret) ->
    Auth = kz_term:to_lower_binary(
             kz_binary:hexencode(
               crypto:hash('sha', [Salt, Secret])
              )
            ),

    PreHash = [ProviderId
              ,TimestampHex
              ,Auth
              ],

    Hash = crypto:hash('sha', PreHash),

    kz_binary:hexencode(Hash).

-spec split_api_token(kz_term:ne_binary()) -> {kz_term:ne_binary(), integer(), kz_term:ne_binary()}.
split_api_token(Token) ->
    [?VERSION, Salt, TimestampHex, Auth] = binary:split(kz_term:to_lower_binary(Token), <<":">>, ['global']),
    {Salt, decode_timestamp(TimestampHex), Auth}.
