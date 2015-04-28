%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Ubiquiti SSO Utilities
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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

-include("../crossbar.hrl").
-define(U_CONFIG_CAT, <<"crossbar.ubiquiti">>).

-define(VERSION, <<"1">>).
-define(EXPIRES, whapps_config:get_integer(?U_CONFIG_CAT, <<"api_token_expires_s">>, 1800)).
-define(SECRET, whapps_config:get(?U_CONFIG_CAT, <<"api_secret">>)).
-define(SALT_LENGTH, whapps_config:get_integer(?U_CONFIG_CAT, <<"salt_length">>, 20)).

-spec create_api_token(ne_binary()) -> ne_binary().
-spec create_api_token(ne_binary(), ne_binary()) -> ne_binary().
create_api_token(ProviderId) ->
    create_api_token(ProviderId, ?SECRET).

create_api_token(ProviderId, <<_/binary>> = Secret) ->
    Salt = wh_util:rand_hex_binary(?SALT_LENGTH),
    ExpireTime = wh_util:current_unix_tstamp() + ?EXPIRES,
    make_api_token(ProviderId, ExpireTime, Salt, Secret);
create_api_token(_ProviderId, 'undefined') ->
    throw({'error', 'no_api_secret'}).

-spec make_api_token(ne_binary(), integer(), ne_binary(), ne_binary()) -> ne_binary().
make_api_token(ProviderId, Timestamp, Salt, Secret) ->
    TimestampHex = encode_timestamp(Timestamp),
    wh_util:join_binary(
      [?VERSION % Version
       ,Salt
       ,TimestampHex
       ,auth_hash(ProviderId, TimestampHex, Salt, Secret)
      ]
      ,<<":">>
     ).

-spec encode_timestamp(integer()) -> ne_binary().
encode_timestamp(Timestamp) when is_integer(Timestamp) ->
    wh_util:to_lower_binary(integer_to_list(Timestamp, 16)).

-spec decode_timestamp(ne_binary()) -> integer().
decode_timestamp(<<_/binary>> = TimestampHex) ->
    list_to_integer(binary_to_list(TimestampHex), 16).

-spec auth_hash(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
auth_hash(ProviderId, TimestampHex, Salt, Secret) ->
    Auth = wh_util:to_lower_binary(
             wh_util:hexencode_binary(
               crypto:hash('sha', [Salt, Secret])
              )
            ),

    PreHash = [ProviderId
               ,TimestampHex
               ,Auth
              ],

    Hash = crypto:hash('sha', PreHash),

    wh_util:hexencode_binary(Hash).

-spec split_api_token(ne_binary()) -> {ne_binary(), integer(), ne_binary()}.
split_api_token(Token) ->
    [?VERSION, Salt, TimestampHex, Auth] = binary:split(wh_util:to_lower_binary(Token), <<":">>, ['global']),
    {Salt, decode_timestamp(TimestampHex), Auth}.
