%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Ubiquiti SSO Utilities
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ubiquiti_util).

-export([make_api_token/4
         ,split_api_token/1
        ]).

-include("../crossbar.hrl").

-define(VERSION, <<"1">>).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,

    ApiToken = <<"1:966d1579e4c0c324c0d95c266adce307bd1e7e08:5373a4ce:edd438990c1fc47a8c780d11fb4bbb0c49b35731">>,
    Generated = make_api_token(ProviderId, Timestamp, Salt, Secret),

    ?assertEqual(ApiToken, Generated).

split_api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,
    Auth = auth_hash(ProviderId, encode_timestamp(Timestamp), Salt, Secret),

    Generated = make_api_token(ProviderId, Timestamp, Salt, Secret),
    {Salt1, Timestamp1, Auth1} = split_api_token(Generated),

    ?assertEqual(Salt, Salt1),
    ?assertEqual(Timestamp, Timestamp1),
    ?assertEqual(Auth, Auth1).

-endif.
