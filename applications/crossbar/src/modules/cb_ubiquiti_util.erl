%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Ubiquiti SSO Utilities
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ubiquiti_util).

-export([api_token/4]).

-include("../crossbar.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec api_token(ne_binary(), integer(), ne_binary(), ne_binary()) -> ne_binary().
api_token(ProviderId, Timestamp, Salt, Secret) ->
    TimestampHex = wh_util:to_lower_binary(integer_to_list(Timestamp, 16)),
    wh_util:join_binary(
      [<<"1">> % Version
       ,Salt
       ,TimestampHex
       ,auth_hash(ProviderId, TimestampHex, Salt, Secret)
      ]
      ,<<":">>
     ).

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

    ?debugFmt("prehash: ~s~n", [PreHash]),

    Hash = crypto:hash('sha', PreHash),

    ?debugFmt("hashe: ~s~n", [bstr:hexencode(Hash)]),

    wh_util:hexencode_binary(Hash).

-ifdef(TEST).

api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,
    TimestampHex = wh_util:to_lower_binary(integer_to_list(Timestamp, 16)),

    ApiToken = <<"1:966d1579e4c0c324c0d95c266adce307bd1e7e08:5373a4ce:61e46d33721f4de8c806b079cd1b6aecb210dd70">>,
    Generated = api_token(ProviderId, Timestamp, Salt, Secret),

    Hash = auth_hash(ProviderId, TimestampHex, Salt, Secret),

    ?debugFmt("hash: ~s~n", [Hash]),
    ?debugFmt("api: ~s~n", [ApiToken]),
    ?debugFmt("gen: ~s~n", [Generated]),

    ?assertEqual(ApiToken, Generated).

-endif.
