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

-spec api_token(ne_binary(), integer(), ne_binary(), ne_binary()) -> ne_binary().
api_token(ProviderId, Timestamp, Salt, Secret) ->
    TimestampHex = wh_util:to_lower_binary(integer_to_list(Timestamp, 16)),
    wh_util:join_binary(
      [<<"1">> % Version
       ,wh_util:to_binary(Salt)
       ,TimestampHex
       ,auth_hash(ProviderId, TimestampHex, Salt, Secret)
      ]
      ,<<":">>
     ).

-spec auth_hash(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
auth_hash(ProviderId, TimestampHex, Salt, Secret) ->
    ProviderLen = byte_size(ProviderId),
    TimestampLen = byte_size(TimestampHex),
    SaltLen = byte_size(Salt),
    wh_util:to_hex_binary(
      crypto:hash('sha'
                  ,<<ProviderId:ProviderLen/binary
                     ,TimestampHex:TimestampLen/binary
                     ,(wh_util:to_hex_binary(crypto:hash('sha', <<Salt:SaltLen/binary, Secret/binary>>)))/binary
                   >>
                 )).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,

    ApiToken = <<"1:966d1579e4c0c324c0d95c266adce307bd1e7e08:5373a4ce:61e46d33721f4de8c806b079cd1b6aecb210dd70">>,
    Generated = api_token(ProviderId, Timestamp, Salt, Secret),

    ?debugFmt("tstamp hex: ~s~n", [wh_util:to_lower_binary(integer_to_list(Timestamp, 16))]),
    ?debugFmt("api: ~s~n", [ApiToken]),
    ?debugFmt("gen: ~s~n", [Generated]),

    Hash = auth_hash(ProviderId, <<"5373f465">>, Salt, Secret),
    ?debugFmt("hash: ~s~n", [Hash]),

    ?assertEqual(ApiToken, Generated).
-endif.
