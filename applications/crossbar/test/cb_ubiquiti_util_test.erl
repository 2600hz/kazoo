%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_ubiquiti_util_test).

-include_lib("eunit/include/eunit.hrl").

make_api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,

    ApiToken = <<"1:966d1579e4c0c324c0d95c266adce307bd1e7e08:5373a4ce:edd438990c1fc47a8c780d11fb4bbb0c49b35731">>,
    Generated = cb_ubiquiti_util:make_api_token(ProviderId, Timestamp, Salt, Secret),

    ?assertEqual(ApiToken, Generated).

split_api_token_test() ->
    ProviderId = <<"2600hz">>,
    Secret     = <<"f1986434eb540c8c956eb0a21094c38c6f9f12bf">>,
    Salt       = <<"966d1579e4c0c324c0d95c266adce307bd1e7e08">>,
    Epoch      = 1400080558,
    Expiration = 7200,
    Timestamp = Epoch + Expiration,
    Auth = cb_ubiquiti_util:auth_hash(ProviderId, cb_ubiquiti_util:encode_timestamp(Timestamp), Salt, Secret),

    Generated = cb_ubiquiti_util:make_api_token(ProviderId, Timestamp, Salt, Secret),
    {Salt1, Timestamp1, Auth1} = cb_ubiquiti_util:split_api_token(Generated),

    ?assertEqual(Salt, Salt1),
    ?assertEqual(Timestamp, Timestamp1),
    ?assertEqual(Auth, Auth1).
