%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_notifications_test).

-include_lib("eunit/include/eunit.hrl").

merge_available_test() ->
    Available = kz_json:decode(<<"[{\"id\":\"o1\",\"k1\":\"v1\"},{\"id\":\"o2\",\"k2\":\"v2\"},{\"id\":\"o3\",\"k3\":\"v3\"}]">>),
    AccountAvailable = kz_json:decode(<<"[{\"id\":\"o1\",\"k1\":\"a1\"},{\"id\":\"o2\",\"k2\":\"a2\"}]">>),

    Merged = cb_notifications:merge_available(AccountAvailable, Available),

    ?assertEqual(<<"a1">>, kz_json:get_value([2,<<"k1">>], Merged)),
    ?assertEqual(<<"a2">>, kz_json:get_value([1,<<"k2">>], Merged)),
    ?assertEqual(<<"v3">>, kz_json:get_value([3,<<"k3">>], Merged)).
