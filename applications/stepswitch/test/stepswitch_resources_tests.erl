%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(stepswitch_resources_tests).

-include_lib("eunit/include/eunit.hrl").
-include("stepswitch.hrl").


global_resources_test_() ->
    GlobalJObjs = fixture("test/global_resources.json"),
    [G1,G2,G3] = stepswitch_resources:resources_from_jobjs(GlobalJObjs),
    [?_assertEqual(<<"d60be7f888d8b7d937a846e07b927886">>, stepswitch_resources:get_resrc_id(G1))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_weight(G1))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_grace_period(G1))
    ,?_assertEqual(<<"d4bc772163c9ab2ebc4ee79637ae3433">>, stepswitch_resources:get_resrc_id(G2))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_weight(G2))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_grace_period(G2))
    ,?_assertEqual(<<"7e6ab78098a1a788019a54ed4f51ba82">>, stepswitch_resources:get_resrc_id(G3))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_weight(G3))
    ,?_assertEqual(3, stepswitch_resources:get_resrc_grace_period(G3))
    ].

local_resources_test_() ->
    LocalJObjs = fixture("test/local_resources.json"),
    [L1,L2,L3,L4,L5,L6] = stepswitch_resources:fetch_local_resources(?ACCOUNT_ID, LocalJObjs),
    [?_assertEqual(<<"ea2d868dad3629254d57d5a1f6a69cde">>, stepswitch_resources:get_resrc_id(L1))
    ,?_assertEqual(<<"LVL3">>, stepswitch_resources:get_resrc_name(L1))
    ,?_assertEqual(4, stepswitch_resources:get_resrc_weight(L1))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L1))
    ,?_assertEqual(<<"66f10c653f8e65e0e16f0ba6788bfe95-caribbean">>, stepswitch_resources:get_resrc_id(L2))
    ,?_assertEqual(<<"Bandwidth Template - caribbean">>, stepswitch_resources:get_resrc_name(L2))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_weight(L2))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L2))
    ,?_assertEqual(<<"6365415261d12830250e11116743f42a-toll_us">>, stepswitch_resources:get_resrc_id(L3))
    ,?_assertEqual(<<"TLNX - toll_us">>, stepswitch_resources:get_resrc_name(L3))
    ,?_assertEqual(1, stepswitch_resources:get_resrc_weight(L3))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L3))
    ,?_assertEqual(<<"1c30a2ada029e2b31654ae2c3937b04c-unknown">>, stepswitch_resources:get_resrc_id(L4))
    ,?_assertEqual(<<"ccaacacaccc - unknown">>, stepswitch_resources:get_resrc_name(L4))
    ,?_assertEqual(2, stepswitch_resources:get_resrc_weight(L4))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L4))
    ,?_assertEqual(<<"1c30a2ada029e2b31654ae2c3937b04c-did_us">>, stepswitch_resources:get_resrc_id(L5))
    ,?_assertEqual(<<"ccaacacaccc - did_us">>, stepswitch_resources:get_resrc_name(L5))
    ,?_assertEqual(2, stepswitch_resources:get_resrc_weight(L5))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L5))
    ,?_assertEqual(<<"1c30a2ada029e2b31654ae2c3937b04c-caribbean">>, stepswitch_resources:get_resrc_id(L6))
    ,?_assertEqual(<<"ccaacacaccc - caribbean">>, stepswitch_resources:get_resrc_name(L6))
    ,?_assertEqual(2, stepswitch_resources:get_resrc_weight(L6))
    ,?_assertEqual(5, stepswitch_resources:get_resrc_grace_period(L6))
    ].


%% Internals

fixture(Path) ->
    {ok,Bin} = file:read_file(Path),
    case lists:suffix(".json", Path) of
        false -> Bin;
        true -> kz_json:decode(Bin)
    end.
