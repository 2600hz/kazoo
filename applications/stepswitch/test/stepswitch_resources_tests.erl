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
    Gs = stepswitch_resources:get_props(),
    [?_assert(g(I) =:= maps:with(maps:keys(g(I)), GI))
    || I <- lists:seq(1, length(Gs)),
       GI <- [maps:from_list(lists:nth(I, Gs))]
    ].

local_resources_test_() ->
    Ls = stepswitch_resources:get_props(?ACCOUNT_ID),
    [io:format(user, "\n>>> ~p\n", [L]) || L <- Ls],
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


%% Intrernals

g(1) ->
    #{<<"Name">> => <<"Dash 911 - emergency">>
     ,<<"ID">> => <<"d60be7f888d8b7d937a846e07b927886-emergency">>
     ,<<"Rev">> => <<"3-52105eda324b1bbac691f0623a5551fe">>
     ,<<"Weight">> => 1
     ,<<"Global">> => true
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Require-Flags">> => false
     ,<<"Is-Emergency">> => true
     ,<<"T38">> => false
     ,<<"Bypass-Media">> => false
     ,<<"Grace-Period">> => 5
     ,<<"Flags">> => [<<"CallerID">>,<<"CNAM">>,<<"US48">>,<<"Alaska">>,<<"Hawaii">>,<<"TollFree">>]
     ,<<"Codecs">> => [<<"PCMU">>]
     ,<<"Rules">> => [<<"^(911)$">>]
     ,<<"Caller-ID-Rules">> => []
     };

g(2) ->
    #{<<"Name">> => <<"FlowRoute Domestic - tollfree_us">>
     ,<<"ID">> => <<"d4bc772163c9ab2ebc4ee79637ae3433-tollfree_us">>
     ,<<"Rev">> => <<"12-e8b8bf95f9576005fd133c7aa17f1d6b">>
     ,<<"Weight">> => 50
     ,<<"Global">> => true
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Require-Flags">> => false
     ,<<"Is-Emergency">> => false
     ,<<"T38">> => false
     ,<<"Bypass-Media">> => false
     ,<<"Grace-Period">> => 5
     ,<<"Flags">> => [<<"fax">>]
     ,<<"Codecs">> => [<<"PCMU">>]
     ,<<"Rules">> => [<<"^\\+1((?:800|88\\d|877|866|855|844|833|822)\\d{7})$">>]
     ,<<"Caller-ID-Rules">> => []
     };

g(3) ->
    #{<<"Name">> => <<"FlowRoute Domestic - did_us">>
     ,<<"ID">> => <<"d4bc772163c9ab2ebc4ee79637ae3433-did_us">>
     ,<<"Rev">> => <<"12-e8b8bf95f9576005fd133c7aa17f1d6b">>
     ,<<"Weight">> => 50
     ,<<"Global">> => true
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Require-Flags">> => false
     ,<<"Is-Emergency">> => false
     ,<<"T38">> => false
     ,<<"Bypass-Media">> => false
     ,<<"Grace-Period">> => 5
     ,<<"Flags">> => [<<"fax">>]
     ,<<"Codecs">> => [<<"PCMU">>]
     ,<<"Rules">> => [<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>]
     ,<<"Caller-ID-Rules">> => []
     };

g(4) ->
    #{<<"Name">> => <<"Flowroute Int. - international">>
     ,<<"ID">> => <<"7e6ab78098a1a788019a54ed4f51ba82-international">>
     ,<<"Rev">> => <<"5-fbdd5c963775f45cb27f01cc9a778660">>
     ,<<"Weight">> => 50
     ,<<"Global">> => true
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Require-Flags">> => false
     ,<<"Is-Emergency">> => false
     ,<<"T38">> => false
     ,<<"Bypass-Media">> => false
     ,<<"Grace-Period">> => 5
     ,<<"Flags">> => []
     ,<<"Codecs">> => [<<"PCMU">>]
     ,<<"Rules">> => [<<"^(011\\d*)$|^(00\\d*)$">>]
     ,<<"Caller-ID-Rules">> => []
     }.
