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
    Disabled = <<"c9bb38dfdff0f4738636e7beac700d3b-tollfree_us">>, %% enabled:false
    Ids = [<<"6365415261d12830250e11116743f42a-toll_us">>
          ,<<"1c30a2ada029e2b31654ae2c3937b04c-caribbean">>
          ,<<"1c30a2ada029e2b31654ae2c3937b04c-did_us">>
          ,<<"1c30a2ada029e2b31654ae2c3937b04c-unknown">>
          ,Disabled
          ,<<"ea2d868dad3629254d57d5a1f6a69cde">>
          ,<<"66f10c653f8e65e0e16f0ba6788bfe95-caribbean">>
          ],
    [case Id =:= Disabled of
         true -> ?_assertEqual(l(Id), Props);
         false -> ?_assert(l(Id) =:= maps:with(maps:keys(l(Id)), maps:from_list(Props)))
     end
     || Id <- Ids,
        Props <- [stepswitch_resources:get_props(Id, ?ACCOUNT_ID)]
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
     ,<<"Rules">> => [<<"^(911)\$">>]
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
     ,<<"Rules">> => [<<"^\\+1((?:800|88\\d|877|866|855|844|833|822)\\d{7})\$">>]
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
     ,<<"Rules">> => [<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>]
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
     ,<<"Rules">> => [<<"^(011\\d*)\$|^(00\\d*)\$">>]
     ,<<"Caller-ID-Rules">> => []
     }.


l(<<"6365415261d12830250e11116743f42a-toll_us">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"6365415261d12830250e11116743f42a-toll_us">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"TLNX - toll_us">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-287505ba35236b47a47011dba53ef7a8">>
     ,<<"Rules">> => [<<"^\\+1(900\\d{7})\$">>]
     ,<<"T38">> => false
     ,<<"Weight">> => 1
     };

l(<<"1c30a2ada029e2b31654ae2c3937b04c-caribbean">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"1c30a2ada029e2b31654ae2c3937b04c-caribbean">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"ccaacacaccc - caribbean">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-707061f7bff991b38d004f125b026416">>
     ,<<"Rules">> =>
          [<<"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})\$">>]
     ,<<"T38">> => false
     ,<<"Weight">> => 2
     };

l(<<"1c30a2ada029e2b31654ae2c3937b04c-did_us">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"1c30a2ada029e2b31654ae2c3937b04c-did_us">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"ccaacacaccc - did_us">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-707061f7bff991b38d004f125b026416">>
     ,<<"Rules">> => [<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>]
     ,<<"T38">> => false
     ,<<"Weight">> => 2
     };

l(<<"1c30a2ada029e2b31654ae2c3937b04c-unknown">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"1c30a2ada029e2b31654ae2c3937b04c-unknown">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"ccaacacaccc - unknown">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-707061f7bff991b38d004f125b026416">>
     ,<<"Rules">> => [<<"^(.{10})\$">>]
     ,<<"T38">> => false
     ,<<"Weight">> => 2
     };

l(<<"c9bb38dfdff0f4738636e7beac700d3b-tollfree_us">>) ->
    undefined;

l(<<"ea2d868dad3629254d57d5a1f6a69cde">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"ea2d868dad3629254d57d5a1f6a69cde">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"LVL3">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-c5dc54badfa28c27ae31280d981f0127">>
     ,<<"Rules">> => []
     ,<<"T38">> => false
     ,<<"Weight">> => 4
     };

l(<<"66f10c653f8e65e0e16f0ba6788bfe95-caribbean">>) ->
    #{<<"Bypass-Media">> => false
     ,<<"Caller-ID-Rules">> => []
     ,<<"Codecs">> => []
     ,<<"Flags">> => []
     ,<<"Format-From-URI">> => false
     ,<<"From-Account-Realm">> => false
     ,<<"Global">> => false
     ,<<"Grace-Period">> => 5
     ,<<"ID">> => <<"66f10c653f8e65e0e16f0ba6788bfe95-caribbean">>
     ,<<"Is-Emergency">> => false
     ,<<"Name">> => <<"Bandwidth Template - caribbean">>
     ,<<"Require-Flags">> => false
     ,<<"Rev">> => <<"2-cdc562eaa1501cfeb29a4ec28441adc3">>
     ,<<"Rules">> =>
          [<<"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})\$">>]
     ,<<"T38">> => false
     ,<<"Weight">> => 5
     }.
