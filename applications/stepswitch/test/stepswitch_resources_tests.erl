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


global_test_() ->
    GlobalJObjs = fixture("test/global_resources.json"),
    LocalJObjs = fixture("test/local_resources.json"),
    LocalResources = stepswitch_resources:fetch_local_resources(?ACCOUNT_ID, LocalJObjs),
    [?_assertEqual(3, length(stepswitch_resources:resources_from_jobjs(GlobalJObjs)))
    ,?_assertEqual(6, length(LocalResources))
    ].


%% Internals

fixture(Path) ->
    {ok,Bin} = file:read_file(Path),
    case lists:suffix(".json", Path) of
        false -> Bin;
        true -> kz_json:decode(Bin)
    end.
