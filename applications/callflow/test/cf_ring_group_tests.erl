%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author SIPLABS LLC (Ilya Ashchepkov)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_ring_group_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HELP_10392_ACCOUNT_ID, <<"account0000000000000000000000001">>).
-define(HELP_10392_USER_ID, <<"user0000000000000000000000000001">>).
-define(HELP_10392_DEVICE_ID, <<"device00000000000000000000000001">>).
-define(HELP_10392_GROUP_ID, <<"group000000000000000000000000001">>).

weighted_random_sort_test_() ->
    EndpointsInt = [{1, <<"ep1">>}
                   ,{2, <<"ep2">>}
                   ,{3, <<"ep3">>}
                   ],
    Endpoints = cf_ring_group:weighted_random_sort(EndpointsInt),

    ?debugFmt("~ninit: ~p~nafter: ~p~n", [EndpointsInt, Endpoints]),

    [?_assertEqual(length(EndpointsInt), length(Endpoints))
     |
     [?_assertEqual('true', lists:member(X, EndpointsInt))
      || X <- Endpoints
     ]
    ].

%% HELP-260010392
help_10392_test_() ->
    {'setup'
    ,fun() -> kz_fixturedb_util:start_me('true') end
    ,fun kz_fixturedb_util:stop_me/1
    ,fun(_ReturnOfSetup) ->
             [{"HELP-10392: Resolve RG endpoints' in order", help_10392()}
             ,{"HELP-10392: Sort endpoints by delay (Ring strategy=simultaneous (at the same time))", get_endpoints()}
             ,{"Ring strategy=single (in order)", get_endpoints_in_order()}
             ]
     end
    }.

help_10392() ->
    Endpoint = help_10392_rg_member(<<"user">>, ?HELP_10392_USER_ID),
    User = kz_json:set_value(<<"delay">>, 0, Endpoint),
    Group = help_10392_rg_member(<<"group">>, ?HELP_10392_GROUP_ID),
    EndpointDelay10 = {?HELP_10392_DEVICE_ID
                      ,help_10392_add_source(help_10392_add_group_weight(Endpoint))
                      },
    EndpointDelay0 = {?HELP_10392_DEVICE_ID, help_10392_add_source(User)},

    Call = kapps_call:set_account_id(?HELP_10392_ACCOUNT_ID, kapps_call:new()),

    Resp0 = cf_ring_group:resolve_endpoint_ids(help_10392_data([User, Group]), Call),
    Resp1 = cf_ring_group:resolve_endpoint_ids(help_10392_data([Group, User]), Call),
    Resp2 = cf_ring_group:resolve_endpoint_ids(help_10392_data([User, User]), Call),
    Resp3 = cf_ring_group:resolve_endpoint_ids(help_10392_data([Group, Group]), Call),

    Expected0 = [EndpointDelay0, EndpointDelay10],
    Expected1 = [EndpointDelay10, EndpointDelay0],
    Expected2 = [EndpointDelay0],
    Expected3 = [EndpointDelay10],

    [{"Honor RG member's order: [User, Group]", ?_assertEqual(Expected0, Resp0)}
    ,{"Honor RG member's order: [Group, User]", ?_assertEqual(Expected1, Resp1)}
    ,{"Honor RG member's order: [User, User]", ?_assertEqual(Expected2, Resp2)}
    ,{"Honor RG member's order: [Group, Group]", ?_assertEqual(Expected3, Resp3)}
    ].

get_endpoints() ->
    Call = help_10392_call(),

    %% User with delay = 10.
    MemberUser10 = help_10392_rg_member(<<"user">>, ?HELP_10392_USER_ID),
    %% User with delay = 0.
    MemberUser0 = kz_json:set_value(<<"delay">>, 0, MemberUser10),
    %% Group with delay = 10.
    MemberGroup10 = help_10392_rg_member(<<"group">>, ?HELP_10392_GROUP_ID),
    %% Group with delay = 0.
    MemberGroup0 = kz_json:set_value(<<"delay">>, 0, MemberGroup10),

    %% Build and Endpoint with delay = 0.
    {'ok', [ExpectedDelay0]} = kz_endpoint:build(?HELP_10392_DEVICE_ID, MemberGroup0, Call),
    ExpectedDelay10 = kz_json:set_value(<<"Endpoint-Delay">>, <<"10">>, ExpectedDelay0),

    Expected = [ExpectedDelay0, ExpectedDelay10],

    Resp0 = cf_ring_group:get_endpoints(help_10392_data([MemberUser0, MemberGroup10]), Call),
    Resp1 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup0, MemberUser10]), Call),
    Resp2 = cf_ring_group:get_endpoints(help_10392_data([MemberUser10, MemberGroup0]), Call),
    Resp3 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup10, MemberUser0]), Call),
    Resp4 = cf_ring_group:get_endpoints(help_10392_data([MemberUser0, MemberGroup0]), Call),
    Resp5 = cf_ring_group:get_endpoints(help_10392_data([MemberUser10, MemberGroup10]), Call),
    Resp6 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup10, MemberUser0, MemberGroup10]), Call),
    Resp7 = cf_ring_group:get_endpoints(help_10392_data([MemberUser10, MemberGroup0, MemberUser10]), Call),
    Resp8 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup0, MemberUser10, MemberGroup0]), Call),

    [{"Sort RG member's by delay: [0, 10] -> [0, 10]"
     ,?_assert(match_jobjs_delay(Expected, Resp0))
     }
    ,{"Sort RG member's by delay: [0, 10] -> [0, 10]"
     ,?_assert(match_jobjs_delay(Expected, Resp1))
     }
    ,{"Sort RG member's by delay: [10, 0] -> [0, 10]"
     ,?_assert(match_jobjs_delay(Expected, Resp2))
     }
    ,{"Sort RG member's by delay: [10, 0] -> [0, 10]"
     ,?_assert(match_jobjs_delay(Expected, Resp3))
     }
    ,{"RG members with same id, timeout, and delay are included only once"
     ,?_assert(match_jobjs_delay([ExpectedDelay0], Resp4))
     }
    ,{"RG members with same id, timeout, and delay are included only once"
     ,?_assert(match_jobjs_delay([ExpectedDelay10], Resp5))
     }
    ,{"RG members with same id, timeout, and delay are included only once"
     ,?_assert(match_jobjs_delay(Expected, Resp6))
     }
    ,{"RG members with same id, timeout, and delay are included only once"
     ,?_assert(match_jobjs_delay(Expected, Resp7))
     }
    ,{"RG members with same id, timeout, and delay are included only once"
     ,?_assert(match_jobjs_delay(Expected, Resp8))
     }
    ].

get_endpoints_in_order() ->
    Call = help_10392_call(),

    %% User with delay = 10.
    MemberUser10 = help_10392_rg_member(<<"user">>, ?HELP_10392_USER_ID),
    %% User with delay = 0.
    MemberUser0 = kz_json:set_value(<<"delay">>, 0, MemberUser10),
    %% Group with delay = 10.
    MemberGroup10 = help_10392_rg_member(<<"group">>, ?HELP_10392_GROUP_ID),
    %% Group with delay = 0.
    MemberGroup0 = kz_json:set_value(<<"delay">>, 0, MemberGroup10),

    %% Build and Endpoint with delay = 0.
    {'ok', [ExpectedDelay0]} = kz_endpoint:build(?HELP_10392_DEVICE_ID, MemberGroup0, Call),
    ExpectedDelay10 = kz_json:set_value(<<"Endpoint-Delay">>, <<"10">>, ExpectedDelay0),

    Expected0 = [ExpectedDelay0, ExpectedDelay10],
    Expected1 = [ExpectedDelay10, ExpectedDelay0],

    Resp0 = cf_ring_group:get_endpoints(help_10392_data([MemberUser10, MemberUser0], <<"single">>), Call),
    Resp1 = cf_ring_group:get_endpoints(help_10392_data([MemberUser0, MemberUser10], <<"single">>), Call),
    Resp2 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup0, MemberGroup10], <<"single">>), Call),
    Resp3 = cf_ring_group:get_endpoints(help_10392_data([MemberGroup10, MemberGroup0], <<"single">>), Call),

    [{"When ring-strategy=single (in order) don't order endpoints by delay"
     ,?_assert(match_jobjs_delay(Expected1, Resp0))
     }
    ,{"When ring-strategy=single (in order) don't order endpoints by delay"
     ,?_assert(match_jobjs_delay(Expected0, Resp1))
     }
    ,{"When ring-strategy=single (in order) don't order endpoints by delay"
     ,?_assert(match_jobjs_delay(Expected0, Resp2))
     }
    ,{"When ring-strategy=single (in order) don't order endpoints by delay"
     ,?_assert(match_jobjs_delay(Expected1, Resp3))
     }
    ].

match_jobjs_delay(L0, L1) ->
    match_jobjs_delay(L0, L1, 'false').

match_jobjs_delay([], [], Result) ->
    Result;
match_jobjs_delay([Expected | Tail0], [Received | Tail1], _) ->
    case kz_json:are_json_objects([Expected, Received])
        andalso same_delay(Expected, Received)
    of
        'false' -> 'false';
        'true' -> match_jobjs_delay(Tail0, Tail1, 'true')
    end;
match_jobjs_delay(_, _, _) ->
    'false'.

same_delay(JObj1, JObj2) ->
    Key = <<"Endpoint-Delay">>,
    kz_json:get_value(Key, JObj1) =:= kz_json:get_value(Key, JObj2).

%% Ring group member object
help_10392_rg_member(MemberType, MemberId) ->
    kz_json:from_list([{<<"endpoint_type">>, MemberType}
                      ,{<<"id">>, MemberId}
                      ,{<<"delay">>, 10}
                      ,{<<"timeout">>, 20}
                      ]).

help_10392_add_group_weight(RGMember) ->
    kz_json:set_value(<<"weight">>, 20, RGMember).

help_10392_data(TestEndpoints) ->
    help_10392_data(TestEndpoints, <<"simultaneous">>).

help_10392_data(TestEndpoints, Strategy) ->
    kz_json:from_list([{<<"name">>, <<"RG HELP-10392">>}
                      ,{<<"endpoints">>, TestEndpoints}
                      ,{<<"strategy">>, Strategy}
                      ,{<<"timeout">>, 30}
                      ,{<<"repeats">>, 1}
                      ,{<<"ignore_forward">>, true}
                      ]).

help_10392_add_source(RGMember) ->
    kz_json:set_value(<<"source">>, <<"cf_ring_group">>, RGMember).

help_10392_call() ->
    Routines = [{fun kapps_call:set_account_id/2, ?HELP_10392_ACCOUNT_ID}
               ,{fun kapps_call:set_account_db/2, kz_util:format_account_db(?HELP_10392_ACCOUNT_ID)}
               ,{fun kapps_call:set_request/2, <<"+14158867901@4a6863.sip.2600hz.local">>}
               ,{fun kapps_call:set_to/2, <<"+14158867901@4a6863.sip.2600hz.local">>}
               ,{fun kapps_call:set_from/2, <<"+14158867900@4a6863.sip.2600hz.local">>}
               ,{fun kapps_call:set_call_id/2, kz_binary:rand_hex(6)}
               ,{fun kapps_call:set_resource_type/2, <<"audio">>}
               ,{fun kapps_call:set_caller_id_name/2, <<"Test Device 1">>}
               ,{fun kapps_call:set_caller_id_number/2, <<"+14158867900">>}
               ,{fun kapps_call:set_custom_channel_var/3, <<"Metaflow-App">>, 'false'}
               ],
    kapps_call:exec(Routines, kapps_call:new()).
