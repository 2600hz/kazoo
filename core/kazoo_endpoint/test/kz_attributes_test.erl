%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_attributes_test).

-include_lib("eunit/include/eunit.hrl").

get_flags_test_() ->
    Call = create_kapps_call(),
    [{"verify that get flags will pull the static and dynamic flags from the account as well as endpoint"
     ,?_assertEqual([<<"sys_info">>], kz_attributes:get_flags(<<"callflows">>, Call))
     }
    ].

process_dynamic_flags_test_() ->
    Call = create_kapps_call(),
    [{"verify that dynamic CCVs can be fetched and are converted to binary"
     ,?_assertEqual([<<"sys_info">>], kz_attributes:process_dynamic_flags([<<"custom_channel_vars.authorizing_type">>], Call))
     }
    ,{"verify that exported kapps_call functions can be used"
     ,?_assertEqual([<<"20255520140">>], kz_attributes:process_dynamic_flags([<<"to_user">>], Call))
     }
    ,{"verify that non-exported kapps_call functions dont crash"
     ,?_assertEqual([], kz_attributes:process_dynamic_flags([<<"not_exported">>], Call))
     }
    ,{"verify that the zone name can be resolved"
     ,?_assertEqual([<<"local">>], kz_attributes:process_dynamic_flags([<<"zone">>], Call))
     }
    ,{"verify that dynamic flags are added to a provided list of static flags"
     ,?_assertEqual([<<"local">>, <<"static">>], kz_attributes:process_dynamic_flags([<<"zone">>], [<<"static">>], Call))
     }
    ].

create_kapps_call() ->
    RouteReq = kz_json:load_fixture_from_file('kazoo_call', "fixtures/route_req", "inbound-onnet-trunkstore.json"),
    RouteWin = kz_json:load_fixture_from_file('kazoo_call', "fixtures/route_win", "inbound-onnet-trunkstore.json"),
    kapps_call:from_route_win(RouteWin, kapps_call:from_route_req(RouteReq)).
