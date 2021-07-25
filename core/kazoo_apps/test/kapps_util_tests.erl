%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2021, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

kapps_util_with_fixtures_test_() ->
    {'setup'
    ,fun kzd_test_fixtures:setup/0
    ,fun kzd_test_fixtures:cleanup/1
    ,fun(_) -> [test_get_master_account_id()] end
    }.

test_get_master_account_id() ->
    [{"Verify the fixture provides the master account"
     ,?_assertEqual({'ok', ?FIXTURE_MASTER_ACCOUNT_ID}, kapps_util:get_master_account_id())
     }
    ].
