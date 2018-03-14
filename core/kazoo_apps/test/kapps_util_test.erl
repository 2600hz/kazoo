%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_util_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

get_master_account_id_test() ->
    [{"Verify the fixture provides the master account"
     ,?_assertEqual(?FIXTURE_MASTER_ACCOUNT_ID, kapps_util:get_master_account_id())
     }
    ].
