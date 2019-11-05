%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(skel_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

kz_services_test_() ->
    {'setup'
    ,fun setup/0
    ,fun cleanup/1
    ,fun(Map) ->
             [test_fetch(Map)]
     end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {'ok', _} = application:ensure_all_started('kazoo_config'),
    {'ok', CachesPid} = kazoo_caches_sup:start_link(),
    LinkPid = kazoo_fixturedb:start(),

    %% NOTE: The child services doc does not exist in the fixture so as
    %%       to test creation on first request
    #{pid => LinkPid
     ,caches_pid => CachesPid
     ,master_services => kz_services:fetch(?FIXTURE_MASTER_ACCOUNT_ID)
     ,reseller_services => kz_services:fetch(?FIXTURE_RESELLER_ACCOUNT_ID)
     ,parent_services => kz_services:fetch(?FIXTURE_PARENT_ACCOUNT_ID)
     ,child_services => kz_services:fetch(?FIXTURE_CHILD_ACCOUNT_ID)
     }.

cleanup(#{pid := LinkPid, caches_pid := CachesPid}) ->
    _ = erlang:exit(LinkPid, 'normal'),
    _ = erlang:exit(CachesPid, 'normal'),
    _ = application:stop('kazoo_config'),
    ?LOG_DEBUG(":: Stopped Kazoo FixtureDB").

test_fetch(#{master_services := MasterServices
            ,reseller_services := ResellerServices
            ,parent_services := ParentServices
            ,child_services := ChildServices
            }) ->
    [{"Verify master account id"
     ,?_assertEqual(?FIXTURE_MASTER_ACCOUNT_ID, kz_services:account_id(MasterServices))
     }
    ,{"Verify master services doc"
     ,?_assert(kz_json:is_json_object(kz_services:services_jobj(MasterServices)))
     }
    ,{"Verify reseller account id"
     ,?_assertEqual(?FIXTURE_RESELLER_ACCOUNT_ID, kz_services:account_id(ResellerServices))
     }
    ,{"Verify reseller services doc"
     ,?_assert(kz_json:is_json_object(kz_services:services_jobj(ResellerServices)))
     }
    ,{"Verify parent account id"
     ,?_assertEqual(?FIXTURE_PARENT_ACCOUNT_ID, kz_services:account_id(ParentServices))
     }
    ,{"Verify parent services doc"
     ,?_assert(kz_json:is_json_object(kz_services:services_jobj(ParentServices)))
     }
    ,{"Verify child service doc create has the correct account id"
     ,?_assertEqual(?FIXTURE_CHILD_ACCOUNT_ID, kz_services:account_id(ChildServices))
     }
    ,{"Verify child services doc creation"
     ,?_assert(kz_json:is_json_object(kz_services:services_jobj(ChildServices)))
     }
    ].
