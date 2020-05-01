%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_tests).

-include_lib("eunit/include/eunit.hrl").

get_views_json_test_() ->
    ViewListing = kapps_util:get_views_json(kazoo_apps, "views"),
    AccountView = kapps_util:get_view_json(kazoo_apps, <<"views/accounts.json">>),
    [?_assertEqual(9, length(ViewListing))
    ,?_assert(lists:all(fun verify_listing/1, ViewListing))
    ,?_assertMatch({<<"_design/accounts">>,_}, AccountView)
    ,?_assert(verify_listing(AccountView))
    ].

verify_listing({Name, View}) ->
    kz_term:is_ne_binary(Name)
        andalso kz_json:is_json_object(View)
        andalso not kz_json:is_empty(View).
