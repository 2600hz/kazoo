%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_reconcile_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [keeps_carrier()
    ,sets_carrier_for_mobile()
    ].

keeps_carrier() ->
    Num = ?BW_EXISTING_DID,
    {ok, N0} = knm_number:get(Num),
    PN0 = knm_number:phone_number(N0),
    Options = [{assign_to, ?RESELLER_ACCOUNT_ID} | knm_number_options:default()],
    {ok, N1} = knm_number:reconcile(Num, Options),
    PN1 = knm_number:phone_number(N1),
    [?_assert(knm_phone_number:is_dirty(PN1))
    ,?_assertNotEqual(knm_carriers:default_carrier(), knm_phone_number:module_name(PN0))
    ,{"Verify number carrier stayed the same"
     ,?_assertEqual(knm_phone_number:module_name(PN0), knm_phone_number:module_name(PN1))
     }
    ].

sets_carrier_for_mobile() ->
    Num = ?BW_EXISTING_DID,
    {ok, N0} = knm_number:get(Num),
    PN0 = knm_number:phone_number(N0),
    Options = [{assign_to, ?RESELLER_ACCOUNT_ID}
              ,{module_name, ?CARRIER_MDN}
               | knm_number_options:default()
              ],
    {ok, N1} = knm_number:reconcile(Num, Options),
    PN1 = knm_number:phone_number(N1),
    [?_assert(knm_phone_number:is_dirty(PN1))
    ,?_assertNotEqual(knm_carriers:default_carrier(), knm_phone_number:module_name(PN0))
    ,?_assertNotEqual(?CARRIER_MDN, knm_phone_number:module_name(PN0))
    ,{"Verify number carrier was indeed updated for mobile"
     ,?_assertEqual(?CARRIER_MDN, knm_phone_number:module_name(PN1))
     }
    ].
