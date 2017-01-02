%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_reconcile_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

keeps_carrier_test_() ->
    Num = ?BW_EXISTING_DID,
    Options = [{assign_to, ?RESELLER_ACCOUNT_ID} | knm_number_options:default()],
    {ok, N0} = knm_number:get(Num),
    {ok, N1} = knm_number:reconcile(Num, Options),
    [?_assertNotEqual(knm_carriers:default_carrier()
                     ,knm_phone_number:module_name(knm_number:phone_number(N0))
                     )
    ,{"Verify number carrier stayed the same"
     ,?_assertEqual(knm_phone_number:module_name(knm_number:phone_number(N0))
                   ,knm_phone_number:module_name(knm_number:phone_number(N1))
                   )
     }
    ].

sets_carrier_for_mobile_test_() ->
    Num = ?BW_EXISTING_DID,
    Options = [{assign_to, ?RESELLER_ACCOUNT_ID} | knm_number_options:default()],
    {ok, N0} = knm_number:get(Num),
    {ok, N1} = knm_number:reconcile(Num, [{module_name, ?CARRIER_MDN}|Options]),
    [?_assertNotEqual(knm_carriers:default_carrier()
                     ,knm_phone_number:module_name(knm_number:phone_number(N0))
                     )
    ,?_assertNotEqual(?CARRIER_MDN
                     ,knm_phone_number:module_name(knm_number:phone_number(N0))
                     )
    ,{"Verify number carrier was indeed updated for mobile"
     ,?_assertEqual(?CARRIER_MDN
                   ,knm_phone_number:module_name(knm_number:phone_number(N1))
                   )
     }
    ].
