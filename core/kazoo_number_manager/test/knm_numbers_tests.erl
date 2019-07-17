%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_numbers_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-define(NOT_NUM, <<"NOT a number">>).

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [get_numbers()
    ,create_number()
    ,create_new()
    ,move()
    ,update()
    ,attempt_setting_e911_on_disallowed_number()
    ,delete()
    ,reconcile()
    ,reserve()
    ,assign_to_app()
    ,release()
    ].

n_x(X, Ret) ->
    lists:nth(X, maps:get(ok, Ret)).

pn_x(X, Ret) ->
    knm_number:phone_number(n_x(X, Ret)).


get_numbers() ->
    Ret = knm_numbers:get([?TEST_AVAILABLE_NUM]),
    [?_assertEqual(#{}, maps:get(ko, Ret))
    ,?_assertMatch([_], maps:get(ok, Ret))
    ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(pn_x(1, Ret)))
    ,?_assertMatch(#{ko := #{?NOT_NUM := 'not_reconcilable'}}
                  ,knm_numbers:get([?NOT_NUM], [])
                  )
    ,?_assertMatch(#{ko := #{?TEST_CREATE_NUM := not_found}}
                  ,knm_numbers:get([?TEST_CREATE_NUM], [])
                  )
    ,?_assertMatch(#{ko := #{?NOT_NUM := 'not_reconcilable'}
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, ?NOT_NUM])
                  )
    ,?_assertMatch(#{ko := #{?NOT_NUM := 'not_reconcilable'}
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, ?NOT_NUM
                                   ,?TEST_AVAILABLE_NUM, ?NOT_NUM])
                  )
    ,?_assertMatch(#{ko := #{?NOT_NUM := 'not_reconcilable'
                            ,?TEST_CREATE_NUM := not_found
                            }
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, ?NOT_NUM
                                   ,?TEST_AVAILABLE_NUM, ?NOT_NUM
                                   ,?TEST_CREATE_NUM])
                  )
    ].


create_test_e911() ->
    kz_json:from_list(
      [{?E911_STREET1, <<"301 marina blvd.">>}
      ,{?E911_CITY, <<"San Francisco">>}
      ,{?E911_STATE, <<"CA">>}
      ,{?E911_ZIP, <<"94123">>}
      ]).

create_number() ->
    Num = ?TEST_TELNYX_NUM,
    E911 = create_test_e911(),
    JObj = kz_json:from_list([{?FEATURE_E911, E911}]),
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ],
    Ret = knm_numbers:create([Num], [{'public_fields', JObj}|Options]),
    [?_assertEqual(#{}, maps:get(ko, Ret))
    ,?_assertMatch([_], maps:get(ok, Ret))
    ,?_assertEqual(true, knm_number:is_number(n_x(1, Ret)))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(pn_x(1, Ret)))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret)))
    ,{"Verify feature is properly set"
     ,?_assert(kz_json:are_equal(E911, knm_phone_number:feature(pn_x(1, Ret), ?FEATURE_E911)))
     }
    ,{"Verify we are keeping track of intermediary address_id"
     ,?_assertEqual(<<"421564943280637078">>
                   ,kz_json:get_value(<<"address_id">>, knm_phone_number:carrier_data(pn_x(1, Ret)))
                   )
     }
    ].

create_new() ->
    Num = ?TEST_TELNYX_NUM,
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ],
    Ret = knm_numbers:create([Num, ?NOT_NUM, ?TEST_CREATE_NUM], Options),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret))
    ,?_assertMatch([_, _], maps:get(ok, Ret))
    ,?_assertEqual(true, knm_number:is_number(n_x(1, Ret)))
    ,?_assertEqual(true, knm_number:is_number(n_x(2, Ret)))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(pn_x(1, Ret)))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(pn_x(2, Ret)))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret)))
    ,?_assert(knm_phone_number:is_dirty(pn_x(2, Ret)))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret)))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(2, Ret)))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(1, Ret)))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(2, Ret)))
    ].


move() ->
    Ret = knm_numbers:move([?NOT_NUM, ?TEST_AVAILABLE_NUM], ?CHILD_ACCOUNT_ID),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret))
    ,?_assertMatch([_], maps:get(ok, Ret))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret)))
    ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(pn_x(1, Ret)))
    ,{"verify assigned_to is child account"
     ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret)))
     }
    ,{"verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(1, Ret)))
     }
    ].


update() ->
    NotDefault = true,
    Setters = [{fun knm_phone_number:set_ported_in/2, NotDefault}],
    Ret0 = knm_numbers:update([?NOT_NUM, ?TEST_AVAILABLE_NUM], []),
    Ret = knm_numbers:update([?NOT_NUM, ?TEST_AVAILABLE_NUM], Setters),
    [?_assertEqual(false, knm_phone_number:is_dirty(pn_x(1, Ret0)))
    ,{"verify ported_in is set to default"
     ,?_assertNotEqual(NotDefault, knm_phone_number:ported_in(pn_x(1, Ret0)))
     }
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret))
    ,?_assertMatch([_], maps:get(ok, Ret))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret)))
    ,{"verify number was indeed updated"
     ,?_assertEqual(NotDefault, knm_phone_number:ported_in(pn_x(1, Ret)))
     }
    ].

attempt_setting_e911_on_disallowed_number() ->
    JObj = kz_json:from_list(
             [{?FEATURE_E911
              ,kz_json:from_list(
                 [{?E911_STREET1, <<"140 Geary St.">>}
                 ,{?E911_STREET2, <<"3rd floor">>}
                 ,{?E911_CITY, <<"San Francisco">>}
                 ,{?E911_STATE, <<"CA">>}
                 ,{?E911_ZIP, <<"94108">>}
                 ])
              }
             ]),
    Options = [{auth_by, ?RESELLER_ACCOUNT_ID}
              ,{public_fields, JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?BW_EXISTING_DID,
    {ok, N} = knm_number:get(Num),
    PN = knm_number:phone_number(N),
    Msg = kz_json:from_list(
            [{<<"code">>, 403}
            ,{<<"error">>, <<"forbidden">>}
            ,{<<"message">>, <<"requestor is unauthorized to perform operation">>}
            ]),
    [{"Verify feature is not set"
     ,?_assertEqual(undefined, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature is still not set"
     ,?_assertMatch(#{ko := #{Num := Msg}}, knm_numbers:update([N], Updates, Options))
     }
    ].


delete() ->
    Ret = knm_numbers:delete([?NOT_NUM, ?TEST_AVAILABLE_NUM], knm_number_options:default()),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret))
    ,?_assertMatch([_], maps:get(ok, Ret))
    ,{"verify number was indeed deleted"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(pn_x(1, Ret)))
     }
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret)))
    ].


reconcile() ->
    Ret0 = knm_numbers:reconcile([?NOT_NUM], []),
    Ret1 = knm_numbers:reconcile([?NOT_NUM, ?TEST_AVAILABLE_NUM], knm_number_options:default()),
    Ret2 = knm_numbers:reconcile([?NOT_NUM, ?TEST_AVAILABLE_NUM]
                                ,[{assign_to, ?RESELLER_ACCOUNT_ID} | knm_number_options:default()]),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret0))
    ,?_assertEqual([], maps:get(ok, Ret0))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable', ?TEST_AVAILABLE_NUM => error_assign_to_undefined()}
                  ,maps:get(ko, Ret1))
    ,?_assertEqual([], maps:get(ok, Ret1))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret2))
    ,?_assertMatch([_], maps:get(ok, Ret2))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret2)))
    ,{"verify number is now in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(1, Ret2)))
     }
    ,{"verify number is indeed owned by account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret2)))
     }
    ].


error_assign_to_undefined() ->
    kz_json:from_list(
      [{<<"code">>, 400}
      ,{<<"error">>, <<"assign_failure">>}
      ,{<<"cause">>, <<"field_undefined">>}
      ,{<<"message">>, <<"invalid account to assign to">>}
      ]).


reserve() ->
    AssignToChild = [{assign_to, ?CHILD_ACCOUNT_ID} | knm_number_options:default()],
    Ret1 = knm_numbers:reserve([?NOT_NUM, ?TEST_AVAILABLE_NUM]
                              ,[{assign_to,?RESELLER_ACCOUNT_ID}, {auth_by,?MASTER_ACCOUNT_ID}]),
    Ret2b = knm_numbers:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM], knm_number_options:default()),
    Ret2 = knm_numbers:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM]
                              ,[{assign_to,?RESELLER_ACCOUNT_ID} | knm_number_options:default()]),
    Ret3 = knm_numbers:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM], AssignToChild),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret1))
    ,?_assertMatch([_], maps:get(ok, Ret1))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret2))
    ,?_assertMatch([_], maps:get(ok, Ret2))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'
                    ,?TEST_IN_SERVICE_NUM => error_assign_to_undefined()
                    }, maps:get(ko, Ret2b))
    ,?_assertEqual([], maps:get(ok, Ret2b))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret3))
    ,?_assertMatch([_], maps:get(ok, Ret3))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret1)))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(pn_x(1, Ret1)))
     }
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret1)))
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(pn_x(1, Ret1)))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret2)))
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(pn_x(1, Ret2)))
    ,{"verify number is now reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(pn_x(1, Ret2)))
     }
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret2)))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret3)))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(pn_x(1, Ret3)))
     }
    ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret3)))
    ,?_assertEqual([?CHILD_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(pn_x(1, Ret3)))
    ].


assign_to_app() ->
    MyApp = <<"my_app">>,
    Ret1 = knm_numbers:get([?NOT_NUM, ?TEST_AVAILABLE_NUM]),
    Ret2 = knm_numbers:assign_to_app([?NOT_NUM, ?TEST_AVAILABLE_NUM], MyApp),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret1))
    ,?_assertMatch([_], maps:get(ok, Ret1))
    ,{"Verify number is not already assigned to MyApp"
     ,?_assertNotEqual(MyApp, knm_phone_number:used_by(pn_x(1, Ret1)))
     }
    ,?_assertEqual(false, knm_phone_number:is_dirty(pn_x(1, Ret1)))
    ,?_assertEqual(true,  knm_phone_number:is_dirty(pn_x(1, Ret2)))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret2))
    ,?_assertMatch([_], maps:get(ok, Ret2))
    ,{"Verify number is now used by MyApp"
     ,?_assertEqual(MyApp, knm_phone_number:used_by(pn_x(1, Ret2)))
     }
    ].


release() ->
    Ret1 = knm_numbers:release([?NOT_NUM, ?TEST_IN_SERVICE_WITH_HISTORY_NUM]),
    Ret2 = knm_numbers:release([?NOT_NUM, ?TEST_IN_SERVICE_MDN], knm_number_options:mdn_options()),
    Ret3 = knm_numbers:release([?NOT_NUM, ?TEST_IN_SERVICE_BAD_CARRIER_NUM]),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret1))
    ,?_assertMatch([_], maps:get(ok, Ret1))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret1)))
    ,{"Verify number went from in_service to reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(pn_x(1, Ret1)))
     }
    ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:assigned_to(pn_x(1, Ret1)))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret2))
    ,?_assertMatch([_], maps:get(ok, Ret2))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret2)))
    ,{"Verify number went from in_service to deleted"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(pn_x(1, Ret2)))
     }
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get(ko, Ret3))
    ,?_assertMatch([_], maps:get(ok, Ret3))
    ,?_assert(knm_phone_number:is_dirty(pn_x(1, Ret3)))
    ,{"Verify number went from in_service to available"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(pn_x(1, Ret3)))
     }
    ].
