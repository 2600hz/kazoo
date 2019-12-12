%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_ops_tests).

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
    ,reserve()
    ,assign_to_app()
    ,release()
    ].

n_x(X, Ret) ->
    lists:nth(X, maps:get('succeeded', Ret)).

get_numbers() ->
    Ret = knm_ops:get([?TEST_AVAILABLE_NUM]),
    [?_assertEqual(#{}, maps:get('failed', Ret))
    ,?_assertMatch([_], maps:get('succeeded', Ret))
    ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(n_x(1, Ret)))
    ,?_assertMatch(#{'failed' := #{?NOT_NUM := 'not_reconcilable'}}
                  ,knm_ops:get([?NOT_NUM], [])
                  )
    ,?_assertMatch(#{'failed' := #{?TEST_CREATE_NUM := not_found}}
                  ,knm_ops:get([?TEST_CREATE_NUM], [])
                  )
    ,?_assertMatch(#{'failed' := #{?NOT_NUM := 'not_reconcilable'}
                    ,'succeeded' := [_]
                    }
                  ,knm_ops:get([?TEST_AVAILABLE_NUM, ?NOT_NUM])
                  )
    ,?_assertMatch(#{'failed' := #{?NOT_NUM := 'not_reconcilable'}
                    ,'succeeded' := [_]
                    }
                  ,knm_ops:get([?TEST_AVAILABLE_NUM, ?NOT_NUM
                                   ,?TEST_AVAILABLE_NUM, ?NOT_NUM])
                  )
    ,?_assertMatch(#{'failed' := #{?NOT_NUM := 'not_reconcilable'
                                  ,?TEST_CREATE_NUM := not_found
                                  }
                    ,'succeeded' := [_]
                    }
                  ,knm_ops:get([?TEST_AVAILABLE_NUM, ?NOT_NUM
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
    Ret = knm_ops:create([Num], [{'public_fields', JObj}|Options]),
    [?_assertEqual(#{}, maps:get('failed', Ret))
    ,?_assertMatch([_], maps:get('succeeded', Ret))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(n_x(1, Ret)))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret)))
    ,{"Verify feature is properly set"
     ,?_assert(kz_json:are_equal(E911, knm_phone_number:feature(n_x(1, Ret), ?FEATURE_E911)))
     }
    ,{"Verify we are keeping track of intermediary address_id"
     ,?_assertEqual(<<"421564943280637078">>
                   ,kz_json:get_value(<<"address_id">>, knm_phone_number:carrier_data(n_x(1, Ret)))
                   )
     }
    ].

create_new() ->
    Num = ?TEST_TELNYX_NUM,
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ],
    Ret = knm_ops:create([Num, ?NOT_NUM, ?TEST_CREATE_NUM], Options),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret))
    ,?_assertMatch([_, _], maps:get('succeeded', Ret))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(n_x(1, Ret)))
    ,?_assertEqual(true, knm_phone_number:is_phone_number(n_x(2, Ret)))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret)))
    ,?_assert(knm_phone_number:is_dirty(n_x(2, Ret)))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret)))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(2, Ret)))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(n_x(1, Ret)))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(n_x(2, Ret)))
    ].


move() ->
    Ret = knm_ops:move([?NOT_NUM, ?TEST_AVAILABLE_NUM], ?CHILD_ACCOUNT_ID),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret))
    ,?_assertMatch([_], maps:get('succeeded', Ret))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret)))
    ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(n_x(1, Ret)))
    ,{"verify assigned_to is child account"
     ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret)))
     }
    ,{"verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(n_x(1, Ret)))
     }
    ].


update() ->
    NotDefault = true,
    Setters = [{fun knm_phone_number:set_ported_in/2, NotDefault}],
    Ret0 = knm_ops:update([?NOT_NUM, ?TEST_AVAILABLE_NUM], []),
    Ret = knm_ops:update([?NOT_NUM, ?TEST_AVAILABLE_NUM], Setters),
    [?_assertEqual(false, knm_phone_number:is_dirty(n_x(1, Ret0)))
    ,{"verify ported_in is set to default"
     ,?_assertNotEqual(NotDefault, knm_phone_number:ported_in(n_x(1, Ret0)))
     }
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret))
    ,?_assertMatch([_], maps:get('succeeded', Ret))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret)))
    ,{"verify number was indeed updated"
     ,?_assertEqual(NotDefault, knm_phone_number:ported_in(n_x(1, Ret)))
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
    Options = [{'auth_by', ?RESELLER_ACCOUNT_ID}
              ,{'public_fields', JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?BW_EXISTING_DID,
    [PN] = knm_pipe:succeeded(knm_ops:get([Num])),
    Msg = kz_json:from_list(
            [{<<"code">>, 403}
            ,{<<"error">>, <<"forbidden">>}
            ,{<<"message">>, <<"requestor is unauthorized to perform operation">>}
            ]),
    [{"Verify feature is not set"
     ,?_assertEqual('undefined', knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature is still not set"
     ,?_assertMatch(#{'failed' := #{Num := Msg}}, knm_ops:update([PN], Updates, Options))
     }
    ].


delete() ->
    Ret = knm_ops:delete([?NOT_NUM, ?TEST_AVAILABLE_NUM], knm_options:default()),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret))
    ,?_assertMatch([_], maps:get('succeeded', Ret))
    ,{"verify number was indeed deleted"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(n_x(1, Ret)))
     }
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret)))
    ].


error_assign_to_undefined() ->
    kz_json:from_list(
      [{<<"code">>, 400}
      ,{<<"error">>, <<"assign_failure">>}
      ,{<<"cause">>, <<"field_undefined">>}
      ,{<<"message">>, <<"invalid account to assign to">>}
      ]).


reserve() ->
    AssignToChild = [{assign_to, ?CHILD_ACCOUNT_ID} | knm_options:default()],
    Ret1 = knm_ops:reserve([?NOT_NUM, ?TEST_AVAILABLE_NUM]
                              ,[{assign_to,?RESELLER_ACCOUNT_ID}, {auth_by,?MASTER_ACCOUNT_ID}]),
    Ret2b = knm_ops:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM], knm_options:default()),
    Ret2 = knm_ops:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM]
                              ,[{assign_to,?RESELLER_ACCOUNT_ID} | knm_options:default()]),
    Ret3 = knm_ops:reserve([?NOT_NUM, ?TEST_IN_SERVICE_NUM], AssignToChild),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret1))
    ,?_assertMatch([_], maps:get('succeeded', Ret1))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret2))
    ,?_assertMatch([_], maps:get('succeeded', Ret2))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'
                    ,?TEST_IN_SERVICE_NUM => error_assign_to_undefined()
                    }, maps:get('failed', Ret2b))
    ,?_assertEqual([], maps:get('succeeded', Ret2b))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret3))
    ,?_assertMatch([_], maps:get('succeeded', Ret3))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret1)))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(n_x(1, Ret1)))
     }
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret1)))
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(n_x(1, Ret1)))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret2)))
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(n_x(1, Ret2)))
    ,{"verify number is now reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(n_x(1, Ret2)))
     }
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret2)))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret3)))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(n_x(1, Ret3)))
     }
    ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret3)))
    ,?_assertEqual([?CHILD_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(n_x(1, Ret3)))
    ].


assign_to_app() ->
    MyApp = <<"my_app">>,
    Ret1 = knm_ops:get([?NOT_NUM, ?TEST_AVAILABLE_NUM]),
    Ret2 = knm_ops:assign_to_app([?NOT_NUM, ?TEST_AVAILABLE_NUM], MyApp),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret1))
    ,?_assertMatch([_], maps:get('succeeded', Ret1))
    ,{"Verify number is not already assigned to MyApp"
     ,?_assertNotEqual(MyApp, knm_phone_number:used_by(n_x(1, Ret1)))
     }
    ,?_assertEqual(false, knm_phone_number:is_dirty(n_x(1, Ret1)))
    ,?_assertEqual(true,  knm_phone_number:is_dirty(n_x(1, Ret2)))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret2))
    ,?_assertMatch([_], maps:get('succeeded', Ret2))
    ,{"Verify number is now used by MyApp"
     ,?_assertEqual(MyApp, knm_phone_number:used_by(n_x(1, Ret2)))
     }
    ].


release() ->
    Ret1 = knm_ops:release([?NOT_NUM, ?TEST_IN_SERVICE_WITH_HISTORY_NUM]),
    Ret2 = knm_ops:release([?NOT_NUM, ?TEST_IN_SERVICE_MDN], knm_options:mdn_options()),
    Ret3 = knm_ops:release([?NOT_NUM, ?TEST_IN_SERVICE_BAD_CARRIER_NUM]),
    [?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret1))
    ,?_assertMatch([_], maps:get('succeeded', Ret1))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret1)))
    ,{"Verify number went from in_service to reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(n_x(1, Ret1)))
     }
    ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:assigned_to(n_x(1, Ret1)))
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret2))
    ,?_assertMatch([_], maps:get('succeeded', Ret2))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret2)))
    ,{"Verify number went from in_service to deleted"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(n_x(1, Ret2)))
     }
    ,?_assertEqual(#{?NOT_NUM => 'not_reconcilable'}, maps:get('failed', Ret3))
    ,?_assertMatch([_], maps:get('succeeded', Ret3))
    ,?_assert(knm_phone_number:is_dirty(n_x(1, Ret3)))
    ,{"Verify number went from in_service to available"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(n_x(1, Ret3)))
     }
    ].
