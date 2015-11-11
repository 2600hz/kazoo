%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_create_new_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

create_new_number_test_() ->
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
             ,{'assign_to', ?RESELLER_ACCOUNT_ID}
             ,{'dry_run', 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],
    {'ok', N} = knm_number:create(?TEST_CREATE_NUM, Props),
    PN = knm_number:phone_number(N),

    [{"Verify phone number is assigned to reseller account"
      ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
     ,{"Verify new phone number was authorized by master account"
       ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
      }
     ,{"Verify new phone number database is properly set"
       ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
      }
     ,{"Verify new phone number is in RESERVED state"
       ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(PN))
      }
     ,{"Verify the reseller account is listed in reserve history"
       ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
      }
     ,{"Verify the local carrier module is being used"
       ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
      }
    ].

create_existing_number_test_() ->
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
             ,{'assign_to', ?RESELLER_ACCOUNT_ID}
             ,{'dry_run', 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],

    {'ok', N} = knm_number:create(?TEST_AVAILABLE_NUM, Props),
    PN = knm_number:phone_number(N),

    [{"Verify phone number is assigned to reseller account"
      ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
     ,{"Verify new phone number was authorized by master account"
       ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
      }
     ,{"Verify new phone number database is properly set"
       ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
      }
     ,{"Verify new phone number is in RESERVED state"
       ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(PN))
      }
     ,{"Verify the reseller account is listed in reserve history"
       ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
      }
     ,{"Verify the local carrier module is being used"
       ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
      }
    ].

create_existing_in_service_test_() ->
    InServicePN =
        knm_phone_number:set_state(
          knm_phone_number:from_json(?AVAILABLE_NUMBER)
          ,?NUMBER_STATE_IN_SERVICE
         ),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
             ,{'assign_to', ?RESELLER_ACCOUNT_ID}
             ,{'dry_run', 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],

    Resp = knm_number:attempt(fun knm_number:create_or_load/3
                              ,[?TEST_AVAILABLE_NUM, Props, {'ok', InServicePN}]
                             ),

    [{"Verifying that IN SERVICE numbers can't be created"
      ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 409, <<"number_exists">>, ?TEST_AVAILABLE_NUM)
    ].

create_dry_run_test_() ->
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
             ,{'assign_to', ?RESELLER_ACCOUNT_ID}
             ,{'dry_run', 'true'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],
    {'dry_run', Services, Charges} =
        knm_number:create(?TEST_CREATE_NUM, Props),

    %% Eventually make a stub service plan to test this
    [{"Verify charges for dry_run"
      ,?_assertEqual(0, Charges)
     }
     ,{"Verify services for dry_run"
       ,?_assertEqual('undefined', Services)
       }
    ].

create_checks_test_() ->
    create_available_checks()
        ++ load_existing_checks().

load_existing_checks() ->
    PN = knm_phone_number:from_json(?AVAILABLE_NUMBER),
    [existing_in_state(
       knm_phone_number:set_state(PN, State)
       ,IsAllowed
      )
     || {State, IsAllowed} <- [{?NUMBER_STATE_AVAILABLE, 'true'}
                               ,{?NUMBER_STATE_DELETED, 'false'}
                               ,{?NUMBER_STATE_DISCONNECTED, 'false'}
                               ,{?NUMBER_STATE_DISCOVERY, 'false'}
                               ,{?NUMBER_STATE_IN_SERVICE, 'false'}
                               ,{?NUMBER_STATE_PORT_IN, 'false'}
                               ,{?NUMBER_STATE_PORT_OUT, 'false'}
                               ,{?NUMBER_STATE_RELEASED, 'false'}
                               ,{?NUMBER_STATE_RESERVED, 'false'}
                              ]
    ].

existing_in_state(PN, 'false') ->
    State = wh_util:to_list(knm_phone_number:state(PN)),

    Resp = knm_number:attempt(fun knm_number:ensure_can_load_to_create/1
                              ,[PN]
                             ),

    [{lists:flatten(["Ensure number in ", State, " cannot be 'created'"])
      ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 409, <<"number_exists">>, ?TEST_AVAILABLE_NUM)
    ];
existing_in_state(PN, 'true') ->
    State = wh_util:to_list(knm_phone_number:state(PN)),

    [{lists:flatten(["Ensure number in ", State, " can be 'created'"])
     ,?_assert(knm_number:ensure_can_load_to_create(PN))
    }].

create_available_checks() ->
    [create_with_no_auth_by()
     ,create_with_disallowed_account()
     ,create_with_number_porting()
     ,create_new_number()
    ].

create_with_no_auth_by() ->
    Resp = knm_number:attempt(fun knm_number:ensure_can_create/2
                              ,[?TEST_CREATE_NUM, []]
                             ),
    [{"Ensure unauthorized error thrown when no auth_by supplied"
      ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 403, <<"forbidden">>)
    ].

create_with_disallowed_account() ->
    Resp = knm_number:attempt(fun knm_number:ensure_can_create/2
                              ,[?TEST_CREATE_NUM
                                ,[{'auth_by', ?RESELLER_ACCOUNT_ID}
                                  ,{<<"auth_by_account">>
                                    ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'false')
                                   }
                                 ]
                               ]
                             ),

    [{"Ensure unauthorized error when auth_by account isn't allowed to create numbers"
      ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 403, <<"forbidden">>)
    ].

create_with_number_porting() ->
    Resp = knm_number:attempt(fun knm_number:ensure_can_create/2
                              ,[?TEST_AVAILABLE_NUM %% pretend it is porting
                                ,[{'auth_by', ?RESELLER_ACCOUNT_ID}
                                  ,{<<"auth_by_account">>
                                    ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
                                   }
                                 ]
                               ]),

    [{"Ensure number_is_porting error when auth_by account isn't allowed to create numbers"
      ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 400, <<"number_is_porting">>, ?TEST_AVAILABLE_NUM)
    ].

check_error_response(E, Code, Error) ->
    check_error_response(E, Code, Error, 'undefined').
check_error_response(E, Code, Error, Cause) ->
    check_error_response(E, Code, Error, Cause, 'undefined').
check_error_response({'error', JObj}, Code, Error, Cause, Message) ->
    validate_errors(JObj
                    ,[{Code, fun knm_errors:code/1, "Verify 'code' is set properly"}
                      ,{Error, fun knm_errors:error/1, "Verify 'error' is set properly"}
                      ,{Cause, fun knm_errors:cause/1, "Verify 'cause' is set properly"}
                      ,{Message, fun knm_errors:message/1, "Verify 'message' is set properly"}
                     ]).

validate_errors(JObj, Vs) ->
    validate_errors(JObj, Vs, []).

validate_errors(JObj, [{'undefined', _, _}|Vs], Tests) ->
    validate_errors(JObj, Vs, Tests);
validate_errors(_JObj, [], Tests) ->
    Tests;
validate_errors(JObj, [{V, F, L}|Vs], Tests) ->
    validate_errors(JObj
                    ,Vs
                    ,[{L, ?_assertEqual(V, F(JObj))} | Tests]
                   ).

create_new_number() ->
    {"Ensure success when auth_by account is allowed to create numbers"
     ,?_assert(knm_number:ensure_can_create(
                 ?TEST_CREATE_NUM
                 ,[{'auth_by', ?RESELLER_ACCOUNT_ID}
                   ,{<<"auth_by_account">>
                     ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
                    }
                  ]
                )
              )
    }.
