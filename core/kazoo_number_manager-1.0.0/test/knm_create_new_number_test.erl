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
    Props = [{<<"auth_by">>, ?MASTER_ACCOUNT_ID}
             ,{<<"assign_to">>, ?RESELLER_ACCOUNT_ID}
             ,{<<"dry_run">>, 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],
    N = knm_number:create_or_load(?TEST_CREATE_NUM, Props, {'error', 'not_found'}),
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

create_existing_number_test() ->
    PhoneNumber = knm_phone_number:from_json(?EXISTING_NUMBER),
    AvailPhoneNumber = knm_phone_number:set_state(PhoneNumber, ?NUMBER_STATE_AVAILABLE),

    Props = [{<<"auth_by">>, ?MASTER_ACCOUNT_ID}
             ,{<<"assign_to">>, ?RESELLER_ACCOUNT_ID}
             ,{<<"dry_run">>, 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],

    N = knm_number:create_or_load(?TEST_EXISTING_NUM, Props, {'ok', AvailPhoneNumber}),
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
     ,{"Verifying that IN SERVICE numbers can't be created"
       ,?_assertException('throw'
                          ,{'error', 'number_exists', ?TEST_EXISTING_NUM}
                          ,knm_number:create_or_load(?TEST_EXISTING_NUM, Props, {'ok', PhoneNumber})
                         )
      }
    ].
