%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-define(MASTER_ACCOUNT_ID, <<"master_account_6992af0e9504d0b27">>).
-define(RESELLER_ACCOUNT_ID, <<"reseller_account_b113394f16cb76d">>).

-define(PVT_TREE, [?MASTER_ACCOUNT_ID
                   ,?RESELLER_ACCOUNT_ID
                  ]).

-define(RESELLER_ACCOUNT_DOC
        ,wh_json:from_list(
           [{<<"_id">>, ?RESELLER_ACCOUNT_ID}]
          )
       ).

-define(EXISTING_NUMBER
        ,wh_json:from_list(
           [{<<"_id">>, ?TEST_EXISTING_NUM}
            ,{<<"_rev">>, <<"10-7dd6a1523e81a4e3c2689140ed3a8e69">>}
            ,{<<"pvt_modified">>, 63565934349}
            ,{<<"pvt_features">>, []}
            ,{<<"pvt_assigned_to">>, ?RESELLER_ACCOUNT_ID}
            ,{<<"pvt_reserve_history">>, [?RESELLER_ACCOUNT_ID]}
            ,{<<"pvt_module_name">>, ?CARRIER_LOCAL}
            ,{<<"pvt_number_state">>, ?NUMBER_STATE_IN_SERVICE}
            ,{<<"pvt_db_name">>, <<"numbers%2F%2B1555">>}
            ,{<<"pvt_created">>, 63565934344}
            ,{<<"pvt_authorizing_account">>, ?MASTER_ACCOUNT_ID}
            ,{<<"used_by">>, <<"callflow">>}
           ]
          )
       ).

create_test_() ->
    create_available_checks()
        ++ load_existing_checks().

load_existing_checks() ->
    PN = knm_phone_number:from_json(?EXISTING_NUMBER),
    [existing_in_available_state(PN)
     | existing_in_other_states(PN)
    ].

existing_in_available_state(PN) ->
    {"Ensure number in AVAILABLE state can be 'created'"
     ,?_assert(knm_number:ensure_can_load_to_create(
                 knm_phone_number:set_state(PN, ?NUMBER_STATE_AVAILABLE)
                )
              )
    }.

existing_in_other_states(PN) ->
    [existing_in_other_state(
       knm_phone_number:set_state(PN, State)
      )
     || State <- [?NUMBER_STATE_PORT_IN
                  ,?NUMBER_STATE_PORT_OUT
                  ,?NUMBER_STATE_DISCOVERY
                  ,?NUMBER_STATE_IN_SERVICE
                  ,?NUMBER_STATE_RELEASED
                  ,?NUMBER_STATE_RESERVED
                  ,?NUMBER_STATE_DISCONNECTED
                  ,?NUMBER_STATE_DELETED
                 ]
    ].

existing_in_other_state(PN) ->
    State = wh_util:to_list(knm_phone_number:state(PN)),

    {lists:flatten(["Ensure number in ", State, " cannot be 'created'"])
     ,?_assertException('throw'
                        ,{'error', 'number_exists', ?TEST_EXISTING_NUM}
                        ,knm_number:ensure_can_load_to_create(PN)
                       )
    }.

create_available_checks() ->
    [create_with_no_auth_by()
     ,create_with_disallowed_account()
     ,create_with_number_porting()
     ,create_new_number()
    ].

create_with_no_auth_by() ->
    {"Ensure unauthorized error thrown when no auth_by supplied"
     ,?_assertException('throw'
                        ,{'error', 'unauthorized'}
                        ,knm_number:ensure_can_create(?TEST_CREATE_NUM, [])
                       )
    }.

create_with_disallowed_account() ->
    {"Ensure unauthorized error when auth_by account isn't allowed to create numbers"
     ,?_assertException('throw'
                        ,{'error', 'unauthorized'}
                        ,knm_number:ensure_can_create(
                           ?TEST_CREATE_NUM
                           ,[{<<"auth_by">>, ?RESELLER_ACCOUNT_ID}
                             ,{<<"auth_by_account">>
                               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'false')
                              }
                            ]
                          )
                       )
    }.

create_with_number_porting() ->
    {"Ensure number_is_porting error when auth_by account isn't allowed to create numbers"
     ,?_assertException('throw'
                        ,{'error', 'number_is_porting', ?TEST_EXISTING_NUM}
                        ,knm_number:ensure_can_create(
                           ?TEST_EXISTING_NUM %% pretend it is porting
                           ,[{<<"auth_by">>, ?RESELLER_ACCOUNT_ID}
                             ,{<<"auth_by_account">>
                               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
                              }
                            ]
                          )
                       )
    }.

create_new_number() ->
    {"Ensure number_is_porting error when auth_by account isn't allowed to create numbers"
     ,?_assert(knm_number:ensure_can_create(
                 ?TEST_CREATE_NUM
                 ,[{<<"auth_by">>, ?RESELLER_ACCOUNT_ID}
                   ,{<<"auth_by_account">>
                         ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
                    }
                  ]
                )
              )
    }.
