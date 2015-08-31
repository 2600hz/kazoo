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

create_checks_test_() ->
    create_available_checks()
        ++ load_existing_checks().

load_existing_checks() ->
    PN = knm_phone_number:from_json(?EXISTING_NUMBER),
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

    {lists:flatten(["Ensure number in ", State, " cannot be 'created'"])
     ,?_assertException('throw'
                        ,{'error', 'number_exists', ?TEST_EXISTING_NUM}
                        ,knm_number:ensure_can_load_to_create(PN)
                       )
    };
existing_in_state(PN, 'true') ->
    State = wh_util:to_list(knm_phone_number:state(PN)),

    {lists:flatten(["Ensure number in ", State, " can be 'created'"])
     ,?_assert(knm_number:ensure_can_load_to_create(PN))
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
