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

create_new_number_test() ->
    Props = [{<<"auth_by">>, ?MASTER_ACCOUNT_ID}
             ,{<<"assign_to">>, ?RESELLER_ACCOUNT_ID}
             ,{<<"dry_run">>, 'false'}
             ,{<<"auth_by_account">>
               ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')
              }
            ],
    knm_number:create_or_load(?TEST_CREATE_NUM, Props, {'error', 'not_found'}).
