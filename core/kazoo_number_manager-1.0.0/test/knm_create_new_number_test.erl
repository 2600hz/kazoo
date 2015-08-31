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
    N = knm_number:create_or_load(?TEST_CREATE_NUM, Props, {'error', 'not_found'}),
    PN = knm_number:phone_number(N),
    ?debugFmt("pn: ~p~n", [PN]),
    ?assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN)),
    ?assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN)),
    ?assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN)),
    ?assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(PN)),
    ?assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN)),
    ?assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN)).
