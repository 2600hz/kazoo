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

get_available_test_() ->
    [fun available_as_owner/0
     ,fun available_as_parent/0
     ,fun available_as_rando/0
    ].

available_as_owner() ->
    available_as(?RESELLER_ACCOUNT_ID).

available_as_parent() ->
    available_as(?MASTER_ACCOUNT_ID).

available_as_rando() ->
    available_as(wh_util:rand_hex_binary(16)).

available_as(AuthAccountId) ->
    case knm_number:get(?TEST_AVAILABLE_NUM
                       ,[{'auth_by', AuthAccountId}]
                       )
    of
        {'ok', Number} -> available_tests(Number);
        {'error', Error} -> unavailable_tests(Error)
    end.

unavailable_tests(ErrorJObj) ->
    [{"verify unavailable number error code"
      ,?_assertEqual(403, knm_errors:code(ErrorJObj))
     }
     ,{"verify unavailable number error"
      ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].

available_tests(Number) ->
    PhoneNumber = knm_number:phone_number(Number),

    [{"Verify available phone number"
      ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(PhoneNumber))
      }
     ,{"Verify available number module"
       ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PhoneNumber))
      }
     ,{"Verify available number state"
       ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PhoneNumber))
      }
    ].

get_unreconcilable_number_test_() ->
    [{"Verify non-reconcilable numbers result in errors"
      ,?_assertMatch({'error', 'not_reconcilable'}
                     ,knm_number:get(<<"1000">>)
                    )
     }
    ].
