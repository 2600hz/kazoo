%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_numbers_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

get_test_() ->
    Ret = knm_numbers:get([?TEST_AVAILABLE_NUM]),
    NotNum = <<"NOT a number">>,
    UnknownNum = ?TEST_CREATE_NUM,
    [?_assertMatches(#{ko := #{}
                      ,ok := [_]
                      }, Ret)
    ,?_assertEqual(?TEST_AVAILABLE_NUM
                  ,knm_phone_number:number(knm_number:phone_number(hd(maps:get(ok, Ret))))
                  )
    ,?_assertMatches(#{ko := #{NotNum => not_reconcilable}}
                    ,km_numbers:get([NotNum], [])
                    )
    ,?_assertMatches(#{ko := #{UnknownNum => not_found}}
                    ,km_numbers:get([UnknownNum], [])
                    )
    ,?_assertMatches(#{ko := #{NotNum => not_reconcilable}
                      ,ok := [_]
                      }
                    ,knm_numbers:get([?TEST_AVAILABLE_NUM, NotNum])
                    )
    ,?_assertMatches(#{ko := #{NotNum => not_reconcilable}
                      ,ok := [_]
                      }
                    ,knm_numbers:get([?TEST_AVAILABLE_NUM, NotNum
                                     ,?TEST_AVAILABLE_NUM, NotNum])
                    )
    ].
