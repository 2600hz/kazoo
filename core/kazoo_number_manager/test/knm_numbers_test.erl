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
    [?_assertMatch(#{ko := #{}
                    ,ok := [_]
                    }, Ret)
    ,?_assertEqual(?TEST_AVAILABLE_NUM
                  ,knm_phone_number:number(knm_number:phone_number(hd(maps:get(ok, Ret))))
                  )
    ,?_assertMatch(#{ko := #{NotNum := not_reconcilable}}
                  ,knm_numbers:get([NotNum], [])
                  )
    ,?_assertMatch(#{ko := #{?TEST_CREATE_NUM := not_found}}
                  ,knm_numbers:get([?TEST_CREATE_NUM], [])
                  )
    ,?_assertMatch(#{ko := #{NotNum := not_reconcilable}
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, NotNum])
                  )
    ,?_assertMatch(#{ko := #{NotNum := not_reconcilable}
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, NotNum
                                   ,?TEST_AVAILABLE_NUM, NotNum])
                  )
    ,?_assertMatch(#{ko := #{NotNum := not_reconcilable
                            ,?TEST_CREATE_NUM := not_found
                            }
                    ,ok := [_]
                    }
                  ,knm_numbers:get([?TEST_AVAILABLE_NUM, NotNum
                                   ,?TEST_AVAILABLE_NUM, NotNum
                                   ,?TEST_CREATE_NUM])
                  )
    ].
