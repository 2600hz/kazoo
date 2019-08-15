%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_services_tests).

-include_lib("eunit/include/eunit.hrl").


bom_1() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:new()}
      ,{<<"phone_numbers">>, kz_json:from_list(
                               [{<<"did_us">>, 1}
                               ])
       }
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

bom_2() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:new()}
      ,{<<"phone_numbers">>, kz_json:new()}
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

eom_1() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:from_list(
                                 [{<<"local">>, 130}
                                 ])
       }
      ,{<<"phone_numbers">>, kz_json:from_list(
                               [{<<"did_us">>, 1}
                               ])
       }
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

rows_for_missing_eom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201504">>,
    Expected =
        [#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"branding">>
          ,<<"item">> => <<"whitelabel">>
          ,<<"quantity_bom">> => <<"0">>
          ,<<"quantity_eom">> => undefined
          }
        ,#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"ips">>
          ,<<"item">> => <<"dedicated">>
          ,<<"quantity_bom">> => <<"0">>
          ,<<"quantity_eom">> => undefined
          }
        ],
    ?assertEqual(Expected, kt_services:rows_for_quantities(AccountId, YYYY, MM, bom_2(), kz_json:new())).

rows_for_missing_bom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201504">>,
    Expected =
        [#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"branding">>
          ,<<"item">> => <<"whitelabel">>
          ,<<"quantity_bom">> => undefined
          ,<<"quantity_eom">> => <<"0">>
          }
        ,#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"ips">>
          ,<<"item">> => <<"dedicated">>
          ,<<"quantity_bom">> => undefined
          ,<<"quantity_eom">> => <<"0">>
          }
        ],
    ?assertEqual(Expected, kt_services:rows_for_quantities(AccountId, YYYY, MM, kz_json:new(), bom_2())).

rows_for_bom_and_eom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201606">>,
    Expected =
        [#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"branding">>
          ,<<"item">> => <<"whitelabel">>
          ,<<"quantity_bom">> => <<"0">>
          ,<<"quantity_eom">> => <<"0">>
          }
        ,#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"ips">>
          ,<<"item">> => <<"dedicated">>
          ,<<"quantity_bom">> => <<"0">>
          ,<<"quantity_eom">> => <<"0">>
          }
        ,#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"number_services">>
          ,<<"item">> => <<"local">>
          ,<<"quantity_bom">> => undefined
          ,<<"quantity_eom">> => <<"130">>
          }
        ,#{<<"account_id">> => AccountId
          ,<<"year">> => YYYY
          ,<<"month">> => MM
          ,<<"category">> => <<"phone_numbers">>
          ,<<"item">> => <<"did_us">>
          ,<<"quantity_bom">> => <<"1">>
          ,<<"quantity_eom">> => <<"1">>
          }
        ],
    ?assertEqual(Expected, kt_services:rows_for_quantities(AccountId, YYYY, MM, bom_1(), eom_1())).
