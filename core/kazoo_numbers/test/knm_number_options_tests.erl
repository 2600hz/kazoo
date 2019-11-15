%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_number_options_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

to_phone_number_setters_test_() ->
    A_1 = kz_json:from_list([{<<"a">>, 1}]),
    M_1 = ?CARRIER_LOCAL,
    [?_assertEqual([{fun knm_phone_number:reset_doc/2, A_1}]
                  ,knm_number_options:to_phone_number_setters([{'public_fields', A_1}])
                  )
    ,?_assertEqual([{fun knm_phone_number:set_auth_by/2, ?KNM_DEFAULT_AUTH_BY}
                   ,{fun knm_phone_number:set_ported_in/2, 'false'}
                   ,{fun knm_phone_number:set_dry_run/2, [[[]]]}
                   ]
                  ,knm_number_options:to_phone_number_setters(
                     [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
                     ,{'ported_in', 'false'}
                     ,{<<"batch_run">>, 'false'}
                     ,{'dry_run', [[[]]]}
                     ])
                  )
    ,?_assertEqual([{fun knm_phone_number:set_module_name/2, M_1}]
                  ,knm_number_options:to_phone_number_setters(
                     [{module_name, M_1}
                     ,{module_name, <<"blaaa">>}
                     ,{module_name, ?CARRIER_MDN}
                     ])
                  )
    ].
