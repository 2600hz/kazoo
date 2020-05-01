%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Limits document: tests
%%% @author Sean Wysor
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_limits_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOC, kz_json:set_values([{<<"allow_postpay">>, 'false'}
                                ,{<<"allow_prepay">>, 'true'}
                                ,{<<"authz_resource_type">>, []}
                                ,{<<"burst_trunks">>, 0}
                                ,{<<"calls">>, 4}
                                ,{<<"enabled">>, 'true'}
                                ,{<<"inbound_trunks">>, 0}
                                ,{<<"max_postpay_account">>, 0}
                                ,{<<"outbound_trunks">>, 0}
                                ,{<<"reserve_amount">>, 0}
                                ,{<<"resource_consuming_calls">>, 4}
                                ,{<<"soft_limit_inbound">>, 'false'}
                                ,{<<"soft_limit_outbound">>, 'false'}
                                ]
                               ,kz_json:new()
                               )
       ).
-define(EMPTY, kz_json:new()).

-spec calls_test_() -> 'ok'.
calls_test_() ->
    [?_assertEqual(4, kzd_limits:calls(?DOC))
    ,?_assertEqual(4, kzd_limits:calls(kzd_limits:set_pvt_calls(?DOC, -1)))
    ,?_assertEqual(10, kzd_limits:calls(kz_json:delete_key(<<"calls">>, kzd_limits:set_pvt_calls(?DOC, 10))))
    ,?_assertEqual(-1, kzd_limits:calls(kz_json:delete_key(<<"calls">>, ?DOC)))
    ,?_assertEqual(5, kzd_limits:calls(kzd_limits:set_calls(kzd_limits:set_pvt_calls(?DOC, 10), 5)))
    ,?_assertEqual(5, kzd_limits:calls(kzd_limits:set_calls(kzd_limits:set_pvt_calls(?DOC, 5), 10)))
    ].

-spec resource_consuming_calls_test_() -> 'ok'.
resource_consuming_calls_test_() ->
    [?_assertEqual(4, kzd_limits:resource_consuming_calls(?DOC))
    ,?_assertEqual(4, kzd_limits:resource_consuming_calls(kzd_limits:set_pvt_resource_consuming_calls(?DOC, -1)))
    ,?_assertEqual(10, kzd_limits:resource_consuming_calls(kz_json:delete_key(<<"resource_consuming_calls">>, kzd_limits:set_pvt_resource_consuming_calls(?DOC, 10))))
    ,?_assertEqual(-1, kzd_limits:resource_consuming_calls(kz_json:delete_key(<<"resource_consuming_calls">>, ?DOC)))
    ,?_assertEqual(5, kzd_limits:resource_consuming_calls(kzd_limits:set_resource_consuming_calls(kzd_limits:set_pvt_resource_consuming_calls(?DOC, 10), 5)))
    ,?_assertEqual(5, kzd_limits:resource_consuming_calls(kzd_limits:set_resource_consuming_calls(kzd_limits:set_pvt_resource_consuming_calls(?DOC, 5), 10)))
    ].
