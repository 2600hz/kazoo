%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_doc_tests).

-include_lib("eunit/include/eunit.hrl").

patch_test_() ->
    RequestData = kz_json:from_list([{<<"enabled">>, 'true'}]),
    ExistingDoc = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                    ,{<<"enabled">>, 'false'}
                                    ,{<<"elbow">>, <<"grease">>}
                                    ]),

    PatchedJObj = crossbar_doc:patch_the_doc(RequestData, ExistingDoc),

    [?_assert(kz_json:is_true(<<"enabled">>, PatchedJObj))].

patch_recursive_test_() ->
    RequestData = kz_json:from_list([{<<"enabled">>, 'true'}
                                    ,{<<"sip">>, kz_json:from_list([{<<"username">>, <<"me123">>}])}
                                    ,{<<"new">>, <<"field">>}
                                    ,{<<"nested">>, kz_json:from_list([{<<"another">>, <<"one">>}])}
                                    ]),

    ExistingDoc = kz_json:from_list([{<<"foo">>, <<"bar">>}
                                    ,{<<"enabled">>, 'false'}
                                    ,{<<"elbow">>, <<"grease">>}
                                    ,{<<"sip">>, kz_json:from_list([{<<"username">>, <<"foo">>}
                                                                   ,{<<"password">>, <<"bar">>}
                                                                   ]
                                                                  )
                                     }
                                    ]),

    PatchedJObj = crossbar_doc:patch_the_doc(RequestData, ExistingDoc),

    [?_assert(kz_json:is_true(<<"enabled">>, PatchedJObj))
    ,?_assertMatch(<<"grease">>, kz_json:get_value(<<"elbow">>, PatchedJObj))
    ,?_assertMatch(<<"field">>, kz_json:get_value(<<"new">>, PatchedJObj))
    ,?_assertMatch(<<"one">>, kz_json:get_value([<<"nested">>, <<"another">>], PatchedJObj))
    ,?_assertMatch(<<"me123">>, kz_json:get_value([<<"sip">>, <<"username">>], PatchedJObj))
    ,?_assertMatch(<<"bar">>, kz_json:get_value([<<"sip">>, <<"password">>], PatchedJObj))
    ].
