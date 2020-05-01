%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_directory_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("callflow/src/module/cf_directory.hrl").

-define(NAMES, [{<<"Terry">>, <<"White">>}
               ,{<<"Empty">>, <<"158">>}
               ,{<<"Alyssa">>, <<"Gruzwalski">>}
               ,{<<"Lauren">>, <<"Underwood">>}
               ,{<<"Julie">>, <<"Bloomenstein">>}
               ,{<<"Laurie">>, <<"Zeidman">>}
               ,{<<"Empty">>, <<"162">>}
               ,{<<"NaChelle">>, <<"Webster">>}
               ,{<<"Robert">>, <<"Sosin">>}
               ,{<<"Dawna">>, <<"Wilson">>}
               ,{<<"Greg">>, <<"Shanaberger">>}
               ,{<<"Mort">>, <<"Noveck">>}
               ,{<<"Rob">>, <<"Lech">>}
               ,{<<"Jessica">>, <<"Kalvenas">>}
               ,{<<"Ralph">>, <<"Sosin">>}
               ,{<<"Empty">>, <<"160">>}
               ,{<<"Daniel">>, <<"Noveck">>}
               ]).

-define(DIR_USERS, [cf_directory:get_directory_user(kz_json:from_list([{<<"first_name">>, First}
                                                                      ,{<<"last_name">>, Last}
                                                                      ])
                                                   ,'undefined'
                                                   )
                    || {First, Last} <- ?NAMES
                   ]).

sort_order_test_() ->
    [#directory_user{first_name=Alpha} | _] = cf_directory:sort_users(?DIR_USERS, 'first'),
    [#directory_user{last_name=Omega} | _] = cf_directory:sort_users(?DIR_USERS, 'last'),
    [{"Sort by first name", ?_assertEqual(<<"Alyssa">>, Alpha)}
    ,{"Sort by last name", ?_assertEqual(<<"158">>, Omega)}
    ].

filter_users_test_() ->
    SortedFirst = cf_directory:sort_users(?DIR_USERS, 'first'),
    SortedLast = cf_directory:sort_users(?DIR_USERS, 'last'),

    [#directory_user{first_name=AlphaFirst} | _]
        = cf_directory:filter_users(SortedFirst, <<"528">>, 'first'),

    [#directory_user{last_name=OmegaLast
                    ,first_name=OmegaFirst
                    } | _]
        = cf_directory:filter_users(SortedLast, <<"668">>, 'last'),

    [{"Filtered first_name sort", ?_assertEqual(<<"Lauren">>, AlphaFirst)}
    ,{"Filtered last_name sort - last name", ?_assertEqual(<<"Noveck">>, OmegaLast)}
    ,{"Filtered last_name sort - first name", ?_assertEqual(<<"Daniel">>, OmegaFirst)}
    ].
