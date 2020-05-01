%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Execute call commands
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_command_tests).

-include_lib("eunit/include/eunit.hrl").

all_conference_flags_test() ->
    JObj = kz_json:from_list([{<<"Mute">>, 'true'}
                             ,{<<"Deaf">>, 'true'}
                             ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator,deaf}">>, ecallmgr_call_command:get_conference_flags(JObj)).

two_conference_flags_test() ->
    JObj = kz_json:from_list([{<<"Mute">>, 'true'}
                             ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator}">>, ecallmgr_call_command:get_conference_flags(JObj)).

one_conference_flag_test() ->
    JObj = kz_json:from_list([{<<"Mute">>, 'true'}]),
    ?assertEqual(<<"+flags{mute}">>, ecallmgr_call_command:get_conference_flags(JObj)).

no_conference_flags_test() ->
    JObj = kz_json:new(),
    ?assertEqual(<<>>, ecallmgr_call_command:get_conference_flags(JObj)).

tones_test() ->
    Tones =
        [kz_json:from_list([{<<"Frequencies">>, [1000, <<"2000">>]}
                           ,{<<"Duration-ON">>, 30000}
                           ,{<<"Duration-OFF">>, <<"1000">>}
                           ]
                          )
        ,kz_json:from_list([{<<"Frequencies">>, [1000, <<"2000">>, 3000, <<"4000">>]}
                           ,{<<"Duration-ON">>, <<"30000">>}
                           ,{<<"Duration-OFF">>, 1000}
                           ,{<<"Volume">>, 25}
                           ,{<<"Repeat">>, 3}
                           ]
                          )
        ],
    ?assertEqual({<<"playback">>
                 ,"tone_stream://%(30000,1000,1000,2000);v=25;l=3;%(30000,1000,1000,2000,3000,4000)"
                 }
                ,ecallmgr_call_command:tones_app(Tones)
                ).
