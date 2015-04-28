%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%% Execute call commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_call_command_test).

-include_lib("eunit/include/eunit.hrl").

all_conference_flags_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}
                              ,{<<"Deaf">>, 'true'}
                              ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator,deaf}">>, ecallmgr_call_command:get_conference_flags(JObj)).

two_conference_flags_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}
                              ,{<<"Moderator">>, 'true'}
                             ]),
    ?assertEqual(<<"+flags{mute,moderator}">>, ecallmgr_call_command:get_conference_flags(JObj)).

one_conference_flag_test() ->
    JObj = wh_json:from_list([{<<"Mute">>, 'true'}]),
    ?assertEqual(<<"+flags{mute}">>, ecallmgr_call_command:get_conference_flags(JObj)).

no_conference_flags_test() ->
    JObj = wh_json:new(),
    ?assertEqual(<<>>, ecallmgr_call_command:get_conference_flags(JObj)).
