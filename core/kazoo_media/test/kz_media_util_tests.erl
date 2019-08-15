%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Tests for kz_media_util
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_util_tests).

-include_lib("eunit/include/eunit.hrl").

get_prompt_test_() ->
    Tests = [{"untouched tone_stream", [<<"tone_stream://%(250,250,480,620);loops=25">>], <<"tone_stream://%(250,250,480,620);loops=25">>}
            ,{"untouched tone_stream with lang", [<<"tone_stream://%(250,250,480,620);loops=25">>, <<"en-us">>], <<"tone_stream://%(250,250,480,620);loops=25">>}
            ,{"untouched tone_stream with lang and account", [<<"tone_stream://%(250,250,480,620);loops=25">>, <<"en-us">>, kz_binary:rand_hex(16)], <<"tone_stream://%(250,250,480,620);loops=25">>}

            ,{"untouched prompt", [<<"prompt://system_media/vm-full/en-us">>], <<"prompt://system_media/vm-full/en-us">>}
            ,{"full prompt path", [<<"vm-full">>], <<"prompt://system_media/vm-full/en-us">>}
            ,{"full prompt path with lang", [<<"vm-full">>, <<"mk-bs">>], <<"prompt://system_media/vm-full/mk-bs">>}
            ],

    [{Description, ?_assertEqual(Expected, apply_get_prompt(Args))}
     || {Description, Args, Expected} <- Tests
    ].

apply_get_prompt([Name]) ->
    kz_media_util:get_prompt(Name);
apply_get_prompt([Name, Lang]) ->
    kz_media_util:get_prompt(Name, Lang);
apply_get_prompt([Name, Lang, AccountId]) ->
    kz_media_util:get_prompt(Name, Lang, AccountId).
