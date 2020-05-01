%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_tests).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_speech.hrl").

-define(ASR_PROVIDER_DEFAULT, <<"ispeech">>).
-define(ASR_ACCEPT_DEFAULT, [<<"audio/mpeg">>, <<"audio/wav">>, <<"application/wav">>]).

-define(ASR_PREF_GOOGLE, <<"application/wav">>).
-define(ASR_PREF_ISPEECH, <<"application/wav">>).

%%------------------------------------------------------------------------------
%% Test system default provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_default_test_() ->
    [{"kazoo_asr system default provider abstraction."
     ,default_asr_provider_test()}
    ,{"kazoo_asr system default accepted content types test."
     ,default_asr_accept_test()}
    ].

%%------------------------------------------------------------------------------
%% Test google asr provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_google_test_() ->
    {'setup'
    ,fun mock_me/0
    ,fun cleanup/1
    ,fun(_) -> [{"kazoo_asr google provider abstraction."
                ,google_asr_provider_test()}
               ]
     end
    }.

%%------------------------------------------------------------------------------
%% Test ispeech provider through kazoo_asr abstraction
%%------------------------------------------------------------------------------
asr_ispeech_test_() ->
    {'setup'
    ,fun mock_me/0
    ,fun cleanup/1
    ,fun(_) -> [{"kazoo_asr ispeech provider abstraction."
                ,ispeech_asr_provider_test()}
               ]
     end
    }.

%%------------------------------------------------------------------------------
%% Intialize test fixtures
%%------------------------------------------------------------------------------
mock_me() -> meck:new('kapps_config', [unstick]).

%%------------------------------------------------------------------------------
%% Teardown fixtures
%%------------------------------------------------------------------------------
cleanup(_) -> meck:unload().

%%------------------------------------------------------------------------------
%%  Mock google kapps_config calls
%%------------------------------------------------------------------------------
config_asr_google(_, <<"asr_provider">>, _) -> <<"google">>.

%%------------------------------------------------------------------------------
%% Mock ispeech kapps_config calls
%%------------------------------------------------------------------------------
config_asr_ispeech(_, <<"asr_provider">>, _) -> <<"ispeech">>.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
default_asr_provider_test() ->
    [{"Checking system default ASR provider"
     ,?_assertEqual(kazoo_asr:default_provider(), ?ASR_PROVIDER_DEFAULT)}
    ].

default_asr_accept_test() ->
    [{"Checking system default accepted content type"
     ,?_assertEqual(kazoo_asr:accepted_content_types(), ?ASR_ACCEPT_DEFAULT)}
    ].

google_asr_provider_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_google/3),
    [{"Checking google is default ASR"
     ,?_assertEqual(kazoo_asr:default_provider(), <<"google">>)}
    ].

ispeech_asr_provider_test() ->
    meck:expect('kapps_config', 'get_ne_binary', fun config_asr_ispeech/3),
    [{"Checking ispeech is default ASR"
     ,?_assertEqual(kazoo_asr:default_provider(), <<"ispeech">>)}
    ].
