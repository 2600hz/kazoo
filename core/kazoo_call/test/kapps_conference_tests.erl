%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_conference_tests).

-include_lib("eunit/include/eunit.hrl").

-define(APP, 'kazoo_call').

from_doc_test_() ->
    {'setup'
    ,fun kazoo_call_test_util:setup_db/0
    ,fun kazoo_call_test_util:terminate_db/1
    ,fun(_Setup) ->
             [{"Test TwiML-supplied conference doc", twiml_conf_doc()}]
     end
    }.

twiml_conf_doc() ->
    {'ok', ConfDoc} = kz_json:fixture(?APP, "fixtures/twiml_conference_doc.json"),
    Conf = kapps_conference:from_conference_doc(ConfDoc),

    API = kz_json:from_list([{<<"Conference-Doc">>, kz_doc:public_fields(ConfDoc)}]),
    APIConf = kapps_conference:from_json(API),

    lists:foldl(fun(C, Tests) -> tests(C, Tests, ConfDoc) end
               ,[]
               ,[Conf, APIConf]
               ).

tests(C, Tests, ConfDoc) ->
    [{"same name", ?_assertEqual(kzd_conferences:name(ConfDoc), kapps_conference:name(C))}
    ,{"same id", ?_assertEqual(kz_doc:id(ConfDoc), kapps_conference:id(C))}
    ,{"same play_welcome", ?_assertEqual(kzd_conferences:play_welcome(ConfDoc), kapps_conference:play_welcome(C))}
    ,{"same play_entry_tone", ?_assertEqual(kzd_conferences:play_entry_tone(ConfDoc), kapps_conference:play_entry_tone(C))}
    ,{"same require_moderator", ?_assertEqual(kzd_conferences:require_moderator(ConfDoc), kapps_conference:require_moderator(C))}
    ,{"same wait_for_moderator", ?_assertEqual(kzd_conferences:wait_for_moderator(ConfDoc), kapps_conference:wait_for_moderator(C))}
    ,{"same profile_name", ?_assertEqual(kzd_conferences:profile_name(ConfDoc), kapps_conference:profile_name(C))}
    ,{"same profile", ?_assertEqual({kzd_conferences:profile_name(ConfDoc), kzd_conferences:profile(ConfDoc)}, kapps_conference:profile(C))}
     | Tests
    ].
