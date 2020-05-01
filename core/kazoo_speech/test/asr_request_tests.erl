%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(asr_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include("kazoo_speech.hrl").

-define(ACCOUNT_ID, <<"account0000000000000000000000003">>).
-define(ACCOUNT_DB, <<"account%2Fac%2Fco%2Funt0000000000000000000000003">>).
-define(ASR_PROVIDER, <<"google">>).
-define(CALL_ID, <<"36-52896@10.26.0.167">>).
-define(DESCRIPTION, <<(?ASR_PROVIDER)/binary, " ASR transcription">>).
-define(RESELLER_ID, <<"account0000000000000000000000002">>).
-define(MEDIA_ID, <<"201710-vm_message0000000000000000000001">>).

%%%-----------------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------------
-spec render_test_() -> any().
render_test_() ->
    {'setup'
    ,fun setup_fixtures/0
    ,fun cleanup/1
    ,fun(_) -> [{"API from_call tests", api_from_call()}
               ,{"API setter tests", api_setter()}
               ,{"API from voicemail tests", api_from_voicemail()}
               ]
     end
    }.

%%%-----------------------------------------------------------------------------
%%% intialize fixture and startup fixturedb
%%%-----------------------------------------------------------------------------
setup_fixtures() ->
    ?LOG_DEBUG(":: Setting up asr_request tests"),
    kz_fixturedb_util:start_me().

%%%-----------------------------------------------------------------------------
%%% teardown fixture and fixturedb
%%%-----------------------------------------------------------------------------
cleanup(Pid) -> kz_fixturedb_util:stop_me(Pid).

%%%-----------------------------------------------------------------------------
%%% Who you gonna call?
%%%-----------------------------------------------------------------------------
ghostbusers() ->
    JObj = kz_json:from_list_recursive([{<<"Call-ID">>, ?CALL_ID}
                                       ,{<<"Account-DB">>, ?ACCOUNT_DB}
                                       ,{<<"Account-ID">>, ?ACCOUNT_ID}
                                       ]),

    kapps_call:from_json(JObj).

%%%-----------------------------------------------------------------------------
%%% Test Cases
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% API Test: request instantiation from call object
%%%-----------------------------------------------------------------------------
api_from_call() ->
    Call = ghostbusers(),
    Req = asr_request:from_call(Call),
    ct:print("~p~n", [kzs_plan:plan()]),
    [{"verify account_db"
     ,?_assertEqual(?ACCOUNT_DB, asr_request:account_db(Req))}
    ,{"verify account_id"
     ,?_assertEqual(?ACCOUNT_ID, asr_request:account_id(Req))}
    ,{"verify call_id"
     ,?_assertEqual(?CALL_ID, asr_request:call_id(Req))}
    ,{"verify reseller_id"
     ,?_assertEqual(?RESELLER_ID, asr_request:reseller_id(Req))}
    ].

%%%-----------------------------------------------------------------------------
%%% API Test: request instantiation from voicemail
%%%-----------------------------------------------------------------------------
api_from_voicemail() ->
    Call = ghostbusers(),
    Req = asr_request:from_voicemail(Call, ?MEDIA_ID),
    [{"verify account_db"
     ,?_assertEqual(?ACCOUNT_DB, asr_request:account_db(Req))}
    ,{"verify account_id"
     ,?_assertEqual(?ACCOUNT_ID, asr_request:account_id(Req))}
    ,{"verify call_id"
     ,?_assertEqual(?CALL_ID, asr_request:call_id(Req))}
    ,{"verify media_id"
     ,?_assertEqual(?MEDIA_ID, asr_request:media_id(Req))}
    ,{"verify reseller_id"
     ,?_assertEqual(?RESELLER_ID, asr_request:reseller_id(Req))}
    ].

%%%-----------------------------------------------------------------------------
%%% API Test: setters and getters
%%%-----------------------------------------------------------------------------
api_setter() ->
    Setters = [{fun asr_request:set_account_db/2, ?ACCOUNT_DB}
              ,{fun asr_request:set_account_id/2, ?ACCOUNT_ID}
              ,{fun asr_request:set_asr_provider/2, ?ASR_PROVIDER}
              ,{fun asr_request:set_description/2, ?DESCRIPTION}
              ,{fun asr_request:set_media_id/2, ?MEDIA_ID}
              ,{fun asr_request:set_reseller_id/2, ?RESELLER_ID}
              ],
    Req = asr_request:setters(asr_request:new(), Setters),
    [{"set account db"
     ,?_assertEqual(?ACCOUNT_DB, asr_request:account_db(Req))}
    ,{"set account_id"
     ,?_assertEqual(?ACCOUNT_ID, asr_request:account_id(Req))}
    ,{"set asr_provider"
     ,?_assertEqual(?ASR_PROVIDER, asr_request:asr_provider(Req))}
    ,{"set description"
     ,?_assertEqual(?DESCRIPTION, asr_request:description(Req))}
    ,{"set media_id"
     ,?_assertEqual(?MEDIA_ID, asr_request:media_id(Req))}
    ,{"set reseller_id"
     ,?_assertEqual(?RESELLER_ID, asr_request:reseller_id(Req))}
    ].
