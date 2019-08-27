%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_media).

%% API requests
-export([summary/2
        ,create/3
        ,fetch/3, fetch/4
        ,fetch_binary/3
        ,update/3, update/4
        ,update_binary/4
        ,patch/4
        ,delete/3
        ]).

-export([seq/0
        ,cleanup/0
        ,new_media_doc/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,media_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_media:doc()) -> pqc_cb_api:response().
create(API, AccountId, MediaJObj) ->
    URL = media_url(AccountId),

    Expectations = [#expectation{response_codes = [201]
                                ,response_headers = [{"content-type", "application/json"}
                                                    ,{"location", {'match', expected_location_value(URL)}}
                                                    ]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(MediaJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, MediaId) ->
    fetch(API, AccountId, MediaId, <<"application/json">>).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, MediaId, AcceptType) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", kz_term:to_list(AcceptType)}]
                                }],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,media_url(AccountId, MediaId)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, kz_term:to_list(AcceptType)}])
                           ).

-spec fetch_binary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_binary(API, AccountId, MediaId) ->
    AcceptType = "audio/mp3",
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", AcceptType}]
                                }],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,media_bin_url(AccountId, MediaId)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, AcceptType}])
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_media:doc()) -> pqc_cb_api:response().
update(API, AccountId, MediaJObj) ->
    URL = media_url(AccountId, kz_doc:id(MediaJObj)),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(MediaJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), iodata()) -> pqc_cb_api:response().
update(API, AccountId, MediaId, Data) ->
    URL = media_url(AccountId, MediaId),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API, [{<<"content-type">>, "audio/mp3"}])
                           ,Data
                           ).

-spec update_binary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), iodata()) -> pqc_cb_api:response().
update_binary(API, AccountId, MediaId, Data) ->
    URL = media_bin_url(AccountId, MediaId),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API, [{<<"content-type">>, "audio/mp3"}])
                           ,Data
                           ).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, MediaId, PatchJObj) ->
    URL = media_url(AccountId, MediaId),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(PatchJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, MediaId) ->
    URL = media_url(AccountId, MediaId),
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec media_url(kz_term:ne_binary()) -> string().
media_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "media"], "/").

-spec media_bin_url(kz_term:ne_binary(), kz_term:ne_binaryy()) -> string().
media_bin_url(AccountId, MediaId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "media", kz_term:to_list(MediaId), "raw"], "/").

-spec media_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
media_url(AccountId, MediaId) ->
    string:join([media_url(AccountId), kz_term:to_list(MediaId)], "/").


-spec seq() -> 'ok'.
seq() ->
    Fs = [fun seq_media_file/0],
    run_funs(Fs).

run_funs([]) -> 'ok';
run_funs([F|Fs]) ->
    _ = F(),
    cleanup(),
    run_funs(Fs).

seq_media_file() ->
    API = pqc_cb_api:init_api(['crossbar', 'media_mgr'], ['cb_media']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value([<<"data">>], kz_json:decode(EmptySummaryResp)),

    CreateMetaResp = create(API, AccountId, new_media_doc()),
    lager:info("created media meta: ~s", [CreateMetaResp]),
    CreatedMeta = kz_json:get_json_value([<<"data">>], kz_json:decode(CreateMetaResp)),
    MediaId = kz_doc:id(CreatedMeta),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [MediaSummary] = kz_json:get_list_value([<<"data">>], kz_json:decode(SummaryResp)),
    'true' = kz_doc:id(CreatedMeta) =:= kz_doc:id(MediaSummary),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),

    UploadResp = update_binary(API, AccountId, MediaId, MP3),
    lager:info("upload resp: ~s", [UploadResp]),
    UploadedMediaMeta = kz_json:get_json_value([<<"data">>], kz_json:decode(UploadResp)),
    MediaId = kz_doc:id(UploadedMediaMeta),

    UpdateResp = update(API, AccountId, MediaId, MP3),
    lager:info("update resp: ~s", [UploadResp]),
    UpdatedMediaMeta = kz_json:get_json_value([<<"data">>], kz_json:decode(UpdateResp)),
    MediaId = kz_doc:id(UpdatedMediaMeta),

    FetchedMedia = fetch_binary(API, AccountId, MediaId),
    lager:info("fetched binary: ~p", [FetchedMedia]),
    MP3 = FetchedMedia,

    FetchedMediaAgain = fetch(API, AccountId, MediaId, <<"audio/mp3">>),
    lager:info("fetched binary again: ~p", [FetchedMediaAgain]),
    MP3 = FetchedMediaAgain,

    MediaName = kz_media_util:media_path(<<"/", AccountId/binary, "/", MediaId/binary>>, kz_binary:rand_hex(16)),

    lager:info("fetching URL for ~s", [MediaName]),
    {'ok', [AMQPResp|_]} = pqc_media_mgr:request_media_url(MediaName, <<"new">>),
    lager:info("fetched URL for ~s: ~p", [MediaName, AMQPResp]),
    StreamURL = kz_json:get_ne_binary_value(<<"Stream-URL">>, AMQPResp),
    lager:info("streaming from ~s", [StreamURL]),
    {'ok', 200, _, FetchedMP3} = kz_http:get(kz_term:to_list(StreamURL)),
    lager:info("streamed: ~p", [FetchedMP3]),
    MP3 = FetchedMP3,

    DeleteResp = delete(API, AccountId, MediaId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptySummaryAgain = summary(API, AccountId),
    lager:info("empty summary again: ~s", [EmptySummaryAgain]),
    [] = kz_json:get_list_value([<<"data">>], kz_json:decode(EmptySummaryAgain)),

    cleanup(API),
    lager:info("FINISHED MEDIA SEQ").

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

-spec create_account(pqc_cb_api:state()) -> kz_term:ne_binary().
create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

%% take http://whatever:port/v2/... and get /v2/.../{regex}
expected_location_value(URL) ->
    expected_location_value(URL, "(\\w{32})").

expected_location_value(URL, Id) ->
    {'match', [_Host, Path]} = re:run(URL, "^(.+)(/v2/.+$)", [{'capture','all_but_first', 'list'}]),
    Path ++ [$/ | kz_term:to_list(Id)].

-spec new_media_doc() -> kzd_media:doc().
new_media_doc() ->
    Set = [{fun kzd_media:set_name/2, kz_binary:rand_hex(6)}],
    kz_doc:public_fields(kz_json:exec_first(Set, kzd_media:new())).
