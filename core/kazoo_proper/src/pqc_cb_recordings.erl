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
-module(pqc_cb_recordings).

-export([list_recordings/2
        ,create_recording/2
        ,fetch_recording/3
        ,fetch_recording_binary/3, fetch_recording_tunneled/3
        ,delete_recording/3
        ]).

%% -export([command/2
%%          ,next_state/3
%%          ,postcondition/3
%%         ]).

-export([seq/0]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_recordings">>]).

-spec list_recordings(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                             {'error', 'not_found'} |
                             {'ok', kz_json:objects()}.
list_recordings(API, AccountId) ->
    Expectations = [#expectation{response_codes = [200]}],
    case pqc_cb_api:make_request(Expectations
                                ,fun kz_http:get/2
                                ,recordings_url(AccountId)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("listing recordings errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            ?DEBUG("listing recordings: ~s", [Response]),
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_recording(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
                             {'ok', kz_json:object()} |
                             {'error', 'not_found'}.
fetch_recording(API, AccountId, RecordingId) ->
    Expectations = [#expectation{response_codes = [200]}],
    case pqc_cb_api:make_request(Expectations
                                ,fun kz_http:get/2
                                ,recordings_url(AccountId, RecordingId)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetching recording errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            ?DEBUG("fetching recording: ~s", [Response]),
            {'ok', kz_json:get_json_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_recording_binary(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
                                    {'ok', kz_term:ne_binary()} |
                                    {'error', 'not_found'}.
fetch_recording_binary(API, AccountId, RecordingId) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "audio/mpeg"}]
                                }],
    case pqc_cb_api:make_request(Expectations
                                ,fun kz_http:get/2
                                ,recordings_url(AccountId, RecordingId)
                                ,pqc_cb_api:request_headers(API, [{<<"accept">>, "audio/mpeg"}])
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetching binary errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', Response}
    end.

-spec fetch_recording_tunneled(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
                                      {'ok', kz_term:ne_binary()} |
                                      {'error', 'not_found'}.
fetch_recording_tunneled(API, AccountId, RecordingId) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "audio/mpeg"}]
                                }],
    case pqc_cb_api:make_request(Expectations
                                ,fun kz_http:get/2
                                ,recordings_url(AccountId, RecordingId) ++ "?accept=audio/mpeg"
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetching binary/tunneled errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', Response}
    end.

-spec create_recording(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                              {'ok', kzd_call_recordings:doc()}.
create_recording(_API, AccountId) ->
    MODB = kz_util:format_account_mod_id(AccountId),

    BaseMediaDoc = create_recording_doc(),
    MediaDoc = kz_doc:update_pvt_parameters(BaseMediaDoc, MODB, [{'type', kzd_call_recordings:type()}]),
    ?INFO("saving to ~s: ~p", [MODB, MediaDoc]),
    {'ok', Doc} = kazoo_modb:save_doc(MODB, MediaDoc, [{'ensure_saved', 'true'}]),
    {'ok', _} = create_attachment(MODB, kz_doc:id(Doc)),
    kz_datamgr:open_cache_doc(MODB, kz_doc:id(Doc)).

-define(RECORDING_ID, <<"bf8a6522730f93248d41f2521cfe2b95">>).
create_recording_doc() ->
    lists:foldl(fun({F, V}, Doc) -> F(Doc, V) end
               ,kzd_call_recordings:new()
               ,[{fun kzd_call_recordings:set_id/2, ?RECORDING_ID}
                ,{fun kzd_call_recordings:set_description/2, <<"pqc_cb_recordings test">>}
                ,{fun kzd_call_recordings:set_source_type/2, kz_term:to_binary(?MODULE)}
                ]
               ).

-define(MP3_FILE, filename:join([code:priv_dir('kazoo_proper'), <<"mp3.mp3">>])).
create_attachment(MODB, DocId) ->
    File = ?MP3_FILE,
    AName = filename:basename(File, <<".mp3">>),
    {'ok', Contents} = file:read_file(File),
    ?INFO("adding attachment to ~s/~s: ~s", [MODB, DocId, AName]),
    kz_datamgr:put_attachment(MODB, DocId, AName, Contents, [{'content_type', kz_mime:from_filename(File)}]).

-spec delete_recording(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) ->
                              {'ok', kz_json:object()} |
                              {'error', 'not_found'}.
delete_recording(API, AccountId, RecordingId) ->
    Expectations = [#expectation{response_codes = [200, 404]}],
    case pqc_cb_api:make_request(Expectations
                                ,fun kz_http:delete/2
                                ,recordings_url(AccountId, RecordingId)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("delete recording errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            JObj = kz_json:decode(Response),
            case kz_json:get_integer_value(<<"error">>, JObj) of
                404 -> {'error', 'not_found'};
                _Code -> {'ok', kz_json:get_json_value(<<"data">>, JObj)}
            end
    end.

recordings_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "recordings"], "/").

recordings_url(AccountId, RecordingId) ->
    string:join([recordings_url(AccountId), kz_term:to_list(RecordingId)], "/").

-spec seq() -> 'ok'.
seq() ->
    _ = init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),
    File = ?MP3_FILE,
    {'ok', Contents} = file:read_file(File),

    try
        AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
        AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
        ?INFO("created account ~s", [AccountId]),

        {'ok', []} = list_recordings(API, AccountId),
        ?INFO("no recordings available yet", []),

        {'ok', RecordingDoc} = create_recording(API, AccountId),
        ?INFO("created recording ~p", [RecordingDoc]),

        {'ok', [RecordingSummary]} = list_recordings(API, AccountId),
        ?INFO("recording available: ~p", [RecordingSummary]),

        'true' = kz_doc:id(RecordingSummary) =:= kz_doc:id(RecordingDoc),
        ?INFO("recording is available"),

        {'ok', Recording} = fetch_recording(API, AccountId, kz_doc:id(RecordingSummary)),
        ?INFO("recording meta: ~p", [Recording]),

        {'ok', Contents} = fetch_recording_binary(API, AccountId, kz_doc:id(RecordingSummary)),
        ?INFO("fetched MP3"),

        {'ok', Contents} = fetch_recording_tunneled(API, AccountId, kz_doc:id(RecordingSummary)),
        ?INFO("fetched tunneled MP3"),

        {'ok', Deleted} = delete_recording(API, AccountId, kz_doc:id(RecordingSummary)),
        ?INFO("deleted recording: ~p", [Deleted]),

        {'ok', []} = list_recordings(API, AccountId),
        ?INFO("no recordings available again", []),

        io:format(?MODULE_STRING":seq/0 was successful~n")
    catch
        ?STACKTRACE(_E, _R, ST)
        ?INFO(?MODULE_STRING ":seq/0 failed ~s: ~p", [_E, _R]),
        _ = [?INFO("st: ~p", [S]) || S <- ST],
        io:format(?MODULE_STRING ":seq/0 failed: ~s: ~p", [_E, _R])
    after
        pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
        pqc_cb_api:cleanup(API)
    end,
    ?INFO("seq finished running: ~p", [API]),
    io:format('user', "logs in /tmp/~s.log~n", [maps:get('request_id', API)]).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_recordings', 'cb_accounts']
        ],
    ?INFO("INIT FINISHED").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).
