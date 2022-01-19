%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_storage).

%% Manual testing
-export([seq/0, blacklisted_url/0
        ,cleanup/0
        ,storage_doc/1
        ,help_14316/0, help_14308/0
        ]).

%% API Shims
-export([create/3, create/4
        ,update/3, update/4
        ]).
%%         ,fetch/1
%%         ]).

%% PropEr callbacks
%% -export([command/1
%%         ,initial_state/0
%%         ,next_state/3
%%         ,postcondition/3
%%         ,precondition/2

%%         ,correct/0
%%         ,correct_parallel/0
%%         ]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).
-define(UUID, <<"2426cb457dc530acc881977ccbc9a7a7">>).

-define(BASE64_ENCODED, 'true').
-define(SEND_MULTIPART, 'true').

-spec create(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
          pqc_cb_api:response().
create(API, AccountId, StorageDoc) ->
    create(API, AccountId, StorageDoc, 'undefined').

-spec create(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary() | kz_json:object(), kz_term:api_boolean()) ->
          pqc_cb_api:response().
create(API, AccountId, ?NE_BINARY=UUID, ValidateSettings) ->
    create(API, AccountId, storage_doc(UUID), ValidateSettings);
create(API, AccountId, StorageDoc, ValidateSettings) ->
    StorageURL = storage_url(AccountId, ValidateSettings),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),
    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,StorageURL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(StorageDoc))
                           ).

-spec update(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
          pqc_cb_api:response().
update(API, AccountId, StorageDoc) ->
    update(API, AccountId, StorageDoc, 'undefined').

-spec update(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary() | kz_json:object(), kz_term:api_boolean()) ->
          pqc_cb_api:response().
update(API, AccountId, <<UUID/binary>>, ValidateSettings) ->
    update(API, AccountId, storage_doc(UUID), ValidateSettings);
update(API, AccountId, StorageDoc, ValidateSettings) ->
    StorageURL = storage_url(AccountId, ValidateSettings),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:post/3
                           ,StorageURL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(StorageDoc))
                           ).

storage_url('undefined') ->
    string:join([pqc_cb_api:v2_base_url(), "storage"], "/");
storage_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "storage"], "/").

storage_url(AccountId, 'false') ->
    storage_url(AccountId) ++ "?validate_settings=false";
storage_url(AccountId, _ValidateSettings) ->
    storage_url(AccountId).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init_system(),
    API = pqc_cb_api:authenticate(),
    pqc_kazoo_model:new(API).

init_system() ->
    TestId = kz_binary:rand_hex(5),
    kz_util:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'media_mgr']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_storage', 'cb_vmboxes']
        ],

    _HTTPD = pqc_httpd:start_link(TestId),
    lager:info("HTTPD started: ~p", [_HTTPD]),

    lager:info("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    Tests = [fun base_test/0
            ,fun skip_validation_test/0
            ,fun global_test/0
            ,fun missing_ref_test/0
            ,fun blacklisted_url/0
            ,fun help_14316/0
            ,fun help_14308/0
            ],
    lists:foreach(fun run_test/1, Tests).

run_test(TestFun) ->
    TestFun(),
    cleanup().

-spec help_14308() -> 'ok'.
help_14308() ->
    lager:info("VANILLA URL"),
    API = init_api(),

    AccountId = create_account(API),

    UUID = kz_binary:rand_hex(16),
    Base = pqc_httpd:base_url(),
    URL = <<Base/binary, ?MODULE_STRING>>,

    HandlerSettings = kz_json:from_list([{<<"handler">>, <<"http">>}
                                        ,{<<"name">>, <<"recording_server">>}
                                        ,{<<"settings">>
                                         ,kz_json:from_list([{<<"url">>, URL}
                                                            ,{<<"verb">>, <<"post">>}
                                                            ,{<<"base64_encode_data">>, 'false'}
                                                            ,{<<"field_list">>, []}
                                                            ])
                                         }
                                        ]),
    Attachments = kz_json:from_list([{UUID, HandlerSettings}]),

    Plan = kz_json:set_value([<<"modb">>, <<"types">>, <<"mailbox_message">>, <<"attachments">>, <<"handler">>], UUID, kz_json:new()),

    StorageDoc = kz_json:from_list([{<<"attachments">>, Attachments}
                                   ,{<<"plan">>, Plan}
                                   ]),

    kzs_plan:allow_validation_overrides(),
    CreatedResp = create(API, AccountId, StorageDoc, 'false'),
    lager:info("created resp: ~s", [CreatedResp]),

    _ = help_14308_test_vm(API, AccountId),

    cleanup(API),
    lager:info("FINISHED VANILLA URL").

-spec help_14316() -> 'ok'.
help_14316() ->
    lager:info("CALL RECORDING TEST"),
    API = init_api(),

    AccountId = create_account(API),

    UUID = kz_binary:rand_hex(16),
    URL = <<"http://2600.hz/index.fu?key=abc123&env=ext&reseller_id=42&reseller_prefix=prefix&account_prefix=pdp&recording=">>,

    HandlerSettings = kz_json:from_list([{<<"handler">>, <<"http">>}
                                        ,{<<"name">>, <<"recording_server">>}
                                        ,{<<"settings">>
                                         ,kz_json:from_list([{<<"url">>, URL}
                                                            ,{<<"verb">>, <<"post">>}
                                                            ,{<<"base64_encode_data">>, 'false'}
                                                            ])
                                         }
                                        ]),
    Attachments = kz_json:from_list([{UUID, HandlerSettings}]),

    Plan = kz_json:set_value([<<"modb">>, <<"types">>, <<"call_recording">>, <<"attachments">>, <<"handler">>], UUID, kz_json:new()),

    StorageDoc = kz_json:from_list([{<<"attachments">>, Attachments}
                                   ,{<<"plan">>, Plan}
                                   ]),

    kzs_plan:allow_validation_overrides(),
    CreatedResp = create(API, AccountId, StorageDoc, 'false'),
    lager:info("created resp: ~s", [CreatedResp]),

    %% The data cache isn't populated right away, give it a second
    timer:sleep(50),

    StoreURL = kzc_recording:should_store_recording(AccountId, 'undefined'),
    {'true', 'local'} = StoreURL,
    lager:info("call recordings configured to store locally"),

    'true' = has_expected_plan(AccountId, <<"call_recording">>, URL),

    UpdateStorage = kz_json:set_value([<<"plan">>, <<"modb">>, <<"types">>, <<"mailbox_message">>, <<"attachments">>, <<"handler">>], UUID, StorageDoc),

    UpdatedResp = update(API, AccountId, UpdateStorage, 'false'),
    lager:info("updated resp: ~s", [UpdatedResp]),

    %% The data cache isn't populated right away, give it a second
    timer:sleep(50),

    'true' = has_expected_plan(AccountId, <<"mailbox_message">>, URL),

    %% just checking if we see the reload in kzs_plan
    lager:info("hotload kzs_plan: ~p : ~p", [whereis(kazoo_bindings), kazoo_maintenance:hotload(kzs_plan)]),
    %% give it time to reload
    timer:sleep(150),

    'true' = has_expected_plan(AccountId, <<"mailbox_message">>, URL),

    cleanup(API),

    %% The data cache isn't populated right away, give it a second
    timer:sleep(50),

    has_expected_plan(AccountId, <<"call_recording">>, 'undefined'),
    has_expected_plan(AccountId, <<"mailbox_message">>, 'undefined'),

    lager:info("FINISHED HELP 14316").

has_expected_plan(AccountId, DocType, 'undefined') ->
    DBPlan = kzs_plan:get_dataplan(kz_util:format_account_mod_id(AccountId), DocType),
    lager:info("~s plan: ~p", [DocType, DBPlan]),
    'false' = maps:is_key('att_handler', DBPlan),
    'true';
has_expected_plan(AccountId, DocType, <<URL/binary>>) ->
    DBPlan = kzs_plan:get_dataplan(kz_util:format_account_mod_id(AccountId), DocType),
    lager:info("~s plan: ~p", [DocType, DBPlan]),
    {'kz_att_http', HandlerMap} = maps:get('att_handler', DBPlan),
    'false' = maps:get('base64_encode_data', HandlerMap),
    URL = maps:get('url', HandlerMap),
    <<"post">> = maps:get('verb', HandlerMap),
    lager:info("call recordings configured to store to configured URL"),
    'true'.

-spec blacklisted_url() -> 'ok'.
blacklisted_url() ->
    lager:info("BLACKLIST TEST"),

    API = init_api(),

    AccountId = create_account(API),

    StorageDoc = storage_doc(kz_binary:rand_hex(16), <<"https://ignore@me:0.0.0.0">>),
    {'error', ShouldFailToCreate} = create(API, AccountId, StorageDoc),
    lager:info("should fail: ~s", [ShouldFailToCreate]),

    cleanup(API),
    lager:info("FINISHED BLACKLIST").

skip_validation_test() ->
    lager:info("SKIP TEST"),

    API = init_api(),

    AccountId = create_account(API),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    {'error', ShouldFailToCreate} = create(API, AccountId, StorageDoc, 'false'),
    lager:info("should fail: ~s", [ShouldFailToCreate]),

    check_if_allowed(kz_json:decode(ShouldFailToCreate), 'false'),

    kzs_plan:allow_validation_overrides(),
    lager:info("allowing validation overrides"),

    ShouldSucceedToCreate = create(API, AccountId, StorageDoc, 'false'),
    lager:info("should succeed: ~s", [ShouldSucceedToCreate]),

    check_if_allowed(kz_json:decode(ShouldSucceedToCreate), 'true'),

    lager:info("created without validation successfully"),

    kzs_plan:disallow_validation_overrides(),
    lager:info("dis-allowing validation overrides"),

    {'error', ShouldAgainFailToCreate} = create(API, AccountId, StorageDoc, 'false'),
    lager:info("should fail again: ~s", [ShouldAgainFailToCreate]),
    check_if_allowed(kz_json:decode(ShouldAgainFailToCreate), 'false'),

    cleanup(API),
    lager:info("FINISHED SKIP TEST").

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

check_if_allowed(RespJObj, ShouldAllow) ->
    Errored = 'undefined' =:= kz_json:get_json_value([<<"data">>, <<"validate_settings">>], RespJObj),
    lager:info("request errored: ~p", [Errored]),
    ShouldAllow = Errored.

base_test() ->
    lager:info("BASE TEST"),
    API = init_api(),

    AccountId = create_account(API),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    CreatedStorage = create(API, AccountId, StorageDoc),
    lager:info("created storage: ~p", [CreatedStorage]),

    Test = pqc_httpd:get_req([<<?MODULE_STRING>>, AccountId]),
    lager:info("test created ~p", [Test]),

    _ = test_vm_message(API, AccountId),

    cleanup(API),
    lager:info("FINISHED").

init_api() ->
    Model = initial_state(),
    pqc_kazoo_model:api(Model).

global_test() ->
    lager:info("GLOBAL TEST"),

    API = init_api(),

    AccountId = create_account(API),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    CreatedStorage = create(API, 'undefined', StorageDoc),
    lager:info("created storage: ~p", [CreatedStorage]),

    Test = pqc_httpd:get_req([<<?MODULE_STRING>>, <<"system_data">>]),
    lager:info("test created ~p", [Test]),

    _ = test_vm_message(API, AccountId),
    cleanup(API),
    lager:info("FINISHED GLOBAL").

help_14308_test_vm(API, AccountId) ->
    CreateBox = pqc_cb_vmboxes:create_box(API, AccountId, <<"1010">>),
    lager:info("create VM box: ~p", [CreateBox]),
    BoxId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(CreateBox)),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),
    CreateVM = create_voicemail(API, AccountId, BoxId, MP3),
    lager:info("create VM: ~p", [CreateVM]),

    CreatedVM = kz_json:decode(CreateVM),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"media_id">>], CreatedVM),

    {'ok', GetVM} = pqc_httpd:wait_for_req([<<?MODULE_STRING>>]),
    lager:info("get VM: ~p", [GetVM]),
    {[RequestBody], [_AttachmentName]} = kz_json:get_values(GetVM),

    'true' = handle_multipart_store(MediaId, MP3, RequestBody),
    lager:info("got mp3 data on our web server!"),

    %% pqc_httpd:update_req([<<?MODULE_STRING>>, AccountId, MediaId, AttachmentName], MP3),
    %% lager:info("updating media to non-encoded MP3"),

    MetadataResp = pqc_cb_vmboxes:fetch_message_metadata(API, AccountId, BoxId, MediaId),
    lager:info("message ~s meta: ~s", [MediaId, MetadataResp]),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"media_id">>], kz_json:decode(MetadataResp)),

    MessageBin = pqc_cb_vmboxes:fetch_message_binary(API, AccountId, BoxId, MediaId),
    lager:info("message bin =:= MP3: ~p", [MessageBin =:= MP3]),
    MessageBin = MP3.

test_vm_message(API, AccountId) ->
    CreateBox = pqc_cb_vmboxes:create_box(API, AccountId, <<"1010">>),
    lager:info("create VM box: ~p", [CreateBox]),
    BoxId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(CreateBox)),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),
    CreateVM = create_voicemail(API, AccountId, BoxId, MP3),
    lager:info("create VM: ~p", [CreateVM]),

    CreatedVM = kz_json:decode(CreateVM),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"media_id">>], CreatedVM),

    {'ok', GetVM} = pqc_httpd:wait_for_req([<<?MODULE_STRING>>, AccountId, MediaId]),
    lager:info("get VM: ~p", [GetVM]),
    {[RequestBody], [_AttachmentName]} = kz_json:get_values(GetVM),

    'true' = handle_multipart_store(MediaId, MP3, RequestBody),
    lager:info("got mp3 data on our web server!"),

    %% pqc_httpd:update_req([<<?MODULE_STRING>>, AccountId, MediaId, AttachmentName], MP3),
    %% lager:info("updating media to non-encoded MP3"),

    MetadataResp = pqc_cb_vmboxes:fetch_message_metadata(API, AccountId, BoxId, MediaId),
    lager:info("message ~s meta: ~s", [MediaId, MetadataResp]),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"media_id">>], kz_json:decode(MetadataResp)),

    MessageBin = pqc_cb_vmboxes:fetch_message_binary(API, AccountId, BoxId, MediaId),
    lager:info("message bin =:= MP3: ~p", [MessageBin =:= MP3]),
    MessageBin = MP3,

    %% Tests that the media file can be retrieved from storage and proxied via KAZOO
    URL = kvm_message:media_url(AccountId, MediaId),
    lager:info("fetched URL: ~s", [URL]),
    {'ok', 200, _RespHeaders, FetchBin} = kz_http:get(URL),
    lager:info("resp headers: ~p", [_RespHeaders]),

    %% media_mgr adds ShoutCAST related data
    MP3Size = byte_size(MP3),
    <<FetchedMP3:MP3Size/binary, _/binary>> = FetchBin,
    lager:info("fetched bin =:= MP3: ~p", [FetchedMP3 =:= MP3]),
    FetchedMP3 = MP3.

create_voicemail(API, AccountId, BoxId, MP3) ->
    MessageJObj = default_message(),
    pqc_cb_vmboxes:new_message(API, AccountId, BoxId, MessageJObj, MP3).

default_message() ->
    kz_json:from_list([{<<"folder">>, <<"new">>}
                      ,{<<"caller_id_name">>, <<?MODULE_STRING>>}
                      ,{<<"caller_id_number">>, <<?MODULE_STRING>>}
                      ]).

handle_multipart_store(MediaId, MP3, RequestBody) ->
    handle_multipart_contents(MediaId, MP3, binary:split(RequestBody, <<"\r\n">>, ['global'])).

handle_multipart_contents(_MediaId, _MP3, []) -> 'true';
handle_multipart_contents(MediaId, MP3, [<<>> | Parts]) ->
    handle_multipart_contents(MediaId, MP3, Parts);
handle_multipart_contents(MediaId, MP3, [<<"content-type: application/json">>, <<>>, JSON | Parts]) ->
    lager:info("json body: ~s", [JSON]),
    JObj = kz_json:decode(JSON),
    MediaId = kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], JObj),
    lager:info("got expected media id ~s", [MediaId]),

    kz_json:all(fun({MessageKey, MessageValue}) ->
                        MessageValue =:= kz_json:get_value([<<"metadata">>, MessageKey], JObj)
                end
               ,default_message()
               ),

    handle_multipart_contents(MediaId, MP3, Parts);
handle_multipart_contents(MediaId, MP3, [<<"content-type: audio/mp3">>, <<>>, Base64MP3 | Parts]) ->
    case handle_mp3_contents(MP3, Base64MP3, ?BASE64_ENCODED) of
        'true' ->
            handle_multipart_contents(MediaId, MP3, Parts);
        'false' -> 'false'
    end;
handle_multipart_contents(MediaId, MP3, [<<"content-type: audio/mpeg">>, <<>>, Base64MP3 | Parts]) ->
    case handle_mp3_contents(MP3, Base64MP3, ?BASE64_ENCODED) of
        'true' ->
            handle_multipart_contents(MediaId, MP3, Parts);
        'false' -> 'false'
    end;
handle_multipart_contents(MediaId, MP3, [_Part | Parts]) ->
    ?DEBUG("skipping part ~s", [_Part]),
    handle_multipart_contents(MediaId, MP3, Parts).

handle_mp3_contents(MP3, Base64MP3, 'true') ->
    lager:info("checking base64-encoded data"),
    case base64:decode(Base64MP3) of
        MP3 ->
            lager:info("got expected mp3 data"),
            'true';
        _Data ->
            ?ERROR("failed to decode to mp3: ~w", [Base64MP3]),
            'false'
    end.

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() ->
    kzs_plan:disallow_validation_overrides(),
    kzs_plan:reset_system_dataplan(),
    pqc_httpd:stop().

-spec storage_doc(kz_term:ne_binary()) -> kzd_storage:doc().
storage_doc(UUID) ->
    storage_doc(UUID, 'undefined').

storage_doc(UUID, URL) ->
    kz_json:from_list([{<<"attachments">>, storage_attachments(UUID, URL)}
                      ,{<<"plan">>, storage_plan(UUID)}
                      ]).

storage_attachments(UUID, URL) ->
    kz_json:from_list([{UUID, http_handler(URL)}]).

http_handler(URL) ->
    kz_json:from_list([{<<"handler">>, <<"http">>}
                      ,{<<"name">>, <<?MODULE_STRING>>}
                      ,{<<"settings">>, http_handler_settings(URL)}
                      ]).

http_handler_settings('undefined') ->
    Base = pqc_httpd:base_url(),
    URL = <<Base/binary, ?MODULE_STRING>>,
    http_handler_settings(URL);
http_handler_settings(<<URL/binary>>) ->
    kz_json:from_list([{<<"url">>, URL}
                      ,{<<"verb">>, <<"post">>}
                      ,{<<"send_multipart">>, ?SEND_MULTIPART}
                      ,{<<"base64_encode_data">>, ?BASE64_ENCODED}
                      ,{<<"field_list">>, [kz_json:from_list([{<<"arg">>, <<"account_id">>}])
                                          ,kz_json:from_list([{<<"arg">>, <<"id">>}])
                                          ,kz_json:from_list([{<<"group">>
                                                              ,[kz_json:from_list([{<<"arg">>, <<"attachment">>}])
                                                               ,kz_json:from_list([{<<"const">>, <<"?from="?MODULE_STRING>>}])
                                                               ]
                                                              }
                                                             ])
                                          ]
                       }
                      ]).

storage_plan(UUID) ->
    storage_plan(UUID, 'undefined').

storage_plan(AttUUID, ConnUUID) ->
    kz_json:from_list([{<<"modb">>, modb_plan(AttUUID, ConnUUID)}]).

modb_plan(AttUUID, ConnUUID) ->
    kz_json:from_list([{<<"types">>, modb_types(AttUUID, ConnUUID)}]).

modb_types(AttUUID, ConnUUID) ->
    kz_json:from_list([{<<"mailbox_message">>, mailbox_handler(AttUUID, ConnUUID)}]).

mailbox_handler(AttUUID, ConnUUID) ->
    Handler = kz_json:from_list([{<<"handler">>, AttUUID}]),
    kz_json:from_list([{<<"attachments">>, Handler}
                      ,{<<"connection">>, ConnUUID}
                      ]).

storage_doc_missing_conn(AttUUID, ConnUUID) ->
    kz_json:from_list([{<<"attachments">>, storage_attachments(AttUUID, 'undefined')}
                      ,{<<"plan">>, storage_plan(AttUUID, ConnUUID)}
                      ]).

-spec missing_ref_test() -> 'ok'.
missing_ref_test() ->
    lager:info("GLOBAL TEST"),

    API = init_api(),

    AccountId = create_account(API),

    MissingConnDoc = storage_doc_missing_conn(kz_binary:rand_hex(16), kz_binary:rand_hex(16)),
    {'error', ErrMsg} = create(API, AccountId, MissingConnDoc),
    lager:info("failed to create storage: ~s", [ErrMsg]).
