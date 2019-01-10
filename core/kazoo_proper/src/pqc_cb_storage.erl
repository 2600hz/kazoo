%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_storage).

%% Manual testing
-export([seq/0
        ,cleanup/0
        ]).

%% API Shims
-export([create/3, create/4]).
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

-define(ACCOUNT_NAMES, [<<"account_for_storage">>]).
-define(UUID, <<"2426cb457dc530acc881977ccbc9a7a7">>).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
                    pqc_cb_api:response().
create(API, ?NE_BINARY=AccountId, StorageDoc) ->
    create(API, AccountId, StorageDoc, 'undefined').

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), kz_term:api_boolean()) ->
                    pqc_cb_api:response().
create(API, ?NE_BINARY=AccountId, ?NE_BINARY=UUID, ValidateSettings) ->
    create(API, AccountId, storage_doc(UUID), ValidateSettings);
create(API, ?NE_BINARY=AccountId, StorageDoc, ValidateSettings) ->
    StorageURL = storage_url(AccountId, ValidateSettings),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),
    pqc_cb_api:make_request([201, 400]
                           ,fun kz_http:put/3
                           ,StorageURL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(StorageDoc))
                           ).

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
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_storage', 'cb_vmboxes']
        ],

    _HTTPD = pqc_httpd:start_link(TestId),
    ?INFO("HTTPD started: ~p", [_HTTPD]),

    ?INFO("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    base_test(),
    cleanup(),
    skip_validation_test(),
    cleanup().

skip_validation_test() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    ShouldFailToCreate = create(API, AccountId, StorageDoc, 'false'),
    ?INFO("should fail: ~s", [ShouldFailToCreate]),

    check_if_allowed(kz_json:decode(ShouldFailToCreate), 'false'),

    kzs_plan:allow_validation_overrides(),
    ?INFO("allowing validation overrides"),

    ShouldSucceedToCreate = create(API, AccountId, StorageDoc, 'false'),
    ?INFO("should succeed: ~s", [ShouldSucceedToCreate]),

    check_if_allowed(kz_json:decode(ShouldSucceedToCreate), 'true'),

    ?INFO("created without validation successfully"),

    kzs_plan:disallow_validation_overrides(),
    ?INFO("dis-allowing validation overrides"),

    ShouldAgainFailToCreate = create(API, AccountId, StorageDoc, 'false'),
    ?INFO("should fail again: ~s", [ShouldAgainFailToCreate]),
    check_if_allowed(kz_json:decode(ShouldAgainFailToCreate), 'false'),

    cleanup(API),
    ?INFO("FINISHED NON-VALIDATION").

check_if_allowed(RespJObj, ShouldAllow) ->
    Errored = 'undefined' =:= kz_json:get_json_value([<<"data">>, <<"validate_settings">>], RespJObj),
    ?INFO("request errored: ~p", [Errored]),
    ShouldAllow = Errored.

base_test() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    CreatedStorage = create(API, AccountId, StorageDoc),
    ?INFO("created storage: ~p", [CreatedStorage]),

    Test = pqc_httpd:get_req([<<?MODULE_STRING>>, AccountId]),
    ?INFO("test created ~p", [Test]),

    CreateBox = pqc_cb_vmboxes:create_box(API, AccountId, <<"1010">>),
    ?INFO("create VM box: ~p", [CreateBox]),
    BoxId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(CreateBox)),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),
    CreateVM = create_voicemail(API, AccountId, BoxId, MP3),
    ?INFO("create VM: ~p", [CreateVM]),

    CreatedVM = kz_json:decode(CreateVM),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"media_id">>], CreatedVM),

    GetVM = pqc_httpd:wait_for_req([<<?MODULE_STRING>>, AccountId, MediaId]),
    ?INFO("get VM: ~p", [GetVM]),
    {[RequestBody], [_AttachmentName]} = kz_json:get_values(GetVM),

    handle_multipart_store(MediaId, MP3, RequestBody),
    ?INFO("got mp3 data on our web server!"),

    cleanup(API),
    ?INFO("FINISHED").

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

handle_multipart_contents(_MediaId, _MP3, []) -> 'ok';
handle_multipart_contents(MediaId, MP3, [<<>> | Parts]) ->
    handle_multipart_contents(MediaId, MP3, Parts);
handle_multipart_contents(MediaId, MP3, [<<"content-type: application/json">>, <<>>, JSON | Parts]) ->
    ?INFO("json body: ~s", [JSON]),
    JObj = kz_json:decode(JSON),
    MediaId = kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], JObj),
    ?INFO("got expected media id ~s", [MediaId]),

    kz_json:all(fun({MessageKey, MessageValue}) ->
                        MessageValue =:= kz_json:get_value([<<"metadata">>, MessageKey], JObj)
                end
               ,default_message()
               ),

    handle_multipart_contents(MediaId, MP3, Parts);
handle_multipart_contents(MediaId, MP3, [<<"content-type: audio/mp3">>, <<>>, MP3 | Parts]) ->
    ?INFO("got expected mp3 data"),
    handle_multipart_contents(MediaId, MP3, Parts);
handle_multipart_contents(MediaId, MP3, [_Part | Parts]) ->
    ?DEBUG("skipping part ~s", [_Part]),
    handle_multipart_contents(MediaId, MP3, Parts).

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() ->
    kzs_plan:disallow_validation_overrides(),
    pqc_httpd:stop().

storage_doc(UUID) ->
    kz_json:from_list([{<<"attachments">>, storage_attachments(UUID)}
                      ,{<<"plan">>, storage_plan(UUID)}
                      ]).

storage_attachments(UUID) ->
    kz_json:from_list([{UUID, http_handler()}]).

http_handler() ->
    kz_json:from_list([{<<"handler">>, <<"http">>}
                      ,{<<"name">>, <<?MODULE_STRING>>}
                      ,{<<"settings">>, http_handler_settings()}
                      ]).

http_handler_settings() ->
    Base = pqc_httpd:base_url(),
    URL = <<Base/binary, ?MODULE_STRING>>,

    kz_json:from_list([{<<"url">>, URL}
                      ,{<<"verb">>, <<"POST">>}
                      ,{<<"send_multipart">>, 'true'}
                      ]).

storage_plan(UUID) ->
    kz_json:from_list([{<<"modb">>, modb_plan(UUID)}]).

modb_plan(UUID) ->
    kz_json:from_list([{<<"types">>, modb_types(UUID)}]).

modb_types(UUID) ->
    kz_json:from_list([{<<"mailbox_message">>, mailbox_handler(UUID)}]).

mailbox_handler(UUID) ->
    Handler = kz_json:from_list([{<<"handler">>, UUID}]),
    kz_json:from_list([{<<"attachments">>, Handler}]).
