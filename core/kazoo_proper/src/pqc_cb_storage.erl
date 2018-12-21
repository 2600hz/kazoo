%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_storage).
%% -behaviour(proper_statem).

%% Manual testing
-export([seq/0]).

%% API Shims
%% -export([create/2
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

create(API, ?NE_BINARY=AccountId, ?NE_BINARY=UUID) ->
    create(API, AccountId, storage_doc(UUID));
create(API, ?NE_BINARY=AccountId, StorageDoc) ->
    StorageURL = storage_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),
    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,StorageURL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(StorageDoc))
                           ).

storage_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "storage"], "/").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    init_system(),
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
            Mod <- ['cb_storage']
        ],
    ?INFO("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    StorageDoc = storage_doc(kz_binary:rand_hex(16)),
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    Created = create(API, AccountId, StorageDoc),
    ?INFO("created storage: ~p", [Created]),

    cleanup(API),
    ?INFO("FINISHED").

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    pqc_cb_api:cleanup(API).

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
    kz_json:from_list([{<<"url">>, <<"https://localhost/kazoo">>}
                      ,{<<"verb">>, <<"POST">>}
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
