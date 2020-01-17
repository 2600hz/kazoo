%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_connectivity).

-export([create/3
        ,update/3
        ,summary/2
        ,delete/3
        ]).

%% Manual testing
-export([seq/0
        ,cleanup/0
        ]).

%% API Shims

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_connectivity:doc()) -> pqc_cb_api:response().
create(API, AccountId, ConnectivityDoc) ->
    URL = connectivity_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(ConnectivityDoc),

    pqc_cb_api:make_request(#{'response_codes' => [201]}
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_connectivity:doc()) -> pqc_cb_api:response().
update(API, AccountId, ConnectivityDoc) ->
    URL = connectivity_url(AccountId, kz_doc:id(ConnectivityDoc)),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(ConnectivityDoc),

    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:post/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,connectivity_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ConnectivityId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:delete/2
                           ,connectivity_url(AccountId, ConnectivityId)
                           ,pqc_cb_api:request_headers(API)
                           ).

connectivity_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "connectivity"], "/").

connectivity_url(AccountId, ConnectivityId) ->
    string:join([connectivity_url(AccountId), kz_term:to_list(ConnectivityId)], "/").

-spec seq() -> 'ok'.
seq() ->
    API = init_api(),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    AccountRealm = kzd_accounts:realm(AccountDoc),
    lager:info("created account ~s ~s", [AccountId, AccountRealm]),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    Doc = connectivity_doc(),
    CreatedResp = create(API, AccountId, Doc),
    lager:info("created resp: ~s", [CreatedResp]),
    CreatedDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(CreatedResp)),
    AccountRealm = kzd_connectivity:account_auth_realm(CreatedDoc),

    SummaryResp = summary(API, AccountId),
    lager:info("summary: ~s", [SummaryResp]),
    [SummaryId] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    SummaryId = kz_doc:id(CreatedDoc),

    Edited = kzd_connectivity:set_account_auth_realm(CreatedDoc, kz_binary:rand_hex(4)),
    EditedResp = update(API, AccountId, Edited),
    lager:info("edited resp: ~s", [EditedResp]),
    EditedDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(EditedResp)),
    AccountRealm = kzd_connectivity:account_auth_realm(EditedDoc),
    SummaryId = kz_doc:id(EditedDoc),

    DeleteResp = delete(API, AccountId, SummaryId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty again: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    lager:info("COMPLETED SUCCESSFULLY"),
    cleanup(API).

-spec cleanup() -> any().
cleanup() ->
    lager:info("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    pqc_cb_api:cleanup(API).

connectivity_doc() ->
    kz_doc:setters(kzd_connectivity:new()
                  ,[{fun kzd_connectivity:set_account_auth_realm/2, kz_binary:rand_hex(4)}
                   ,{fun kzd_connectivity:set_name/2, <<?MODULE_STRING>>}
                   ,{fun kzd_connectivity:set_servers/2, []}
                   ]).

init_api() ->
    Model = initial_state(),
    pqc_kazoo_model:api(Model).

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
            App <- ['crossbar', 'trunkstore']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_connectivity']
        ],

    ?INFO("INIT FINISHED").
