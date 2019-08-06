%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_webhooks).

%% API functions
-export([list_available/1
        ,samples/1, sample/2

         %% Account operations
        ,summary/2
        ]).

%% Manual test functions
-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_webhooks">>]).

-spec list_available(pqc_cb_api:state()) -> pqc_cb_api:response().
list_available(API) ->
    URL = base_webhooks_url(),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    WebhooksURL = webhooks_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,WebhooksURL
                           ,RequestHeaders
                           ).

-spec samples(pqc_cb_api:state()) -> pqc_cb_api:response().
samples(API) ->
    URL = samples_url(),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec sample(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
sample(API, SampleId) ->
    URL = sample_url(SampleId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec base_webhooks_url() -> string().
base_webhooks_url() ->
    string:join([pqc_cb_api:v2_base_url(), "webhooks"], "/").

-spec webhooks_url(kz_term:ne_binary()) -> string().
webhooks_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "webhooks"], "/").

%% -spec webhook_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
%% webhook_url(AccountId, WebhookId) ->
%%     string:join([pqc_cb_accounts:account_url(AccountId), "webhooks", kz_term:to_list(WebhookId)], "/").

-spec samples_url() -> string().
samples_url() ->
    string:join([base_webhooks_url(), "samples"], "/").

-spec sample_url(kz_term:ne_binary()) -> string().
sample_url(SampleId) ->
    string:join([base_webhooks_url(), "samples", kz_term:to_list(SampleId)], "/").

-spec seq() -> 'ok'.
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AvailableResp = list_available(API),
    lager:info("available: ~s", [AvailableResp]),
    Available = kz_json:get_list_value(<<"data">>, kz_json:decode(AvailableResp)),
    'true' = ([] =/= Available),

    SamplesResp = samples(API),
    lager:info("samples: ~s", [SamplesResp]),
    Samples = [_|_] = kz_json:get_list_value(<<"data">>, kz_json:decode(SamplesResp)),

    lists:all(fun(SampleId) -> can_fetch_sample(API, SampleId) end, Samples),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    SummaryResp = summary(API, AccountId),
    lager:info("summary: ~s", [SummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),

    cleanup(API),
    lager:info("FINISHED").

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
            App <- ['crossbar', 'webhooks']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_webhooks']
        ],
    lager:info("INIT FINISHED").

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

can_fetch_sample(API, SampleId) ->
    SampleResp = sample(API, SampleId),
    lager:info("sample for ~s: ~s", [SampleId, SampleResp]),
    [] =/= kz_json:get_list_value(<<"data">>, kz_json:decode(SampleResp)).
