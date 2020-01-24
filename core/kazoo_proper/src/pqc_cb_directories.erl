%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_directories).

-export([create/3
        ,fetch/3, fetch/4
        ]).

-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_directories:doc()) -> pqc_cb_api:response().
create(API, AccountId, Directory) ->
    URL = directories_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope = pqc_cb_api:create_envelope(Directory),

    pqc_cb_api:make_request([#{'response_codes' => [201]}]
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, DirectoryId) ->
    fetch(API, AccountId, DirectoryId, 'undefined').

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_proplist()) -> pqc_cb_api:response().
fetch(API, AccountId, DirectoryId, QueryString) ->
    URL = directory_url(AccountId, DirectoryId) ++ querystring(QueryString),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([#{'response_codes' => [200]}]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

directories_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "directories"], "/").

directory_url(AccountId, DirectoryId) ->
    string:join([directories_url(AccountId), kz_term:to_list(DirectoryId)], "/").

querystring('undefined') -> "";
querystring([]) -> "";
querystring(QS) -> ["?", kz_http_util:props_to_querystring(QS)].

-spec seq() -> 'ok'.
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    CreateResp = create(API, AccountId, directories_doc()),
    lager:info("created directory: ~s", [CreateResp]),
    Directory = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    DirectoryId = kz_doc:id(Directory),

    Users = create_users(API, AccountId, DirectoryId),
    UserIds = [kz_doc:id(User) || User <- Users],

    FetchResp = fetch(API, AccountId, DirectoryId, [{<<"paginate">>, 'false'}]),
    lager:info("fetched directory: ~s", [FetchResp]),
    FetchedUsers = kzd_directories:users(kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp))),
    lager:info("fetched users: ~p", [FetchedUsers]),
    lager:info("user ids: ~p", [UserIds]),

    'true' = length(UserIds) =:= length(FetchedUsers),
    'true' = lists:all(fun(FetchedUser) ->
                               lists:member(kz_json:get_ne_binary_value(<<"user_id">>, FetchedUser)
                                           ,UserIds
                                           )
                       end
                      ,FetchedUsers
                      ),

    cleanup(API),
    lager:info("FINISHED SEQ").

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
            Mod <- ['cb_directories', 'cb_users_v2']
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

directories_doc() ->
    kz_json:exec_first(
      [{fun kzd_directories:set_name/2, <<?MODULE_STRING>>}]
     ,kzd_directories:new()
     ).

-spec create_users(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> [kzd_users:doc()].
create_users(API, AccountId, DirectoryId) ->
    [create_user(API, AccountId, DirectoryId, N) || N <- lists:seq(1, 100)].

-spec create_user(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), 1..100) -> kzd_users:doc().
create_user(API, AccountId, DirectoryId, NthUser) ->
    Directories = kz_json:from_list([{DirectoryId, kz_binary:rand_hex(16)}]),
    UserDoc = kzd_users:set_directories(pqc_cb_users:user_doc(), Directories),

    Results = pqc_cb_users:create(API, AccountId, kz_json:set_value(<<"nth">>, NthUser, UserDoc)),
    kz_json:get_json_value(<<"data">>, kz_json:decode(Results)).
