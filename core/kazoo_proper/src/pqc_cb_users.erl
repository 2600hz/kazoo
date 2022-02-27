%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_users).

-export([create/3
        ,fetch/3
        ,delete/3
        ]).

-export([user_doc/0]).

-export([seq/0
        ,cleanup/0
        ]).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_users:doc()) -> pqc_cb_api:response().
create(API, AccountId, User) ->
    URL = users_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope = pqc_cb_api:create_envelope(User),

    pqc_cb_api:make_request([#{'response_codes' => [201]}]
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, UserId) ->
    URL = user_url(AccountId, UserId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([#{'response_codes' => [200]}]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, <<UserId/binary>>) ->
    URL = user_url(AccountId, UserId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([#{'response_codes' => [200]}]
                           ,fun kz_http:delete/2
                           ,URL
                           ,RequestHeaders
                           ).

users_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "users"], "/").

user_url(AccountId, UserId) ->
    string:join([users_url(AccountId), kz_term:to_list(UserId)], "/").

-spec user_doc() -> kzd_users:doc().
user_doc() ->
    kz_json:exec_first(
      [{fun kzd_users:set_first_name/2, kz_binary:rand_hex(5)}
      ,{fun kzd_users:set_last_name/2, kz_binary:rand_hex(5)}
      ]
     ,kzd_users:new()
     ).

-spec seq() -> 'ok'.
seq() ->
    _ = init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, <<?MODULE_STRING>>),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
    lager:info("created account ~s", [AccountId]),

    Id = kz_datamgr:get_uuid(),
    NewUser = kz_json:set_value(<<"id">>, Id, user_doc()),
    lager:info("trying to create new entity with id ~s", [Id]),
    <<CreateResp/binary>> = create(API, AccountId, NewUser),
    lager:info("created: ~s", [CreateResp]),

    Id = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(CreateResp)),

    <<FetchResp/binary>> = fetch(API, AccountId, Id),
    lager:info("fetched by Request's id: ~s", [FetchResp]),
    Id = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(FetchResp)),

    lager:info("COMPLETED SUCCESSFULLY!"),
    _ = cleanup(API).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_users', 'cb_accounts']
        ],
    lager:info("INIT FINISHED").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    lager:info("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec cleanup() -> any().
cleanup() ->
    lager:info("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, [<<?MODULE_STRING>>]),
    pqc_cb_api:cleanup(API).
