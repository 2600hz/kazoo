%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2021, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_users).

-export([create/3
        ,delete/3
        ]).

-export([user_doc/0]).

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
