%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_users).

-export([create/3]).

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

users_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "users"], "/").

-spec user_doc() -> kzd_users:doc().
user_doc() ->
    kz_json:exec_first(
      [{fun kzd_users:set_first_name/2, kz_binary:rand_hex(5)}
      ,{fun kzd_users:set_last_name/2, kz_binary:rand_hex(5)}
      ]
     ,kzd_users:new()
     ).
