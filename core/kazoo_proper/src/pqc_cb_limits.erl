%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_limits).

-export([fetch/2
        ,update/3
        ]).

-include("kazoo_proper.hrl").

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, <<AccountId/binary>>) ->
    URL = limits_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
update(API, <<AccountId/binary>>, JObj) ->
    URL = limits_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope = pqc_cb_api:create_envelope(JObj),

    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

limits_url(<<AccountId/binary>>) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "limits"], "/").
