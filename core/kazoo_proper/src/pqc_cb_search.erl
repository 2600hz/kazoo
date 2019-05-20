%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_search).

-export([search_account_by_name/2]).

-include("kazoo_proper.hrl").

-spec search_account_by_name(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
search_account_by_name(API, Name) ->
    URL = search_url(API),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Querystring = kz_http_util:props_to_querystring(
                    [{<<"t">>, <<"account">>}
                    ,{<<"q">>, <<"name">>}
                    ,{<<"v">>, Name}
                    ]
                   ),
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL ++ [$? | Querystring]
                           ,RequestHeaders
                           ).

-spec search_url(pqc_cb_api:state()) -> string().
search_url(API) ->
    string:join([pqc_cb_accounts:account_url(API), "search"], "/").
