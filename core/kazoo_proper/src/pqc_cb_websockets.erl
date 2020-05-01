%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2020-, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_websockets).

%% API functions
-export([available/1

         %% Account operations
        ,summary/2
        ,details/3
        ]).

-include("kazoo_proper.hrl").

-spec available(pqc_cb_api:state()) -> pqc_cb_api:response().
available(API) ->
    URL = base_websockets_url(),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    WebsocketsURL = websockets_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,WebsocketsURL
                           ,RequestHeaders
                           ).

-spec details(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
details(API, AccountId, WebsocketId) ->
    URL = websocket_url(AccountId, WebsocketId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec base_websockets_url() -> string().
base_websockets_url() ->
    string:join([pqc_cb_api:v2_base_url(), "websockets"], "/").

-spec websockets_url(kz_term:ne_binary()) -> string().
websockets_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "websockets"], "/").

-spec websocket_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
websocket_url(AccountId, WebsocketId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "websockets", kz_term:to_list(WebsocketId)], "/").
