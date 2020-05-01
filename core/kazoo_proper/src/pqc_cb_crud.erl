%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_crud).

%% API requests
-export([summary/2, summary/3, summary/4
        ,create/3, create/4, create/5
        ,fetch/2, fetch/3, fetch/4
        ,update/3, update/4, update/5
        ,patch/3, patch/4, patch/5
        ,delete/2, delete/3, delete/4
        ]).

-export([collection_url/2, entity_url/3]).

-include("kazoo_proper.hrl").

-spec summary(pqc_cb_api:state(), string()) -> pqc_cb_api:response().
summary(API, URL) ->
    Expectations = [#expectation{response_codes = [200]}],
    summary(API, URL, Expectations).

-spec summary(pqc_cb_api:state(), string(), expectations()) -> pqc_cb_api:response().
summary(API, URL, Expectations) ->
    summary(API, URL, Expectations, pqc_cb_api:request_headers(API)).

-spec summary(pqc_cb_api:state(), string(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
summary(_API, URL, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec create(pqc_cb_api:state(), string(), kz_json:object()) -> pqc_cb_api:response().
create(API, URL, RequestEnvelope) ->
    Expectations = [#expectation{response_codes = [201]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    create(API, URL, RequestEnvelope, Expectations).

-spec create(pqc_cb_api:state(), string(), kz_json:object(), expectations()) -> pqc_cb_api:response().
create(API, URL, RequestEnvelope, Expectations) ->
    create(API, URL, RequestEnvelope, Expectations, pqc_cb_api:request_headers(API)).

-spec create(pqc_cb_api:state(), string(), kz_json:object(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
create(_API, URL, RequestEnvelope, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec fetch(pqc_cb_api:state(), string()) -> pqc_cb_api:response().
fetch(API, URL) ->
    Expectations = [#expectation{response_codes = [200]}],
    fetch(API, URL, Expectations).

-spec fetch(pqc_cb_api:state(), string(), expectations()) -> pqc_cb_api:response().
fetch(API, URL, Expectations) ->
    fetch(API, URL, Expectations, pqc_cb_api:request_headers(API)).

-spec fetch(pqc_cb_api:state(), string(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
fetch(_API, URL, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec update(pqc_cb_api:state(), string(), kz_json:object()) -> pqc_cb_api:response().
update(API, URL, RequestEnvelope) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    update(API, URL, RequestEnvelope, Expectations).

-spec update(pqc_cb_api:state(), string(), kz_json:object(), expectations()) -> pqc_cb_api:response().
update(API, URL, RequestEnvelope, Expectations) ->
    update(API, URL, RequestEnvelope, Expectations, pqc_cb_api:request_headers(API)).

-spec update(pqc_cb_api:state(), string(), kz_json:object(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
update(_API, URL, RequestEnvelope, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec patch(pqc_cb_api:state(), string(), kz_json:object()) -> pqc_cb_api:response().
patch(API, URL, RequestEnvelope) ->
    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],
    patch(API, URL, RequestEnvelope, Expectations).

-spec patch(pqc_cb_api:state(), string(), kz_json:object(), expectations()) -> pqc_cb_api:response().
patch(API, URL, RequestEnvelope, Expectations) ->
    patch(API, URL, RequestEnvelope, Expectations, pqc_cb_api:request_headers(API)).

-spec patch(pqc_cb_api:state(), string(), kz_json:object(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
patch(_API, URL, RequestEnvelope, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec delete(pqc_cb_api:state(), string()) -> pqc_cb_api:response().
delete(API, URL) ->
    delete(API, URL, [#expectation{response_codes = [200]}]).

-spec delete(pqc_cb_api:state(), string(), expectations()) -> pqc_cb_api:response().
delete(API, URL, Expectations) ->
    delete(API, URL, Expectations, pqc_cb_api:request_headers(API)).

-spec delete(pqc_cb_api:state(), string(), expectations(), kz_http:headers()) -> pqc_cb_api:response().
delete(_API, URL, Expectations, RequestHeaders) ->
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,RequestHeaders
                           ).


-spec collection_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
collection_url(AccountId, Collection) ->
    string:join([pqc_cb_accounts:account_url(AccountId), kz_term:to_list(Collection)], "/").

-spec entity_url(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> string().
entity_url(AccountId, Collection, EntityId) ->
    string:join([collection_url(AccountId, Collection), kz_term:to_list(EntityId)], "/").
