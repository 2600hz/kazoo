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
-module(pqc_cb_menus).

%% API requests
-export([summary/2
        ,create/3
        ,fetch/3
        ,update/3
        ,patch/4
        ,delete/3
        ]).

-export([seq/0
        ,cleanup/0
        ,new_menu/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,menus_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_menus:doc()) -> pqc_cb_api:response().
create(API, AccountId, MenuJObj) ->
    URL = menus_url(AccountId),

    Expectations = [#expectation{response_codes = [201]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(MenuJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, MenuId) ->
    Expectations = [#expectation{response_codes = [200]}],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,menu_url(AccountId, MenuId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_menu:doc()) -> pqc_cb_api:response().
update(API, AccountId, MenuJObj) ->
    URL = menu_url(AccountId, kz_doc:id(MenuJObj)),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(MenuJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, MenuId, PatchJObj) ->
    URL = menu_url(AccountId, MenuId),

    Expectations = [#expectation{response_codes = [200]}],

    Envelope = pqc_cb_api:create_envelope(PatchJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, MenuId) ->
    URL = menu_url(AccountId, MenuId),
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec menus_url(kz_term:ne_binary()) -> string().
menus_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "menus"], "/").

-spec menu_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
menu_url(AccountId, MenuId) ->
    string:join([menus_url(AccountId), kz_term:to_list(MenuId)], "/").

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_menus']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    ?INFO("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    MenuJObj = new_menu(),
    CreateResp = create(API, AccountId, MenuJObj),
    ?INFO("created menu ~s", [CreateResp]),
    CreatedMenu = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    MenuId = kz_doc:id(CreatedMenu),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, MenuId, Patch),
    ?INFO("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    ?INFO("summary resp: ~s", [SummaryResp]),
    [SummaryMenu] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    MenuId = kz_doc:id(SummaryMenu),

    DeleteResp = delete(API, AccountId, MenuId),
    ?INFO("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    ?INFO("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED MENU SEQ").

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

-spec create_account(pqc_cb_api:state()) -> kz_term:ne_binary().
create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

-spec new_menu() -> kzd_menus:doc().
new_menu() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_menus:set_name/2, kz_binary:rand_hex(4)}]
                        ,kzd_menus:new()
                        )
     ).
