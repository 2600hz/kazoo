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
-module(pqc_cb_clicktocall).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_clicktocall/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, clicktocall_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_clicktocall:doc()) -> pqc_cb_api:response().
create(API, AccountId, ClicktocallJObj) ->
    Envelope = pqc_cb_api:create_envelope(ClicktocallJObj),
    pqc_cb_crud:create(API, clicktocall_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, ClicktocallId) ->
    pqc_cb_crud:fetch(API, clicktocall_url(AccountId, ClicktocallId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_clicktocall:doc()) -> pqc_cb_api:response().
update(API, AccountId, ClicktocallJObj) ->
    Envelope = pqc_cb_api:create_envelope(ClicktocallJObj),
    pqc_cb_crud:update(API, clicktocall_url(AccountId, kz_doc:id(ClicktocallJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, ClicktocallId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, clicktocall_url(AccountId, ClicktocallId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ClicktocallId) ->
    pqc_cb_crud:delete(API, clicktocall_url(AccountId, ClicktocallId)).


-spec clicktocall_url(kz_term:ne_binary()) -> string().
clicktocall_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"clicktocall">>).

-spec clicktocall_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
clicktocall_url(AccountId, ClicktocallId) ->
    pqc_cb_crud:entity_url(AccountId, <<"clicktocall">>, ClicktocallId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_clicktocall']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    ClicktocallJObj = new_clicktocall(),
    CreateResp = create(API, AccountId, ClicktocallJObj),
    lager:info("created clicktocall ~s", [CreateResp]),
    CreatedClicktocall = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    ClicktocallId = kz_doc:id(CreatedClicktocall),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, ClicktocallId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryClicktocall] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    ClicktocallId = kz_doc:id(SummaryClicktocall),

    DeleteResp = delete(API, AccountId, ClicktocallId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED CLICKTOCALL SEQ").

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

-spec new_clicktocall() -> kzd_clicktocall:doc().
new_clicktocall() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_clicktocall:set_name/2, kz_binary:rand_hex(4)}
                         ,{fun kzd_clicktocall:set_extension/2, <<"2600">>}
                         ]
                        ,kzd_clicktocall:new()
                        )
     ).
