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
-module(pqc_cb_faxboxes).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_faxbox/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, faxboxes_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_faxboxes:doc()) -> pqc_cb_api:response().
create(API, AccountId, FaxboxJObj) ->
    Envelope = pqc_cb_api:create_envelope(FaxboxJObj),
    pqc_cb_crud:create(API, faxboxes_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, FaxboxId) ->
    pqc_cb_crud:fetch(API, faxbox_url(AccountId, FaxboxId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_faxboxes:doc()) -> pqc_cb_api:response().
update(API, AccountId, FaxboxJObj) ->
    Envelope = pqc_cb_api:create_envelope(FaxboxJObj),
    pqc_cb_crud:update(API, faxbox_url(AccountId, kz_doc:id(FaxboxJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, FaxboxId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, faxbox_url(AccountId, FaxboxId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, FaxboxId) ->
    pqc_cb_crud:delete(API, faxbox_url(AccountId, FaxboxId)).


-spec faxboxes_url(kz_term:ne_binary()) -> string().
faxboxes_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"faxboxes">>).

-spec faxbox_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
faxbox_url(AccountId, FaxboxId) ->
    pqc_cb_crud:entity_url(AccountId, <<"faxboxes">>, FaxboxId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_faxboxes']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    FaxboxJObj = new_faxbox(),
    CreateResp = create(API, AccountId, FaxboxJObj),
    lager:info("created faxbox ~s", [CreateResp]),
    CreatedFaxbox = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    FaxboxId = kz_doc:id(CreatedFaxbox),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, FaxboxId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryFaxbox] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    FaxboxId = kz_doc:id(SummaryFaxbox),

    DeleteResp = delete(API, AccountId, FaxboxId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED FAXBOXES SEQ").

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

-spec new_faxbox() -> kzd_faxboxes:doc().
new_faxbox() ->
    kz_doc:public_fields(
      kzd_faxbox:set_name(kzd_faxbox:new()
                         ,kz_binary:rand_hex(4)
                         )
     ).
