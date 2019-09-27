%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_lists_v2).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_list/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, lists_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_lists:doc()) -> pqc_cb_api:response().
create(API, AccountId, ListJObj) ->
    Envelope = pqc_cb_api:create_envelope(ListJObj),
    pqc_cb_crud:create(API, lists_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, ListId) ->
    pqc_cb_crud:fetch(API, list_url(AccountId, ListId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_list:doc()) -> pqc_cb_api:response().
update(API, AccountId, ListJObj) ->
    Envelope = pqc_cb_api:create_envelope(ListJObj),
    pqc_cb_crud:update(API, list_url(AccountId, kz_doc:id(ListJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, ListId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, list_url(AccountId, ListId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ListId) ->
    pqc_cb_crud:delete(API, list_url(AccountId, ListId)).


-spec lists_url(kz_term:ne_binary()) -> string().
lists_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"lists">>).

-spec list_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
list_url(AccountId, ListId) ->
    pqc_cb_crud:entity_url(AccountId, <<"lists">>, ListId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_lists_v2']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    ListJObj = new_list(),
    CreateResp = create(API, AccountId, ListJObj),
    lager:info("created list ~s", [CreateResp]),
    CreatedList = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    ListId = kz_doc:id(CreatedList),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, ListId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryList] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    ListId = kz_doc:id(SummaryList),

    DeleteResp = delete(API, AccountId, ListId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED LIST SEQ").

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

-spec new_list() -> kzd_lists:doc().
new_list() ->
    kz_doc:public_fields(
      kzd_lists:set_name(kzd_lists:new()
                        ,kz_binary:rand_hex(4)
                        )
     ).
