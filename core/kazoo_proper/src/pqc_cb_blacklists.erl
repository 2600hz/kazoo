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
-module(pqc_cb_blacklists).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_blacklist/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, blacklists_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_blacklists:doc()) -> pqc_cb_api:response().
create(API, AccountId, BlacklistJObj) ->
    Envelope = pqc_cb_api:create_envelope(BlacklistJObj),
    pqc_cb_crud:create(API, blacklists_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, BlacklistId) ->
    pqc_cb_crud:fetch(API, blacklist_url(AccountId, BlacklistId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_blacklist:doc()) -> pqc_cb_api:response().
update(API, AccountId, BlacklistJObj) ->
    Envelope = pqc_cb_api:create_envelope(BlacklistJObj),
    pqc_cb_crud:update(API, blacklist_url(AccountId, kz_doc:id(BlacklistJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, BlacklistId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, blacklist_url(AccountId, BlacklistId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, BlacklistId) ->
    pqc_cb_crud:delete(API, blacklist_url(AccountId, BlacklistId)).


-spec blacklists_url(kz_term:ne_binary()) -> string().
blacklists_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"blacklists">>).

-spec blacklist_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
blacklist_url(AccountId, BlacklistId) ->
    pqc_cb_crud:entity_url(AccountId, <<"blacklists">>, BlacklistId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_blacklists']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    BlacklistJObj = new_blacklist(),
    CreateResp = create(API, AccountId, BlacklistJObj),
    lager:info("created blacklist ~s", [CreateResp]),
    CreatedBlacklist = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    BlacklistId = kz_doc:id(CreatedBlacklist),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, BlacklistId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryBlacklist] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    BlacklistId = kz_doc:id(SummaryBlacklist),

    DeleteResp = delete(API, AccountId, BlacklistId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED BLACKLIST SEQ").

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

-spec new_blacklist() -> kzd_blacklists:doc().
new_blacklist() ->
    kz_doc:public_fields(
      kzd_blacklists:set_name(kzd_blacklists:new()
                             ,kz_binary:rand_hex(4)
                             )
     ).
