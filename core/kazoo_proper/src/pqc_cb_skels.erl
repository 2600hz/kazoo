%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_skels).

%% API Shims
-export([summary/2
        ,create/3
        ,fetch/3
        ,update/3
        ,delete/3
        ,patch/4
        ]).

%% Manual testing
-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, skels_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_skels:doc()) -> pqc_cb_api:response().
create(API, AccountId, SkelJObj) ->
    Envelope = pqc_cb_api:create_envelope(SkelJObj),
    pqc_cb_crud:create(API, skels_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, SkelId) ->
    pqc_cb_crud:fetch(API, skel_url(AccountId, SkelId)).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, SkelId) ->
    pqc_cb_crud:delete(API, skel_url(AccountId, SkelId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_skels:doc()) -> pqc_cb_api:response().
update(API, AccountId, SkelJObj) ->
    SkelId = kz_doc:id(SkelJObj),
    Envelope = pqc_cb_api:create_envelope(SkelJObj),
    pqc_cb_crud:update(API, skel_url(AccountId, SkelId), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, SkelId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, skel_url(AccountId, SkelId), Envelope).

skels_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"skels">>).

skel_url(AccountId, SkelId) ->
    pqc_cb_crud:entity_url(AccountId, <<"skels">>, SkelId).

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_skels']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    SkelJObj = new_skel(),
    CreateResp = create(API, AccountId, SkelJObj),
    lager:info("created skel ~s", [CreateResp]),
    CreatedSkel = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    SkelId = kz_doc:id(CreatedSkel),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, SkelId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SkelId] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),

    DeleteResp = delete(API, AccountId, SkelId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED SKEL SEQ").

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

-spec new_skel() -> kzd_skels:doc().
new_skel() ->
    kz_doc:public_fields(kzd_skels:new()).
