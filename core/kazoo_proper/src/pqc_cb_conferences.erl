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
-module(pqc_cb_conferences).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([patch/4]).
-export([delete/3]).
-export([update/3]).

-export([seq/0
        ,cleanup/0
        ,new_conference/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, conferences_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_conferences:doc()) -> pqc_cb_api:response().
create(API, AccountId, ConferenceJObj) ->
    Envelope = pqc_cb_api:create_envelope(ConferenceJObj),
    pqc_cb_crud:create(API, conferences_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, ConferenceId) ->
    pqc_cb_crud:fetch(API, conference_url(AccountId, ConferenceId)).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, ConferenceId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, conference_url(AccountId, ConferenceId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ConferenceId) ->
    pqc_cb_crud:delete(API, conference_url(AccountId, ConferenceId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_conference:doc()) -> pqc_cb_api:response().
update(API, AccountId, ConferenceJObj) ->
    Envelope = pqc_cb_api:create_envelope(ConferenceJObj),
    pqc_cb_crud:update(API, conference_url(AccountId, kz_doc:id(ConferenceJObj)), Envelope).


-spec conferences_url(kz_term:ne_binary()) -> string().
conferences_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"conferences">>).

-spec conference_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
conference_url(AccountId, ConferenceId) ->
    pqc_cb_crud:entity_url(AccountId, <<"conferences">>, ConferenceId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_conferences']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    ConferenceJObj = new_conference(),
    CreateResp = create(API, AccountId, ConferenceJObj),
    lager:info("created conference ~s", [CreateResp]),
    CreatedConference = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    ConferenceId = kz_doc:id(CreatedConference),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, ConferenceId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryConference] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    ConferenceId = kz_doc:id(SummaryConference),

    DeleteResp = delete(API, AccountId, ConferenceId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED CONFERENCE SEQ").

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

-spec new_conference() -> kzd_conferences:doc().
new_conference() ->
    kz_doc:public_fields(kzd_conferences:new()).
