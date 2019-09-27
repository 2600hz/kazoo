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
-module(pqc_cb_queues).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_queue/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, queues_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_queues:doc()) -> pqc_cb_api:response().
create(API, AccountId, QueueJObj) ->
    Envelope = pqc_cb_api:create_envelope(QueueJObj),
    pqc_cb_crud:create(API, queues_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, QueueId) ->
    pqc_cb_crud:fetch(API, queue_url(AccountId, QueueId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_queue:doc()) -> pqc_cb_api:response().
update(API, AccountId, QueueJObj) ->
    Envelope = pqc_cb_api:create_envelope(QueueJObj),
    pqc_cb_crud:update(API, queue_url(AccountId, kz_doc:id(QueueJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, QueueId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, queue_url(AccountId, QueueId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, QueueId) ->
    pqc_cb_crud:delete(API, queue_url(AccountId, QueueId)).


-spec queues_url(kz_term:ne_binary()) -> string().
queues_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"queues">>).

-spec queue_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
queue_url(AccountId, QueueId) ->
    pqc_cb_crud:entity_url(AccountId, <<"queues">>, QueueId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar', 'acdc'], ['cb_queues']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    QueueJObj = new_queue(),
    CreateResp = create(API, AccountId, QueueJObj),
    lager:info("created queue ~s", [CreateResp]),
    CreatedQueue = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    QueueId = kz_doc:id(CreatedQueue),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, QueueId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryQueue] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    QueueId = kz_doc:id(SummaryQueue),

    DeleteResp = delete(API, AccountId, QueueId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED QUEUE SEQ").

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

-spec new_queue() -> kzd_queues:doc().
new_queue() ->
    kz_doc:public_fields(
      kzd_queues:set_name(kzd_queues:new()
                         ,kz_binary:rand_hex(4)
                         )
     ).
