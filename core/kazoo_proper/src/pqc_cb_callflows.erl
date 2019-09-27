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
-module(pqc_cb_callflows).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_callflow/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, callflows_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_callflows:doc()) -> pqc_cb_api:response().
create(API, AccountId, CallflowJObj) ->
    Envelope = pqc_cb_api:create_envelope(CallflowJObj),
    pqc_cb_crud:create(API, callflows_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, CallflowId) ->
    pqc_cb_crud:fetch(API, callflow_url(AccountId, CallflowId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_callflow:doc()) -> pqc_cb_api:response().
update(API, AccountId, CallflowJObj) ->
    Envelope = pqc_cb_api:create_envelope(CallflowJObj),
    pqc_cb_crud:update(API, callflow_url(AccountId, kz_doc:id(CallflowJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, CallflowId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, callflow_url(AccountId, CallflowId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, CallflowId) ->
    pqc_cb_crud:delete(API, callflow_url(AccountId, CallflowId)).


-spec callflows_url(kz_term:ne_binary()) -> string().
callflows_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"callflows">>).

-spec callflow_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
callflow_url(AccountId, CallflowId) ->
    pqc_cb_crud:entity_url(AccountId, <<"callflows">>, CallflowId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_callflows']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    CallflowJObj = new_callflow(),
    CreateResp = create(API, AccountId, CallflowJObj),
    lager:info("created callflow ~s", [CreateResp]),
    CreatedCallflow = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    CallflowId = kz_doc:id(CreatedCallflow),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, CallflowId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryCallflow] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    CallflowId = kz_doc:id(SummaryCallflow),

    DeleteResp = delete(API, AccountId, CallflowId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED CALLFLOW SEQ").

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

-spec new_callflow() -> kzd_callflows:doc().
new_callflow() ->
    kz_doc:public_fields(
      kzd_callflows:set_numbers(kzd_callflows:new()
                               ,[<<"2600">>]
                               )
     ).
