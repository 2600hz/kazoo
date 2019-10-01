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
-module(pqc_cb_temporal_rules).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_temporal_rule/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, temporal_rules_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_temporal_rules:doc()) -> pqc_cb_api:response().
create(API, AccountId, Temporal_ruleJObj) ->
    Envelope = pqc_cb_api:create_envelope(Temporal_ruleJObj),
    pqc_cb_crud:create(API, temporal_rules_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, Temporal_ruleId) ->
    pqc_cb_crud:fetch(API, temporal_rule_url(AccountId, Temporal_ruleId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_temporal_rule:doc()) -> pqc_cb_api:response().
update(API, AccountId, Temporal_ruleJObj) ->
    Envelope = pqc_cb_api:create_envelope(Temporal_ruleJObj),
    pqc_cb_crud:update(API, temporal_rule_url(AccountId, kz_doc:id(Temporal_ruleJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, Temporal_ruleId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, temporal_rule_url(AccountId, Temporal_ruleId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, Temporal_ruleId) ->
    pqc_cb_crud:delete(API, temporal_rule_url(AccountId, Temporal_ruleId)).


-spec temporal_rules_url(kz_term:ne_binary()) -> string().
temporal_rules_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"temporal_rules">>).

-spec temporal_rule_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
temporal_rule_url(AccountId, Temporal_ruleId) ->
    pqc_cb_crud:entity_url(AccountId, <<"temporal_rules">>, Temporal_ruleId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_temporal_rules']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    Temporal_ruleJObj = new_temporal_rule(),
    CreateResp = create(API, AccountId, Temporal_ruleJObj),
    lager:info("created temporal_rule ~s", [CreateResp]),
    CreatedTemporal_rule = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    Temporal_ruleId = kz_doc:id(CreatedTemporal_rule),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, Temporal_ruleId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryTemporal_rule] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    Temporal_ruleId = kz_doc:id(SummaryTemporal_rule),

    DeleteResp = delete(API, AccountId, Temporal_ruleId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED TEMPORAL_RULE SEQ").

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

-spec new_temporal_rule() -> kzd_temporal_rules:doc().
new_temporal_rule() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_temporal_rules:set_cycle/2, <<"daily">>}
                         ,{fun kzd_temporal_rules:set_name/2, kz_binary:rand_hex(4)}
                         ]
                        ,kzd_temporal_rules:new()
                        )
     ).
