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
-module(pqc_cb_resources).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_resource/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, resources_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_resources:doc()) -> pqc_cb_api:response().
create(API, AccountId, ResourceJObj) ->
    Envelope = pqc_cb_api:create_envelope(ResourceJObj),
    pqc_cb_crud:create(API, resources_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, ResourceId) ->
    pqc_cb_crud:fetch(API, resource_url(AccountId, ResourceId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_resource:doc()) -> pqc_cb_api:response().
update(API, AccountId, ResourceJObj) ->
    Envelope = pqc_cb_api:create_envelope(ResourceJObj),
    pqc_cb_crud:update(API, resource_url(AccountId, kz_doc:id(ResourceJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, ResourceId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, resource_url(AccountId, ResourceId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ResourceId) ->
    pqc_cb_crud:delete(API, resource_url(AccountId, ResourceId)).


-spec resources_url(kz_term:ne_binary()) -> string().
resources_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"resources">>).

-spec resource_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
resource_url(AccountId, ResourceId) ->
    pqc_cb_crud:entity_url(AccountId, <<"resources">>, ResourceId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_resources']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    ResourceJObj = new_resource(),
    CreateResp = create(API, AccountId, ResourceJObj),
    lager:info("created resource ~s", [CreateResp]),
    CreatedResource = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    ResourceId = kz_doc:id(CreatedResource),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, ResourceId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryResource] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    ResourceId = kz_doc:id(SummaryResource),

    DeleteResp = delete(API, AccountId, ResourceId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED RESOURCE SEQ").

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

-spec new_resource() -> kzd_resources:doc().
new_resource() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_resources:set_name/2, kz_binary:rand_hex(4)}
                         ,{fun kzd_resources:set_gateways/2, []}
                         ]
                        ,kzd_resources:new()
                        )
     ).
