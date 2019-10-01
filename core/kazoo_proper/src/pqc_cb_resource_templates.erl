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
-module(pqc_cb_resource_templates).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_resource_template/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, resource_templates_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_resource_templates:doc()) -> pqc_cb_api:response().
create(API, AccountId, Resource_templateJObj) ->
    Envelope = pqc_cb_api:create_envelope(Resource_templateJObj),
    pqc_cb_crud:create(API, resource_templates_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, Resource_templateId) ->
    pqc_cb_crud:fetch(API, resource_template_url(AccountId, Resource_templateId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_resource_template:doc()) -> pqc_cb_api:response().
update(API, AccountId, Resource_templateJObj) ->
    Envelope = pqc_cb_api:create_envelope(Resource_templateJObj),
    pqc_cb_crud:update(API, resource_template_url(AccountId, kz_doc:id(Resource_templateJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, Resource_templateId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, resource_template_url(AccountId, Resource_templateId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, Resource_templateId) ->
    pqc_cb_crud:delete(API, resource_template_url(AccountId, Resource_templateId)).


-spec resource_templates_url(kz_term:ne_binary()) -> string().
resource_templates_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"resource_templates">>).

-spec resource_template_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
resource_template_url(AccountId, Resource_templateId) ->
    pqc_cb_crud:entity_url(AccountId, <<"resource_templates">>, Resource_templateId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_resource_templates']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    Resource_templateJObj = new_resource_template(),
    CreateResp = create(API, AccountId, Resource_templateJObj),
    lager:info("created resource_template ~s", [CreateResp]),
    CreatedResource_template = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    Resource_templateId = kz_doc:id(CreatedResource_template),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, Resource_templateId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryResource_template] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    Resource_templateId = kz_doc:id(SummaryResource_template),

    DeleteResp = delete(API, AccountId, Resource_templateId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED RESOURCE_TEMPLATE SEQ").

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

-spec new_resource_template() -> kzd_resource_templates:doc().
new_resource_template() ->
    kz_doc:public_fields(
      kz_json:from_list([{<<"template_name">>, kz_binary:rand_hex(4)}])
     ).
