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
-module(pqc_cb_connectivity).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_connectivity/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, connectivity_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_connectivity:doc()) -> pqc_cb_api:response().
create(API, AccountId, ConnectivityJObj) ->
    Envelope = pqc_cb_api:create_envelope(ConnectivityJObj),
    pqc_cb_crud:create(API, connectivity_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, ConnectivityId) ->
    pqc_cb_crud:fetch(API, connectivity_url(AccountId, ConnectivityId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_connectivity:doc()) -> pqc_cb_api:response().
update(API, AccountId, ConnectivityJObj) ->
    Envelope = pqc_cb_api:create_envelope(ConnectivityJObj),
    pqc_cb_crud:update(API, connectivity_url(AccountId, kz_doc:id(ConnectivityJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, ConnectivityId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, connectivity_url(AccountId, ConnectivityId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, ConnectivityId) ->
    pqc_cb_crud:delete(API, connectivity_url(AccountId, ConnectivityId)).


-spec connectivity_url(kz_term:ne_binary()) -> string().
connectivity_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"connectivity">>).

-spec connectivity_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
connectivity_url(AccountId, ConnectivityId) ->
    pqc_cb_crud:entity_url(AccountId, <<"connectivity">>, ConnectivityId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_connectivity']),
    AccountId = create_account(API),
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    AccountRealm = kzd_accounts:realm(AccountDoc),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    ConnectivityJObj = new_connectivity(),
    CreateResp = create(API, AccountId, ConnectivityJObj),
    lager:info("created connectivity ~s", [CreateResp]),
    CreatedConnectivity = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    ConnectivityId = kz_doc:id(CreatedConnectivity),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, ConnectivityId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [ConnectivityId] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),

    Edited = kzd_connectivity:set_account_auth_realm(CreatedConnectivity, kz_binary:rand_hex(4)),
    EditedResp = update(API, AccountId, Edited),
    lager:info("edited resp: ~s", [EditedResp]),
    EditedDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(EditedResp)),
    AccountRealm = kzd_connectivity:account_auth_realm(EditedDoc),
    ConnectivityId = kz_doc:id(EditedDoc),

    DeleteResp = delete(API, AccountId, ConnectivityId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED CONNECTIVITY SEQ").

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

-spec new_connectivity() -> kzd_connectivity:doc().
new_connectivity() ->
    kz_doc:setters(kzd_connectivity:new()
                  ,[{fun kzd_connectivity:set_account_auth_realm/2, kz_binary:rand_hex(4)}
                   ,{fun kzd_connectivity:set_name/2, <<?MODULE_STRING>>}
                   ,{fun kzd_connectivity:set_servers/2, []}
                   ]).
