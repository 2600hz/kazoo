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
-module(pqc_cb_devices).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([patch/4]).
-export([update/3]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_device/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, devices_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_devices:doc()) -> pqc_cb_api:response().
create(API, AccountId, DeviceJObj) ->
    Envelope = pqc_cb_api:create_envelope(DeviceJObj),
    pqc_cb_crud:create(API, devices_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, DeviceId) ->
    pqc_cb_crud:fetch(API, device_url(AccountId, DeviceId)).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, DeviceId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, device_url(AccountId, DeviceId), Envelope).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_device:doc()) -> pqc_cb_api:response().
update(API, AccountId, DeviceJObj) ->
    Envelope = pqc_cb_api:create_envelope(DeviceJObj),
    pqc_cb_crud:update(API, device_url(AccountId, kz_doc:id(DeviceJObj)), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, DeviceId) ->
    pqc_cb_crud:delete(API, device_url(AccountId, DeviceId)).


-spec devices_url(kz_term:ne_binary()) -> string().
devices_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"devices">>).

-spec device_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
device_url(AccountId, DeviceId) ->
    pqc_cb_crud:entity_url(AccountId, <<"devices">>, DeviceId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_devices']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    DeviceJObj = new_device(),
    CreateResp = create(API, AccountId, DeviceJObj),
    lager:info("created device ~s", [CreateResp]),
    CreatedDevice = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    DeviceId = kz_doc:id(CreatedDevice),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, DeviceId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryDevice] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    DeviceId = kz_doc:id(SummaryDevice),

    DeleteResp = delete(API, AccountId, DeviceId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED DEVICE SEQ").

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

-spec new_device() -> kzd_devices:doc().
new_device() ->
    kz_doc:public_fields(
      kzd_devices:set_name(kzd_devices:new()
                          ,kz_binary:rand_hex(4)
                          )
     ).
