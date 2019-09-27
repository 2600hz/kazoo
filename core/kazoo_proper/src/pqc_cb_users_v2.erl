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
-module(pqc_cb_users_v2).

-export([summary/2]).
-export([create/3]).
-export([fetch/3]).
-export([update/3]).
-export([delete/3]).
-export([patch/4]).

-export([seq/0
        ,cleanup/0
        ,new_user/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, users_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_users:doc()) -> pqc_cb_api:response().
create(API, AccountId, UserJObj) ->
    Envelope = pqc_cb_api:create_envelope(UserJObj),
    pqc_cb_crud:create(API, users_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, UserId) ->
    pqc_cb_crud:fetch(API, user_url(AccountId, UserId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_user:doc()) -> pqc_cb_api:response().
update(API, AccountId, UserJObj) ->
    Envelope = pqc_cb_api:create_envelope(UserJObj),
    pqc_cb_crud:update(API, user_url(AccountId, kz_doc:id(UserJObj)), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, UserId) ->
    pqc_cb_crud:delete(API, user_url(AccountId, UserId)).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, UserId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, user_url(AccountId, UserId), Envelope).


-spec users_url(kz_term:ne_binary()) -> string().
users_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"users">>).

-spec user_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
user_url(AccountId, UserId) ->
    pqc_cb_crud:entity_url(AccountId, <<"users">>, UserId).


-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_users_v2']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    UserJObj = new_user(),
    CreateResp = create(API, AccountId, UserJObj),
    lager:info("created user ~s", [CreateResp]),
    CreatedUser = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    UserId = kz_doc:id(CreatedUser),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, UserId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryUser] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    UserId = kz_doc:id(SummaryUser),

    DeleteResp = delete(API, AccountId, UserId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED USER SEQ").

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

-spec new_user() -> kzd_users:doc().
new_user() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_users:set_first_name/2, kz_binary:rand_hex(4)}
                         ,{fun kzd_users:set_last_name/2, kz_binary:rand_hex(4)}
                         ]
                        ,kzd_users:new()
                        )
     ).
