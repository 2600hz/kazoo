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
-module(pqc_cb_directories).

-export([summary/2]).
-export([create/3]).
-export([fetch/3, fetch/4]).
-export([update/3]).
-export([patch/4]).
-export([delete/3]).

-export([seq/0
        ,cleanup/0
        ,new_directory/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, directories_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_directories:doc()) -> pqc_cb_api:response().
create(API, AccountId, DirectoryJObj) ->
    Envelope = pqc_cb_api:create_envelope(DirectoryJObj),
    pqc_cb_crud:create(API, directories_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, DirectoryId) ->
    fetch(API, AccountId, DirectoryId, 'undefined').

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_proplist()) -> pqc_cb_api:response().
fetch(API, AccountId, DirectoryId, QueryString) ->
    pqc_cb_crud:fetch(API, directory_url(AccountId, DirectoryId) ++ querystring(QueryString)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_directorie:doc()) -> pqc_cb_api:response().
update(API, AccountId, DirectoryJObj) ->
    Envelope = pqc_cb_api:create_envelope(DirectoryJObj),
    pqc_cb_crud:update(API, directory_url(AccountId, kz_doc:id(DirectoryJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, DirectoryId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, directory_url(AccountId, DirectoryId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, DirectoryId) ->
    pqc_cb_crud:delete(API, directory_url(AccountId, DirectoryId)).

-spec directories_url(kz_term:ne_binary()) -> string().
directories_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"directories">>).

-spec directory_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
directory_url(AccountId, DirectoryId) ->
    pqc_cb_crud:entity_url(AccountId, <<"directories">>, DirectoryId).

querystring('undefined') -> "";
querystring([]) -> "";
querystring(QS) -> ["?", kz_http_util:props_to_querystring(QS)].

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_directories']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    DirectoryJObj = new_directory(),
    CreateResp = create(API, AccountId, DirectoryJObj),
    lager:info("created directorie ~s", [CreateResp]),
    CreatedDirectorie = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    DirectoryId = kz_doc:id(CreatedDirectorie),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, DirectoryId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryDirectorie] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    DirectoryId = kz_doc:id(SummaryDirectorie),

    Users = create_users(API, AccountId, DirectoryId),
    UserIds = [kz_doc:id(User) || User <- Users],

    FetchResp = fetch(API, AccountId, DirectoryId, [{<<"paginate">>, 'false'}]),
    lager:info("fetched directory: ~s", [FetchResp]),
    FetchedUsers = kzd_directories:users(kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp))),

    'true' = length(UserIds) =:= length(FetchedUsers),
    'true' = lists:all(fun(FetchedUser) ->
                               lists:member(kz_json:get_ne_binary_value(<<"user_id">>, FetchedUser)
                                           ,UserIds
                                           )
                       end
                      ,FetchedUsers
                      ),

    DeleteResp = delete(API, AccountId, DirectoryId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED DIRECTORIE SEQ").

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

-spec new_directory() -> kzd_directories:doc().
new_directory() ->
    kz_doc:public_fields(
      kzd_directories:set_name(kzd_directories:new()
                              ,kz_binary:rand_hex(4)
                              )
     ).

-spec create_users(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> [kzd_users:doc()].
create_users(API, AccountId, DirectoryId) ->
    [create_user(API, AccountId, DirectoryId, N) || N <- lists:seq(1, 100)].

-spec create_user(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), 1..100) -> kzd_users:doc().
create_user(API, AccountId, DirectoryId, NthUser) ->
    Directories = kz_json:from_list([{DirectoryId, kz_binary:rand_hex(16)}]),
    UserDoc = kzd_users:set_directories(pqc_cb_users:new_user(), Directories),

    Results = pqc_cb_users:create(API, AccountId, kz_json:set_value(<<"nth">>, NthUser, UserDoc)),
    kz_json:get_json_value(<<"data">>, kz_json:decode(Results)).
