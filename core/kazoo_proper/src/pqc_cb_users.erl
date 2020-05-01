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
-module(pqc_cb_users).

%% API requests
-export([summary/2
        ,create/3
        ,fetch/3
        ,update/3
        ,patch/4
        ,delete/3
        ]).

-export([seq/0
        ,cleanup/0
        ,new_user/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,users_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_users:doc()) -> pqc_cb_api:response().
create(API, AccountId, UserJObj) ->
    URL = users_url(AccountId),

    Expectations = [#expectation{response_codes = [201]
                                ,response_headers = [{"content-type", "application/json"}
                                                    ,{"location", {'match', expected_location_value(URL)}}
                                                    ]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(UserJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, UserId) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,user_url(AccountId, UserId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_users:doc()) -> pqc_cb_api:response().
update(API, AccountId, UserJObj) ->
    URL = user_url(AccountId, kz_doc:id(UserJObj)),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(UserJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, UserId, PatchJObj) ->
    URL = user_url(AccountId, UserId),

    Expectations = [#expectation{response_codes = [200]
                                ,response_headers = [{"content-type", "application/json"}]
                                }
                   ],

    Envelope = pqc_cb_api:create_envelope(PatchJObj),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:patch/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, UserId) ->
    URL = user_url(AccountId, UserId),
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec users_url(kz_term:ne_binary()) -> string().
users_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "users"], "/").

-spec user_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
user_url(AccountId, UserId) ->
    string:join([users_url(AccountId), kz_term:to_list(UserId)], "/").

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_users']),
    AccountId = create_account(API),

    'ok' = create_simple_user(API, AccountId),
    wrong_format_check(API, AccountId),

    cleanup(API),
    ?INFO("FINISHED USERS SEQ").

create_simple_user(API, AccountId) ->
    EmptySummaryResp = summary(API, AccountId),
    ?INFO("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    UserJObj = new_user(),
    CreateResp = create(API, AccountId, UserJObj),
    ?INFO("created user ~s", [CreateResp]),
    CreatedUser = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    UserId = kz_doc:id(CreatedUser),
    'true' = user_docs_match(UserJObj, CreatedUser),

    UserWithEmail = kzd_users:set_email(CreatedUser, <<(kz_binary:rand_hex(4))/binary, "@2600hz.com">>),
    UpdateResp = update(API, AccountId, UserWithEmail),
    ?INFO("updated to ~s", [UpdateResp]),
    'true' = user_docs_match(UserWithEmail, kz_json:get_json_value(<<"data">>, kz_json:decode(UpdateResp))),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchedUser = kz_json:merge(UserWithEmail, Patch),

    PatchResp = patch(API, AccountId, UserId, Patch),
    ?INFO("patched to ~s", [PatchResp]),
    'true' = user_docs_match(PatchedUser, kz_json:get_json_value(<<"data">>, kz_json:decode(PatchResp))),

    SummaryResp = summary(API, AccountId),
    ?INFO("summary resp: ~s", [SummaryResp]),
    [SummaryUser] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    UserId = kz_doc:id(SummaryUser),

    DeleteResp = delete(API, AccountId, UserId),
    ?INFO("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    ?INFO("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),
    ?INFO("FINISHED SIMPLE USER").

wrong_format_check(API, AccountId) ->
    ShortEmail = kzd_users:set_email(new_user(), <<"e">>),
    {'error', ShortEmailError} = create(API, AccountId, ShortEmail),
    lager:info("validation failed creating empty email: ~s", [ShortEmailError]),

    WrongFormat = kzd_users:set_email(new_user(), kz_binary:rand_hex(4)),
    {'error', WrongFormatError} = create(API, AccountId, WrongFormat),
    lager:info("validation failed creating user: ~s", [WrongFormatError]),
    'true' = kzd_users:email(WrongFormat) =:= kz_json:get_ne_binary_value([<<"data">>, <<"email">>, <<"wrong_format">>, <<"value">>]
                                                                         ,kz_json:decode(WrongFormatError)
                                                                         ),
    lager:info("FINISHED WRONG FORMAT CHECK").

user_docs_match(Model, RespJObj) ->
    kz_json:all(fun({ModelKey, ModelValue}) -> user_setting_matches(ModelKey, ModelValue, RespJObj) end
               ,Model
               ).

user_setting_matches(ModelKey, ModelValue, RespJObj) ->
    case kz_json:get_value(ModelKey, RespJObj) of
        ModelValue -> 'true';
        _V ->
            ?INFO("key ~s is ~p instead of ~p", [ModelKey, _V, ModelValue]),
            'false'
    end.

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

-spec create_account(pqc_cb_api:state()) -> kz_term:ne_binary().
create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

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

%% take http://whatever:port/v2/... and get /v2/.../{regex}
expected_location_value(URL) ->
    expected_location_value(URL, "(\\w{32})").

expected_location_value(URL, Id) ->
    {'match', [_Host, Path]} = re:run(URL, "^(.+)(/v2/.+$)", [{'capture','all_but_first', 'list'}]),
    Path ++ [$/ | kz_term:to_list(Id)].
