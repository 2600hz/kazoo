%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_accounts).

-export([create_account/2, create_account/3
        ,delete_account/2
        ,patch_account/3
        ,fetch_account/2

        ,cleanup_accounts/1, cleanup_accounts/2

        ,command/2
        ,symbolic_account_id/2

        ,next_state/3
        ,postcondition/3

         %% kapps_maintenance:check_release callback
        ,seq/0
        ]).

-export([account_url/1]).

-export_type([account_id/0]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_accounts">>]).

-type account_id() :: {'call', 'pqc_kazoo_model', 'account_id_by_name', [pqc_cb_api:state() | proper_types:type()]} |
                      kz_term:ne_binary().

-spec command(pqc_kazoo_model:model(), kz_term:ne_binary() | proper_types:type()) ->
                     {'call', ?MODULE, 'create_account', [pqc_cb_api:state() | proper_types:term()]}.
command(Model, Name) ->
    {'call', ?MODULE, 'create_account', [pqc_kazoo_model:api(Model), Name]}.

-spec symbolic_account_id(pqc_kazoo_model:model(), kz_term:ne_binary() | proper_types:type()) ->
                                 account_id().
symbolic_account_id(Model, Name) ->
    {'call', 'pqc_kazoo_model', 'account_id_by_name', [Model, Name]}.

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_account(API, NewAccountName) ->
    create_account(API, NewAccountName, pqc_cb_api:auth_account_id(API)).

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_account(API, NewAccountName, AccountId) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    Resp = pqc_cb_api:make_request([#expectation{response_codes=[201, 500]}]
                                  ,fun kz_http:put/3
                                  ,account_url(AccountId)
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(RequestEnvelope)
                                  ),
    is_binary(Resp)
        andalso allow_number_additions(pqc_cb_response:account_id(Resp)),
    Resp.

-spec allow_number_additions(kz_term:ne_binary()) -> {'ok', kzd_accounts:doc()}.
allow_number_additions(AccountId) ->
    {'ok', _Account} = kzd_accounts:update(AccountId
                                          ,[{kzd_accounts:path_allow_number_additions(), 'true'}]
                                          ).

-spec fetch_account(pqc_cb_api:statE(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_account(API, AccountId) ->
    pqc_cb_api:make_request([#expectation{response_codes=[200]}]
                           ,fun kz_http:get/2
                           ,account_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec patch_account(pqc_cb_api:state(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch_account(API, AccountId, ReqJObj) ->
    RequestEnvelope = pqc_cb_api:create_envelope(ReqJObj),

    pqc_cb_api:make_request([#expectation{response_codes=[200]}]
                           ,fun kz_http:patch/3
                           ,account_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec delete_account(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete_account(API, AccountId) ->
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([#expectation{response_codes=[200]}]
                           ,fun kz_http:delete/2
                           ,account_url(AccountId)
                           ,RequestHeaders
                           ).

-spec cleanup_accounts(kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(AccountNames) ->
    cleanup_accounts(pqc_cb_api:authenticate(), AccountNames).

-spec cleanup_accounts(pqc_cb_api:state(), kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(API, AccountNames) ->
    _ = [cleanup_account(API, AccountName) || AccountName <- AccountNames],
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB).

-spec cleanup_account(pqc_cb_api:state(), kz_term:ne_binary()) -> 'ok'.
cleanup_account(API, AccountName) ->
    _Attempt = try pqc_cb_search:search_account_by_name(API, AccountName) of
                   ?FAILED_RESPONSE ->
                       ?ERROR("failed to search for account by name ~s~n", [AccountName]);
                   APIResp ->
                       Data = pqc_cb_response:data(APIResp),
                       case kz_json:get_ne_binary_value([1, <<"id">>], Data) of
                           'undefined' ->
                               check_accounts_db(AccountName);
                           AccountId -> delete_account(API, AccountId)
                       end
               catch
                   'throw':{'error', 'socket_closed_remotely'} ->
                       ?ERROR("broke the SUT cleaning up account ~s (~p)~n", [AccountName, API])
               end,
    timer:sleep(1000).% was needed to stop overwhelming the socket, at least locally

check_accounts_db(Name) ->
    AccountName = kzd_accounts:normalize_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_name">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} -> ?ERROR("failed to list by name: ~p", [_E]);
        {'ok', JObjs} ->
            ?INFO("deleting from ~s: ~p~n", [?KZ_ACCOUNTS_DB, JObjs]),
            kz_datamgr:del_docs(?KZ_ACCOUNTS_DB, JObjs)
    end.

-spec account_url(account_id() | map()) -> string().
account_url(#{}=API) ->
    account_url(pqc_cb_api:auth_account_id(API));
account_url(AccountId) ->
    string:join([pqc_cb_api:v2_base_url(), "accounts", kz_term:to_list(AccountId)], "/").

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model
          ,APIResp
          ,{'call', ?MODULE, 'create_account', [_API, Name]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_account_missing/2, [Name]}
                           ,{fun pqc_kazoo_model:add_account/3, [Name, APIResp]}
                           ]).

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model
             ,{'call', _, 'create_account', [_API, Name]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:account_id_by_name(Model, Name) of
        'undefined' ->
            ?INFO("no account by the name of ~s, should be an account id in ~s"
                 ,[Name, APIResult]
                 ),
            'undefined' =/= pqc_cb_response:account_id(APIResult);
        _AccountId ->
            ?INFO("account ~s (~s) found, API should be an error: ~s"
                 ,[Name, _AccountId, APIResult]
                 ),
            500 =:= pqc_cb_response:error_code(APIResult)
    end.

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_accounts']),
    'ok' = enable_and_delete_topup(API),
    enable_and_disable_account_using_patch(API).

-spec enable_and_delete_topup(pqc_cb_api:state()) -> 'ok'.
enable_and_delete_topup(API) ->
    ?INFO("STARTING ENABLE_AND_DISABLE_TOPUP TEST"),
    %% Make sure everything is clean for the test.
    _ = cleanup(API),

    AccountResp = create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    AccountJObj = kz_json:get_value(<<"data">>, kz_json:decode(AccountResp)),
    AccountId = kz_json:get_binary_value(<<"id">>, AccountJObj),
    TopupConfig = kz_json:from_list([{<<"threshold">>,10},{<<"amount">>,50}]),
    RequestData = kz_json:set_value(<<"topup">>, TopupConfig, AccountJObj),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    Resp = topup_request(API, AccountId, RequestEnvelope),
    ?INFO("enable topup resp: ~s", [Resp]),

    RespJObj = pqc_cb_response:data(Resp),
    'true' = kz_json:are_equal(TopupConfig, kz_json:get_ne_value(<<"topup">>, RespJObj)),
    RequestData1 = kz_json:delete_key(<<"topup">>, RespJObj),
    RequestEnvelope1 = pqc_cb_api:create_envelope(RequestData1),

    Resp1 = topup_request(API, AccountId, RequestEnvelope1),
    ?INFO("disable topup resp: ~s", [Resp1]),

    'undefined' = kz_json:get_ne_value(<<"topup">>, kz_json:decode(Resp1)),

    _ = cleanup(API),
    ?INFO("FINISHED ENABLE_AND_DISABLE_TOPUP TEST").

-spec enable_and_disable_account_using_patch(pqc_cb_api:state()) -> 'ok'.
enable_and_disable_account_using_patch(API) ->
    ?INFO("STARTING ENABLE_AND_DISABLE_ACCOUNT_USING_PATCH TEST"),
    %% Make sure everything is clean for the test.
    _ = cleanup(API),

    AccountResp = create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_binary_value(<<"id">>, pqc_cb_response:data(AccountResp)),

    Fetched = pqc_cb_response:data(fetch_account(API, AccountId)),
    'true' = kz_json:is_true(<<"enabled">>, Fetched, 'true'),

    ?INFO("disabling account"),
    ReqJObj = kz_json:from_list([{<<"enabled">>, 'false'}]),
    Disabled = pqc_cb_response:data(patch_account(API, AccountId, ReqJObj)),
    'false' = kz_json:is_true(<<"enabled">>, Disabled, 'true'),

    ?INFO("enabling account"),
    ReqJObj1 = kz_json:from_list([{<<"enabled">>, 'true'}]),
    Enabled = pqc_cb_response:data(patch_account(API, AccountId, ReqJObj1)),
    'true' = kz_json:is_true(<<"enabled">>, Enabled, 'true'),

    _ = cleanup(API),
    ?INFO("FINISHED ENABLE_AND_DISABLE_ACCOUNT_USING_PATCH TEST").

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API).

-spec topup_request(pqc_cb_api:state(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
topup_request(API, AccountId, RequestEnvelope) ->
    pqc_cb_api:make_request([#expectation{response_codes=[200]}]
                           ,fun kz_http:post/3
                           ,account_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(RequestEnvelope)
                           ).
