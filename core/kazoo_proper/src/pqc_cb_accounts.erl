-module(pqc_cb_accounts).

-export([create_account/2
        ,delete_account/2
        ,cleanup_accounts/1, cleanup_accounts/2

        ,command/2
        ,symbolic_account_id/2

        ,next_state/3
        ,postcondition/3
        ]).

-export([account_url/1]).

-export_type([account_id/0]).

-include("kazoo_proper.hrl").

-type account_id() :: {'call', 'pqc_kazoo_model', 'account_id_by_name', [pqc_cb_api:state() | proper_types:type()]} |
                      ne_binary().

-spec command(pqc_kazoo_model:model(), ne_binary() | proper_types:type()) ->
                     {'call', ?MODULE, 'create_account', [pqc_cb_api:state() | proper_types:term()]}.
command(Model, Name) ->
    {'call', ?MODULE, 'create_account', [pqc_kazoo_model:api(Model), Name]}.

-spec symbolic_account_id(pqc_kazoo_model:model(), ne_binary() | proper_types:type()) ->
                                 account_id().
symbolic_account_id(Model, Name) ->
    {'call', 'pqc_kazoo_model', 'account_id_by_name', [Model, Name]}.

-spec create_account(pqc_cb_api:state(), ne_binary()) -> binary().
create_account(API, NewAccountName) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    Resp = pqc_cb_api:make_request([201, 500]
                                  ,fun kz_http:put/3
                                  ,account_url(pqc_cb_api:auth_account_id(API))
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(RequestEnvelope)
                                  ),
    is_binary(NewAccountId = pqc_cb_response:account_id(Resp))
        andalso allow_number_additions(NewAccountId),
    Resp.

-spec allow_number_additions(ne_binary()) -> {'ok', kz_account:doc()}.
allow_number_additions(AccountId) ->
    {'ok', _Account} = kz_util:set_allow_number_additions(AccountId, 'true').

-spec delete_account(pqc_cb_api:state(), ne_binary()) -> binary().
delete_account(API, AccountId) ->
    URL = account_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200], fun kz_http:delete/2, URL, RequestHeaders).

-spec cleanup_accounts(ne_binaries()) -> 'ok'.
-spec cleanup_accounts(pqc_cb_api:state(), ne_binaries()) -> 'ok'.
cleanup_accounts(AccountNames) ->
    cleanup_accounts(pqc_cb_api:authenticate(), AccountNames).

cleanup_accounts(API, AccountNames) ->
    _ = [cleanup_account(API, AccountName) || AccountName <- AccountNames],
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB).

-spec cleanup_account(pqc_cb_api:state(), ne_binary()) -> 'ok'.
cleanup_account(API, AccountName) ->
    _ = try pqc_cb_search:search_account_by_name(API, AccountName) of
            ?FAILED_RESPONSE ->
                ?INFO("failed to search for account by name ~s~n", [AccountName]);
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
    AccountName = kz_util:normalize_account_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_name">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _} -> 'ok';
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
