-module(pqc_cb_accounts).

-export([create_account/2
        ,delete_account/2
        ,cleanup_accounts/1, cleanup_accounts/2
        ]).

-export([account_url/1]).

-include("kazoo_proper.hrl").

-spec create_account(pqc_cb_api:state(), ne_binary()) -> binary().
create_account(API, NewAccountName) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope  = pqc_cb_api:create_envelope(RequestData),

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
    'ok'.

-spec cleanup_account(pqc_cb_api:state(), ne_binary()) -> 'ok'.
cleanup_account(API, AccountName) ->
    _ = try pqc_cb_search:search_account_by_name(API, AccountName) of
            ?FAILED_RESPONSE ->
                io:format("failed to search for account by name ~s~n", [AccountName]);
            APIResp ->
                Data = pqc_cb_response:data(APIResp),
                case kz_json:get_ne_binary_value([1, <<"id">>], Data) of
                    'undefined' -> 'ok';
                    AccountId -> delete_account(API, AccountId)
                end
        catch
            'throw':{'error', 'socket_closed_remotely'} ->
                io:format("broke the SUT cleaning up account ~s (~p)~n", [AccountName, API])
        end,
    timer:sleep(1000).% was needed to stop overwhelming the socket, at least locally

-spec account_url(ne_binary() | map()) -> string().
account_url(#{}=API) ->
    account_url(pqc_cb_api:auth_account_id(API));
account_url(AccountId) ->
    string:join([pqc_cb_api:v2_base_url(), "accounts", kz_term:to_list(AccountId)], "/").
