%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%%
%%% A user can also supply a refresh token to retrieve a new access token
%%% or revoke one or all of their active refresh tokens.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_token_auth).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,post/2
        ,delete/1, delete/2, delete/3
        ,authenticate/1, early_authenticate/1
        ,authorize/1
        ]).

-include("crossbar.hrl").

-define(REFRESH_PATH_TOKEN, <<"refresh">>).
-define(REFRESH_TOKENS_PATH_TOKEN, <<"refresh_tokens">>).

-define(LOOP_TIMEOUT,
        kapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

-define(PERCENT_OF_TIMEOUT,
        kapps_config:get_integer(?APP_NAME, <<"expiry_percentage">>, 75)).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.early_authenticate">>, ?MODULE, 'early_authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.token_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.token_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.token_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.token_auth">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.token_auth">>, ?MODULE, 'delete'),
    ok.

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() -> [?HTTP_DELETE, ?HTTP_GET].

allowed_methods(?REFRESH_PATH_TOKEN) -> [?HTTP_POST];
allowed_methods(?REFRESH_TOKENS_PATH_TOKEN) -> [?HTTP_DELETE].

allowed_methods(?REFRESH_TOKENS_PATH_TOKEN, _RefreshToken) -> [?HTTP_DELETE].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.

resource_exists(?REFRESH_PATH_TOKEN) -> 'true';
resource_exists(?REFRESH_TOKENS_PATH_TOKEN) -> 'true'.

resource_exists(?REFRESH_TOKENS_PATH_TOKEN, _RefreshToken) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    cb_context:put_reqid(Context),
    validate_1(Context, cb_context:req_verb(Context)).

validate(Context, ?REFRESH_PATH_TOKEN) ->
    cb_context:put_reqid(Context),
    AuthDoc = cb_context:auth_doc(Context),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_doc/2, AuthDoc}
                       ]);
validate(Context, ?REFRESH_TOKENS_PATH_TOKEN) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        _ -> cb_context:set_resp_status(Context, 'success')
    end.

validate(Context, ?REFRESH_TOKENS_PATH_TOKEN, RefreshToken) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        _ -> validate_3(Context, RefreshToken)
    end.

-spec validate_1(cb_context:context(), ne_binary()) -> cb_context:context().
validate_1(Context, ?HTTP_GET) ->
    JObj = crossbar_util:response_auth(
             kz_doc:public_fields(cb_context:auth_doc(Context))
            ),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, JObj}
              ],
    cb_context:setters(Context, Setters);
validate_1(Context, ?HTTP_DELETE) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        AuthDoc ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_doc/2, AuthDoc}
                               ])
    end.

-spec validate_3(cb_context:context(), ne_binary()) -> cb_context:context().
validate_3(Context, RefreshToken) ->
    case kz_datamgr:open_cache_doc(?KZ_TOKEN_DB, RefreshToken) of
        {'ok', Doc} ->
            validate_delete_refresh_token_doc(Context, Doc);
        {'error', _} -> Context
    end.

-spec validate_delete_refresh_token_doc(cb_context:context(), kz_json:object()) ->
                                               cb_context:context().
validate_delete_refresh_token_doc(Context, Doc) ->
    AuthDoc = cb_context:auth_doc(Context),
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, AuthDoc),
    OwnerId = kz_json:get_ne_binary_value(<<"owner_id">>, AuthDoc),
    DocAccountId = kz_json:get_ne_binary_value(<<"account_id">>, Doc),
    DocOwnerId = kz_json:get_ne_binary_value(<<"owner_id">>, Doc),
    case AccountId =:= DocAccountId
        andalso OwnerId =:= DocOwnerId of
        'true' ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_doc/2, Doc}
                               ]);
        'false' -> Context
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?REFRESH_PATH_TOKEN) ->
    cb_context:put_reqid(Context),

    RefreshToken = cb_context:auth_refresh_token(Context),
    case kz_datamgr:del_doc(?KZ_TOKEN_DB, RefreshToken) of
        {'ok', _} ->
            crossbar_auth:create_auth_token(Context, ?MODULE);
        {'error', E} ->
            lager:debug("failed to delete refresh token ~s: ~p", [RefreshToken, E]),
            crossbar_util:response('error', <<"could not consume refresh token">>, Context)
    end.

-spec delete(cb_context:context()) -> cb_context:context().
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context) ->
    cb_context:put_reqid(Context),

    AuthToken = cb_context:auth_token(Context),
    case kz_datamgr:del_doc(?KZ_TOKEN_DB, AuthToken) of
        {'ok', _} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, 'undefined'}
                               ,{fun cb_context:set_doc/2, 'undefined'}
                               ,{fun cb_context:set_auth_doc/2, 'undefined'}
                               ,{fun cb_context:set_auth_token/2, 'undefined'}
                               ,{fun cb_context:set_auth_account_id/2, 'undefined'}
                               ]);
        {'error', _E} ->
            lager:debug("failed to delete auth token ~s: ~p", [AuthToken, _E]),
            Context
    end.

delete(Context, ?REFRESH_TOKENS_PATH_TOKEN) ->
    cb_context:put_reqid(Context),

    AuthDoc = cb_context:auth_doc(Context),
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, AuthDoc),
    OwnerId = kz_json:get_ne_binary_value(<<"owner_id">>, AuthDoc),
    case kz_datamgr:get_results(?KZ_TOKEN_DB
                               ,<<"token_auth/refresh_tokens_listing">>
                               ,[{'key', [AccountId, OwnerId]}]
                               ) of
        {'ok', Docs} -> delete_refresh_tokens(Context, Docs);
        {'error', _} ->
            crossbar_util:response('error', <<"could not delete refresh tokens">>, Context)
    end.

delete(Context, ?REFRESH_TOKENS_PATH_TOKEN, _RefreshToken) ->
    cb_context:put_reqid(Context),

    Doc = cb_context:doc(Context),
    delete_refresh_tokens(Context, [Doc]).

-spec delete_refresh_tokens(cb_context:context(), kz_json:objects()) ->
                                   cb_context:context().
delete_refresh_tokens(Context, []) ->
    cb_context:set_resp_status(Context, 'success');
delete_refresh_tokens(Context, [Doc|Docs]) ->
    case kz_datamgr:del_doc(?KZ_TOKEN_DB, Doc) of
        {'ok', _} -> delete_refresh_tokens(Context, Docs);
        {'error', _} ->
            crossbar_util:response('error', <<"could not delete refresh tokens">>, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    %% if the request is directly for token_auth (only)
    %% then allow GET and DELETE
    [{<<"token_auth">>, []}] =:= cb_context:req_nouns(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          boolean() |
                          {'true' | 'halt', cb_context:context()}.
-spec authenticate(cb_context:context(), api_ne_binary(), req_nouns(), atom()) ->
                          boolean() |
                          {'true' | 'halt', cb_context:context()}.
authenticate(Context) ->
    cb_context:put_reqid(Context),
    authenticate(Context, cb_context:auth_account_id(Context), cb_context:req_nouns(Context), cb_context:auth_token_type(Context)).

authenticate(Context, _, [{<<"token_auth">>, [?REFRESH_PATH_TOKEN]}], _) ->
    case is_rate_limited(Context) of
        'true' ->
            lager:info("checking for x-auth-refresh-token"),
            check_refresh_token(Context
                               ,cb_context:auth_refresh_token(Context)
                               );
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, ?NE_BINARY = _AccountId, _ReqNouns, 'x-auth-token') -> 'true';
authenticate(Context, 'undefined', _ReqNouns, 'x-auth-token') ->
    _ = cb_context:put_reqid(Context),
    case is_rate_limited(Context) of
        'true' ->
            lager:info("checking for x-auth-token"),
            check_auth_token(Context
                            ,cb_context:auth_token(Context)
                            ,cb_context:magic_pathed(Context)
                            );
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, _AccountId, _ReqNouns, _TokenType) -> 'false'.

-spec is_rate_limited(cb_context:context()) -> boolean().
is_rate_limited(Context) ->
    _ = cb_context:put_reqid(Context),
    kz_buckets:consume_tokens(?APP_NAME
                             ,cb_modules_util:bucket_name(Context)
                             ,cb_modules_util:token_cost(Context)
                             ).

-spec early_authenticate(cb_context:context()) ->
                                boolean() |
                                {'true', cb_context:context()}.
-spec early_authenticate(cb_context:context(), atom() | api_binary()) ->
                                boolean() |
                                {'true', cb_context:context()}.
early_authenticate(Context) ->
    cb_context:put_reqid(Context),
    early_authenticate(Context, cb_context:auth_token_type(Context)).

early_authenticate(Context, 'x-auth-token') ->
    early_authenticate_token(Context, cb_context:auth_token(Context));
early_authenticate(_Context, _TokenType) -> 'false'.

-spec early_authenticate_token(cb_context:context(), api_binary()) ->
                                      boolean() |
                                      {'true', cb_context:context()}.
early_authenticate_token(Context, AuthToken) when is_binary(AuthToken) ->
    validate_auth_token(Context, AuthToken);
early_authenticate_token(_Context, 'undefined') -> 'true'.

-spec check_auth_token(cb_context:context(), api_binary(), boolean()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_auth_token(_Context, <<>>, MagicPathed) ->
    lager:info("empty auth token - magic path'd: ~p", [MagicPathed]),
    MagicPathed;
check_auth_token(_Context, 'undefined', MagicPathed) ->
    lager:info("auth token not found - magic path'd: ~p", [MagicPathed]),
    MagicPathed;
check_auth_token(Context, AuthToken, _MagicPathed) ->
    validate_auth_token(Context, AuthToken).

-spec check_refresh_token(cb_context:context(), api_binary()) ->
                                 'false' |
                                 {'true', cb_context:context()}.
check_refresh_token(_Context, 'undefined') -> 'false';
check_refresh_token(Context, RefreshToken) ->
    validate_refresh_token(Context, RefreshToken).

-spec validate_auth_token(cb_context:context(), ne_binary()) ->
                                 boolean() |
                                 {'true', cb_context:context()}.
validate_auth_token(Context, ?NE_BINARY = AuthToken) ->
    Options = [{<<"account_id">>, cb_context:req_header(Context, <<"x-auth-account-id">>)}],
    lager:debug("checking auth token"),
    case crossbar_auth:validate_auth_token(AuthToken, props:filter_undefined(Options)) of
        {'ok', JObj} ->
            is_account_expired(Context, JObj);
        {'error', <<"token expired">>} ->
            lager:info("provided auth token has expired"),

            {'halt', crossbar_util:response_401(Context)};
        {'error', 'not_found'} ->
            lager:info("provided auth token was not found"),
            {'halt', crossbar_util:response_401(Context)};
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec validate_refresh_token(cb_context:context(), ne_binary()) ->
                                    {'true', cb_context:context()}.
validate_refresh_token(Context, RefreshToken) ->
    case crossbar_auth:validate_refresh_token(RefreshToken) of
        {'ok', JObj} ->
            is_account_expired(Context, JObj);
        {'error', <<"token expired">>} ->
            lager:info("provided refresh token has expired"),

            {'halt', crossbar_util:response_401(Context)};
        {'error', 'not_found'} ->
            lager:info("provided refresh token was not found"),
            {'halt', crossbar_util:response_401(Context)};
        {'error', E} ->
            lager:error("error validating refresh token: ~p", [E]),
            {'halt', crossbar_util:response_401(Context)}
    end.

-spec is_account_expired(cb_context:context(), kz_json:object()) ->
                                boolean() |
                                {'halt', cb_context:context()}.
is_account_expired(Context, JObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, JObj),
    case kz_util:is_account_expired(AccountId) of
        'false' -> check_as(Context, JObj);
        {'true', Expired} ->
            _ = kz_util:spawn(fun kz_util:maybe_disable_account/1, [AccountId]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            Context1 = cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context),
            {'halt', Context1}
    end.

-spec check_as(cb_context:context(), kz_json:object()) ->
                      boolean() |
                      {'true', cb_context:context()}.
check_as(Context, JObj) ->
    case kz_json:get_ne_binary_value(<<"account_id">>, JObj, 'undefined') of
        'undefined' -> {'true', set_auth_doc(Context, JObj)};
        AccountId -> check_as_payload(Context, JObj, AccountId)
    end.

-spec check_as_payload(cb_context:context(), kz_json:object(), ne_binary()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_as_payload(Context, JObj, AccountId) ->
    case {kz_json:get_value([<<"as">>, <<"account_id">>], JObj, 'undefined')
         ,kz_json:get_value([<<"as">>, <<"owner_id">>], JObj, 'undefined')
         }
    of
        {'undefined', _} -> {'true', set_auth_doc(Context, JObj)};
        {_, 'undefined'} -> {'true', set_auth_doc(Context, JObj)};
        {AsAccountId, AsOwnerId} -> check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId)
    end.

-spec check_descendants(cb_context:context(), kz_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                               boolean() |
                               {'true', cb_context:context()}.
check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId) ->
    case kapps_util:account_descendants(AccountId) of
        [] -> false;
        Descendants ->
            case lists:member(AsAccountId, Descendants) of
                'false' -> 'false';
                'true' ->
                    JObj1 = kz_json:set_values([{<<"account_id">>, AsAccountId}
                                               ,{<<"owner_id">>, AsOwnerId}
                                               ]
                                              ,JObj
                                              ),
                    {'true', set_auth_doc(Context, JObj1)}
            end
    end.

-spec set_auth_doc(cb_context:context(), kz_json:object()) -> cb_context:context().
set_auth_doc(Context, JObj) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_auth_doc/2, JObj}
                       ,{fun cb_context:set_auth_account_id/2
                        ,kz_json:get_ne_binary_value(<<"account_id">>, JObj)
                        }
                       ]).
