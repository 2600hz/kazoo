%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Token auth module
%%% This is a simple auth mechanism, once the user has acquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_token_auth).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,delete/1
        ,authenticate/1, early_authenticate/1
        ,authorize/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.early_authenticate">>, ?MODULE, 'early_authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.token_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.token_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.token_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.token_auth">>, ?MODULE, 'delete'),
    ok.

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_DELETE, ?HTTP_GET].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    _ = cb_context:put_reqid(Context),
    validate(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate(Context, ?HTTP_GET) ->
    JObj = crossbar_util:response_auth(
             kz_doc:public_fields(cb_context:auth_doc(Context))
            ),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, JObj}
              ],
    cb_context:setters(Context, Setters);
validate(Context, ?HTTP_DELETE) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        AuthDoc ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_doc/2, AuthDoc}
                               ])
    end.

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    _ = cb_context:put_reqid(Context),

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    %% if the request is directly for token_auth (only)
    %% then allow GET and DELETE
    [{<<"token_auth">>, []}] =:= cb_context:req_nouns(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec authenticate(cb_context:context()) ->
                          boolean() |
                          {'true' | 'stop', cb_context:context()}.
authenticate(Context) ->
    _ = cb_context:put_reqid(Context),
    authenticate(Context, cb_context:auth_account_id(Context), cb_context:auth_token_type(Context)).

-spec authenticate(cb_context:context(), kz_term:api_ne_binary(), atom()) ->
                          boolean() |
                          {'true' | 'stop', cb_context:context()}.
authenticate(_Context, ?NE_BINARY = _AccountId, 'x-auth-token') -> 'true';
authenticate(Context, 'undefined', 'x-auth-token') ->
    _ = cb_context:put_reqid(Context),
    Bucket = cb_modules_util:bucket_name(Context),
    Cost = cb_modules_util:token_cost(Context),

    case kz_buckets:consume_tokens(?APP_NAME, Bucket, Cost) of
        'true' ->
            lager:info("checking for x-auth-token"),
            check_auth_token(Context
                            ,cb_context:auth_token(Context)
                            ,cb_context:magic_pathed(Context)
                            );
        'false' ->
            lager:warning("bucket ~s does not have enough tokens(~b needed) for this request"
                         ,[Bucket, Cost]
                         ),
            lager:info("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'stop', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, _AccountId, _TokenType) -> 'false'.

-spec early_authenticate(cb_context:context()) ->
                                boolean() |
                                {'true', cb_context:context()}.
early_authenticate(Context) ->
    _ = cb_context:put_reqid(Context),
    early_authenticate(Context, cb_context:auth_token_type(Context)).

-spec early_authenticate(cb_context:context(), atom() | kz_term:api_binary()) ->
                                boolean() |
                                {'true', cb_context:context()}.
early_authenticate(Context, 'x-auth-token') ->
    early_authenticate_token(Context, cb_context:auth_token(Context));
early_authenticate(_Context, _TokenType) -> 'false'.

-spec early_authenticate_token(cb_context:context(), kz_term:api_binary()) ->
                                      boolean() |
                                      {'true', cb_context:context()}.
early_authenticate_token(Context, AuthToken) when is_binary(AuthToken) ->
    validate_auth_token(Context, AuthToken);
early_authenticate_token(_Context, 'undefined') -> 'true'.

-spec check_auth_token(cb_context:context(), kz_term:api_binary(), boolean()) ->
                              boolean() |
                              {'true', cb_context:context()} |
                              {'stop', cb_context:context()}.
check_auth_token(_Context, <<>>, MagicPathed) ->
    lager:info("empty auth token - magic path'd: ~p", [MagicPathed]),
    MagicPathed;
check_auth_token(_Context, 'undefined', MagicPathed) ->
    lager:info("auth token not found - magic path'd: ~p", [MagicPathed]),
    MagicPathed;
check_auth_token(Context, AuthToken, _MagicPathed) ->
    validate_auth_token(Context, AuthToken).

-spec validate_auth_token(cb_context:context(), kz_term:ne_binary()) ->
                                 boolean() |
                                 {'true', cb_context:context()} |
                                 {'stop', cb_context:context()}.
validate_auth_token(Context, ?NE_BINARY = AuthToken) ->
    Options = [{<<"account_id">>, cb_context:req_header(Context, <<"x-auth-account-id">>)}],
    lager:debug("checking auth token"),
    case crossbar_auth:validate_auth_token(AuthToken, props:filter_undefined(Options)) of
        {'ok', JObj} ->
            is_account_expired(Context, JObj);
        {'error', Error} when Error =:= 'token_expired'
                              orelse Error =:= <<"token expired">> ->
            lager:info("provided auth token has expired"),
            {'stop', crossbar_util:response_401(Context)};
        {'error', 'not_found'} ->
            lager:info("provided auth token was not found"),
            {'stop', crossbar_util:response_401(Context)};
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec is_account_expired(cb_context:context(), kz_json:object()) ->
                                boolean() |
                                {'true', cb_context:context()} |
                                {'stop', cb_context:context()}.
is_account_expired(Context, JObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, JObj),
    case kzd_accounts:is_expired(AccountId) of
        'false' -> check_as(Context, JObj);
        {'true', Expired} ->
            _ = kz_process:spawn(fun crossbar_util:maybe_disable_account/1, [AccountId]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            Context1 = cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context),
            {'stop', Context1}
    end.

-spec check_as(cb_context:context(), kz_json:object()) ->
                      boolean() |
                      {'true', cb_context:context()}.
check_as(Context, JObj) ->
    case kz_json:get_ne_binary_value(<<"account_id">>, JObj, 'undefined') of
        'undefined' -> {'true', set_auth_doc(Context, JObj)};
        AccountId -> check_as_payload(Context, JObj, AccountId)
    end.

-spec check_as_payload(cb_context:context(), kz_json:object(), kz_term:ne_binary()) ->
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

-spec check_descendants(cb_context:context(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
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
    AuthAccountId = kz_json:get_ne_binary_value(<<"account_id">>, JObj),
    OwnerId = kz_json:get_ne_binary_value(<<"ownerid">>, JObj),
    cb_context:setters(Context
                      ,[{fun cb_context:set_auth_doc/2, JObj}
                       ,{fun cb_context:set_auth_account_id/2, AuthAccountId}
                        | maybe_add_is_admins(AuthAccountId, OwnerId)
                       ]).

-spec maybe_add_is_admins(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> cb_context:setters().
maybe_add_is_admins(?NE_BINARY = AuthAccountId, ?NE_BINARY = OwnerId) ->
    [{fun cb_context:set_is_superduper_admin/2, cb_context:is_superduper_admin(AuthAccountId)}
    ,{fun cb_context:set_is_account_admin/2, cb_context:is_account_admin(AuthAccountId, OwnerId)}
    ];
maybe_add_is_admins(?NE_BINARY = AuthAccountId, 'undefined') ->
    [{fun cb_context:set_is_superduper_admin/2, cb_context:is_superduper_admin(AuthAccountId)}];
maybe_add_is_admins(_, _) ->
    [].
