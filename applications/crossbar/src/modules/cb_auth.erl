%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_auth).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,authorize/1, authorize/2, authorize/3
        ,authenticate/1, authenticate/2
        ,validate_resource/1, validate_resource/2, validate_resource/3
        ,validate/1, validate/2, validate/3
        ,put/1, put/2, put/3
        ,post/2, post/3
        ,delete/3
        ]).

-include("crossbar.hrl").

-define(CALLBACK_PATH, <<"callback">>).

-define(AUTHORIZE_PATH, <<"authorize">>).
-define(TOKENINFO_PATH, <<"tokeninfo">>).
-define(LINKS_PATH, <<"links">>).
-define(APPS_PATH, <<"apps">>).
-define(PROVIDERS_PATH, <<"providers">>).
-define(KEYS_PATH, <<"keys">>).
-define(WHITELABEL_PATH, <<"whitelabel">>).

-define(LINKS_VIEW, <<"users/list_linked_users">>).
-define(PROVIDERS_VIEW, <<"providers/list_by_type">>).
-define(PROVIDERS_APP_VIEW, <<"apps/list_by_provider">>).
-define(APPS_VIEW, <<"apps/list_by_account">>).
-define(KEYS_VIEW, <<"auth/list_keys">>).

-define(PUBLIC_KEY_MIME, [{<<"application">>, <<"x-pem-file">>}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate.auth">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.auth">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.auth">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.auth">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.validate.auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.auth">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.auth">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.auth">>, ?MODULE, 'delete'),
    ok.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?APPS_PATH) -> [?HTTP_GET];
allowed_methods(?AUTHORIZE_PATH) -> [?HTTP_PUT];
allowed_methods(?CALLBACK_PATH) -> [?HTTP_PUT];
allowed_methods(?KEYS_PATH) -> [?HTTP_GET];
allowed_methods(?LINKS_PATH) -> [?HTTP_GET];
allowed_methods(?PROVIDERS_PATH) -> [?HTTP_GET];
allowed_methods(?TOKENINFO_PATH) -> [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?APPS_PATH, _AppId) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE];
allowed_methods(?KEYS_PATH, _KeyId) -> [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?LINKS_PATH, _LinkId) -> [?HTTP_GET , ?HTTP_PUT , ?HTTP_DELETE];
allowed_methods(?PROVIDERS_PATH, _ProviderId) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> boolean().
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> boolean().
resource_exists(?APPS_PATH) -> 'true';
resource_exists(?AUTHORIZE_PATH) -> 'true';
resource_exists(?CALLBACK_PATH) -> 'true';
resource_exists(?KEYS_PATH) -> 'true';
resource_exists(?LINKS_PATH) -> 'true';
resource_exists(?PROVIDERS_PATH) -> 'true';
resource_exists(?TOKENINFO_PATH) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(?APPS_PATH, _AppId) -> 'true';
resource_exists(?KEYS_PATH, _KeyId) -> 'true';
resource_exists(?LINKS_PATH, _LinkId) -> 'true';
resource_exists(?PROVIDERS_PATH, _ProviderId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) -> Context.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, _) -> Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, ?KEYS_PATH, _KeyId) ->
    cb_context:set_content_types_provided(Context, [{'to_json', ?JSON_CONTENT_TYPES}
                                                   ,{'to_binary', ?PUBLIC_KEY_MIME}
                                                   ]);
content_types_provided(Context, _, _) ->
    Context.
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context) ->
    authorize_nouns(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context, PathToken) ->
    authorize_nouns(Context, PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context, PathToken, Id) ->
    authorize_nouns(Context, PathToken, Id, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%% authorize /auth
-spec authorize_nouns(cb_context:context(), req_verb(), req_nouns()) -> boolean().
authorize_nouns(C, ?HTTP_PUT, [{<<"auth">>, _}]) -> authorize_action(C, cb_context:req_value(C, <<"action">>));
authorize_nouns(C, ?HTTP_PUT, [{<<"auth">>, _}, {<<"accounts">>, _}]) -> cb_context:is_account_admin(C);
authorize_nouns(C, ?HTTP_PUT, [{<<"auth">>, _}, {<<"users">>, [UserId]}, {<<"accounts">>, [AccountId]}]) ->
    cb_context:is_account_admin(C)
    %% Permit a user to reset their own signature secret
        orelse (cb_context:auth_account_id(C) =:= AccountId
                andalso cb_context:auth_user_id(C) =:= UserId
               );
authorize_nouns(C, _, _) -> {'stop', cb_context:add_system_error('forbidden', C)}.

%% authorize /auth/{nouns}
-spec authorize_nouns(cb_context:context(), path_token(), req_verb(), req_nouns()) -> boolean().
authorize_nouns(_, ?APPS_PATH,             ?HTTP_GET,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?AUTHORIZE_PATH,        ?HTTP_PUT,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?CALLBACK_PATH,         ?HTTP_PUT,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(C, ?KEYS_PATH,             ?HTTP_GET,   [{<<"auth">>, _}]) -> cb_context:is_account_admin(C);
authorize_nouns(_, ?LINKS_PATH,            ?HTTP_GET,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?PROVIDERS_PATH,        ?HTTP_GET,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?TOKENINFO_PATH,        ?HTTP_GET,   [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?TOKENINFO_PATH,        ?HTTP_POST,  [{<<"auth">>, _}]) -> 'true';
authorize_nouns(C, _, _, _) -> {'stop', cb_context:add_system_error('forbidden', C)}.

%% authorize /auth/{nouns}/{id}
-spec authorize_nouns(cb_context:context(), path_token(), path_token(), req_verb(), req_nouns()) -> boolean().
authorize_nouns(_, ?APPS_PATH,      _Id,           ?HTTP_GET,    [{<<"auth">>, _}]) -> 'true';
authorize_nouns(C, ?KEYS_PATH,      _Id,           ?HTTP_GET,    [{<<"auth">>, _}]) -> cb_context:is_account_admin(C);
authorize_nouns(C, ?KEYS_PATH,      _Id,           ?HTTP_PUT,    [{<<"auth">>, _}]) -> cb_context:is_superduper_admin(C);
authorize_nouns(_, ?LINKS_PATH,     _Id,           ?HTTP_GET,    [{<<"auth">>, _}]) -> 'true';
%% monster-ui still uses this (accounts/123/auth/links)
authorize_nouns(_, ?LINKS_PATH,     _Id,           ?HTTP_GET,    [{<<"auth">>, _}, {<<"accounts">>, _}]) -> 'true';
authorize_nouns(_, ?LINKS_PATH,     _Id,           ?HTTP_PUT,    [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?LINKS_PATH,     _Id,           ?HTTP_DELETE, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, ?PROVIDERS_PATH, _Id,           ?HTTP_GET,    [{<<"auth">>, _}]) -> 'true';
authorize_nouns(C, ?PROVIDERS_PATH, _Id,           ?HTTP_POST,   [{<<"auth">>, _}]) -> cb_context:is_superduper_admin(C);
authorize_nouns(C, ?PROVIDERS_PATH, _Id,           ?HTTP_DELETE, [{<<"auth">>, _}]) -> cb_context:is_superduper_admin(C);
authorize_nouns(C, _, _, _, _) -> {'stop', cb_context:add_system_error('forbidden', C)}.

-spec authorize_action(cb_context:context(), kz_json:api_json_term()) -> boolean().
authorize_action(C, <<"reset_signature_secret">>) -> cb_context:is_superduper_admin(C);
authorize_action(_, <<"refresh_token">>) -> 'true';
authorize_action(_, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(_) -> 'false'.

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, PathToken) ->
    authenticate_nouns(PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate_nouns(path_token(), http_method(), req_nouns()) -> boolean().
authenticate_nouns(?CALLBACK_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?AUTHORIZE_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?TOKENINFO_PATH, ?HTTP_GET, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?TOKENINFO_PATH, ?HTTP_POST, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(_, _, _) -> 'false'.

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> cb_context:set_db_name(Context, ?KZ_AUTH_DB).

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, _Path) -> cb_context:set_db_name(Context, ?KZ_AUTH_DB).

-spec validate_resource(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
validate_resource(Context, _Path, _Id) -> cb_context:set_db_name(Context, ?KZ_AUTH_DB).

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_action(Context, cb_context:req_value(Context, <<"action">>), cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Path) ->
    validate_path(Context, Path, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Path, Id) ->
    validate_path(Context, Path, Id, cb_context:req_verb(Context)).

%% validating /auth
-spec validate_action(cb_context:context(), kz_term:api_binary(), http_method()) -> cb_context:context().
validate_action(Context, <<"reset_signature_secret">>, ?HTTP_PUT) ->
    case cb_context:req_nouns(Context) of
        [{<<"auth">>, _}] -> reset_system_identity_secret(Context);
        _ -> reset_identity_secret(Context)
    end;
validate_action(Context, <<"refresh_token">>, ?HTTP_PUT) ->
    Doc = kz_json:from_list([{<<"account_id">>, cb_context:auth_account_id(Context)}
                            ,{<<"owner_id">>, cb_context:auth_user_id(Context)}
                            ]),
    Context1 = cb_context:set_doc(Context, Doc),
    crossbar_auth:create_auth_token(Context1, ?MODULE);
validate_action(Context, _Action, _Method) ->
    lager:debug("unknown action ~s on ~s", [_Action, _Method]),
    cb_context:add_system_error(<<"action required">>, Context).

-spec validate_path(cb_context:context(), path_token(), http_method()) -> cb_context:context().
%% validating /auth/authorize
validate_path(Context, ?AUTHORIZE_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.authorize">>, Context, fun maybe_authorize/1);

%% validating /auth/callback
validate_path(Context, ?CALLBACK_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.callback">>, Context, fun maybe_authenticate/1);

%% validating /auth/tokeninfo
validate_path(Context, ?TOKENINFO_PATH, ?HTTP_GET) ->
    case cb_context:req_param(Context, <<"token">>) of
        'undefined' -> crossbar_util:response('error', <<"missing token in params">>, 404, Context);
        Token -> validate_token_info(Context, Token)
    end;
validate_path(Context, ?TOKENINFO_PATH, ?HTTP_POST) ->
    case kz_json:get_ne_binary_value(<<"token">>, cb_context:req_data(Context)) of
        'undefined' ->
            lager:debug("tokeninfo called with no token in the request : ~p", [cb_context:req_data(Context)]),
            crossbar_util:response('error', <<"missing token in request">>, 404, Context);
        Token ->
            lager:debug("validating posted tokeninfo : ~p", [Token]),
            validate_token_info(Context, Token)
    end;

%% validating /auth/links
validate_path(Context, ?LINKS_PATH, ?HTTP_GET) ->
    Options = [{'key', [cb_context:auth_account_id(Context), cb_context:auth_user_id(Context)]}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?LINKS_VIEW, Options, Context, fun normalize_view/2);

%% validating /auth/providers
validate_path(Context, ?PROVIDERS_PATH, ?HTTP_GET) ->
    crossbar_doc:load_view(?PROVIDERS_VIEW, [], Context, fun normalize_view/2);
validate_path(Context, ?PROVIDERS_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.provider">>, Context, fun add_provider/1);

%% validating /auth/apps
validate_path(Context, ?APPS_PATH, ?HTTP_GET) ->
    Options = [{'key', account_id(Context)}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?APPS_VIEW, Options, Context, fun normalize_view/2);
validate_path(Context, ?APPS_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.app">>, Context, fun add_app/1);

%% validating /auth/keys
validate_path(Context, ?KEYS_PATH, ?HTTP_GET) ->
    keys_summary(Context).

-spec validate_path(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
%% validating /auth/apps/{app_id}
validate_path(Context, ?APPS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));
validate_path(Context, ?APPS_PATH, Id, ?HTTP_POST) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));
validate_path(Context, ?APPS_PATH, Id, ?HTTP_DELETE) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));

%% validating /auth/providers/{provider_id}
validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>));
validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_POST) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>));
validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_DELETE) ->
    Options = [{'key', Id}],
    case kz_datamgr:get_result_keys(cb_context:db_name(Context), ?PROVIDERS_APP_VIEW, Options) of
        {'ok', []} -> Context;
        {'ok', _} -> cb_context:add_system_error(<<"apps exist for provider">>, Context);
        {'error', _E} ->
            lager:info("failed to get result keys: ~p", [_E]),
            cb_context:add_system_error(<<"datastore error">>, Context)
    end;

%% validating /auth/links/{link_id}
validate_path(Context, ?LINKS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>));
validate_path(Context, ?LINKS_PATH, Id, ?HTTP_PUT) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>));
validate_path(Context, ?LINKS_PATH, Id, ?HTTP_DELETE) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>));

%% validating /auth/keys/{key_id}
validate_path(Context, ?KEYS_PATH, Id, ?HTTP_PUT) ->
    case cb_context:req_value(Context, <<"action">>) of
        <<"reset_private_key">> -> reset_private_key(Context, Id);
        _Action ->
            lager:debug("unknown action: ~s", [_Action]),
            cb_context:add_system_error(<<"action required">>, Context)
    end;
validate_path(Context, ?KEYS_PATH, Id, ?HTTP_GET) ->
    get_public_key(Context, Id).

-spec validate_token_info(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_token_info(Context, Token) ->
    Options = [{'force_profile_update', 'true'}],
    case crossbar_auth:validate_auth_token(Token, Options) of
        {'error', {Code, Error}} when is_integer(Code) ->
            lager:debug("validate token info error ~p : ~p", [Code, Error]),
            crossbar_util:response('error', Error, Code, Context);
        {'error', Error} ->
            lager:debug("validate token info error ~p", [Error]),
            crossbar_util:response('error', Error, 401, Context);
        {'ok', Claims} ->
            lager:debug("token is valid, sending info"),
            send_token_info(Context, Token, Claims)
    end.

-spec send_token_info(cb_context:context(), binary(), kz_json:object()) -> cb_context:context().
send_token_info(Context, Token, Claims) ->
    AccountId = kz_json:get_value(<<"account_id">>, Claims),
    OwnerId = kz_json:get_value(<<"owner_id">>, Claims),
    Props = [{<<"account_id">>, AccountId}
            ,{<<"owner_id">>, OwnerId}
            ],
    Resp = crossbar_util:response_auth(kz_json:from_list(Props), AccountId, OwnerId),
    Setters = [{fun cb_context:set_auth_token/2, Token}],
    crossbar_util:response(Resp, cb_context:setters(Context, Setters)).


-spec put(cb_context:context()) -> cb_context:context().
put(Context) -> Context.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?AUTHORIZE_PATH) ->
    crossbar_auth:create_auth_token(Context, ?MODULE);
put(Context, ?CALLBACK_PATH) ->
    crossbar_auth:create_auth_token(Context, ?MODULE);
put(Context, ?APPS_PATH) ->
    crossbar_doc:save(Context);
put(Context, ?PROVIDERS_PATH) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?LINKS_PATH, AuthId) ->
    AccountId = cb_context:auth_account_id(Context),
    OwnerId = cb_context:auth_user_id(Context),
    case kz_auth:link(AccountId, OwnerId, AuthId) of
        'ok' -> Context;
        {'error', Error} -> cb_context:add_system_error(Error, Context)
    end;
put(Context, ?KEYS_PATH, _KeyId) ->
    Context.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?TOKENINFO_PATH) ->
    Context.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?APPS_PATH, _Id) ->
    crossbar_doc:save(Context);
post(Context, ?PROVIDERS_PATH, _Id) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?LINKS_PATH, AuthId) ->
    case kz_auth:unlink(AuthId) of
        'ok' -> Context;
        {'error', Error} -> cb_context:add_system_error(Error, Context)
    end;
delete(Context, ?APPS_PATH, _Id) ->
    crossbar_doc:delete(Context);
delete(Context, ?KEYS_PATH, _Id) ->
    crossbar_doc:delete(Context);
delete(Context, ?PROVIDERS_PATH, _Id) ->
    crossbar_doc:delete(Context).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_authenticate(cb_context:context()) -> cb_context:context().
maybe_authenticate(Context) ->
    case kz_auth:authenticate(cb_context:doc(Context)) of
        {'ok', Claims} ->
            lager:debug("verified auth: ~p",[Claims]),
            Doc = kz_json:set_value(<<"Claims">>, kz_json:from_list(Claims), kz_json:new()),
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_doc/2, Doc}
                      ],
            cb_context:setters(Context, Setters);
        {'error', _R} ->
            lager:debug("error authenticating user : ~p",[_R]),
            cb_context:add_system_error('invalid_credentials', Context)
    end.

-spec maybe_authorize(cb_context:context()) -> cb_context:context().
maybe_authorize(Context) ->
    case kz_auth:validate_token(cb_context:doc(Context)) of
        {'ok', Claims} ->
            lager:debug("verified auth: ~p", [Claims]),
            Doc = kz_json:set_value(<<"Claims">>, Claims, kz_json:new()),
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_doc/2, Doc}
                      ],
            cb_context:setters(Context, Setters);
        {'error', _R} ->
            lager:debug("error authenticating user : ~p",[_R]),
            cb_context:add_system_error('invalid_credentials', Context)
    end.

-spec normalize_view(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec account_id(cb_context:context()) -> kz_term:ne_binary().
account_id(Context) ->
    {'ok', Master} = kapps_util:get_master_account_id(),
    Source = [cb_context:req_param(Context, <<"account_id">>)
             ,cb_context:account_id(Context)
             ,cb_context:auth_account_id(Context)
             ,Master
             ],
    AccountId = hd(lists:filter(fun('undefined') -> 'false';
                                   (_Id) -> 'true'
                                end, Source)),
    case kz_services_reseller:is_reseller(AccountId)
        orelse AccountId =:= Master
    of
        'true' -> AccountId;
        'false' -> kz_services_reseller:get_id(AccountId)
    end.

-spec add_app(cb_context:context()) -> cb_context:context().
add_app(Context) ->
    Context.

-spec add_provider(cb_context:context()) -> cb_context:context().
add_provider(Context) ->
    Context.

-spec reset_system_identity_secret(cb_context:context()) -> cb_context:context().
reset_system_identity_secret(Context) ->
    case kz_auth_identity:reset_system_secret() of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        {'error', _Reason} ->
            lager:warning("failed to reset system identity secret: ~p", [_Reason]),
            cb_context:add_system_error('datastore_fault', Context)
    end.

-spec reset_identity_secret(cb_context:context()) -> cb_context:context().
reset_identity_secret(Context) ->
    Claims = props:filter_undefined(
               [{<<"account_id">>, cb_context:account_id(Context)}
               ,{<<"owner_id">>, cb_context:user_id(Context)}
               ]
              ),
    case kz_auth_identity:reset_secret(Claims) of
        'ok' -> cb_context:set_resp_status(Context, 'success');
        {'error', _} -> cb_context:add_system_error('datastore_fault', Context)
    end.

-spec keys_summary(cb_context:context()) -> cb_context:context().
keys_summary(Context) ->
    #{pvt_server_key := KeyId} = kz_auth_apps:get_auth_app(<<"kazoo">>),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, [KeyId]}
              ],
    cb_context:setters(Context, Setters).

-spec reset_private_key(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
reset_private_key(Context, KeyId) ->
    case kz_auth_keys:reset_private_key(KeyId) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, KeyId, Context)
    end.

-spec get_public_key(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
get_public_key(Context, KeyId) ->
    lager:debug("trying to get public key ~s", [KeyId]),
    C1 = crossbar_doc:load(KeyId, Context, ?TYPE_CHECK_OPTION(<<"system_key">>)),
    case cb_context:resp_status(C1) of
        'success' -> load_public_key(C1, KeyId);
        _ -> C1
    end.

-spec load_public_key(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_public_key(Context, KeyId) ->
    case kz_datamgr:fetch_attachment(?KZ_AUTH_DB, KeyId, <<"private_key.pem">>) of
        {'ok', PemContents} ->
            try get_public_from_private_key(PemContents) of
                PublicKeyPem -> set_public_key_response(Context, PublicKeyPem, find_accept_type(Context))
            catch
                _T:_E ->
                    lager:debug("failed to get public key ~s: ~p:~p", [KeyId, _T, _E]),
                    cb_context:add_system_error('datastore_fault', Context)
            end;
        {'error', _Reason} ->
            lager:debug("failed to get private key ~s attachment: ~p", [KeyId, _Reason]),
            JObj = kz_json:from_list([{<<"message">>, <<"failed to get public key attachment">>}]),
            cb_context:add_system_error('datastore_fault', JObj, Context)
    end.

-spec get_public_from_private_key(binary()) -> binary().
get_public_from_private_key(PemContents) ->
    PrivateKey = kz_auth_keys:from_pem(PemContents),
    PublicKey = kz_auth_keys:get_public_key_from_private_key(PrivateKey),
    kz_auth_keys:to_pem(PublicKey).

-spec set_public_key_response(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
set_public_key_response(Context, PublicKeyPem, <<"application/json">>) ->
    RespDoc = kz_json:from_list([{<<"public_key_pem">>, PublicKeyPem}]),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, RespDoc}
              ],
    cb_context:setters(Context, Setters);
set_public_key_response(Context, PublicKeyPem, <<"application/x-pem-file">>=CT) ->
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, PublicKeyPem}
              ,{fun cb_context:add_resp_headers/2
               ,#{<<"content-type">> => CT
                 ,<<"content-disposition">> => <<"attachment; filename=public_key.pem">>
                 }
               }
              ],
    cb_context:setters(Context, Setters).

%% @doc Find Mime type we should return from Accept header or payload if provided by module
%% (temporary, better to make generic function to use across crossbar module)
-spec find_accept_type(cb_context:context()) -> kz_term:ne_binary().
find_accept_type(Context) ->
    Acceptable = accept_values(Context),
    find_accept_type(Context, Acceptable).

find_accept_type(_Context, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)|_Acceptable]) ->
    <<"application/json">>;
find_accept_type(_Context, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)|_Acceptable]) ->
    <<"application/json">>;
find_accept_type(_Context, [?MEDIA_VALUE(<<"*">>, <<"*">>, _, _, _)|_Acceptable]) ->
    <<"application/json">>;
find_accept_type(_Context, [?MEDIA_VALUE(Type, SubType, _, _, _)|_Acceptable]) ->
    case [{Type, SubType}] of
        ?PUBLIC_KEY_MIME -> <<Type/binary, "/", SubType/binary>>;
        _ -> <<"application/json">>
    end.

-spec accept_values(cb_context:context()) -> media_values().
accept_values(Context) ->
    AcceptValue = cb_context:req_header(Context, <<"accept">>),
    Tunneled = cb_context:req_value(Context, <<"accept">>),
    media_values(AcceptValue, Tunneled).

-spec media_values(kz_term:api_binary(), kz_term:api_binary()) -> media_values().
media_values('undefined', 'undefined') ->
    lager:debug("no accept headers, assuming JSON"),
    [?MEDIA_VALUE(<<"application">>, <<"json">>)];
media_values(AcceptValue, 'undefined') ->
    case cb_modules_util:parse_media_type(AcceptValue) of
        {'error', 'badarg'} -> media_values('undefined', 'undefined');
        AcceptValues -> lists:reverse(lists:keysort(2, AcceptValues))
    end;
media_values(AcceptValue, Tunneled) ->
    case cb_modules_util:parse_media_type(Tunneled) of
        {'error', 'badarg'} -> media_values(AcceptValue, 'undefined');
        TunneledValues ->
            lager:debug("using tunneled accept value ~s", [Tunneled]),
            lists:reverse(lists:keysort(2, TunneledValues))
    end.
