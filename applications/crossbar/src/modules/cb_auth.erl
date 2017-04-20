%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_auth).

-export([init/0
        ,allowed_methods/1, allowed_methods/2
        ,resource_exists/1, resource_exists/2
        ,authorize/2, authorize/3
        ,authenticate/2
        ,validate_resource/1, validate_resource/2, validate_resource/3
        ,validate/2, validate/3
        ,put/2, put/3
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
-define(PROVIDERS_VIEW, <<"providers/list_by_id">>).
-define(PROVIDERS_APP_VIEW, <<"apps/list_by_provider">>).
-define(APPS_VIEW, <<"apps/list_by_account">>).
-define(KEYS_VIEW, <<"auth/list_keys">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate.auth">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.auth">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.auth">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.validate.auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.auth">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.auth">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.auth">>, ?MODULE, 'delete'),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?TOKENINFO_PATH) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(?AUTHORIZE_PATH) -> [?HTTP_PUT];
allowed_methods(?CALLBACK_PATH) -> [?HTTP_PUT];
allowed_methods(?LINKS_PATH) -> [?HTTP_GET];
allowed_methods(?PROVIDERS_PATH) -> [?HTTP_GET];
allowed_methods(?APPS_PATH) -> [?HTTP_GET];
allowed_methods(?KEYS_PATH) -> [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?LINKS_PATH, _LinkId) -> [?HTTP_GET , ?HTTP_PUT , ?HTTP_DELETE];
allowed_methods(?PROVIDERS_PATH, _ProviderId) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE];
allowed_methods(?APPS_PATH, _AppId) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE];
allowed_methods(?KEYS_PATH, _KeyId) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> boolean().
resource_exists(?TOKENINFO_PATH) -> 'true';
resource_exists(?AUTHORIZE_PATH) -> 'true';
resource_exists(?CALLBACK_PATH) -> 'true';
resource_exists(?LINKS_PATH) -> 'true';
resource_exists(?PROVIDERS_PATH) -> 'true';
resource_exists(?APPS_PATH) -> 'true';
resource_exists(?KEYS_PATH) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(?LINKS_PATH, _LinkId) -> 'true';
resource_exists(?PROVIDERS_PATH, _ProviderId) -> 'true';
resource_exists(?APPS_PATH, _AppId) -> 'true';
resource_exists(?KEYS_PATH, _KeyId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, PathToken) ->
    authorize_nouns(Context, PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, PathToken, Id) ->
    authorize_nouns(Context, PathToken, Id, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize_nouns(cb_context:context(), path_token(), req_verb(), req_nouns()) -> boolean().
authorize_nouns(_Context, ?CALLBACK_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?AUTHORIZE_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?TOKENINFO_PATH, ?HTTP_GET, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?TOKENINFO_PATH, ?HTTP_POST, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, _, _, _) -> 'false'.

-spec authorize_nouns(cb_context:context(), path_token(), path_token(), req_verb(), req_nouns()) -> boolean().
authorize_nouns(Context, ?LINKS_PATH, _Id, ?HTTP_PUT, [{<<"auth">>, _}]) ->
    cb_context:is_authenticated(Context);
authorize_nouns(Context, ?LINKS_PATH, _Id, ?HTTP_DELETE, [{<<"auth">>, _}]) ->
    cb_context:is_authenticated(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, PathToken) ->
    authenticate_nouns(PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate_nouns(cb_context:context(), path_token(), req_nouns()) -> boolean().
authenticate_nouns(?CALLBACK_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?AUTHORIZE_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?TOKENINFO_PATH, ?HTTP_GET, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?TOKENINFO_PATH, ?HTTP_POST, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(_, _, _) -> 'false'.

-spec validate_resource(cb_context:context()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
validate_resource(Context) -> cb_context:set_account_db(Context, ?KZ_AUTH_DB).
validate_resource(Context, _Path) -> cb_context:set_account_db(Context, ?KZ_AUTH_DB).
validate_resource(Context, _Path, _Id) -> cb_context:set_account_db(Context, ?KZ_AUTH_DB).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Path) ->
    validate_path(Context, Path, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Path, Id) ->
    validate_path(Context, Path, Id, cb_context:req_verb(Context)).

-spec validate_path(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_path(Context, ?AUTHORIZE_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.authorize">>, Context, fun maybe_authorize/1);

validate_path(Context, ?CALLBACK_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.callback">>, Context, fun maybe_authenticate/1);

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

validate_path(Context, ?LINKS_PATH, ?HTTP_GET) ->
    Options = [{'key', [cb_context:auth_account_id(Context), cb_context:auth_user_id(Context)]}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?LINKS_VIEW, Options, Context, fun normalize_view/2);

validate_path(Context, ?PROVIDERS_PATH, ?HTTP_GET) ->
    crossbar_doc:load_view(?PROVIDERS_VIEW, [], Context, fun normalize_view/2);
validate_path(Context, ?PROVIDERS_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.provider">>, Context, fun add_provider/1);

validate_path(Context, ?APPS_PATH, ?HTTP_GET) ->
    Options = [{'key', account_id(Context)}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?APPS_VIEW, Options, Context, fun normalize_view/2);
validate_path(Context, ?APPS_PATH, ?HTTP_PUT) ->
    cb_context:validate_request_data(<<"auth.app">>, Context, fun add_app/1).


-spec validate_path(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().

validate_path(Context, ?APPS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));
validate_path(Context, ?APPS_PATH, Id, ?HTTP_POST) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));
validate_path(Context, ?APPS_PATH, Id, ?HTTP_DELETE) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));

validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>));
validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_POST) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>));
validate_path(Context, ?PROVIDERS_PATH, Id, ?HTTP_DELETE) ->
    Options = [{'key', Id}],
    case kz_datamgr:get_result_keys(cb_context:account_db(Context), ?PROVIDERS_APP_VIEW, Options) of
        [] -> Context;
        _ -> cb_context:add_system_error(<<"apps exist for provider">>, Context)
    end;

validate_path(Context, ?LINKS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>));
validate_path(Context, ?LINKS_PATH, Id, ?HTTP_PUT) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>));
validate_path(Context, ?LINKS_PATH, Id, ?HTTP_DELETE) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"user">>)).

-spec validate_token_info(cb_context:context(), ne_binary()) -> cb_context:context().
validate_token_info(Context, Token) ->
    Options = [{'force_profile_update', 'true'}],
    case crossbar_auth:validate_auth_token(Token, Options) of
        {'error', {Code, Error}} when is_integer(Code) ->
            lager:debug("validate token info error ~p : ~p", [Code, Error]),
            crossbar_util:response('error', Error, Code, Context);
        {'error', 'no associated account_id'} ->
            lager:debug("validate token info error : no_associated_account"),
            crossbar_util:response('error', 'no associated account_id', 404, Context);
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


-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?AUTHORIZE_PATH) ->
    crossbar_auth:create_auth_token(Context, ?MODULE);
put(Context, ?CALLBACK_PATH) ->
    crossbar_auth:create_auth_token(Context, ?MODULE);
put(Context, ?APPS_PATH) ->
    crossbar_doc:save(Context);
put(Context, ?KEYS_PATH) ->
    Context;
put(Context, ?PROVIDERS_PATH) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?LINKS_PATH, AuthId) ->
    AccountId = cb_context:auth_account_id(Context),
    OwnerId = cb_context:auth_user_id(Context),
    case kz_auth:link(AccountId, OwnerId, AuthId) of
        'ok' -> Context;
        {'error', Error} -> cb_context:add_system_error(Error, Context)
    end.

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


%%%===================================================================
%%% Internal functions
%%%===================================================================

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

-spec account_id(cb_contxt:context()) -> ne_binary().
account_id(Context) ->
    {ok, Master} = kapps_util:get_master_account_id(),
    Source = [cb_context:req_param(Context, <<"account_id">>)
             ,cb_context:account_id(Context)
             ,cb_context:auth_account_id(Context)
             ,Master
             ],
    AccountId = hd(lists:filter(fun(undefined) -> false;
                                   (_Id) -> true
                                end, Source)),
    case kz_services:is_reseller(AccountId)
        orelse AccountId =:= Master
    of
        'true' -> AccountId;
        'false' -> kz_services:get_reseller_id(AccountId)
    end.

-spec add_app(cb_context:context()) -> cb_context:context().
add_app(Context) ->
    Context.

-spec add_provider(cb_context:context()) -> cb_context:context().
add_provider(Context) ->
    Context.
