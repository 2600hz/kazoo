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
        ,validate/2, validate/3
        ,put/2, put/3
        ,post/3
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
-define(APPS_VIEW, <<"apps/list_by_account_id">>).
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
allowed_methods(?TOKENINFO_PATH) -> [?HTTP_GET];
allowed_methods(?AUTHORIZE_PATH) -> [?HTTP_PUT];
allowed_methods(?CALLBACK_PATH) -> [?HTTP_PUT];
allowed_methods(?LINKS_PATH) -> [?HTTP_GET];
allowed_methods(?PROVIDERS_PATH) -> [?HTTP_GET];
allowed_methods(?APPS_PATH) -> [?HTTP_GET];
allowed_methods(?KEYS_PATH) -> [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?WHITELABEL_PATH, _Id) -> [?HTTP_GET];
allowed_methods(?LINKS_PATH, _Id) -> [?HTTP_GET , ?HTTP_PUT , ?HTTP_DELETE];
allowed_methods(?PROVIDERS_PATH, _Id) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE];
allowed_methods(?APPS_PATH, _Id) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE];
allowed_methods(?KEYS_PATH, _Id) -> [?HTTP_GET , ?HTTP_POST , ?HTTP_DELETE].


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
resource_exists(?WHITELABEL_PATH, _Id) -> 'true';
resource_exists(?LINKS_PATH, _Id) -> 'true';
resource_exists(?PROVIDERS_PATH, _Id) -> 'true';
resource_exists(?APPS_PATH, _Id) -> 'true';
resource_exists(?KEYS_PATH, _Id) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, PathToken) ->
    authorize_nouns(Context, PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize_nouns(_Context, ?CALLBACK_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?AUTHORIZE_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?TOKENINFO_PATH, ?HTTP_GET, [{<<"auth">>, _}]) -> 'true';

authorize_nouns(_Context, ?APPS_PATH, _, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?PROVIDERS_PATH, _, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?KEYS_PATH, _, [{<<"auth">>, _}]) -> 'true';

authorize_nouns(_Context, ?APPS_PATH, ?HTTP_GET, [{<<"auth">>, _} | _]) -> 'true';
authorize_nouns(_Context, ?PROVIDERS_PATH, _, [{<<"auth">>, _} | _]) -> 'true';
authorize_nouns(_Context, ?KEYS_PATH, _, [{<<"auth">>, _} | _]) -> 'true';

authorize_nouns(Context, ?LINKS_PATH, ?HTTP_GET, [{<<"auth">>, _}]) ->
    cb_context:is_authenticated(Context);
authorize_nouns(_, _, _, _) -> 'false'.

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, PathToken, Id) ->
    authorize_nouns(Context, PathToken, Id, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize_nouns(_Context, ?WHITELABEL_PATH, _Id, _Verb, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?LINKS_PATH, _Id, _Verb, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?APPS_PATH, _Id, _Verb, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?PROVIDERS_PATH, _Id, _Verb, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_Context, ?KEYS_PATH, _Id, _Verb, [{<<"auth">>, _}]) -> 'true';
authorize_nouns(_, _, _, _, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, PathToken) ->
    authenticate_nouns(PathToken, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate_nouns(?CALLBACK_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?AUTHORIZE_PATH, ?HTTP_PUT, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?TOKENINFO_PATH, ?HTTP_GET, [{<<"auth">>, _}]) -> 'true';
authenticate_nouns(?APPS_PATH, ?HTTP_GET, [{<<"auth">>, _} | _]) -> 'true';
authenticate_nouns(_, _, _) -> 'false'.

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
    validate_path(cb_context:set_account_db(Context, ?KZ_AUTH_DB), Path, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Path, Id) ->
    validate_path(cb_context:set_account_db(Context, ?KZ_AUTH_DB), Path, Id, cb_context:req_verb(Context)).

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

validate_path(Context, ?LINKS_PATH, ?HTTP_GET) ->
    Options = [{'key', [cb_context:auth_account_id(Context), cb_context:auth_user_id(Context)]}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?LINKS_VIEW, Options, Context, fun normalize_view/2);

validate_path(Context, ?PROVIDERS_PATH, ?HTTP_GET) ->
    crossbar_doc:load_view(?PROVIDERS_VIEW, ['include_docs'], Context, fun normalize_view_doc/2);

validate_path(Context, ?APPS_PATH, ?HTTP_GET) ->
    Options = [{'key', account_id(Context)}
              ,'include_docs'
              ],
    crossbar_doc:load_view(?APPS_VIEW, Options, Context, fun normalize_view/2).


-spec validate_path(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_path(Context, ?WHITELABEL_PATH, Domain, ?HTTP_GET) ->
    crossbar_doc:load(Domain, Context, ?TYPE_CHECK_OPTION(<<"app">>));

validate_path(Context, ?APPS_PATH, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"app">>));

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
        {'ok', Claims} -> send_token_info(Context, Token, Claims)
    end.

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
    Context;
put(Context, ?KEYS_PATH) ->
    Context;
put(Context, ?PROVIDERS_PATH) ->
    Context.

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?LINKS_PATH, AuthId) ->
    AccountId = cb_context:auth_account_id(Context),
    OwnerId = cb_context:auth_user_id(Context),
    case kz_auth:link(AccountId, OwnerId, AuthId) of
        'ok' -> Context;
        {'error', Error} -> cb_context:add_system_error(Error, Context)
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?APPS_PATH, _Id) ->
    Context;
post(Context, ?PROVIDERS_PATH, _Id) ->
    Context.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?LINKS_PATH, AuthId) ->
    lager:debug("UNLINKING ~p", [AuthId]),
    case kz_auth:unlink(AuthId) of
        'ok' -> Context;
        {'error', Error} -> cb_context:add_system_error(Error, Context)
    end;
delete(Context, ?APPS_PATH, _Id) ->
    Context;
delete(Context, ?KEYS_PATH, _Id) ->
    Context;
delete(Context, ?PROVIDERS_PATH, _Id) ->
    Context.

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

-spec normalize_view_doc(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_doc(JObj, Acc) ->
    [kz_json:get_value(<<"doc">>, JObj)|Acc].

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
