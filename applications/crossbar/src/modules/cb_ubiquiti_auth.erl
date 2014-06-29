%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% Ubiquiti SSO auth module
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ubiquiti_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("../crossbar.hrl").

-define(USERNAME_LIST, <<"users/list_by_username">>).
-define(DEFAULT_LANGUAGE, <<"en-US">>).

-define(U_CONFIG_CAT, <<"crossbar.ubiquiti">>).

-define(UBIQUITI_AUTH_TOKENS, whapps_config:get_integer(?U_CONFIG_CAT, <<"tokens_per_request">>, 35)).
-define(UBIQUITI_RESELLER_ID, whapps_config:get(?U_CONFIG_CAT, <<"sso_reseller_id">>)).

-define(SSO_STAGING_URI, <<"https://sso-stage.ubnt.com/api/sso/v1/">>).
-define(SSO_PROD_URI, <<"https://sso.ubnt.com/api/sso/v1/">>).

-define(SSO_STAGING_ENV, <<"staging">>).
-define(SSO_PROD_ENV, <<"production">>).
-define(SSO_URL_KEY, <<"sso_url">>).

-define(SSO_PROVIDER, <<"ubiquiti">>).

-define(SSO_ENV, whapps_config:get(?U_CONFIG_CAT, <<"sso_environment">>, ?SSO_STAGING_ENV)).
-define(SSO_URL, whapps_config:get(?U_CONFIG_CAT, [?SSO_ENV, ?SSO_URL_KEY])).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ProdURI = whapps_config:get(?U_CONFIG_CAT, [?SSO_PROD_ENV, ?SSO_URL_KEY], ?SSO_PROD_URI),
    _StagURI = whapps_config:get(?U_CONFIG_CAT, [?SSO_STAGING_ENV, ?SSO_URL_KEY], ?SSO_STAGING_URI),

    lager:debug("SSO Environment: ~s", [?SSO_ENV]),
    lager:debug("SSO URI: ~s", [?SSO_URL]),

    case ?UBIQUITI_RESELLER_ID of
        'undefined' -> lager:error("no reseller account id for Ubiquiti has been defined");
        _ResellerId -> lager:debug("SSO Reseller Account ID: ~s", [_ResellerId])
    end,

    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ubiquiti_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ubiquiti_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ubiquiti_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ubiquiti_auth">>, ?MODULE, 'put').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"ubiquiti_auth">>, _}]) -> 'true';
authorize_nouns(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"ubiquiti_auth">>, _}]) -> 'true';
authenticate_nouns([{<<"ubiquiti_auth">>, [<<"recovery">>]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Context1 = consume_tokens(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:validate_request_data(<<"ubiquiti_auth">>, Context, fun maybe_authenticate_user/1);
        _Status -> Context1
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_util:create_auth_token(Context, wh_util:to_binary(?MODULE)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_authenticate_user(cb_context:context()) -> cb_context:context().
maybe_authenticate_user(Context) ->
    LoginURL = crossbar_util:get_path(?SSO_URL, <<"login">>),

    case ibrowse:send_req(wh_util:to_list(LoginURL)
                          ,[{"Content-Type","application/json"}]
                          ,'post'
                          ,wh_json:encode(login_req(Context))
                         )
    of
        {'ok', "200", RespHeaders, RespBody} ->
            lager:debug("successfully authenticated to '~s'", [LoginURL]),
            cb_context:setters(Context, [{fun cb_context:set_doc/2, auth_response(Context, RespHeaders, RespBody)}
                                         ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        {'ok', _RespCode, _RespHeaders, _RespBody} ->
            lager:debug("recv non-200(~s) code from '~s': ~s", [_RespCode, LoginURL, _RespBody]),
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        {'error', _Error} ->
            lager:debug("failed to query '~s': ~p", [LoginURL, _Error]),
            crossbar_util:response('error', <<"failed to query Ubiquiti SSO service">>, 500, Context)
    end.

-spec login_req(cb_context:context()) -> wh_json:object().
login_req(Context) ->
    Data = cb_context:req_data(Context),
    wh_json:set_value(<<"user">>
                      ,wh_json:get_value(<<"username">>, Data)
                      ,wh_json:delete_key(<<"username">>, Data)
                     ).

-spec auth_response(cb_context:context(), wh_proplist(), binary()) -> wh_json:object().
auth_response(_Context, _RespHeaders, RespBody) ->
    wh_json:from_list(
      [{<<"sso">>, wh_json:set_value(<<"provider">>, ?SSO_PROVIDER, wh_json:decode(RespBody))}
       ,{<<"reseller_id">>, ?UBIQUITI_RESELLER_ID}
       ,{<<"is_reseller">>, 'false'}
      ]).

-spec consume_tokens(cb_context:context()) -> cb_context:context().
consume_tokens(Context) ->
    case kz_buckets:consume_tokens_until(cb_modules_util:bucket_name(Context)
                                         ,?UBIQUITI_AUTH_TOKENS
                                        )
    of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('too_many_requests', Context)
    end.
