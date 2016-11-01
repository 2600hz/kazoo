%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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

-include("crossbar.hrl").

-define(U_CONFIG_CAT, <<"crossbar.ubiquiti">>).

-define(UBIQUITI_AUTH_TOKENS, kapps_config:get_integer(?U_CONFIG_CAT, <<"tokens_per_request">>, 35)).
-define(UBIQUITI_PROVIDER_ID, kapps_config:get(?U_CONFIG_CAT, <<"sso_provider_id">>)).

-define(SSO_STAGING_URI, <<"https://sso-stage.ubnt.com/api/sso/v1/">>).
-define(SSO_PROD_URI, <<"https://sso.ubnt.com/api/sso/v1/">>).

-define(SSO_STAGING_ENV, <<"staging">>).
-define(SSO_PROD_ENV, <<"production">>).
-define(SSO_URL_KEY, <<"sso_url">>).

-define(SSO_PROVIDER, <<"ubiquiti">>).

-define(SSO_ENV, kapps_config:get(?U_CONFIG_CAT, <<"sso_environment">>, ?SSO_STAGING_ENV)).
-define(SSO_URL, kapps_config:get(?U_CONFIG_CAT, [?SSO_ENV, ?SSO_URL_KEY])).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ProdURI = kapps_config:get(?U_CONFIG_CAT, [?SSO_PROD_ENV, ?SSO_URL_KEY], ?SSO_PROD_URI),
    _StagURI = kapps_config:get(?U_CONFIG_CAT, [?SSO_STAGING_ENV, ?SSO_URL_KEY], ?SSO_STAGING_URI),

    lager:debug("SSO Environment: ~s", [?SSO_ENV]),
    lager:debug("SSO URI: ~s", [?SSO_URL]),

    case ?UBIQUITI_PROVIDER_ID of
        'undefined' -> lager:error("no provider account id for Ubiquiti has been defined");
        _ProviderId -> lager:debug("SSO Provider Account ID: ~s", [_ProviderId])
    end,

    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ubiquiti_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ubiquiti_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ubiquiti_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ubiquiti_auth">>, ?MODULE, 'put'),
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
    crossbar_auth:create_auth_token(Context, ?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_authenticate_user(cb_context:context()) -> cb_context:context().
maybe_authenticate_user(Context) ->
    LoginURL = crossbar_util:get_path(?SSO_URL, <<"login">>),

    case kz_http:post(kz_util:to_list(LoginURL)
                     ,[{"Content-Type","application/json"}]
                     ,kz_json:encode(login_req(Context))
                     )
    of
        {'ok', 200, RespHeaders, RespBody} ->
            lager:debug("successfully authenticated to '~s'", [LoginURL]),
            cb_context:setters(Context, [{fun cb_context:set_doc/2, auth_response(Context, RespHeaders, RespBody)}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        {'ok', _RespCode, _RespHeaders, _RespBody} ->
            lager:debug("recv non-200(~p) code from '~s': ~s", [_RespCode, LoginURL, _RespBody]),
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        {'error', _Error} ->
            lager:debug("failed to query '~s': ~p", [LoginURL, _Error]),
            crossbar_util:response('error', <<"failed to query Ubiquiti SSO service">>, 500, Context)
    end.

-spec login_req(cb_context:context()) -> kz_json:object().
login_req(Context) ->
    Data = cb_context:req_data(Context),
    kz_json:set_value(<<"user">>
                     ,kz_json:get_value(<<"username">>, Data)
                     ,kz_json:delete_key(<<"username">>, Data)
                     ).

-spec auth_response(cb_context:context(), kz_proplist(), binary()) -> kz_json:object().
auth_response(_Context, _RespHeaders, RespBody) ->
    RespJObj = kz_json:decode(RespBody),
    UUID = kz_json:get_value(<<"uuid">>, RespJObj),

    maybe_add_account_information(UUID
                                 ,kz_json:from_list(
                                    [{<<"sso">>, kz_json:set_value(<<"provider">>, ?SSO_PROVIDER, RespJObj)}]
                                   )
                                 ).

-spec maybe_add_account_information(api_binary(), kz_json:object()) -> kz_json:object().
maybe_add_account_information('undefined', AuthResponse) ->
    AuthResponse;
maybe_add_account_information(UUID, AuthResponse) ->
    Options = [{'key', [?SSO_PROVIDER, UUID]}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_sso">>, Options) of
        {'ok', []} -> AuthResponse;
        {'ok', [AccountJObj]} ->
            AccountId = kz_json:get_value(<<"account_id">>, AccountJObj),
            lager:debug("found account as ~s", [AccountId]),

            kz_json:set_values([{<<"account_id">>, AccountId}
                               ,{<<"is_reseller">>, kz_services:is_reseller(AccountId)}
                               ,{<<"reseller_id">>, kz_services:find_reseller_id(AccountId)}
                               ]
                              ,AuthResponse
                              );
        {'error', _E} ->
            lager:debug("failed to query accounts for uuid ~s: ~p", [UUID, _E]),
            AuthResponse
    end.

-spec consume_tokens(cb_context:context()) -> cb_context:context().
consume_tokens(Context) ->
    case kz_buckets:consume_tokens_until(?APP_NAME
                                        ,cb_modules_util:bucket_name(Context)
                                        ,cb_modules_util:token_cost(Context, ?UBIQUITI_AUTH_TOKENS)
                                        )
    of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('too_many_requests', Context)
    end.
