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
-define(UBIQUITI_AUTH_TOKENS, whapps_config:get_integer(?CONFIG_CAT, <<"ubiquiti_auth_tokens">>, 35)).
-define(UBIQUITI_RESELLER_ID, whapps_config:get_value(?CONFIG_CAT, <<"ubiquiti_sso_reseller_id">>)).

-define(U_CONFIG_CAT, <<"ubiquiti">>).
-define(SSO_URL, whapps_config:get(?U_CONFIG_CAT, <<"sso_url">>, <<"https://sso-stage.ubnt.com/api/sso/v1/">>)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.user_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.user_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.user_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.user_auth">>, ?MODULE, 'put').

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

authorize_nouns([{<<"user_auth">>, _}]) -> 'true';
authorize_nouns(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"user_auth">>, _}]) -> 'true';
authenticate_nouns([{<<"user_auth">>, [<<"recovery">>]}]) -> 'true';
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
    _ = cb_context:put_reqid(Context),
    create_token(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_authenticate_user(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token(cb_context:context()) -> cb_context:context().
create_token(Context) ->
    JObj = cb_context:doc(Context),
    case wh_json:is_empty(JObj) of
        'true' ->
            crossbar_util:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            Token = [{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                     ,{<<"ubiquiti_sso">>, wh_json:object()}
                     ,{<<"reseller_id">>, ?UBIQUITI_RESELLER_ID}
                     ,{<<"is_reseller">>, 'false'}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {'ok', Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(crossbar_util:response_auth(JObj, AccountId, OwnerId)
                                           ,cb_context:setters(Context
                                                               ,[{fun cb_context:set_auth_token/2, AuthToken}
                                                                 ,{fun cb_context:set_auth_doc/2, Doc}
                                                                ])
                                          );
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error('invalid_credentials', Context)
            end
    end.

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
