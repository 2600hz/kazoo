%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communications
%%% @doc Handle CRUD operations for Tray.io solutions
%%% @end
%%% @author Dustin Brett <dustin.brett@ooma.com>
%%%-----------------------------------------------------------------------------
-module(cb_tray).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate_resource/1, validate_resource/2
        ,validate/1, validate/2
        ,post/1
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(ACCESS_TOKEN, <<"access_token">>).
-define(TRAY, <<"tray">>).
-define(ROOT_NOUN, [{?TRAY, []}]).

-define(MOD_CONFIG_CAT, <<?CONFIG_CAT/binary, ".", ?TRAY/binary>>).

-define(APP_URL, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"app_url">>, <<"https://app.tray.io">>)).
-define(REQUEST_ENDPOINT, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"gql_endpoint">>, <<"https://tray.io/graphql">>)).

-define(MASTER_TOKEN, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"master_token">>)).
-define(PARTNER_NAME, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"partner_name">>)).

-define(REQUEST_HEADERS(Token)
       ,[{"Accept", "application/json"}
        ,{"Authorization", <<"Bearer ", Token/binary>>}
        ,{"Content-Type", "application/json"}
        ]).
-define(REQUEST_BODY(Query)
       ,<<"{ \"query\": \"", Query/binary, "\" }">>
       ).

-define(DATA_PATH, <<"data">>).
-define(ERROR_PATH, <<"errors">>).
-define(USER_ID, [<<"node">>, <<"id">>]).

-define(SOLUTION_INSTANCE_POPUP_URL(SolutionInstanceId, AuthorizationCode)
       ,list_to_binary([?APP_URL, "/external/solutions/", ?PARTNER_NAME, "/configure/", SolutionInstanceId, "?code=", AuthorizationCode])
       ).

-define(LIST_AVAILABLE_SOLUTIONS
       ,{<<"query { viewer { solutions { edges { node { id title description tags customFields { key value } } } } } }">>
        ,[<<"viewer">>, <<"solutions">>, <<"edges">>]
        }).
-define(LIST_SOLUTION_INSTANCES
       ,{<<"query { viewer { solutionInstances { edges { node { id name enabled workflows { edges { node { triggerUrl } } } configValues { externalId value } created } } } } }">>
        ,[<<"viewer">>, <<"solutionInstances">>, <<"edges">>]
        }).
-define(GET_USER(AccountId)
       ,{<<"query { users(criteria: { externalUserId: \\\"", AccountId/binary, "\\\" }) { edges { node { id } } } }">>
        ,[<<"users">>, <<"edges">>]
        }).

-define(CREATE_ACCESS_TOKEN(UserId)
       ,{<<"mutation { authorize(input: { userId: \\\"", UserId/binary, "\\\" }) { accessToken } }">>
        ,[<<"authorize">>, <<"accessToken">>]
        }).
-define(CREATE_USER(AccountId, AccountName)
       ,{<<"mutation { createExternalUser(input: { externalUserId: \\\"", AccountId/binary, "\\\", name: \\\"", AccountName/binary, "\\\" }) { userId } }">>
        ,[<<"createExternalUser">>, <<"userId">>]
        }).
-define(CREATE_AUTHORIZATION_CODE(UserId)
       ,{<<"mutation { generateAuthorizationCode(input: {userId: \\\"", UserId/binary, "\\\" }) { authorizationCode } }">>
        ,[<<"generateAuthorizationCode">>, <<"authorizationCode">>]
        }).
-define(CREATE_SOLUTION_INSTANCE(SolutionId, InstanceName)
       ,{<<"mutation { createSolutionInstance(input: { solutionId: \\\"", SolutionId/binary, "\\\", instanceName: \\\"", InstanceName/binary, "\\\" }) { solutionInstance{ id } } }">>
        ,[<<"createSolutionInstance">>, <<"solutionInstance">>, <<"id">>]
        }).
-define(UPDATE_SOLUTION_INSTANCE(SolutionInstanceId, Enabled)
       ,{<<"mutation { updateSolutionInstance(input: { solutionInstanceId: \\\"", SolutionInstanceId/binary, "\\\", enabled: ", (atom_to_binary(Enabled, 'utf8'))/binary, " }) { solutionInstance { enabled } } }">>
        ,[<<"updateSolutionInstance">>, <<"solutionInstance">>, <<"enabled">>]
        }).
-define(DELETE_SOLUTION_INSTANCE(SolutionInstanceId)
       ,{<<"mutation { removeSolutionInstance(input: { solutionInstanceId: \\\"", SolutionInstanceId/binary, "\\\" }) { clientMutationId } }">>
        ,[<<"removeSolutionInstance">>, <<"clientMutationId">>]
        }).

-type request() :: {kz_term:ne_binary(), kz_term:ne_binaries()}.
-type response() :: {'ok' | 'error', kz_json:json_term()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.", ?TRAY/binary>>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.", ?TRAY/binary>>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.", ?TRAY/binary>>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.validate.", ?TRAY/binary>>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.", ?TRAY/binary>>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.", ?TRAY/binary>>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.", ?TRAY/binary>>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_SolutionInstanceId) ->
    [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_SolutionInstanceId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%% @end
%%------------------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) ->
    case {cb_context:req_verb(Context), cb_context:req_nouns(Context)} of
        {?HTTP_POST, ?ROOT_NOUN} -> cb_context:add_system_error('invalid_method', Context);
        _ -> Context
    end.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, _SolutionInstanceId) ->
    case cb_context:account_id(Context) of
        'undefined' -> cb_context:add_system_error('invalid_method', Context);
        _ -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_tray(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, SolutionInstanceId) ->
    validate_tray(Context, SolutionInstanceId, cb_context:req_verb(Context)).

-spec validate_tray(cb_context:context(), http_method()) -> cb_context:context().
validate_tray(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        ?ROOT_NOUN -> list_available_solutions(Context);
        _ -> list_solution_instances(Context)
    end;
validate_tray(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"tray_create">>, Context).

-spec validate_tray(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_tray(Context, SolutionInstanceId, ?HTTP_GET) ->
    get_solution_instance_popup_url(Context, SolutionInstanceId);
validate_tray(Context, _SolutionInstanceId, ?HTTP_PATCH) ->
    cb_context:validate_request_data(<<"tray_toggle">>, Context);
validate_tray(Context, _SolutionInstanceId, ?HTTP_DELETE) ->
    cb_context:set_resp_status(Context, 'success').

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    SolutionId = cb_context:req_value(Context, <<"id">>),
    InstanceName = cb_context:req_value(Context, <<"name">>),
    create_solution_instance(Context, SolutionId, InstanceName).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, SolutionInstanceId) ->
    Enabled = cb_context:req_value(Context, <<"enabled">>),
    update_solution_instance_state(Context, SolutionInstanceId, Enabled).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, SolutionInstanceId) ->
    delete_solution_instance(Context, SolutionInstanceId).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec request(request(), cb_context:context()) -> response().
request(Request, Context) ->
    request(Request, ?MASTER_TOKEN, Context).

-spec request(request(), kz_term:ne_binary(), cb_context:context()) -> response().
request({Query, ResponsePath} = Request, Token, Context) ->
    case kz_http:post(?REQUEST_ENDPOINT, ?REQUEST_HEADERS(Token), ?REQUEST_BODY(Query)) of
        {'ok', 403, _, ResponseBody} -> handle_authorization_issue(Request, Token, ResponseBody, Context);
        {_, _, _, ResponseBody} -> handle_response(ResponsePath, ResponseBody)
    end.

-spec handle_response(kz_term:ne_binaries(), kz_term:text()) -> response().
handle_response(ResponsePath, ResponseBody) ->
    JsonBody = kz_json:decode(ResponseBody),
    case kz_json:get_value([?ERROR_PATH], JsonBody) of
        'undefined' -> {'ok', kz_json:get_ne_value([?DATA_PATH | ResponsePath], JsonBody)};
        Error -> {'error', Error}
    end.

-spec handle_response_error(response(), cb_context:context()) -> cb_context:context().
handle_response_error({'error', [Error | _]=Errors}, Context) ->
    Message = kz_json:get_ne_value(<<"message">>, Error),
    crossbar_util:response('error', Message, 500, Errors, Context).

-spec handle_authorization_issue(request(), kz_term:ne_binary(), kz_term:text(), cb_context:context()) -> response().
handle_authorization_issue(_, 'undefined', ResponseBody, _) ->
    {'error', kz_json:decode(ResponseBody)};
handle_authorization_issue(Request, Token, ResponseBody, Context) ->
    case Token =:= ?MASTER_TOKEN of
        'true' -> {'error', kz_json:decode(ResponseBody)};
        _ -> request(Request, create_access_token(Context), Context)
    end.

-spec get_user_id(kz_term:ne_binary(), cb_context:context()) -> kz_term:api_ne_binary().
get_user_id(AccountId, Context) ->
    case request(?GET_USER(AccountId), Context) of
        {'ok', [User | _]} -> kz_json:get_ne_binary_value(?USER_ID, User);
        _ -> create_user(AccountId, Context)
    end.

-spec create_user(kz_term:ne_binary(), cb_context:context()) -> kz_term:api_ne_binary().
create_user(AccountId, Context) ->
    AccountName = cb_context:account_name(Context),
    case request(?CREATE_USER(AccountId, AccountName), Context) of
        {'ok', Response} -> Response;
        {'error', Error} ->
            lager:debug("failed to create user: ~p", [Error]),
            'undefined'
    end.

-spec get_access_token(cb_context:context()) -> kz_term:api_ne_binary().
get_access_token(Context) ->
    case get_current_access_token(Context) of
        'undefined' -> create_access_token(Context);
        AccessToken -> AccessToken
    end.

-spec get_current_access_token(cb_context:context()) -> kz_term:api_ne_binary().
get_current_access_token(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, ?TRAY) of
        {'ok', Doc} -> kz_json:get_ne_binary_value(?ACCESS_TOKEN, Doc);
        _ -> 'undefined'
    end.

-spec create_access_token(cb_context:context()) -> kz_term:api_ne_binary().
create_access_token(Context) ->
    AccountId = cb_context:account_id(Context),
    UserId = get_user_id(AccountId, Context),
    case request(?CREATE_ACCESS_TOKEN(UserId), Context) of
        {'ok', AccessToken} -> save_access_token(AccessToken, AccountId);
        {'error', Error} ->
            lager:debug("failed to create access token: ~p", [Error]),
            'undefined'
    end.

-spec save_access_token(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
save_access_token(AccessToken, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Doc = [{'update', [{?ACCESS_TOKEN, AccessToken}]}],
    kz_datamgr:update_doc(AccountDb, ?TRAY, Doc),
    AccessToken.

-spec list_available_solutions(cb_context:context()) -> cb_context:context().
list_available_solutions(Context) ->
    case request(?LIST_AVAILABLE_SOLUTIONS, Context) of
        {'ok', AvailableSolutions} -> crossbar_util:response(AvailableSolutions, Context);
        Errors -> handle_response_error(Errors, Context)
    end.

-spec list_solution_instances(cb_context:context()) -> cb_context:context().
list_solution_instances(Context) ->
    case request(?LIST_SOLUTION_INSTANCES, get_access_token(Context), Context) of
        {'ok', SolutionInstances} -> crossbar_util:response(SolutionInstances, Context);
        Errors -> handle_response_error(Errors, Context)
    end.

-spec create_solution_instance(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
create_solution_instance(Context, SolutionId, InstanceName) ->
    case request(?CREATE_SOLUTION_INSTANCE(SolutionId, InstanceName), get_access_token(Context), Context) of
        {'ok', SolutionInstanceId} -> get_solution_instance_popup_url(Context, SolutionInstanceId);
        Errors -> handle_response_error(Errors, Context)
    end.

-spec get_solution_instance_popup_url(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
get_solution_instance_popup_url(Context, SolutionInstanceId) ->
    AccountId = cb_context:account_id(Context),
    UserId = get_user_id(AccountId, Context),
    case request(?CREATE_AUTHORIZATION_CODE(UserId), Context) of
        {'ok', AuthorizationCode} ->
            Url = ?SOLUTION_INSTANCE_POPUP_URL(SolutionInstanceId, AuthorizationCode),
            Response = kz_json:from_list([{<<"popupUrl">>, Url}]),
            crossbar_util:response(Response, Context);
        Errors -> handle_response_error(Errors, Context)
    end.

-spec update_solution_instance_state(cb_context:context(), kz_term:ne_binary(), kz_json:api_json_term()) -> cb_context:context().
update_solution_instance_state(Context, SolutionInstanceId, Enabled) ->
    case request(?UPDATE_SOLUTION_INSTANCE(SolutionInstanceId, Enabled), get_access_token(Context), Context) of
        {'ok', Enabled} -> cb_context:set_resp_status(Context, 'success');
        Errors -> handle_response_error(Errors, Context)
    end.

-spec delete_solution_instance(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
delete_solution_instance(Context, SolutionInstanceId) ->
    case request(?DELETE_SOLUTION_INSTANCE(SolutionInstanceId), get_access_token(Context), Context) of
        {'ok', 'null'} -> cb_context:set_resp_status(Context, 'success');
        Errors -> handle_response_error(Errors, Context)
    end.
