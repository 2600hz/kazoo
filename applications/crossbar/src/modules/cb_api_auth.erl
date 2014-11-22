%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Account API auth module
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it authorizes an account level cred
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_api_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("../crossbar.hrl").

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_API, <<"accounts/listing_by_api">>).
-define(API_AUTH_TOKENS, whapps_config:get_integer(?CONFIG_CAT, <<"api_auth_tokens">>, 35)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.api_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.api_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.api_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.api_auth">>, ?MODULE, 'put').

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

authorize_nouns([{<<"api_auth">>, []}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"api_auth">>, []}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Context1 = consume_tokens(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:validate_request_data(<<"api_auth">>, Context, fun on_successful_validation/1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_util:create_auth_token(Context, ?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    ApiKey = wh_json:get_value(<<"api_key">>, cb_context:doc(Context)),

    case wh_json:is_empty(ApiKey) of
        'true' -> cb_context:add_system_error('invalid_credentials', Context);
        'false' -> validate_by_api_key(Context, ApiKey)
    end.

-spec validate_by_api_key(cb_context:context(), ne_binary()) -> cb_context:context().
-spec validate_by_api_key(cb_context:context(), ne_binary(), wh_json:object() | wh_json:objects()) ->
                                 cb_context:context().
validate_by_api_key(Context, ApiKey) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_API
                                      ,[{'key', ApiKey}]
                                      ,cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' ->
            validate_by_api_key(Context1, ApiKey, cb_context:doc(Context1));
        _Status -> Context1
    end.

validate_by_api_key(Context, ApiKey, []) ->
    lager:debug("api key '~s' not associated with any accounts"
                ,[ApiKey]
               ),
    crossbar_doc:response_bad_identifier(ApiKey, Context);
validate_by_api_key(Context, ApiKey, [Doc|_]) ->
    lager:debug("found multiple accounts with api key '~s', using '~s'"
                ,[ApiKey, wh_json:get_value(<<"id">>, Doc)]
               ),
    validate_by_api_key(Context, ApiKey, Doc);
validate_by_api_key(Context, ApiKey, Doc) ->
    lager:debug("found API key '~s' belongs to account ~s", [ApiKey, wh_json:get_value(<<"id">>, Doc)]),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_doc/2, wh_json:get_value(<<"value">>, Doc)}
                        ]
                      ).

-spec consume_tokens(cb_context:context()) -> cb_context:context().
consume_tokens(Context) ->
    case kz_buckets:consume_tokens_until(cb_modules_util:bucket_name(Context)
                                         ,?API_AUTH_TOKENS
                                        )
    of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('too_many_requests', Context)
    end.
