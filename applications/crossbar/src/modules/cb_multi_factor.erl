%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Multi factor authentication configuration API endpoint
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_multi_factor).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST_ATTEMPT_LOG, <<"auth/login_attempt_by_auth_type">>).

-define(ATTEMPTS, <<"attempts">>).
-define(ATTEMPTS_TYPE, <<"login_attempt">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize.multi_factor">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.multi_factor">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.multi_factor">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.multi_factor">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.multi_factor">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.multi_factor">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.multi_factor">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.multi_factor">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.multi_factor">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context) ->
    authorize_system_multi_factor(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context, _) ->
    authorize_system_multi_factor(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> 'true'.
authorize(_Context, _, _) -> 'true'.

-spec authorize_system_multi_factor(cb_context:context(), req_nouns(), http_method()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize_system_multi_factor(_, [{<<"multi_factor">>, []}], ?HTTP_GET) -> 'true';
authorize_system_multi_factor(C, [{<<"multi_factor">>, []}], ?HTTP_PUT) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_GET) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_POST) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], ?HTTP_PATCH) -> cb_context:is_superduper_admin(C);
authorize_system_multi_factor(C, [{<<"multi_factor">>, _}], _) -> {'stop', cb_context:add_system_error('forbidden', C)};
authorize_system_multi_factor(_, _, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?ATTEMPTS, _AttemptId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /multi_factor => []
%%    /multi_factor/foo => [<<"foo">>]
%%    /multi_factor/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?ATTEMPTS) -> 'true';
resource_exists(_ConfigId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?ATTEMPTS, _AttemptId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /multi_factor might load a list of auth objects
%% /multi_factor/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_multi_factor(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS) ->
    Options = [{'mapper', crossbar_view:map_value_fun()}
              ,{'range_keymap', <<"multi_factor">>}
              ],
    crossbar_view:load_modb(Context, ?CB_LIST_ATTEMPT_LOG, Options);
validate(Context, ConfigId) ->
    case cb_context:req_nouns(Context) of
        [{<<"multi_factor">>, _}] ->
            validate_multi_factor_config(cb_context:set_db_name(Context, ?KZ_AUTH_DB), ConfigId, cb_context:req_verb(Context));
        _ ->
            validate_multi_factor_config(Context, ConfigId, cb_context:req_verb(Context))
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?ATTEMPTS, AttemptId) ->
    read_attempt_log(AttemptId, Context).

-spec validate_multi_factor(cb_context:context(), req_nouns(), http_method()) -> cb_context:context().
validate_multi_factor(Context, [{<<"multi_factor">>, _}], ?HTTP_GET) ->
    system_summary(Context);
validate_multi_factor(Context, [{<<"multi_factor">>, _}], ?HTTP_PUT) ->
    create(cb_context:set_db_name(Context, ?KZ_AUTH_DB));
validate_multi_factor(Context, _, ?HTTP_GET) ->
    summary(Context);
validate_multi_factor(Context, _, ?HTTP_PUT) ->
    create(Context).

-spec validate_multi_factor_config(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_multi_factor_config(Context, ConfigId, ?HTTP_GET) ->
    read(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_POST) ->
    update(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_PATCH) ->
    validate_patch(ConfigId, Context);
validate_multi_factor_config(Context, ConfigId, ?HTTP_DELETE) ->
    read(ConfigId, Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"multi_factor_provider">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>)).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"multi_factor_provider">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load a login attempt log from MODB
%% @end
%%------------------------------------------------------------------------------
-spec read_attempt_log(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read_attempt_log(?MATCH_MODB_PREFIX(YYYY, MM, _) = AttemptId, Context) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    crossbar_doc:load(AttemptId
                     ,cb_context:set_db_name(Context
                                            ,kzs_util:format_account_id(cb_context:account_id(Context)
                                                                       ,Year
                                                                       ,Month
                                                                       )
                                            )
                     ,?TYPE_CHECK_OPTION(?ATTEMPTS_TYPE)
                     ).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Options = [{'startkey', [<<"multi_factor">>]}
              ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
              ,{'unchunkable', 'true'}
              ,{'mapper', crossbar_view:map_value_fun()}
              ],
    C1 = crossbar_view:load(Context, <<"auth/providers_by_type">>, Options),
    C2 = system_summary(Context),
    cb_context:set_resp_data(C1, merge_summary(cb_context:resp_data(C1), cb_context:resp_data(C2))).

system_summary(Context) ->
    Options = [{'startkey', [<<"multi_factor">>]}
              ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
              ,{'mapper', crossbar_view:map_value_fun()}
              ,{'databases', [?KZ_AUTH_DB]}
              ,{'unchunkable', 'true'}
              ],
    crossbar_view:load(Context, <<"providers/list_by_type">>, Options).

-spec merge_summary(kz_json:objects(), kz_json:objects()) -> kz_json:object().
merge_summary(Configured, Available) ->
    kz_json:from_list(
      [{<<"configured">>, Configured}
      ,{<<"multi_factor_providers">>, Available}
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = kz_json:set_value(<<"pvt_provider_type">>, <<"multi_factor">>, cb_context:doc(Context)),
    cb_context:set_doc(Context, kz_doc:set_type(Doc, <<"provider">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"provider">>)).
