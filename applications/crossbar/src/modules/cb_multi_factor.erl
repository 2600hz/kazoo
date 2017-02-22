%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% Multi factor authentication configuration API endpoint
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_multi_factor).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(LISTS_BY_TYPE, <<"multi_factor/lists_by_type">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.multi_factor">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.multi_factor">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.multi_factor">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.multi_factor">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.multi_factor">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.multi_factor">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.multi_factor">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.multi_factor">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /multi_factor => []
%%    /multi_factor/foo => [<<"foo">>]
%%    /multi_factor/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_ConfigId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /multi_factor mights load a list of auth objects
%% /multi_factor/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_multi_factor(Context, cb_context:req_verb(Context)).
validate(Context, ConfigId) ->
    validate_auth_config(Context, ConfigId, cb_context:req_verb(Context)).

-spec validate_multi_factor(cb_context:context(), http_method()) -> cb_context:context().
validate_multi_factor(Context, ?HTTP_GET) ->
    summary(Context);
validate_multi_factor(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_auth_config(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_auth_config(Context, ConfigId, ?HTTP_GET) ->
    read(ConfigId, Context);
validate_auth_config(Context, ConfigId, ?HTTP_POST) ->
    update(ConfigId, Context);
validate_auth_config(Context, ConfigId, ?HTTP_PATCH) ->
    validate_patch(ConfigId, Context);
validate_auth_config(Context, ConfigId, ?HTTP_DELETE) ->
    read(ConfigId, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"auth_provider">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"auth_provider">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"auth_provider">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    ViewOptions = [{'startkey', [<<"multi_factor">>]}
                  ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
                  ],
    add_available_providers(
      crossbar_doc:load_view(?LISTS_BY_TYPE, ViewOptions, Context, fun normalize_view_results/2)
     ).

add_available_providers(Context) ->
    ViewOptions = [{'startkey', [<<"multi_factor">>]}
                  ,{'endkey', [<<"multi_factor">>, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_AUTH_DB, <<"auth/enabled_providers_by_type">>, ViewOptions) of
        {'error', _R} -> Context;
        {'ok', JObjs} ->
            Available = [kz_json:get_value(<<"value">>, J)
                         || J <- JObjs
                        ],
            cb_context:set_resp_data(Context, merge_summary(Context, Available))
    end.

merge_summary(Context, Available) ->
    merge_summary(Context, Available, cb_context:resp_status(Context)).

merge_summary(Context, Available, 'success') ->
    kz_json:from_list(
      [{<<"configured">>, cb_context:doc(Context)}
      ,{<<"available_system_provider">>, Available}
      ]
     );
merge_summary(_Context, Available, _) ->
    kz_json:from_list([{<<"available_system_provider">>, Available}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"auth_provider">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"auth_provider">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
