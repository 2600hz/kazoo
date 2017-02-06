%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_configs).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/2
        ,get/2
        ,put/2
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.configs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.configs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.configs">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.configs">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.configs">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.configs">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.configs">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.configs">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ConfigId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /configs => []
%%    /configs/foo => [<<"foo">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'false'.
-spec resource_exists(path_tokens()) -> 'true'.
resource_exists() -> false.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Config) ->
    validate(Context, cb_context:req_verb(Context), Config).

-spec validate(cb_context:context(), http_method(), path_token()) -> cb_context:context().
validate(Context, ?HTTP_GET, Config) -> validate_get(Config, Context);
validate(Context, ?HTTP_PUT, Config) -> validate_put(Config, Context);
validate(Context, ?HTTP_POST, Config) -> validate_post(Config, Context);
validate(Context, ?HTTP_PATCH, Config) -> validate_patch(Config, Context);
validate(Context, ?HTTP_DELETE, Config) -> validate_delete(Config, Context).

-spec validate_put(ne_binary(), cb_context:context()) -> cb_context:context().
validate_put(Config, Context) ->
    Id = <<(?KZ_ACCOUNT_CONFIGS)/binary, Config/binary>>,
    case kz_datamgr:lookup_doc_rev(cb_context:account_db(Context), Id) of
        {'ok', _} -> cb_context:add_system_error('datastore_conflict', Context);
        {'error', _} -> validate_post(Config, Context)
    end.

%% XXX: probably it's worth to check allowed category name (ecallmgr, crossbar, etc)?
-spec validate_get(ne_binary(), cb_context:context()) -> cb_context:context().
validate_get(_Config, Context) -> Context.

-spec validate_delete(ne_binary(), cb_context:context()) -> cb_context:context().
validate_delete(_Config, Context) -> Context.

-spec validate_post(ne_binary(), cb_context:context()) -> cb_context:context().
validate_post(Config, Context) ->
    JObj = kz_json:public_fields(cb_context:req_data(Context)),
    ResellerConfig = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    FullConfig = kz_json:merge_recursive(JObj, ResellerConfig),
    cb_context:validate_request_data(make_schema_name(Config), cb_context:set_req_data(Context, FullConfig)).

-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Config, Context) ->
    JObj = kz_json:public_fields(cb_context:req_data(Context)),
    BaseConfig = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    FullConfig = kz_json:merge_recursive(JObj, BaseConfig),
    cb_context:validate_request_data(make_schema_name(Config), cb_context:set_req_data(Context, FullConfig)).

-spec get(cb_context:context(), path_token()) -> cb_context:context().
get(Context, Config) ->
    JObj = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    cb_context:setters(Context,[{fun cb_context:set_resp_status/2, success}
        ,{fun cb_context:set_resp_data/2, JObj}
        ,{fun cb_context:set_resp_etag/2, crossbar_doc:rev_to_etag(JObj)}
    ]).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, Config) ->
    save_delta(Context, Config).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Config) ->
    save_delta(Context, Config).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Config) ->
    save_delta(Context, Config).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

-spec make_schema_name(api_ne_binary()) -> ne_binary().
make_schema_name(ConfigName) when is_binary(ConfigName) -> <<"system_config.", ConfigName/binary>>.

save_delta(Context, Config) ->
    ResellerConfig = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    JObjDiff = kz_json:diff(cb_context:doc(Context), ResellerConfig),
    crossbar_doc:save(Context, JObjDiff).
