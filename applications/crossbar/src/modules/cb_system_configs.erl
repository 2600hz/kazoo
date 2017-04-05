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
-module(cb_system_configs).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/2
        ,post/2, post/3
        ,patch/2, patch/3
        ,delete/2, delete/3
        ]).

-include("crossbar.hrl").

-define(DEFAULT, <<"default">>).

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
    _ = kz_datamgr:db_create(?KZ_CONFIG_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_CONFIG_DB, 'crossbar', <<"views/system_configs.json">>),

    _ = crossbar_bindings:bind(<<"*.authorize.system_configs">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.system_configs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.system_configs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.system_configs">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.system_configs">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.system_configs">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.system_configs">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.system_configs">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-type authorize_return() :: boolean() | {'halt', cb_context:context()}.

-spec authorize(cb_context:context()) -> authorize_return().
-spec authorize(cb_context:context(), path_token()) -> authorize_return().
-spec authorize(cb_context:context(), path_token(), path_token()) -> authorize_return().
authorize(Context) ->
    case cb_context:is_superduper_admin(Context)
        andalso cb_context:req_nouns(Context) of
        'false' -> {'halt', cb_context:add_system_error('forbidden', Context)};
        [{<<"system_configs">>, _}] -> 'true';
        _ -> {'halt', cb_context:add_system_error('bad_identifier', Context)}
    end.
authorize(Context, _Id) -> authorize(Context).
authorize(Context, _Id, _Node) -> authorize(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_SystemConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PUT, ?HTTP_PATCH].
allowed_methods(_SystemConfigId, _Node) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /system_configs => []
%%    /system_configs/foo => [<<"foo">>]
%%    /system_configs/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Id) -> 'true'.
resource_exists(_Id, _Node) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /system_configs mights load a list of system_config objects
%% /system_configs/123 might load the system_config object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    summary(set_db_to_system(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_document(set_db_to_system(Context), Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, Node) ->
    validate_document_node(set_db_to_system(Context), Id, cb_context:req_verb(Context), Node).

%%--------------------------------------------------------------------
%% Document API
%%--------------------------------------------------------------------

-spec validate_document(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_document(Context, Id, ?HTTP_GET) ->
    Config =
        case kz_term:is_true(cb_context:req_value(Context, <<"with_defaults">>, false)) of
            true -> kapps_config_doc:config_with_defaults(Id);
            false -> kapps_config_doc:config_with_default_node(Id)
        end,
    crossbar_doc:handle_datamgr_success(set_id(Id, Config), Context);

validate_document(Context, Id, ?HTTP_PUT) ->
    validate_document(Context, Id, ?HTTP_POST);

validate_document(Context, Id, ?HTTP_POST) ->
    RequestData = strip_id(kz_doc:public_fields(cb_context:req_data(Context))),
    DefaultConfig = kapps_config_doc:default_config(Id, kapps_config_doc:get_keys(RequestData)),
    FullConfig = kz_json:merge_recursive(DefaultConfig, RequestData),
    validate_document_request(Context, Id, FullConfig);

validate_document(Context, Id, ?HTTP_PATCH) ->
    RequestData = strip_id(kz_doc:public_fields(cb_context:req_data(Context))),
    StoredConfig = kapps_config_doc:stored_config(Id, kapps_config_doc:get_keys(RequestData)),
    FullConfig = kz_json:merge_recursive(StoredConfig, RequestData),
    validate_document_request(Context, Id, FullConfig);

validate_document(Context, Id, ?HTTP_DELETE) ->
    read_for_delete(Id, Context).

validate_document_request(Context, Id, FullConfig) ->
    cb_context:validate_request_data(kapps_config_util:system_config_document_schema(Id), cb_context:set_req_data(Context, FullConfig),
                                     fun(Ctx) ->
                                             Diff = kapps_config_doc:diff_from_default(Id, FullConfig),
                                             Doc = maybe_set_private_fields(Id, Diff),
                                             cb_context:set_doc(Ctx, Doc)
                                     end
                                    ).


%%--------------------------------------------------------------------
%% Node API
%%--------------------------------------------------------------------

-spec validate_document_node(cb_context:context(), path_token(), http_method(), api_ne_binary()) -> cb_context:context().
validate_document_node(Context, Id, ?HTTP_GET, Node) ->
    Config =
        case kz_term:is_true(cb_context:req_value(Context, <<"with_defaults">>, false)) of
            true -> set_id(Id, Node, kapps_config_doc:stored_node(Id, Node));
            false -> set_id(Id, Node, kz_json:get_value(Node, kapps_config_doc:get_config(Id), kz_json:new()))
        end,
    crossbar_doc:handle_datamgr_success(Config, Context);

validate_document_node(Context, Id, ?HTTP_PUT, Node) -> validate_document_node(Context, Id, ?HTTP_POST, Node);

validate_document_node(Context, Id, ?HTTP_POST, Node) ->
    RequestData = strip_id(kz_doc:public_fields(cb_context:req_data(Context))),
    DefaultConfig = kapps_config_doc:default_node(Id, Node),
    FullConfig = kz_json:merge_recursive(DefaultConfig, RequestData),
    validate_node_request(Context, Id, Node, FullConfig);

validate_document_node(Context, Id, ?HTTP_PATCH, Node) ->
    RequestData = strip_id(kz_doc:public_fields(cb_context:req_data(Context))),
    StoredConfig = kapps_config_doc:stored_node(Id, Node),
    FullConfig = kz_json:merge_recursive(StoredConfig, RequestData),
    validate_node_request(Context, Id, Node, FullConfig);

validate_document_node(Context, Id, ?HTTP_DELETE, _Node) ->
    read_for_delete(Id, Context).

validate_node_request(Context, Id, Node, FullConfig) ->
    cb_context:validate_request_data(kapps_config_util:system_schema(Id), cb_context:set_req_data(Context, FullConfig),
                                     fun(Ctx) ->
                                             NodeValue = kapps_config_doc:diff_node_from_default(Id, Node, FullConfig),
                                             Doc = kz_json:set_value(Node, NodeValue, kapps_config_doc:get_config(Id)),
                                             cb_context:set_doc(Ctx, maybe_set_private_fields(Id, Doc))
                                     end
                                    ).



%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _Id) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    crossbar_doc:save(Context).
post(Context, Id, Node) ->
    Ctx = crossbar_doc:save(Context),
    case cb_context:resp_status(Ctx) of
        success ->
            cb_context:set_resp_data(Ctx, set_id(Id, Node, kz_json:get_value(Node, cb_context:doc(Ctx))));
        _ -> Ctx
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).
patch(Context, Id, Node) ->
    post(Context, Id, Node).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) ->
                    cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) ->
                    cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), api_object() | kz_json:objects()) ->
                    cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context, 'permanent').

delete(Context, Id, Node) ->
    delete(Context, Id, Node, cb_context:doc(Context)).

delete(Context, Id, _Node, 'undefined') ->
    crossbar_util:response_bad_identifier(Id, Context);
delete(Context, Id, Node, Doc) ->
    case kz_json:get_ne_value(Node, Doc) of
        'undefined' -> crossbar_util:response_bad_identifier(Node, Context);
        _NodeValue ->
            Context1 = cb_context:set_doc(Context, kz_json:delete_key(Node, Doc)),
            cb_context:set_resp_data(crossbar_doc:save(Context1), set_id(Id, Node, kz_json:new()))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read_for_delete(ne_binary(), cb_context:context()) -> cb_context:context().
read_for_delete(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"config">>)),
    case cb_context:resp_status(Context) of
        'success' -> Context1;
        _Status ->
            lager:debug("failed to find ~s(~s) for delete", [Id, _Status]),
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    View = <<"system_configs/crossbar_listing">>,
    crossbar_doc:load_view(View, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), ne_binaries()) -> ne_binaries().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"key">>, JObj) | Acc].

-spec set_db_to_system(cb_context:context()) -> cb_context:context().
set_db_to_system(Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_account_db/2, ?KZ_CONFIG_DB}
                       ,{fun cb_context:set_account_id/2, cb_context:auth_account_id(Context)}
                       ]).

-spec maybe_set_private_fields(ne_binary(), kz_json:object()) -> kz_json:object().
maybe_set_private_fields(ConfigId, JObj) ->
    case kapps_config:get_category(ConfigId) of
        {ok, Doc} -> kz_json:merge_recursive(JObj, kz_doc:private_fields(Doc));
        _ -> kz_doc:set_id(JObj, ConfigId)
    end.

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(Id, JObj) ->
    kz_json:set_value(<<"id">>, Id, JObj).

-spec set_id(ne_binary(), ne_binary(), kz_json:object()) -> kz_json:object().
set_id(Id, Node, JObj) ->
    kz_json:set_value(<<"id">>, <<Id/binary, "/", Node/binary>>, JObj).

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj).
