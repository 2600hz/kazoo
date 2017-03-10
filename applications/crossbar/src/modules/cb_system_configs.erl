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
        ,delete/2, delete/3, make_default/2, make_schema/1
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

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
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
-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id, _Node) -> cb_context:is_superduper_admin(Context).

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
    summary(update_db(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_verb(update_db(Context), Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, Node) ->
    validate_system_config(update_db(Context), Id, cb_context:req_verb(Context), Node).

-spec validate_verb(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_verb(Context, Id, ?HTTP_GET) ->
    JObj = get_system_config(Id),
    crossbar_doc:handle_datamgr_success(set_id(Id, JObj), Context);

validate_verb(Context, Id, ?HTTP_PUT) ->
    validate_verb(Context, Id, ?HTTP_POST);

validate_verb(Context, Id, ?HTTP_POST) ->
    RequestData = kz_json:public_fields(strip_id(cb_context:req_data(Context))),
    Doc = maybe_new(kapps_config:get_category(Id)),
    Default = make_default(Id, kz_json:get_keys(RequestData)),
    validate_request(Context, Id, make_schema(Id), kz_json:merge_recursive(Doc, Default));

validate_verb(Context, Id, ?HTTP_PATCH) ->
    Doc = maybe_new(kapps_config:get_category(Id)),
    validate_request(Context, Id, make_schema(Id), kz_json:merge_recursive(Doc, get_system_config(Id)));

validate_verb(Context, Id, ?HTTP_DELETE) ->
    read_for_delete(Id, Context).

-spec validate_system_config(cb_context:context(), path_token(), http_method(), api_ne_binary()) -> cb_context:context().
validate_system_config(Context, Id, ?HTTP_GET, Node) ->
    JObj = get_system_config(Id, Node),
    crossbar_doc:handle_datamgr_success(set_id(Id, Node, JObj), Context);

validate_system_config(Context, Id, ?HTTP_PUT, Node) ->
    validate_with_parent(Context, Id, Node, default(Id));

validate_system_config(Context, Id, ?HTTP_POST, Node) ->
    validate_with_parent(Context, Id, Node, default(Id));

validate_system_config(Context, Id, ?HTTP_PATCH, Node) ->
    validate_with_parent(Context, Id, Node, get_system_config(Id, Node));

validate_system_config(Context, Id, ?HTTP_DELETE, _Node) ->
    read_for_delete(Id, Context).

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
post(Context, Id) ->
    Ctx = crossbar_doc:save(Context),
    case cb_context:resp_status(Ctx) of
        success ->
            cb_context:set_resp_data(Ctx, set_id(Id, get_system_config(Id)));
        _ -> Ctx
    end.
post(Context, Id, Node) ->
    Ctx = crossbar_doc:save(Context),
    case cb_context:resp_status(Ctx) of
        success ->
            cb_context:set_resp_data(Ctx, set_id(<<Id/binary,"/",Node/binary>>, strip_iid(get_system_config(Id, Node))));
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
delete(Context, _Id, Node, Doc) ->
    case kz_json:get_ne_value(Node, Doc) of
        'undefined' -> crossbar_util:response_bad_identifier(Node, Context);
        _NodeValue ->
            Context1 = cb_context:set_doc(Context, kz_json:delete_key(Node, Doc)),
            crossbar_doc:save(Context1)
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

-spec update_db(cb_context:context()) -> cb_context:context().
update_db(Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_account_db/2, ?KZ_CONFIG_DB}
                       ,{fun cb_context:set_account_id/2, cb_context:auth_account_id(Context)}
                       ]).

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(ConfigId, JObj) -> kz_json:set_value(<<"id">>, ConfigId, JObj).

-spec set_id(ne_binary(), ne_binary(), kz_json:object()) -> kz_json:object().
set_id(ConfigId, Node, JObj) ->
    kz_doc:set_id(JObj, <<ConfigId/binary, "/", Node/binary>>).

maybe_set_id(ConfigId, JObj) ->
    case kapps_config:get_category(ConfigId) of
        {ok, Doc} -> kz_json:merge_recursive(Doc, JObj);
        _ -> kz_doc:set_id(JObj, ConfigId)
    end.

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).

-spec strip_iid(kz_json:object()) -> kz_json:object().
strip_iid(JObj) -> kz_json:delete_key(<<"_id">>, JObj, prune).

-spec validate_with_parent(cb_context:context(), ne_binary(), ne_binary(), kz_json:object()) -> cb_context:context().
validate_with_parent(Context, ConfigId, Node, Parent) ->
    RequestData = strip_id(kz_json:public_fields(cb_context:req_data(Context))),
    FullConfig = kz_json:merge_recursive(Parent, RequestData),
    Schema = kapps_config_util:system_schema_name(ConfigId),
    cb_context:validate_request_data(Schema, cb_context:set_req_data(Context, FullConfig),
                                     fun(Ctx) ->
                                             Doc = kz_json:set_value(Node, kz_json:diff(RequestData, Parent),
                                                                     kz_doc:set_id(maybe_new(kapps_config:get_category(ConfigId)), ConfigId)),
                                             cb_context:set_doc(Ctx, Doc)
                                     end
                                    ).

-spec validate_request(cb_context:context(), ne_binary(), ne_binary(), kz_json:object()) -> cb_context:context().
validate_request(Context, Id, Schema, Parent) ->
    RequestData = strip_id(kz_json:public_fields(cb_context:req_data(Context))),
    FullConfig = kz_json:merge_recursive(strip_id(kz_json:public_fields(Parent)), RequestData),
    cb_context:validate_request_data(Schema, cb_context:set_req_data(Context, FullConfig),
                                     fun(Ctx) ->
                                             Doc = maybe_set_id(Id, kz_json:diff(RequestData, Parent)),
                                             cb_context:set_doc(Ctx, Doc)
                                     end
                                    ).

-spec maybe_new({ok, kz_json:object()}) -> kz_json:object().
maybe_new({ok, JObj}) -> JObj;
maybe_new(_) -> kz_json:new().

default(Config) ->
    kz_doc:set_id(kz_json_schema:default_object(kapps_config_util:system_schema(Config)), Config).

-spec get_system_config(ne_binary(), ne_binary()) -> kz_json:object().
get_system_config(Config, Node) ->
    Default = default(Config),
    System = maybe_new(kapps_config:get_category(Config)),
    SystemNode = kz_json:get_value(Node, System, kz_json:new()),
    kz_json:merge_recursive(Default, SystemNode).

-spec get_system_config(ne_binary()) -> kz_json:object().
get_system_config(Config) ->
    System = maybe_new(kapps_config:get_category(Config)),
    Default = make_default(Config, maybe_add_default(kz_json:get_keys(strip_id(kz_json:public_fields(System))))),
    kz_json:merge_recursive(Default, System).

-spec make_schema(ne_binary()) -> kz_json:object().
make_schema(Id) ->
    Flat = [
            {<<"$schema">>,<<"http://json-schema.org/draft-04/schema#">>}
           ,{<<"id">>, <<"system_config">>}
           ,{[<<"patternProperties">>, <<".+">>, <<"$ref">>], kapps_config_util:system_schema_name(Id)}
           ,{[<<"patternProperties">>, <<".+">>, <<"type">>], <<"object">>}
           ,{<<"type">>, <<"object">>}
           ],
    kz_json:expand(kz_json:from_list(Flat)).

-spec make_default(ne_binary(), [ne_binary()]) -> kz_json:object().
make_default(Id, Keys) ->
    lists:foldl(fun(K, Json) -> kz_json:set_value(K, default(Id), Json) end, kz_json:new(), Keys).

maybe_add_default(Keys) ->
    case lists:member(?DEFAULT, Keys) of
        false -> [?DEFAULT | Keys ];
        true -> Keys
    end.
