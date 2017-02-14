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
        ,delete/2, delete/3
        ]).

-include("crossbar.hrl").

-define(DEFAULT_NODE, <<"default">>).

-spec init() -> ok.
init() ->
    _ = kz_datamgr:db_create(?KZ_CONFIG_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_CONFIG_DB, crossbar, <<"views/system_configs.json">>),

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.system_configs">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.system_configs">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.system_configs">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.system_configs">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.system_configs">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.system_configs">>, ?MODULE, delete).

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id) -> cb_context:is_superduper_admin(Context).
authorize(Context, _Id, _Node) -> cb_context:is_superduper_admin(Context).

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_SystemConfigId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE, ?HTTP_PUT].
allowed_methods(_SystemConfigId, _Node) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE, ?HTTP_PUT].

-spec resource_exists() -> true.
-spec resource_exists(path_token()) -> true.
-spec resource_exists(path_token(), path_token()) -> true.
resource_exists() -> true.
resource_exists(_Id) -> true.
resource_exists(_Id, _Node) -> true.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    View = <<"system_configs/crossbar_listing">>,
    crossbar_doc:load_view(View, [], Context, fun normalize_view_results/2).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate(Context, Id, get_node(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ConfigId, Node) ->
    SchemaName = kapps_config_util:system_schema_name(ConfigId),
    try
        {ok, _Schema} = kz_json_schema:load(SchemaName),
        validate(update_db(Context), cb_context:req_verb(Context), ConfigId, Node)
    catch
        _:{badmatch, {invalid_document, Errors} = Error} ->
            lager:error("schema validation error: ~p", [parse_error(Error)]),
            cb_context:failed(Context, Errors);
        _:{badmatch,{error,enoent}} ->
            lager:error("schema validation error: no schema for config: ~p", [SchemaName]),
            cb_context:add_system_error(bad_identifier, Context);
        _:{badmatch,{error,not_found}} ->
            lager:error("schema validation error: no schema for config: ~p", [SchemaName]),
            cb_context:add_system_error(bad_identifier, Context);
        _C:E ->
            lager:error("validation generic error: ~p", [E]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            error_validation(Context)
    end.

-spec validate(cb_context:context(), http_method(), path_token(), path_token()) -> cb_context:context().

validate(Context, ?HTTP_GET, ConfigId, Node) ->
    JObj = kapps_config_util:get_system_config(ConfigId, Node),
    crossbar_doc:handle_datamgr_success(set_id(ConfigId, JObj), Context);

validate(Context, ?HTTP_DELETE, _ConfigId, _Node) ->
    pass_validation(Context, kz_json:new());

validate(Context, ?HTTP_PUT, ConfigId, Node) ->
    validate(Context, ?HTTP_POST, ConfigId, Node);

validate(Context, ?HTTP_PATCH, ConfigId, Node) ->
    Parent = kapps_config_util:get_system_config(ConfigId, Node),
    save_diff(validate_diff(Context, ConfigId, Parent), ConfigId, Node, Parent);

validate(Context, ?HTTP_POST, ConfigId, Node) ->
    Parent = kapps_config_util:get_system_config(ConfigId, Node),
    validate_diff(Context, ConfigId, Parent).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ConfigId, Node) -> post(Context, ConfigId, Node).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ConfigId) -> put(Context, ConfigId, get_node(Context)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ConfigId) -> post(Context, ConfigId, get_node(Context)).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ConfigId, Node) ->
    Parent = kapps_config_util:get_system_config(ConfigId, Node),
    save_diff(Context, ConfigId, Node, Parent).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ConfigId) -> delete(Context, ConfigId, get_node(Context)).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ConfigId, Node) ->
    Parent = kapps_config_util:get_system_config(ConfigId, Node),
    save_diff(Context, ConfigId, Node, Parent).

-spec save_diff(cb_context:context(), path_token(), path_token(), kz_json:object()) -> cb_context:context().
save_diff(Context, ConfigId, Node, Parent) ->
    JObj = cb_context:doc(Context),
    JObjDiff = kz_json:diff(JObj, Parent),
    StoredDocument = kz_json:set_value(<<"_id">>, ConfigId, maybe_new(kapps_config:get_category(ConfigId))),
    Document = kz_json:set_value(Node, JObjDiff, StoredDocument),
    flush(ConfigId, crossbar_doc:save(Context, Document, [])),
    crossbar_doc:handle_datamgr_success(set_id(ConfigId, JObj), Context).

-spec validate_diff(cb_context:context(), path_token(), kz_json:object()) -> cb_context:context().
validate_diff(Context, ConfigId, Parent) ->
    JObj = strip_id(cb_context:req_data(Context)),
    FullConfig = kz_json:merge_recursive(Parent, JObj),
    maybe_validate(ConfigId, FullConfig, Parent),
    pass_validation(Context, FullConfig).

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(ConfigId, JObj) -> kz_json:set_value(<<"id">>, ConfigId, JObj).

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).

-spec pass_validation(cb_context:context(), kz_json:object()) -> cb_context:context().
pass_validation(Context, JObj) ->
    cb_context:setters(Context,[
                                {fun cb_context:set_doc/2, JObj}
                               ,{fun cb_context:set_resp_status/2, success}
                               ]).

-spec error_validation(cb_context:context()) -> cb_context:context().
error_validation(Context) ->
    cb_context:setters(Context, [
                                 {fun cb_context:set_resp_error_code/2, 400}
                                ,{fun cb_context:set_resp_status/2, error}
                                ]).

-spec maybe_validate(ne_binary(), kz_json:object(), kz_json:object()) -> skip_validation | valid.
maybe_validate(ConfigId, ConfigObj, Parent) ->
    SchemaName = kapps_config_util:system_schema_name(ConfigId),
    case validate_schema(SchemaName, Parent) of
        valid ->
            valid = validate_schema(SchemaName, ConfigObj);
        Error ->
            ErrorMsg = parse_error(Error),
            lager:error("Parent configuration for ~p doesn't pass schema ~p validation due to: ~p", [ConfigId, SchemaName, ErrorMsg]),
            skip_validation
    end.

parse_error({invalid_document, [{data_invalid, _Schema, Error, _Doc, Path}]}) -> {Error, Path};
parse_error(X) -> X.

-spec validate_schema(api_binary(), kz_json:object()) -> no_schema_present | valid | {invalid_document, term()}.
validate_schema(Name, JObj) ->
    case kz_json_schema:load(Name) of
        {ok, Schema} ->
            case kz_json_schema:validate(Schema, JObj) of
                {ok, _} -> valid;
                {error, Error} -> {invalid_document, Error}
            end;
        _ -> no_schema_present
    end.

-spec flush(ne_binary(), ne_binarycb_context:context()) -> cb_context:context().
flush(ConfigId, Context) ->
    kapps_config:flush(ConfigId),
    Context.

-spec update_db(cb_context:context()) -> cb_context:context().
update_db(Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_account_db/2, ?KZ_CONFIG_DB}
                       ,{fun cb_context:set_account_id/2, cb_context:auth_account_id(Context)}
                       ]).

-spec normalize_view_results(kz_json:object(), ne_binaries()) -> ne_binaries().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"key">>, JObj) | Acc].

get_node(Context) ->
    cb_context:req_value(Context, <<"node">>, ?DEFAULT_NODE).

-spec maybe_new({ok, kz_json:object()}) -> kz_json:object().
maybe_new({ok, JObj}) -> JObj;
maybe_new(_) -> kz_json:new().
