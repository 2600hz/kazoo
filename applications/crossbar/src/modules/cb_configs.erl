%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_configs).

-export([init/0
        ,allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/2, validate/3
        ,post/2, post/3
        ,delete/2, delete/3
        ]).

-include("crossbar.hrl").
-define(DEFAULT_NODE, <<"default">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.configs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.configs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.configs">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.configs">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.configs">>, ?MODULE, 'delete').

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Config) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE].
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_Node, _Config) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE].

-spec resource_exists() -> false.
-spec resource_exists(path_tokens()) -> true.
-spec resource_exists(path_tokens(), path_tokens()) -> true.
resource_exists() -> false.
resource_exists(_) -> true.
resource_exists(_, _) -> true.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Config) ->
    validate(Context, Config, cb_context:req_value(Context, <<"node">>, ?DEFAULT_NODE)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Config, Node) ->
    try
        validate(Context, cb_context:req_verb(Context), Config, Node)
    catch
        _:{badmatch, {invalid_document, Errors}} ->
            lager:error("schema validation error: ~p", [Errors]),
            cb_context:failed(Context, Errors);
        _C:E ->
            lager:error("validation generic error: ~p", [E]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            error_validation(Context)
    end.

-spec validate(cb_context:context(), http_method(), path_token(), path_token()) -> cb_context:context().

validate(Context, ?HTTP_GET, Config, Node) ->
    JObj = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    crossbar_doc:handle_datamgr_success(set_id(Config, node(Node, JObj)), Context);

validate(Context, ?HTTP_DELETE, Config, Node) ->
    Document = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    pass_validation(Context, kz_json:delete_key(Node, Document));

validate(Context, ?HTTP_PUT, Config, Node) ->
    validate(Context, ?HTTP_POST, Node, Config);

validate(Context, ?HTTP_POST, Config, Node) ->
    JObj = strip_id(cb_context:req_data(Context)),
    Parent = node(Node, kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config)),
    FullConfig = kz_json:merge_recursive(Parent, JObj),
    maybe_validate(Config, FullConfig, Parent),
    pass_validation(Context, FullConfig).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Config) -> post(Context, Config, cb_context:req_value(Context, <<"node">>, ?DEFAULT_NODE)).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, Config, Node) ->
    save_delta(Context, Config, Node).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Config) ->
    delete(Context, Config, cb_context:req_value(Context, <<"node">>, ?DEFAULT_NODE)).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _Node, _Config) ->
    crossbar_doc:save(Context).

save_delta(Context, Config, Node) ->
    Parent = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    JObj = cb_context:doc(Context),
    JObjDiff = kz_json:diff(JObj, node(Node, Parent)),
    StoredDocument = kz_json:private_fields(kapps_account_config:get(cb_context:account_id(Context), Config)),
    Document = kz_json:merge_recursive(StoredDocument, kz_json:set_value(Node, JObjDiff, kz_json:new())),
    crossbar_doc:save(Context, Document, []),
    crossbar_doc:handle_datamgr_success(set_id(Config, JObj), Context).

% shortcuts
node(Node, JObj) -> kz_json:get_value(Node, JObj, kz_json:new()).
doc_id(Config) -> kapps_account_config:config_doc_id(Config).
set_id(Config, JObj) -> kz_json:set_value(<<"id">>, doc_id(Config), JObj).
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).
schema_name(ConfigName) when is_binary(ConfigName) -> <<"system_config.", ConfigName/binary>>.


pass_validation(Context, JObj) ->
    cb_context:setters(Context,[
         {fun cb_context:set_doc/2, JObj}
        ,{fun cb_context:set_resp_status/2, success}
    ]).

error_validation(Context) ->
    cb_context:setters(Context, [
        {fun cb_context:set_resp_error_code/2, 400}
        ,{fun cb_context:set_resp_status/2, error}
    ]).

-spec maybe_validate(ne_binary(), kz_json:object(), kz_json:object()) -> skip_validation | valid.
maybe_validate(ConfigName, Config, Parent) ->
    SchemaName = schema_name(ConfigName),
    case validate_schema(SchemaName, Parent) of
        valid ->
            valid = validate_schema(SchemaName, Config);
        Error ->
            ErrorMsg = parse_error(Error),
            lager:error("Parent configuration for ~p doesn't pass schema ~p validation due to: ~p", [ConfigName, SchemaName, ErrorMsg]),
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
