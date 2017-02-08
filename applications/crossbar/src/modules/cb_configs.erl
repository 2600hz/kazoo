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
        ,allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/2
        ,post/2
        ,delete/2
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

-spec resource_exists() -> false.
-spec resource_exists(path_tokens()) -> true.
resource_exists() -> false.
resource_exists(_) -> true.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Config) ->
    try
        validate(Context, cb_context:req_verb(Context), Config)
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

-spec validate(cb_context:context(), http_method(), path_token()) -> cb_context:context().

validate(Context, ?HTTP_GET, Config) ->
    JObj = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    crossbar_doc:handle_datamgr_success(set_id(Config, JObj), Context);

validate(Context, ?HTTP_DELETE, Config) ->
    Document = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    pass_validation(Context, Document);

validate(Context, ?HTTP_PUT, Config) ->
    validate(Context, ?HTTP_POST, Config);

validate(Context, ?HTTP_POST, Config) ->
    JObj = strip_id(cb_context:req_data(Context)),
    Parent = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    FullConfig = kz_json:merge_recursive(Parent, JObj),
    maybe_validate(Config, FullConfig, Parent),
    pass_validation(Context, FullConfig).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Config) ->
    Parent = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    JObj = cb_context:doc(Context),
    JObjDiff = kz_json:diff(JObj, Parent),
    StoredDocument = kz_json:private_fields(kapps_account_config:get(cb_context:account_id(Context), Config)),
    Document = kz_json:merge_recursive(StoredDocument, JObjDiff),
    flush(crossbar_doc:save(Context, Document, [])),
    crossbar_doc:handle_datamgr_success(set_id(Config, JObj), Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Config) ->
    case kz_json:delete_key(<<"_id">>, cb_context:doc(Context)) of
        {[]} -> Context;
        _ ->
            flush(crossbar_doc:delete(Context, permanent))
    end.

% shortcuts
-spec doc_id(ne_binary()) -> ne_binary().
doc_id(Config) -> kapps_account_config:config_doc_id(Config).

-spec set_id(ne_binary(), kz_json:object()) -> kz_json:object().
set_id(Config, JObj) -> kz_json:set_value(<<"id">>, doc_id(Config), JObj).

-spec strip_id(kz_json:object()) -> kz_json:object().
strip_id(JObj) -> kz_json:delete_key(<<"id">>, JObj, prune).

-spec schema_name(ne_binary()) -> ne_binary().
schema_name(ConfigName) when is_binary(ConfigName) -> <<"system_config.", ConfigName/binary>>.

-spec pass_validation(cb_context:context()) -> cb_context:context().
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

-spec flush(cb_context:context()) -> cb_context:context().
flush(Context) ->
    kapps_account_config:flush(cb_context:account_id(Context)),
    Context.
