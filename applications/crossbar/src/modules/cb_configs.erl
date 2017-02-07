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
        ,post/2
        ,delete/2
        ]).

-export([validate_schema/2]).

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
    _ = crossbar_bindings:bind(<<"*.execute.post.configs">>, ?MODULE, 'post'),
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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
validate(Context, ?HTTP_GET, Config) -> validate_get(Config, Context);
validate(Context, ?HTTP_POST, Config) -> validate_post(Config, Context);
validate(Context, ?HTTP_DELETE, Config) -> validate_delete(Config, Context).

%% XXX: probably it's worth to check allowed category name (ecallmgr, crossbar, etc)?
-spec validate_get(ne_binary(), cb_context:context()) -> cb_context:context().
validate_get(Config, Context) ->
    JObj = kapps_account_config:get_category(cb_context:account_id(Context), Config),
    crossbar_doc:handle_datamgr_success(JObj, Context).

-spec validate_delete(ne_binary(), cb_context:context()) -> cb_context:context().
validate_delete(Config, Context) ->
    pass_validation(Context, kapps_account_config:get(cb_context:account_id(Context), Config)).

-spec validate_post(ne_binary(), cb_context:context()) -> cb_context:context().
validate_post(Config, Context) ->
    JObj = kz_json:public_fields(cb_context:req_data(Context)),
    Parent = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    FullConfig = kz_json:merge_recursive(Parent, JObj),
    maybe_validate(Config, FullConfig, Parent),
    pass_validation(Context, FullConfig).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Config) ->
    save_delta(Context, Config).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context, permanent).

-spec schema_name(api_ne_binary()) -> ne_binary().
schema_name(ConfigName) when is_binary(ConfigName) -> <<"system_config.", ConfigName/binary>>.

save_delta(Context, Config) ->
    Parent = kapps_account_config:get_reseller_category(cb_context:account_id(Context), Config),
    JObjDiff = kz_json:diff(cb_context:doc(Context), Parent),
    StoredDocument = kz_json:private_fields(kapps_account_config:get(cb_context:account_id(Context), Config)),
    Document = kz_json:merge_recursive(StoredDocument, JObjDiff),
    crossbar_doc:save(Context, Document, []).

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

maybe_validate(ConfigName, Config, Parent) ->
    SchemaName = schema_name(ConfigName),
    case validate_schema(SchemaName, Parent) of
        valid ->
            valid = validate_schema(SchemaName, Config);
        Error ->
            lager:error("Parent configuration for ~p doesn't pass schema ~p validation due to: ~p", [ConfigName, SchemaName, Error]),
            skip_validation
    end.

-spec validate_schema(api_binary(), kz_json:object()) -> no_schema_present | valid | {invalid_document, term()}.
validate_schema(Name, Doc) ->
    JObj = kz_json:get_value(<<"default">>, Doc),
    case kz_json_schema:load(Name) of
        {ok, Schema} ->
            case kz_json_schema:validate(Schema, JObj) of
                {ok, _} -> valid;
                {error, Error} -> {invalid_document, Error}
            end;
        _ -> no_schema_present
    end.