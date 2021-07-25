%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-, Voxter Communications
%%% @doc Utility functions for kazoo_documents
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_module_utils).

-include("kz_documents.hrl").

-export([maybe_normalize_emergency_caller_id_number/1
        ,pass_hashes/2
        ,validate_schema/3
        ]).

-define(KEY_EMERGENCY_NUMBER, [<<"caller_id">>, <<"emergency">>, <<"number">>]).

-define(CROSSBAR_CONFIG_CAT, <<"crossbar">>).

-define(SHOULD_ENSURE_SCHEMA_IS_VALID
       ,kapps_config:get_is_true(?CROSSBAR_CONFIG_CAT, <<"ensure_valid_schema">>, 'true')
       ).
-define(SHOULD_FAIL_ON_INVALID_DATA
       ,kapps_config:get_is_true(?CROSSBAR_CONFIG_CAT, <<"schema_strict_validation">>, 'false')
       ).
-define(CROSSBAR_STABILITY_LEVEL
       ,kapps_config:get_binary(?CROSSBAR_CONFIG_CAT, <<"stability_level">>)
       ).

%%------------------------------------------------------------------------------
%% @doc If set, normalize the doc's emergency caller id.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_normalize_emergency_caller_id_number(kz_doc:doc()) -> kz_doc:doc().
maybe_normalize_emergency_caller_id_number(Doc) ->
    case kz_json:get_ne_binary_value(?KEY_EMERGENCY_NUMBER, Doc) of
        'undefined' -> Doc;
        Number ->
            NormalizedNumber = knm_converters:normalize(Number),
            lager:debug("normalizing emergency caller id from ~s to ~s", [Number, NormalizedNumber]),
            kz_json:set_value(?KEY_EMERGENCY_NUMBER, NormalizedNumber, Doc)
    end.

%%------------------------------------------------------------------------------
%% @doc Generate MD5 amd SHA1 combination from a username and password
%% @end
%%------------------------------------------------------------------------------
-spec pass_hashes(kz_term:ne_binary(), kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = kz_term:to_hex_binary(crypto:hash('sha', Creds)),
    MD5 = kz_term:to_hex_binary(crypto:hash('md5', Creds)),
    {MD5, SHA1}.

%%------------------------------------------------------------------------------
%% @doc Validate a doc against a defined schema.
%% `OnSuccess' function will only be called if the Doc passes schema validation.
%% @end
%%------------------------------------------------------------------------------
-spec validate_schema(kz_term:ne_binary() | kzd_schema:doc(), kazoo_documents:doc_validation_acc()
                     ,kazoo_documents:doc_validation_after_fun()) -> kazoo_documents:doc_validation_acc().
validate_schema(<<Schema/binary>>, {Doc, ValidationErrors}, OnSuccess) ->
    lager:debug("validating payload against schema ~s", [Schema]),
    SchemaRequired = ?SHOULD_ENSURE_SCHEMA_IS_VALID,

    case kz_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> validate_schema(SchemaJObj, {Doc, ValidationErrors}, OnSuccess);
        {'error', 'not_found'} when SchemaRequired ->
            lager:error("~s schema not found and is required", [Schema]),
            throw({'system_error', {'datastore_fault', <<"schema '", Schema/binary, "' not found.">>}});
        {'error', 'not_found'} ->
            lager:error("~s schema not found, assuming schema validation passed, continuing anyway", [Schema]),
            validate_schema_passed({Doc, ValidationErrors}, OnSuccess)
    end;
validate_schema(SchemaJObj, {Doc, ValidationErrors}, OnSuccess) ->
    Strict = ?SHOULD_FAIL_ON_INVALID_DATA,
    SystemSL = ?CROSSBAR_STABILITY_LEVEL,
    Options = [{'extra_validator_options', [{'stability_level', SystemSL}]}],

    try kz_json_schema:validate(SchemaJObj, kz_doc:public_fields(Doc), Options) of
        {'ok', JObj} ->
            lager:debug("schema validation passed"),
            validate_schema_passed({JObj, ValidationErrors}, OnSuccess);
        {'error', SchemaErrors} when Strict ->
            lager:debug("schema validation errors when strictly validating"),
            validate_schema_failed({Doc, ValidationErrors}, SchemaErrors);
        {'error', SchemaErrors} ->
            lager:debug("schema validation errors but not strictly validating, trying to fix request"),
            maybe_fix_js_types({Doc, ValidationErrors}, SchemaErrors, SchemaJObj, OnSuccess)
    catch
        ?STACKTRACE('error', 'function_clause', ST)
        lager:error("function clause failure"),
        kz_util:log_stacktrace(ST),
        throw({'system_error', {'function_clause', <<"function clause failure - schema validation failed to run on the server">>}})
        end.

%%------------------------------------------------------------------------------
%% @doc Validate a doc against a defined schema.
%% @end
%%------------------------------------------------------------------------------
-spec validate_schema_passed(kazoo_documents:doc_validation_acc(), kazoo_documents:doc_validation_after_fun()) ->
          kazoo_documents:doc_validation_acc().
validate_schema_passed(ValidateAcc, OnSuccess) ->
    case is_function(OnSuccess, 1) of
        'true' -> OnSuccess(ValidateAcc);
        'false' -> ValidateAcc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_fix_js_types(kazoo_documents:doc_validation_acc(), [jesse_error:error_reason()], kzd_schema:doc()
                        ,kazoo_documents:doc_validation_after_fun()) -> kazoo_documents:doc_validation_acc().
maybe_fix_js_types({Doc, ValidationErrors}, SchemaErrors, SchemaJObj, OnSuccess) ->
    case kz_json_schema:fix_js_types(Doc, SchemaErrors) of
        'false' -> validate_schema_failed({Doc, ValidationErrors}, SchemaErrors);
        {'true', NewDoc} ->
            validate_schema(SchemaJObj, {NewDoc, ValidationErrors}, OnSuccess)
    end.

%%------------------------------------------------------------------------------
%% @doc Add schema errors to doc validation errors.
%% @end
%%------------------------------------------------------------------------------
-spec validate_schema_failed(kazoo_documents:doc_validation_acc(), [jesse_error:error_reason()]) -> kazoo_documents:doc_validation_acc().
validate_schema_failed({Doc, ValidationErrors}, SchemaErrors) ->
    {Doc
    ,[schema_error_to_doc_validation_error(Error) || Error <- SchemaErrors] ++ ValidationErrors
    }.

%%------------------------------------------------------------------------------
%% @doc Format a schema error into a doc validation error.
%% @end
%%------------------------------------------------------------------------------
-spec schema_error_to_doc_validation_error(jesse_error:error_reason()) -> kazoo_documents:doc_validation_error().
schema_error_to_doc_validation_error(Error) ->
    {_ErrorCode, _ErrorMessage, ErrorJObj} = kz_json_schema:error_to_jobj(Error),
    [Key] = kz_json:get_keys(ErrorJObj),
    {[JObj], [Code]} = kz_json:get_values(Key, ErrorJObj),
    lager:debug("schema validation failied ~s ~s: ~p", [Key, Code, JObj]),
    {Key, Code, JObj}.
