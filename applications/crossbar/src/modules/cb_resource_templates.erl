%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for local resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_resource_templates).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"resources/resource_templates">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resource_templates">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.resource_templates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.resource_templates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.resource_templates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.resource_templates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.resource_templates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.resource_templates">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.resource_templates">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    C = determine_template_database(Context),
    validate_resource_templates(cb_context:req_verb(C), C).

-spec validate_resource_templates(http_method(), cb_context:context()) -> cb_context:context().
validate_resource_templates(?HTTP_GET, Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
validate_resource_templates(?HTTP_PUT, Context) ->
    case is_allowed_to_update(Context) of
        'true' -> validate_request('undefined', Context);
        'false' -> forbidden(Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    C = determine_template_database(Context),
    validate_resource_templates(cb_context:req_verb(C), Id, C).

-spec validate_resource_templates(http_method(), path_token(), cb_context:context()) -> cb_context:context().
validate_resource_templates(?HTTP_GET, Id, Context) ->
    crossbar_doc:load(Id, Context);
validate_resource_templates(?HTTP_POST, Id, Context) ->
    case is_allowed_to_update(Context) of
        'true' -> validate_request(Id, Context);
        'false' -> forbidden(Context)
    end;
validate_resource_templates(?HTTP_PATCH, Id, Context) ->
    case is_allowed_to_update(Context) of
        'true' -> validate_patch(Id, Context);
        'false' -> forbidden(Context)
    end;
validate_resource_templates(?HTTP_DELETE, Id, Context) ->
    case is_allowed_to_update(Context) of
        'true' -> crossbar_doc:load(Id, Context);
        'false' -> forbidden(Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) -> crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) -> crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) -> crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) -> crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec determine_template_database(cb_context:context()) -> cb_context:context().
determine_template_database(Context) ->
    Props = wh_json:to_proplist(cb_context:query_string(Context)),
    case props:is_true(<<"local">>, Props, 'false') of
        'false' -> reseller_template_database(Context);
        'true' -> local_template_database(Context)
    end.

-spec reseller_template_database(cb_context:context()) -> cb_context:context().
reseller_template_database(Context) ->
    case wh_services:find_reseller_id(cb_context:account_id(Context)) of
        'undefined' -> Context;
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
            cb_context:set_account_db(Context, ResellerDb)
    end.

-spec local_template_database(cb_context:context()) -> cb_context:context().
local_template_database(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    case wh_services:is_reseller(AccountId) of
        'false' -> reseller_template_database(Context);
        'true' ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            cb_context:set_account_db(Context, AccountDb)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_allowed_to_update(cb_context:context()) -> boolean().
is_allowed_to_update(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    cb_context:account_db(Context) =:= AccountDb.

-spec forbidden(cb_context:context()) -> cb_context:context().
forbidden(Context) ->
    cb_context:add_validation_error(
        <<"Account">>
        ,<<"forbidden">>
        ,wh_json:from_list([
            {<<"message">>, <<"You are not authorized to modify the resource templates">>}
         ])
        ,Context
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(ResourceId, Context) ->
    Context1 = check_template_name(Context),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' -> on_successful_validation(ResourceId,Context1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(ResourceId, Context) ->
    crossbar_doc:patch_and_validate(ResourceId, Context, fun validate_request/2).

-spec check_template_name(cb_context:context()) -> cb_context:context().
check_template_name(Context) ->
    case wh_json:get_ne_value(<<"template_name">>, cb_context:req_data(Context)) of
        'undefined' ->
            cb_context:add_validation_error(
                <<"template_name">>
               ,<<"required">>
               ,wh_json:from_list([
                    {<<"message">>, <<"Template name is required">>}
                 ])
               ,Context
            );
        _Name -> cb_context:set_resp_status(Context, 'success')
    end.

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    JObj = wh_doc:set_type(cb_context:req_data(Context), <<"resource_template">>),
    cb_context:set_resp_status(cb_context:set_doc(Context, JObj), 'success');
on_successful_validation(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' -> merge(Context1);
        _Status -> Context1
    end.

-spec merge(cb_context:context()) -> cb_context:context().
merge(Context) ->
    ReqData = wh_doc:public_fields(cb_context:req_data(Context)),
    Doc = wh_doc:private_fields(cb_context:doc(Context)),
    cb_context:set_doc(Context, wh_json:merge_jobjs(Doc, ReqData)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
