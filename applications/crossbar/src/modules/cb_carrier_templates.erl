%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for local resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_carrier_templates).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"resources/carrier_templates">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".carrier_templates">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.carrier_templates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.carrier_templates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.carrier_templates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.carrier_templates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.carrier_templates">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.carrier_templates">>, ?MODULE, 'delete').

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
resource_exists() -> true.
resource_exists(_) -> true.

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
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request('undefined', Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    validate_request(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ResourceId) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    read(Context, props:is_true(<<"local">>, cb_context:req_nouns(Context), 'false')).

-spec read(boolean(), ne_binary(), cb_context:context()) -> cb_context:context().
read('true', Id, Context) ->
    crossbar_doc:load(Id, Context);
read('false', Id, Context) ->
    case wh_services:find_reseller_id(cb_context:account_id(Context)) of
        'undefined' -> crossbar_doc:load(Id, Context);
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'), 
            crossbar_doc:load(Id, cb_context:set_account_db(Context, ResellerDb))
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
    summary(props:is_true(<<"local">>, cb_context:req_nouns(Context), 'false'), Context).

-spec summary(boolean(), cb_context:context()) -> cb_context:context().
summary('true', Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
summary('false', Context) ->
    case wh_services:find_reseller_id(cb_context:account_id(Context)) of
        'undefined' ->     
            crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
            Ctx = cb_context:set_account_db(Context, ResellerDb),
            crossbar_doc:load_view(?CB_LIST, [], Ctx, fun normalize_view_results/2)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:contex()) -> cb_context:context().
validate_request(ResourceId, Context) ->
    case wh_services:is_reseller(cb_context:account_id()) of
        'true' -> check_resource_name(ResourceId, Context);
        'false' -> cb_context:add_validation_error(<<"Account">>
                                                   ,<<"forbidden">>
                                                   ,<<"Account is not a Reseller">>
                                                   ,Context)
    end.

-spec check_resource_name(api_binary(), cb_context:contex()) -> cb_context:context().
check_resource_name(ResourceId, Context) ->
    case wh_json:get_value(<<"name">>, cb_context:req_data(Context)) of
        'undefined' -> cb_context:add_validation_error(<<"name">>
                                                   ,<<"required">>
                                                   ,<<"Template name is required">>
                                                   ,Context);
        _Name -> check_resource_schema(ResourceId, Context)
    end.

-spec check_resource_schema(api_binary(), cb_context:contex()) -> cb_context:context().
check_resource_schema(ResourceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ResourceId, C) end,
    cb_context:validate_request_data('undefined', Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"carrier_template">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].



