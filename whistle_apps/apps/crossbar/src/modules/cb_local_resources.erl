%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for local resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_local_resources).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"local_resources/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.local_resources">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.local_resources">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.local_resources">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.local_resources">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.local_resources">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.local_resources">>, ?MODULE, delete).

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
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

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
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    validate_request(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

-spec put(#cb_context{}) -> #cb_context{}.
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, ResourceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = maybe_remove_aggregate(ResourceId, Context1),
    Context1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
validate_request(ResourceId, Context) ->
    check_for_registering_gateways(ResourceId, Context).

check_for_registering_gateways(ResourceId, #cb_context{req_data=JObj}=Context) ->    
    case lists:any(fun(Gateway) ->
                           wh_json:is_true(<<"register">>, Gateway)
                               andalso
                                 (not wh_json:is_false(<<"enabled">>, Gateway))
                   end, wh_json:get_value(<<"gateways">>, JObj, []))
    of
        true ->
            check_resource_schema(ResourceId, cb_context:store(aggregate_resource, true, Context));
        false ->
            check_resource_schema(ResourceId, Context)
    end.

check_resource_schema(ResourceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ResourceId, C) end,
    cb_context:validate_request_data(<<"local_resources">>, Context, OnSuccess).

-spec on_successful_validation('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
on_successful_validation(undefined, #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"resource">>, JObj)};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_aggregate_resource(#cb_context{}) -> boolean().
maybe_aggregate_resource(#cb_context{resp_status=success, doc=JObj}=Context) ->
    case wh_util:is_true(cb_context:fetch(aggregate_resource, Context)) of
        false -> 
            ResourceId = wh_json:get_value(<<"_id">>, JObj),
            maybe_remove_aggregate(ResourceId, Context);
        true -> 
            lager:debug("adding resource to the sip auth aggregate"),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, JObj)),
            wapi_switch:publish_reload_gateways(),
            true
    end;
maybe_aggregate_resource(_) -> false.

-spec maybe_remove_aggregate(ne_binary(), #cb_context{}) -> boolean().
maybe_remove_aggregate(ResourceId, #cb_context{resp_status=success}) ->
    case couch_mgr:open_doc(?WH_SIP_DB, ResourceId) of
        {ok, JObj} ->
            couch_mgr:del_doc(?WH_SIP_DB, JObj),
            wapi_switch:publish_reload_gateways(),
            true;
        {error, not_found} -> false
    end;    
maybe_remove_aggregate(_, _) -> false.
