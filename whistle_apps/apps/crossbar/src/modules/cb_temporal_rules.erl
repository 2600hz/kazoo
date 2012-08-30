%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_temporal_rules).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"temporal_rules/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.temporal_rules">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.temporal_rules">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.temporal_rules">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.temporal_rules">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.temporal_rules">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.temporal_rules">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
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
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
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
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_temporal_rule_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_temporal_rule(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_temporal_rule(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_temporal_rule(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_temporal_rule(DocId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_temporal_rule_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_temporal_rule_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new temporal_rule document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_temporal_rule/1 :: (#cb_context{}) -> #cb_context{}.
create_temporal_rule(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"temporal_rules">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{
                 doc=wh_json:set_value(<<"pvt_type">>, <<"temporal_rule">>, JObj)
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a temporal_rule document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_temporal_rule/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_temporal_rule(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing temporal_rule document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_temporal_rule/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_temporal_rule(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"temporal_rules">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
