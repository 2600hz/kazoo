%%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Resources module
%%%
%%% Handle client requests for resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_resources).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"resources/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.resources">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.resources">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.resources">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.resources">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.resources">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.resources">>, ?MODULE, delete).

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
resource_exists() ->
    true.
resource_exists(_) ->
    true.

%%--------------------------------------------------------------------
%% @private
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
    load_resource_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_resource(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_resource(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_resource(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_resource(DocId, Context).

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
-spec load_resource_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_resource_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new resource document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_resource/1 :: (#cb_context{}) -> #cb_context{}.
create_resource(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"local_resources">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{
                 doc=wh_json:set_value(<<"pvt_type">>, <<"resource">>, JObj)
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a resource document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_resource/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_resource(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing resource document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_resource/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_resource(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"local_resources">>) of
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
