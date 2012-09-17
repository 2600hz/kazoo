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

-define(PVT_FUNS, [fun add_pvt_type/2]).
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
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context).

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
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"local_resources">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"local_resources">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, <<"resource">>, JObj).
