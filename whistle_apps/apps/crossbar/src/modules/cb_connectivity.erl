%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for connectivity documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_connectivity).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("src/crossbar.hrl").

-define(CB_LIST, <<"trunkstore/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.connectivity">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.connectivity">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.connectivity">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.connectivity">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.connectivity">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.connectivity">>, ?MODULE, delete).

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
resource_exists() ->
    true.
resource_exists(_) ->
    true.

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
    create(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
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
-spec create(#cb_context{}) -> #cb_context{}.
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(undefined, C) end,
    cb_context:validate_request_data(<<"connectivity">>, Context, OnSuccess).

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
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"connectivity">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
on_successful_validation(undefined, #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"sys_info">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [{reduce, false}], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj)|Acc].
