%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for global resource documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_global_resources).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"global_resources/crossbar_listing">>).
-define(GLOBAL_RESOURCE_DB, <<"offnet">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:revise_doc_from_file(?GLOBAL_RESOURCE_DB, crossbar, "views/global_resources.json"),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.global_resources">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.global_resources">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.global_resources">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.global_resources">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.global_resources">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.global_resources">>, ?MODULE, delete).

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
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(Context) ->
    validate_req(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}).
validate(Context, Id) ->
    validate_req(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}, Id).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:save(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}).

-spec put(#cb_context{}) -> #cb_context{}.
put(Context) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:save(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:delete(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate_req(#cb_context{}) -> #cb_context{}.
-spec validate_req(#cb_context{}, path_token()) -> #cb_context{}.

validate_req(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate_req(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context).

validate_req(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate_req(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context);
validate_req(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(#cb_context{}) -> #cb_context{}.
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(undefined, C) end,
    cb_context:validate_request_data(<<"global_resources">>, Context, OnSuccess).

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
    cb_context:validate_request_data(<<"global_resources">>, Context, OnSuccess).

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
