%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"resources/crossbar_listing">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resources">>).
-define(COLLECTION, <<"collection">>).
-define(JOBS, <<"jobs">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.resources">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.resources">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.resources">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.resources">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.resources">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.resources">>, ?MODULE, 'delete').

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
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?JOBS) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?JOBS, _Id) ->
    [?HTTP_GET].

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
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?JOBS, _ID) -> 'true'.

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
validate(Context) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("validating global resources"),
            validate_resources(cb_context:set_account_db(Context, ?WH_OFFNET_DB)
                               ,cb_context:req_verb(Context)
                              );
        _AccountId ->
            validate_resources(Context, cb_context:req_verb(Context))
    end.

validate_resources(Context, ?HTTP_GET) ->
    summary(Context);
validate_resources(Context, ?HTTP_PUT) ->
    case cb_context:account_db(Context) of
        ?WH_OFFNET_DB -> create(Context);
        _AccountDb -> cb_local_resources:validate_request('undefined', Context)
    end.

validate(Context, ?COLLECTION) ->
    cb_context:set_resp_status(Context, 'success');
validate(Context, Id) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("validating global resource ~s", [Id]),
            validate_resource(cb_context:set_account_db(Context, ?WH_OFFNET_DB)
                              ,Id
                              ,cb_context:req_verb(Context)
                             );
        _AccountId ->
            validate_resource(Context, Id, cb_context:req_verb(Context))
    end.

validate_resource(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_resource(Context, Id, ?HTTP_POST) ->
    case cb_context:account_db(Context) of
        ?WH_OFFNET_DB -> update(Id, Context);
        _AccountDb -> cb_local_resources:validate_request(Id, Context)
    end;
validate_resource(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    cb_context:account_db(Context) =:= ?WH_OFFNET_DB andalso wapi_switch:publish_reload_acls(),
    collection_process(Context, cb_context:req_verb(Context));
post(Context, Id) ->
    do_post(Context, Id, cb_context:account_db(Context)).

-spec do_post(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
do_post(Context, _Id, ?WH_OFFNET_DB) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:save(Save);
do_post(Context, Id, _AccountDb) ->
    cb_local_resources:post(Context, Id).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    _ = maybe_aggregate_resource(Context1),
    Context1.

put(Context, ?COLLECTION) ->
    collection_process(Context, cb_context:req_verb(Context)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    collection_process(Context, cb_context:req_verb(Context));
delete(Context, ResourceId) ->
    Context1 = crossbar_doc:delete(Context),
    _ = maybe_remove_aggregate(ResourceId, Context1),
    Context1.


-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:save(cb_context:set_account_db(Context, ?GLOBAL_RESOURCE_DB)).

put(Context, ?COLLECTION) ->
    _ = wapi_switch:publish_reload_acls(),
    collection_process(Context, cb_context:req_verb(Context)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    _ = wapi_switch:publish_reload_acls(),
    collection_process(Context, cb_context:req_verb(Context));
delete(Context, _) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:delete(cb_context:set_account_db(Context, ?GLOBAL_RESOURCE_DB)).

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
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

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
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_value(<<"pvt_type">>, <<"resource">>, cb_context:doc(Context))
                      );
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).
