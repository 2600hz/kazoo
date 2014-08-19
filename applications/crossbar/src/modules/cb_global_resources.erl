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
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"global_resources/crossbar_listing">>).
-define(GLOBAL_RESOURCE_DB, <<"offnet">>).
-define(COLLECTION, <<"collection">>).


%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:revise_doc_from_file(?GLOBAL_RESOURCE_DB, 'crossbar', "views/global_resources.json"),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.global_resources">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.global_resources">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.global_resources">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.global_resources">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.global_resources">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.global_resources">>, ?MODULE, 'delete').

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
allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
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
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_req(Context#cb_context{db_name=?GLOBAL_RESOURCE_DB}).

validate(Context, ?COLLECTION) ->
    validate_req(cb_context:set_account_db(Context, ?GLOBAL_RESOURCE_DB), ?COLLECTION);
validate(Context, Id) ->
    validate_req(cb_context:set_account_db(Context, ?GLOBAL_RESOURCE_DB), Id).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?COLLECTION) ->
    _ = wapi_switch:publish_reload_acls(),
    collection_process(Context, cb_context:req_verb(Context));
post(Context, _) ->
    _ = wapi_switch:publish_reload_acls(),
    crossbar_doc:save(cb_context:set_account_db(Context, ?GLOBAL_RESOURCE_DB)).

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
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate_req(cb_context:context()) -> cb_context:context().
-spec validate_req(cb_context:context(), path_token()) -> cb_context:context().

validate_req(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate_req(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context).

validate_req(Context, ?COLLECTION) ->
    cb_context:set_resp_status(Context, 'success');
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
-spec create(cb_context:context()) -> cb_context:context().
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

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
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"resources">>, Context, OnSuccess).

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
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), cb_context:context()) -> cb_context:context().
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
%% @end
%%--------------------------------------------------------------------
-spec collection_process(cb_context:context(), ne_binary()) -> cb_context:context().
collection_process(Context, ?HTTP_POST) ->
    ReqData = cb_context:req_data(Context),
    Updates = [{wh_json:get_value(<<"id">>, JObj), clean_resource(JObj)} || JObj <- ReqData],
    Ids = props:get_keys(Updates),
    ViewOptions = [{'keys', Ids}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(?GLOBAL_RESOURCE_DB, ViewOptions) of
        {'error', _R} ->
            lager:error("could not open ~p in ~p", [Ids, ?GLOBAL_RESOURCE_DB]),
            crossbar_util:response('error', <<"failed to open resources">>, Context);
        {'ok', JObjs} ->
            Resources = [update_resource(JObj, Updates) || JObj <- JObjs],
            case couch_mgr:save_docs(?GLOBAL_RESOURCE_DB, Resources) of
                {'error', _R} ->
                    lager:error("failed to update ~p in ~p", [Ids, ?GLOBAL_RESOURCE_DB]),
                    crossbar_util:response('error', <<"failed to update resources">>, Context);
                {'ok', _} ->
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- Resources])
            end
    end;
collection_process(Context, ?HTTP_PUT) ->
    ReqData = cb_context:req_data(Context),
    Options = [{'type', <<"resource">>}],
    Resources = [wh_doc:update_pvt_parameters(JObj, 'undefined', Options) || JObj <- ReqData],
    case couch_mgr:save_docs(?GLOBAL_RESOURCE_DB, Resources) of
        {'error', _R} ->
            lager:error("failed to create resources"),
            crossbar_util:response('error', <<"failed to create resources">>, Context);
        {'ok', JObjs} ->
            Ids = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            ViewOptions = [{'keys', Ids}
                           ,'include_docs'
                          ],
            case couch_mgr:all_docs(?GLOBAL_RESOURCE_DB, ViewOptions) of
                {'error', _R} ->
                    lager:error("could not open ~p in ~p", [Ids, ?GLOBAL_RESOURCE_DB]),
                    cb_context:set_resp_data(Context, Ids);
                {'ok', NewResources} ->
                    cb_context:set_resp_data(Context, [clean_resource(Resource) || Resource <- NewResources])
            end
    end;
collection_process(Context, ?HTTP_DELETE) ->
    ReqData = cb_context:req_data(Context),
    case couch_mgr:del_docs(?GLOBAL_RESOURCE_DB, ReqData) of
        {'error', _R} ->
            lager:error("failed to delete resources"),
            crossbar_util:response('error', <<"failed to delete resources">>, Context);
        {'ok', JObjs} ->
            cb_context:set_resp_data(Context, [wh_json:delete_key(<<"rev">>, JObj) || JObj <- JObjs])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_resource(wh_json:object()) -> wh_json:object().
clean_resource(JObj) ->
    case wh_json:get_value(<<"doc">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"_id">>, JObj) of
                'undefined' ->
                     JObj1 = wh_doc:public_fields(JObj),
                    wh_json:delete_key(<<"id">>, JObj1);
                Id ->
                    JObj1 = wh_json:set_value(<<"id">>, Id, JObj),
                    wh_doc:public_fields(JObj1)
            end;
        Doc -> clean_resource(Doc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_resource(wh_json:object(), wh_proplist()) -> wh_json:object().
update_resource(JObj, Updates) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Id = wh_json:get_value(<<"_id">>, Doc),
    wh_json:merge_recursive([Doc, props:get_value(Id, Updates)]).
