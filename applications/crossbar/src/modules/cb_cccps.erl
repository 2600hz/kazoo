%%%-------------------------------------------------------------------
%%% @copyright 
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cb_cccps).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"cccps/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    maybe_init_db(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cccps">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cccps">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.cccps">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.cccps">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.cccps">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.cccps">>, ?MODULE, 'delete').

-spec maybe_init_db() -> 'ok'.
maybe_init_db() ->
    case couch_mgr:db_exists(<<"cccps">>) of
        'true' -> 
             _ = couch_mgr:revise_doc_from_file(<<"cccps">>, 'crossbar', <<"views/cccps.json">>),
            'ok';
        'false' -> init_db()
    end.

-spec init_db() -> 'ok'.
init_db() ->
    couch_mgr:db_create(<<"cccps">>),
    _ = couch_mgr:revise_doc_from_file(<<"cccps">>, 'crossbar', <<"views/cccps.json">>),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
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
%% Does the path point to a valid resource
%% So /cccps => []
%%    /cccps/foo => [<<"foo">>]
%%    /cccps/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /cccps mights load a list of cccp objects
%% /cccps/123 might load the cccp object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_cccps(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_cccp(Context, Id, cb_context:req_verb(Context)).

-spec validate_cccps(cb_context:context(), http_method()) -> cb_context:context().
validate_cccps(Context, ?HTTP_GET) ->
    summary(Context);
validate_cccps(#cb_context{req_data=ReqData}=Context, ?HTTP_PUT) ->
    case wh_json:get_value(<<"cid">>, ReqData) of
        'undefined' ->
            check_pin(Context);
        _ ->
            check_cid(Context)
    end.

-spec validate_cccp(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_cccp(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_cccp(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_cccp(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context2 = crossbar_doc:save(Context),
    couch_mgr:ensure_saved(<<"cccps">>, cb_context:doc(Context2)),
    Context2.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    Context2 = crossbar_doc:save(Context),
    couch_mgr:ensure_saved(<<"cccps">>, cb_context:doc(Context2)),
    Context2.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    Context2 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context2) of
        'success' ->
            _ = couch_mgr:del_doc(?KZ_CCCPS_DB, wh_doc:id(cb_context:doc(Context2))),
            Context2;
        _ ->
            Context2
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

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
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

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
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), <<"cccp">>));
on_successful_validation(Id, Context) ->
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
%% Checks whether cid and pin are unique
%% @end
%%--------------------------------------------------------------------

-spec check_pin(cb_context:context()) -> cb_context:context().
check_pin(Context) ->
    case unique_pin(Context) of
        'empty' -> create(Context);
        _ ->
            cb_context:add_validation_error(
                <<"cccp">>
                ,<<"unique">>
                ,wh_json:from_list([
                    {<<"message">>, <<"Pin already exists">>}
                 ])
                ,Context
            )
    end.

-spec check_cid(cb_context:context()) -> cb_context:context().
check_cid(#cb_context{req_data=ReqData}=Context) ->
    CID = wh_json:get_value(<<"cid">>, ReqData),
    case wnm_util:is_reconcilable(CID) of
        'false' ->
            cb_context:add_validation_error(
                <<"cccp">>
                ,<<"unique">>
                ,wh_json:from_list([
                    {<<"message">>, <<"Number is non reconcilable">>}
                    ,{<<"cause">>, CID}
                 ])
                ,Context
            );
        'true' ->
            ReqData2 = wh_json:set_value(<<"cid">>, wnm_util:normalize_number(CID), ReqData),
            Context2 = Context#cb_context{req_data=ReqData2},
            case unique_cid(Context2) of
                'empty' -> create(Context2);
                _ ->
                    cb_context:add_validation_error(
                        <<"cccp">>
                        ,<<"unique">>
                        ,wh_json:from_list([
                            {<<"message">>, <<"CID already exists">>}
                            ,{<<"cause">>, CID}
                         ])
                        ,Context
                    )
            end

    end.
-spec unique_cid(cb_context:context()) -> {ok, list()} | 'empty' | 'error'.
unique_cid(#cb_context{req_data=ReqData}) ->
    CID = wh_json:get_value(<<"cid">>, ReqData),
    cccp_util:authorize(CID, <<"cccps/cid_listing">>).

-spec unique_pin(cb_context:context()) -> {ok, list()} | 'empty' | 'error'.
unique_pin(#cb_context{req_data=ReqData}) ->
    Pin = wh_json:get_value(<<"pin">>, ReqData),
    cccp_util:authorize(Pin, <<"cccps/pin_listing">>).

