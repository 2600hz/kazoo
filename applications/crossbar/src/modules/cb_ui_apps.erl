%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_ui_apps).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").


%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ui_apps">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ui_apps">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ui_apps">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ui_apps">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.ui_apps">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.ui_apps">>, ?MODULE, 'delete').

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
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PUT].

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
validate(Context) ->
   validate_set(laod_account(Context), cb_context:req_verb(Context)).

validate(Context, AppId) ->
    validate_set(laod_account(Context), cb_context:req_verb(Context), AppId).

-spec validate_set(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_set(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_set(Context, ?HTTP_GET) ->
    summary(Context).

validate_set(Context, ?HTTP_GET, AppId) ->
    read(AppId, Context);
validate_set(Context, ?HTTP_POST, AppId) ->
    update(AppId, Context);
validate_set(Context, ?HTTP_DELETE, AppId) ->
    uninstall(AppId, Context);
validate_set(Context, ?HTTP_PUT, AppId) ->
    install(AppId, Context).


-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, AppId) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Context1 = crossbar_doc:save(Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    JObj = cb_context:doc(Context1),
                    _ = replicate_account_definition(JObj),
                    RespData = wh_json:get_value([<<"apps">>, AppId], JObj, wh_json:new()),
                    cb_context:set_resp_data(Context, RespData);
                _Status -> Context1
            end;
        _ -> Context
    end.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, AppId) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Context1 = crossbar_doc:save(Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    JObj = cb_context:doc(Context1),
                    _ = replicate_account_definition(JObj),
                    RespData = wh_json:get_value([<<"apps">>, AppId], JObj, wh_json:new()),
                    cb_context:set_resp_data(Context, RespData);
                _Status -> Context1
            end;
        _ -> Context
    end.


-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Context1 = crossbar_doc:save(Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    JObj = cb_context:doc(Context1),
                    _ = replicate_account_definition(JObj),
                    cb_context:set_resp_data(Context, wh_json:new());
                _Status -> Context1
            end;
        _ -> Context
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% load all apps
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            RespData = wh_json:get_value(<<"apps">>, Doc, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _ -> Context
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            RespData = wh_json:get_value([<<"apps">>, Id], Doc, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% install a new app on the account
%% @end
%%--------------------------------------------------------------------
-spec install(ne_binary(), cb_context:context()) -> cb_context:context().
install(AppId, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            Data = cb_context:req_data(Context),
            Apps = wh_json:get_value(<<"apps">>, Doc),

            case wh_json:get_value(AppId, Apps) of
                'undefined' ->
                    UpdatedApps = wh_json:set_value(AppId, Data, Apps),
                    UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
                    cb_context:set_doc(Context, UpdatedDoc);
                _ ->
                    crossbar_util:response('error', <<"Application already installed">>, 400, Context)
            end;
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing app
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(AppId, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            Apps = wh_json:get_value(<<"apps">>, Doc),

            case wh_json:get_value(AppId, Apps) of
                'undefined' ->
                    crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
                _ ->
                    Data = cb_context:req_data(Context),
                    UpdatedApps = wh_json:set_value(AppId, Data, Apps),
                    UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
                    cb_context:set_doc(Context, UpdatedDoc)
            end;
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove app from account
%% valid
%% @end
%%--------------------------------------------------------------------
-spec uninstall(ne_binary(), cb_context:context()) -> cb_context:context().
uninstall(AppId, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            Apps = wh_json:get_value(<<"apps">>, Doc),

            case wh_json:get_value(AppId, Apps) of
                'undefined' ->
                    crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
                _ ->
                    UpdatedApps = wh_json:delete_key(AppId, Apps),
                    UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
                    cb_context:set_doc(Context, UpdatedDoc)
            end;
        _ -> Context
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec laod_account(cb_context:context()) -> cb_context:context();
laod_account(Context) ->
    AccountId = cb_context:account_id(Context),
    crossbar_doc:load(AccountId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replicate_account_definition(wh_json:object()) ->
                                          {'ok', wh_json:object()} |
                                          {'error', _}.
replicate_account_definition(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, JObj));
        _Else ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, JObj))
    end.