%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600hz
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_apps_store).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,content_types_provided/3 ,content_types_provided/4
         ,put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(LOCAL, <<"local">>).
-define(ICON, <<"icon">>).
-define(SCREENSHOT, <<"screenshot">>).
-define(CB_LIST, <<"apps_store/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.content_types_provided.apps_store">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.apps_store">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.apps_store">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.apps_store">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.apps_store">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.apps_store">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.apps_store">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].
-spec allowed_methods(path_token(), path_token()) -> http_methods() | [].
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods() | [].
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_, _) ->
    [?HTTP_GET].
allowed_methods(_, _, _) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(req_nouns(), http_method()) -> boolean().
authorize([{<<"apps_store">>, _}|_], ?HTTP_GET) ->
    'true'.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, Id, ?ICON) ->
    case master_or_local(Context) of
        'master' ->
            content_types_provided_master(Context, cb_context:req_verb(Context), Id, ?ICON);
        'local' -> Context
    end;
content_types_provided(Context, _, _) -> Context.

content_types_provided(Context, Id, ?SCREENSHOT, Number) ->
    case master_or_local(Context) of
        'master' ->
            content_types_provided_master(Context, cb_context:req_verb(Context), Id, ?SCREENSHOT, Number);
        'local' -> Context
    end;
content_types_provided(Context, _, _, _) -> Context.

-spec content_types_provided_master(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided_master(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided_master(Context, ?HTTP_GET, Id, ?ICON) ->
    Context1 = master_app_read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Icon = wh_json:get_value(?ICON, JObj),
            case wh_json:get_value([<<"_attachments">>, Icon], JObj) of
                'undefined' -> Context1;
                Attachment ->
                    CT = wh_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context1
    end;
content_types_provided_master(Context, _, _, _) -> Context.

content_types_provided_master(Context, ?HTTP_GET, Id, ?SCREENSHOT, Number) ->
    Context1 = master_app_read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case maybe_get_screenshot(Number, Context1) of
                'error' -> Context1;
                {'ok', _, Attachment} ->
                    CT = wh_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context
    end;
content_types_provided_master(Context, _, _, _, _) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    case master_or_local(Context) of
        'local' ->
            lager:debug("validating local apps listing"),
            validate_local(load_account(Context), cb_context:req_verb(Context));
        'master' ->
            lager:debug("validating master apps listing"),
            validate_master_app(Context, cb_context:req_verb(Context))
    end.

validate(Context, Id) ->
    case master_or_local(Context) of
        'local' ->
            validate_local(load_account(Context), cb_context:req_verb(Context), Id);
        'master' ->
            validate_master_app(Context, cb_context:req_verb(Context), Id)
    end.

validate(Context, Id, ?ICON) ->
    case master_or_local(Context) of
        'local' ->
            crossbar_util:response_bad_identifier(Id, Context);
        'master' ->
            validate_master_app(Context, cb_context:req_verb(Context), Id, ?ICON)
    end.

validate(Context, Id, ?SCREENSHOT, Number) ->
    case master_or_local(Context) of
        'local' ->
            crossbar_util:response_bad_identifier(Id, Context);
        'master' ->
            validate_master_app(Context, cb_context:req_verb(Context), Id, ?SCREENSHOT, Number)
    end.

-spec validate_master_app(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_master_app(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate_master_app(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec validate_master_app(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate_master_app(Context, ?HTTP_GET) ->
    master_app_summary(Context).

validate_master_app(Context, ?HTTP_GET, Id) ->
    master_app_read(Id, Context).

validate_master_app(Context, ?HTTP_GET, Id, ?ICON) ->
    get_icon(Id, Context).

validate_master_app(Context, ?HTTP_GET, Id, ?SCREENSHOT, Number) ->
    get_sreenshot(Id, Context, Number).

-spec validate_local(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_local(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_local(Context, ?HTTP_GET) ->
    summary(Context).

validate_local(Context, ?HTTP_GET, AppId) ->
    read(AppId, Context);
validate_local(Context, ?HTTP_POST, AppId) ->
    update(AppId, Context);
validate_local(Context, ?HTTP_DELETE, AppId) ->
    uninstall(AppId, Context);
validate_local(Context, ?HTTP_PUT, AppId) ->
    install(AppId, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, AppId) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            RespData = wh_json:get_value([<<"apps">>, AppId], JObj, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, AppId) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            RespData = wh_json:get_value([<<"apps">>, AppId], JObj, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            cb_context:set_resp_data(Context, wh_json:new());
        _Status -> Context1
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec master_or_local(cb_context:context()) -> 'local' | 'master'.
master_or_local(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"apps_store">>, _}, {<<"accounts">>, [AccountId]}|_] ->
            master_or_local_id(AccountId);
        [{<<"apps_store">>, _}|_] -> 'master'
    end.

-spec master_or_local_id(ne_binary()) -> 'local' | 'master'.
master_or_local_id(AccountId) ->
    case whapps_util:get_master_account_id() of
        {'ok', AccountId} -> 'master';
        {'ok', _MasterAccountId} -> 'local'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_icon(ne_binary(), cb_context:context()) -> cb_context:context().
get_icon(Id, Context) ->
    Context1 = master_app_read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Icon = wh_json:get_value(?ICON, JObj),
            case wh_json:get_value([<<"_attachments">>, Icon], JObj) of
                'undefined' -> crossbar_util:response_bad_identifier(Id, Context1);
                Attachment ->
                    lists:foldl(fun({K, V}, C) ->
                                        cb_context:add_resp_header(C, K, V)
                                end
                                ,crossbar_doc:load_attachment(Id, Icon, Context1)
                                ,[{<<"Content-Disposition">>, <<"attachment; filename=", Icon/binary>>}
                                  ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Attachment)}
                                  ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Attachment)}
                                 ])
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_sreenshot(ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
get_sreenshot(Id, Context, Number) ->
    Context1 = master_app_read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case maybe_get_screenshot(Number, Context1) of
                'error' ->
                    crossbar_util:response_bad_identifier(Number, Context1);
                {'ok', Screenshot, Attachment} ->
                    lists:foldl(
                      fun({K, V}, C) ->
                              cb_context:add_resp_header(C, K, V)
                      end
                      ,crossbar_doc:load_attachment(Id, Screenshot, Context1)
                      ,[{<<"Content-Disposition">>, <<"attachment; filename=", Screenshot/binary>>}
                        ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Attachment)}
                        ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Attachment)}
                       ])
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_screenshot(ne_binary(), cb_context:context()) ->
                                  'error' |
                                  {'ok', ne_binary(), ne_binary()}.
maybe_get_screenshot(Number, Context) ->
    Screenshots = wh_json:get_value(<<"screenshots">>, cb_context:doc(Context)),
    try lists:nth(wh_util:to_integer(Number)+1, Screenshots) of
        S -> maybe_get_attachment(Context, S)
    catch
        _:_ -> 'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_attachment(cb_context:context(), ne_binary()) ->
                                  'error' |
                                  {'ok', ne_binary(), wh_json:object()}.
maybe_get_attachment(Context, Name) ->
    case wh_doc:attachment(cb_context:doc(Context), Name) of
        'undefined' -> 'error';
        Attachment -> {'ok', Name, Attachment}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec master_app_read(ne_binary(), cb_context:context()) -> cb_context:context().
master_app_read(Id, Context) ->
    Context1 = set_master_account_db(Context),
    crossbar_doc:load(Id, Context1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec master_app_summary(cb_context:context()) -> cb_context:context().
master_app_summary(Context) ->
    Context1 = set_master_account_db(Context),
    crossbar_doc:load_view(?CB_LIST, [], Context1, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    case wh_json:is_true([<<"value">>, <<"published">>], JObj, 'true') of
        'false' -> Acc;
        'true' ->
            [wh_json:get_value(<<"value">>, JObj)|Acc]
    end.

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
%% @end
%%--------------------------------------------------------------------
-spec set_master_account_db(cb_context:context()) -> cb_context:context().
set_master_account_db(Context) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    cb_context:set_account_db(Context, MasterAccountDb).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_account(cb_context:context()) -> cb_context:context().
load_account(Context) ->
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
