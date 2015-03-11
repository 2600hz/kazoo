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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,content_types_provided/3 ,content_types_provided/4
         ,put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(ICON, <<"icon">>).
-define(SCREENSHOT, <<"screenshot">>).

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
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
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
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, Id, ?ICON) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Icon = wh_json:get_value(?ICON, JObj),
            case wh_doc:attachment_content_type(JObj, Icon) of
                'undefined' -> Context1;
                CT ->
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _ -> Context1
    end;
content_types_provided(Context, _, _) -> Context.

content_types_provided(Context, Id, ?SCREENSHOT, Number) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            case maybe_get_screenshot(Context1, Number) of
                'error' -> Context;
                {'ok', _, Attachment} ->
                    CT = wh_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context1
    end;
content_types_provided(Context, _, _, _) -> Context.

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
    load_apps(Context).

validate(Context, Id) ->
    validate_app(Context, Id, cb_context:req_verb(Context)).

validate(Context, Id, ?ICON) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> get_icon(Context1);
        _ -> Context1
    end.

validate(Context, Id, ?SCREENSHOT, Number) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> get_screenshot(Context1, Number);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            RespData = wh_json:get_value([<<"apps">>, Id], JObj, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            RespData = wh_json:get_value([<<"apps">>, Id], JObj, wh_json:new()),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
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
-spec validate_app(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_app(Context, Id, ?HTTP_GET) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(
                Context1
                ,wh_json:public_fields(cb_context:doc(Context1))
            );
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_PUT) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_modify(Context1, Id, fun install/2);
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_DELETE) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> uninstall(Context1, Id);
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_POST) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_modify(Context1, Id, fun update/2);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_modification(cb_context:context(), ne_binary()) -> cb_context:context().
validate_modification(Context, Id) ->
    Context1 = can_modify(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> load_account(Context1);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec can_modify(cb_context:context(), ne_binary()) -> cb_context:context().
can_modify(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    case cb_apps_util:allowed_app(AccountId, Id) of
        'undefined' ->
            Props = [{'details', Id}],
            cb_context:add_system_error('forbidden', wh_json:from_list(Props), Context);
        App ->
            cb_context:store(
              cb_context:set_resp_status(Context, 'success')
              ,Id
              ,App
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_apps(cb_context:context()) -> cb_context:context().
load_apps(Context) ->
    AccountId = cb_context:account_id(Context),
    Apps = cb_apps_util:allowed_apps(AccountId),
    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_status/2, 'success'}
        ,{fun cb_context:set_resp_data/2, normalize_apps_result(Apps)}
       ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_apps_result(wh_json:objects()) -> wh_json:objects().
-spec normalize_apps_result(wh_json:objects(), wh_json:objects()) -> wh_json:objects().
normalize_apps_result(Apps) ->
    normalize_apps_result(Apps, []).

normalize_apps_result([], Acc) -> Acc;
normalize_apps_result([App|Apps], Acc) ->
    JObj =
        wh_json:from_list(
            props:filter_undefined(
              [{<<"id">>, wh_json:get_value(<<"_id">>, App)}
               ,{<<"name">>, wh_json:get_value(<<"name">>, App)}
               ,{<<"i18n">>, wh_json:get_value(<<"i18n">>, App)}
               ,{<<"tags">>, wh_json:get_value(<<"tags">>, App)}
               ,{<<"api_url">>, wh_json:get_value(<<"api_url">>, App)}
               ,{<<"source_url">>, wh_json:get_value(<<"source_url">>, App)}
              ])
         ),
    normalize_apps_result(Apps, [JObj|Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_app(cb_context:context(), ne_binary()) -> cb_context:context().
load_app(Context, AppId) ->
    AccountId = cb_context:account_id(Context),
    case cb_apps_util:allowed_app(AccountId, AppId) of
        'undefined' ->
            cb_context:add_system_error(
              'bad_identifier'
              ,wh_json:from_list([{'details', AppId}])
              ,Context
             );
        App ->
            cb_context:setters(
              Context
              ,[{fun cb_context:set_doc/2, App}
                ,{fun cb_context:set_resp_status/2, 'success'}
               ]
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% maybe_install a new app on the account
%% @end
%%--------------------------------------------------------------------
-type modify_fun() :: fun((cb_context:context(), ne_binary()) -> cb_context:context()).
-spec maybe_modify(cb_context:context(), ne_binary(), modify_fun()) -> cb_context:context().
maybe_modify(Context, Id, Fun) ->
    case not(wh_service_ui_apps:is_in_use(cb_context:req_data(Context)))
        orelse cb_context:accepting_charges(Context)
    of
        'true' -> Fun(Context, Id);
        'false' -> dry_run(Context, Id, Fun)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(cb_context:context(), ne_binary(), modify_fun()) -> cb_context:context().
dry_run(Context, Id, Fun) ->
    AccountId = cb_context:account_id(Context),
    AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
    Services = wh_services:fetch(AccountId),
    UpdatedServices = wh_service_ui_apps:reconcile(Services, AppName),
    RespJObj = wh_services:dry_run(UpdatedServices),
    case wh_json:is_empty(RespJObj) of
        'true' -> Fun(Context, Id);
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% install a new app on the account
%% @end
%%--------------------------------------------------------------------
-spec install(cb_context:context(), ne_binary()) -> cb_context:context().
install(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = wh_json:get_value(<<"apps">>, Doc),
    case wh_json:get_value(Id, Apps) of
        'undefined' ->
            Data = cb_context:req_data(Context),
            AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedApps =
                wh_json:set_value(
                  Id
                  ,wh_json:set_value(<<"name">>, AppName, Data)
                  ,Apps
                 ),
            UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
            cb_context:set_doc(Context, UpdatedDoc);
        _ ->
            crossbar_util:response('error', <<"Application already installed">>, 400, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove app from account
%% valid
%% @end
%%--------------------------------------------------------------------
-spec uninstall(cb_context:context(), ne_binary()) -> cb_context:context().
uninstall(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = wh_json:get_value(<<"apps">>, Doc),
    case wh_json:get_value(Id, Apps) of
        'undefined' ->
            crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
        _ ->
            UpdatedApps = wh_json:delete_key(Id, Apps),
            UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
            cb_context:set_doc(Context, UpdatedDoc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = wh_json:get_value(<<"apps">>, Doc),
    case wh_json:get_value(Id, Apps) of
        'undefined' ->
            crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
        _ ->
            Data = cb_context:req_data(Context),
            AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedApps =
                wh_json:set_value(
                  Id
                  ,wh_json:set_value(<<"name">>, AppName, Data)
                  ,Apps
                 ),
            UpdatedDoc = wh_json:set_value(<<"apps">>, UpdatedApps, Doc),
            cb_context:set_doc(Context, UpdatedDoc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_icon(cb_context:context()) -> cb_context:context().
get_icon(Context) ->
    JObj = cb_context:doc(Context),
    Icon = wh_json:get_value(?ICON, JObj),
    get_attachment(Context, Icon).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_screenshot(cb_context:context(), ne_binary()) ->
                                  'error' |
                                  {'ok', ne_binary(), wh_json:object()}.
maybe_get_screenshot(Context, Number) ->
    JObj = cb_context:doc(Context),
    Screenshots = wh_json:get_value(<<"screenshots">>, JObj),
    try lists:nth(wh_util:to_integer(Number)+1, Screenshots) of
        Name ->
            case wh_doc:attachment(JObj, Name) of
                'undefined' -> 'error';
                Attachment ->
                    {'ok', Name, Attachment}
            end
    catch
        _:_ -> 'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_screenshot(cb_context:context(), ne_binary()) -> cb_context:context().
get_screenshot(Context, Number) ->
    case maybe_get_screenshot(Context, Number) of
        'error' ->
            crossbar_util:response_bad_identifier(
              <<?SCREENSHOT/binary , "/", Number/binary>>
              ,Context
             );
        {'ok', Name, _} ->
            get_attachment(Context, Name)
    end.

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
%% @end
%%--------------------------------------------------------------------
-spec get_attachment(cb_context:context(), ne_binary()) ->
                            cb_context:context().
-spec get_attachment(cb_context:context(), ne_binary(), wh_json:object(), wh_json:object()) ->
                            cb_context:context().
get_attachment(Context, Id) ->
    JObj = cb_context:doc(Context),
    case wh_doc:attachment(JObj, Id) of
        'undefined' ->
            AppId = wh_json:get_value(<<"_id">>, JObj),
            crossbar_util:response_bad_identifier(AppId, Context);
        Attachment ->
            get_attachment(Context, Id, JObj, Attachment)
    end.

get_attachment(Context, Id, JObj, Attachment) ->
    Db = wh_json:get_value(<<"pvt_account_db">>, JObj),
    AppId = wh_json:get_value(<<"_id">>, JObj),

    case couch_mgr:fetch_attachment(Db, AppId, Id) of
        {'error', R} ->
            Reason = wh_util:to_binary(R),
            lager:error("failed to fetch attachment, ~s in ~s, (account: ~s)", [Id, AppId, Db]),
            cb_context:add_system_error('datastore_fault', wh_json:from_list([{'details', Reason}]), Context);
        {'ok', AttachBin} ->
            add_attachment(Context, Id, Attachment, AttachBin)
    end.

-spec add_attachment(cb_context:context(), ne_binary(), wh_json:object(), binary()) ->
                            cb_context:context().
add_attachment(Context, Id, Attachment, AttachBin) ->
    RespHeaders =
        [{<<"Content-Disposition">>, <<"attachment; filename=", Id/binary>>}
         ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Attachment)}
         ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Attachment)}
        ],
    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_data/2, AttachBin}
        ,{fun cb_context:add_resp_headers/2, RespHeaders}
       ]
     ).

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
