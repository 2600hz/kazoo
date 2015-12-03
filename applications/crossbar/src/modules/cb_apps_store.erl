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
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/3, validate/4
         ,content_types_provided/3 ,content_types_provided/4
         ,put/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(ICON, <<"icon">>).
-define(SCREENSHOT, <<"screenshot">>).
-define(BLACKLIST, <<"blacklist">>).

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
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
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
allowed_methods(?BLACKLIST) ->
    [?HTTP_GET, ?HTTP_POST];
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
    Context1 = load_app_from_master_account(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Icon = wh_json:get_value(?ICON, JObj),
            case wh_doc:attachment_content_type(JObj, Icon) of
                'undefined' -> Context1;
                CT ->
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    lager:debug("found attachement of content type: ~s/~s~n", [Type, SubType]),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _ -> Context1
    end;
content_types_provided(Context, _, _) -> Context.

content_types_provided(Context, Id, ?SCREENSHOT, Number) ->
    Context1 = load_app_from_master_account(Context, Id),
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
%%
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(http_method(), req_nouns()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate(?HTTP_GET, [{<<"apps_store">>,[_Id, ?ICON]}]) ->
    lager:debug("authenticating request"),
    'true';
authenticate(?HTTP_GET, [{<<"apps_store">>,[_Id, ?SCREENSHOT, _Number]}]) ->
    lager:debug("authenticating request"),
    'true';
authenticate(_Verb, _Nouns) ->
    'false'.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(http_method(), req_nouns()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize(?HTTP_GET, [{<<"apps_store">>,[_Id, ?ICON]}]) ->
    lager:debug("authorizing request"),
    'true';
authorize(?HTTP_GET, [{<<"apps_store">>,[_Id, ?SCREENSHOT, _Number]}]) ->
    lager:debug("authorizing request"),
    'true';
authorize(_Verb, _Nouns) ->
    'false'.

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


validate(Context, ?BLACKLIST) ->
    validate_req(Context, cb_context:req_verb(Context));
validate(Context, Id) ->
    validate_app(Context, Id, cb_context:req_verb(Context)).

validate(Context, Id, ?ICON) ->
    Context1 = load_app_from_master_account(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> get_icon(Context1);
        _ -> Context1
    end.

validate(Context, Id, ?SCREENSHOT, Number) ->
    Context1 = load_app_from_master_account(Context, Id),
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
post(Context, ?BLACKLIST) ->
    ReqData = cb_context:req_data(Context),
    Blacklist = wh_json:get_value(<<"blacklist">>, ReqData, []),
    Doc = wh_json:set_value(<<"blacklist">>, Blacklist, cb_context:doc(Context)),
    return_only_blacklist(
        crossbar_doc:save(
            cb_context:set_doc(Context, Doc)
        )
    );
post(Context, Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            RespData = wh_json:get_value(Id, kzd_apps_store:apps(JObj)),
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
            RespData = wh_json:get_value(Id, kzd_apps_store:apps(JObj)),
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
-spec validate_req(cb_context:context(), http_method()) -> cb_context:context().
validate_req(Context, ?HTTP_POST) ->
    validate_blacklist(Context);
validate_req(Context, ?HTTP_GET) ->
    Context1 = validate_blacklist(Context),
    return_only_blacklist(Context1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_blacklist(cb_context:context()) -> cb_context:context().
validate_blacklist(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case wh_util:is_in_account_hierarchy(AuthAccountId, AccountId) of
        'false' -> cb_context:add_system_error('forbidden', Context);
        'true' -> load_apps_store(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec return_only_blacklist(cb_context:context()) -> cb_context:context().
return_only_blacklist(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            RespData = cb_context:resp_data(Context),
            Blacklist = wh_json:get_value(<<"blacklist">>, RespData, []),
            NewRespData =
                wh_json:from_list([
                    {<<"blacklist">>, Blacklist}
                ]),
            cb_context:set_resp_data(Context, NewRespData);
        _ -> Context
    end.

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
        'success' ->
            Callback =
                fun() ->
                    install(Context1, Id)
                end,
            crossbar_services:maybe_dry_run(Context1, Callback, <<"app">>);
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
        'success' ->
            Callback =
                fun() ->
                    update(Context1, Id)
                end,
            crossbar_services:maybe_dry_run(Context1, Callback, <<"app">>);
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
        'success' -> load_apps_store(Context1);
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
    case kzd_app:published(App) of
        'false' -> normalize_apps_result(Apps, Acc);
        'true' ->
            JObj =
                wh_json:from_list(
                    props:filter_undefined([
                        {<<"id">>, kzd_app:id(App)}
                        ,{<<"name">>, kzd_app:name(App)}
                        ,{<<"i18n">>, kzd_app:i18n(App)}
                        ,{<<"tags">>, kzd_app:tags(App)}
                        ,{<<"api_url">>, kzd_app:api_url(App)}
                        ,{<<"source_url">>, kzd_app:source_url(App)}
                        ,{<<"account_id">>, kzd_app:account_id(App)}
                        ,{<<"users">>, wh_json:get_value(<<"users">>, App)}
                        ,{<<"allowed_users">>, wh_json:get_value(<<"allowed_users">>, App)}
                        ,{<<"masqueradable">>, wh_json:is_true(<<"masqueradable">>, App, 'true')}
                    ])
                 ),
            normalize_apps_result(Apps, [JObj|Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_app(cb_context:context(), ne_binary()) -> cb_context:context().
load_app(Context, AppId) ->
    AccountId = cb_context:account_id(Context),
    case cb_apps_util:allowed_app(AccountId, AppId) of
        'undefined' -> bad_app_error(Context, AppId);
        App ->
            cb_context:setters(
              Context
              ,[{fun cb_context:set_doc/2
                 ,wh_json:set_value(<<"account_id">>, kzd_app:account_id(App), App)}
                ,{fun cb_context:set_resp_status/2, 'success'}
               ]
             )
    end.

%% @private
-spec load_app_from_master_account(cb_context:context(), ne_binary()) -> cb_context:context().
load_app_from_master_account(Context, AppId) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    DefaultApps = cb_apps_util:load_default_apps(),
    case [JObj || JObj <- DefaultApps, wh_doc:id(JObj) == AppId] of
        [AppJObj] ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_account_id/2, MasterAccountId}
                                 ,{fun cb_context:set_account_db/2, MasterAccountDb}
                                 ,{fun cb_context:set_doc/2, AppJObj}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]
                              );
        _Else ->
            bad_app_error(Context, AppId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bad_app_error(cb_context:context(), ne_binary()) -> cb_context:context().
bad_app_error(Context, AppId) ->
    cb_context:add_system_error(
      'bad_identifier'
      ,wh_json:from_list([{'details', AppId}])
      ,Context
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% install a new app on the account
%% @end
%%--------------------------------------------------------------------
-spec install(cb_context:context(), ne_binary()) -> cb_context:context().
install(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = kzd_apps_store:apps(Doc),
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
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
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
    Apps = kzd_apps_store:apps(Doc),
    case wh_json:get_value(Id, Apps) of
        'undefined' ->
            crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
        _ ->
            UpdatedApps = wh_json:delete_key(Id, Apps),
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
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
   Apps = kzd_apps_store:apps(Doc),
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
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
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
-spec load_apps_store(cb_context:context()) -> cb_context:context().
load_apps_store(Context) ->
    Context1 = crossbar_doc:load(kzd_apps_store:id(), Context),
    case {cb_context:resp_status(Context1), cb_context:resp_error_code(Context1)} of
        {'error', 404} ->
            cb_context:setters(
              Context
              ,[{fun cb_context:set_resp_status/2, 'success'}
                ,{fun cb_context:set_resp_data/2, wh_json:new()}
               ]
             );
        {'success', _} -> Context1;
        {'error', _} -> Context1
    end.

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
            AppId = wh_doc:id(JObj),
            crossbar_util:response_bad_identifier(AppId, Context);
        Attachment ->
            get_attachment(Context, Id, JObj, Attachment)
    end.

get_attachment(Context, Id, JObj, Attachment) ->
    Db = wh_doc:account_db(JObj),
    AppId = wh_doc:id(JObj),
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
