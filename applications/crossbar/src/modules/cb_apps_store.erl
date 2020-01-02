%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Crossbar API for apps store.
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_apps_store).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,authenticate/1, authenticate/2, authenticate/3, authenticate/4
        ,authorize/1, authorize/2, authorize/3, authorize/4
        ,validate/1, validate/2, validate/3, validate/4
        ,content_types_provided/3 ,content_types_provided/4
        ,put/2
        ,post/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(ICON, <<"icon">>).
-define(SCREENSHOT, <<"screenshot">>).
-define(BLACKLIST, <<"blacklist">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_provided.apps_store">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.apps_store">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.apps_store">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate.apps_store">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.apps_store">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.apps_store">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.apps_store">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.apps_store">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.apps_store">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?BLACKLIST) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_AppId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_AppId, ?ICON) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_AppId, ?SCREENSHOT, _AppScreenshotIndex) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_, _, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, Id, ?ICON) ->
    Context1 = load_app_from_master_account(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Icon = kz_json:get_value(?ICON, JObj),
            case kz_doc:attachment_content_type(JObj, Icon) of
                'undefined' -> Context1;
                CT ->
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    lager:debug("found attachment of content type: ~s/~s~n", [Type, SubType]),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _ -> Context1
    end;
content_types_provided(Context, _, _) -> Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, Id, ?SCREENSHOT, Number) ->
    Context1 = load_app_from_master_account(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            case maybe_get_screenshot(Context1, Number) of
                'error' -> Context;
                {'ok', _, Attachment} ->
                    CT = kz_json:get_value(<<"content_type">>, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context1, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context1
    end;
content_types_provided(Context, _, _, _) -> Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _) ->
    authenticate_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(Context, _, _) ->
    authenticate_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().
authenticate(Context, _, _, _) ->
    authenticate_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate_nouns(http_method(), req_nouns()) -> boolean().
authenticate_nouns(?HTTP_GET, [{<<"apps_store">>,[_Id, ?ICON]}]) ->
    lager:debug("authenticating request"),
    'true';
authenticate_nouns(?HTTP_GET, [{<<"apps_store">>,[_Id, ?SCREENSHOT, _Number]}]) ->
    lager:debug("authenticating request"),
    'true';
authenticate_nouns(_Verb, _Nouns) ->
    'false'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _) ->
    authorize_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _, _) ->
    authorize_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().
authorize(Context, _, _, _) ->
    authorize_nouns(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize_nouns(http_method(), req_nouns()) -> boolean().
authorize_nouns(?HTTP_GET, [{<<"apps_store">>,[_Id, ?ICON]}]) ->
    lager:debug("authorizing request"),
    'true';
authorize_nouns(?HTTP_GET, [{<<"apps_store">>,[_Id, ?SCREENSHOT, _Number]}]) ->
    lager:debug("authorizing request"),
    'true';
authorize_nouns(_Verb, _Nouns) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    load_apps(Context).


-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?BLACKLIST) ->
    validate_req(Context, cb_context:req_verb(Context));
validate(Context, Id) ->
    validate_app(Context, Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, AppId, ?ICON) ->
    Context1 = load_app_from_master_account(Context, AppId),
    case cb_context:resp_status(Context1) of
        'success' -> get_icon(Context1);
        _ -> Context1
    end.

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, AppId, ?SCREENSHOT, Number) ->
    Context1 = load_app_from_master_account(Context, AppId),
    case cb_context:resp_status(Context1) of
        'success' -> get_screenshot(Context1, Number);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?BLACKLIST) ->
    ReqData = cb_context:req_data(Context),
    Blacklist = kz_json:get_value(<<"blacklist">>, ReqData, []),
    Doc = kz_json:set_value(<<"blacklist">>, Blacklist, cb_context:doc(Context)),
    return_only_blacklist(
      crossbar_doc:save(
        cb_context:set_doc(Context, Doc)
       )
     );
post(Context, AppId) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            RespData = kz_json:get_value(AppId, kzd_apps_store:apps(JObj)),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            RespData = kz_json:get_value(Id, kzd_apps_store:apps(JObj)),
            cb_context:set_resp_data(Context, RespData);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context, kz_json:new());
        _Status -> Context1
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_req(cb_context:context(), http_method()) -> cb_context:context().
validate_req(Context, ?HTTP_POST) ->
    validate_blacklist(Context);
validate_req(Context, ?HTTP_GET) ->
    Context1 = validate_blacklist(Context),
    return_only_blacklist(Context1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_blacklist(cb_context:context()) -> cb_context:context().
validate_blacklist(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId) of
        'false' -> cb_context:add_system_error('forbidden', Context);
        'true' -> load_apps_store(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec return_only_blacklist(cb_context:context()) -> cb_context:context().
return_only_blacklist(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            RespData = cb_context:resp_data(Context),
            Blacklist = kz_json:get_value(<<"blacklist">>, RespData, []),
            NewRespData =
                kz_json:from_list([
                                   {<<"blacklist">>, Blacklist}
                                  ]),
            cb_context:set_resp_data(Context, NewRespData);
        _ -> Context
    end.

-spec validate_app(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_app(Context, Id, ?HTTP_GET) ->
    Context1 = load_app(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1
                                    ,kz_doc:public_fields(cb_context:doc(Context1))
                                    );
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_PUT) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> prepare_install(Context1, Id);
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_DELETE) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> prepare_uninstall(Context1, Id);
        _ -> Context1
    end;
validate_app(Context, Id, ?HTTP_POST) ->
    Context1 = validate_modification(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> prepare_update(Context1, Id);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_modification(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_modification(Context, Id) ->
    Context1 = can_modify(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> load_apps_store(Context1);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec can_modify(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
can_modify(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    case cb_apps_util:allowed_app(AccountId, Id) of
        'undefined' ->
            Props = [{<<"details">>, Id}],
            cb_context:add_system_error('forbidden', kz_json:from_list(Props), Context);
        App ->
            cb_context:store(cb_context:set_resp_status(Context, 'success')
                            ,Id
                            ,App
                            )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_apps(cb_context:context()) -> cb_context:context().
load_apps(Context) ->
    AccountId = cb_context:account_id(Context),
    Apps = cb_apps_util:allowed_apps(AccountId),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, normalize_apps_result(Apps)}
                       ]
                      ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec normalize_apps_result(kz_json:objects()) -> kz_json:objects().
normalize_apps_result(Apps) ->
    normalize_apps_result(Apps, []).

-spec normalize_apps_result(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
normalize_apps_result([], Acc) -> Acc;
normalize_apps_result([App|Apps], Acc) ->
    case kzd_app:is_published(App) of
        'false' -> normalize_apps_result(Apps, Acc);
        'true' ->
            JObj =
                kz_json:from_list(
                  [{<<"id">>, kz_doc:id(App)}
                  ,{<<"name">>, kzd_app:name(App)}
                  ,{<<"i18n">>, kzd_app:i18n(App)}
                  ,{<<"tags">>, kzd_app:tags(App)}
                  ,{<<"api_url">>, kzd_app:api_url(App)}
                  ,{<<"source_url">>, kzd_app:source_url(App)}
                  ,{<<"account_id">>, kzd_app:account_id(App)}
                  ,{<<"users">>, kzd_app:users(App)}
                  ,{<<"allowed_users">>, kzd_app:allowed_users(App)}
                  ,{<<"masqueradable">>, kzd_app:masqueradable(App)}
                  ,{<<"phase">>, kzd_app:phase(App)}
                  ,{<<"extends">>, kzd_app:extends(App)}
                  ]),
            normalize_apps_result(Apps, [JObj|Acc])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_app(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_app(Context, AppId) ->
    AccountId = cb_context:account_id(Context),
    case cb_apps_util:allowed_app(AccountId, AppId) of
        'undefined' -> bad_app_error(Context, AppId);
        App ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2
                                ,kz_json:set_value(<<"account_id">>, kzd_app:account_id(App), App)}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]
                              )
    end.

-spec load_app_from_master_account(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_app_from_master_account(Context, AppId) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    DefaultApps = cb_apps_util:load_default_apps(),
    case [JObj || JObj <- DefaultApps, kz_doc:id(JObj) == AppId] of
        [AppJObj] ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_account_id/2, MasterAccountId}
                               ,{fun cb_context:set_doc/2, AppJObj}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]
                              );
        _Else ->
            bad_app_error(Context, AppId)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bad_app_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
bad_app_error(Context, AppId) ->
    cb_context:add_system_error('bad_identifier'
                               ,kz_json:from_list([{<<"details">>, AppId}])
                               ,Context
                               ).

%%------------------------------------------------------------------------------
%% @doc install a new app on the account
%% @end
%%------------------------------------------------------------------------------
-spec prepare_install(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
prepare_install(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = kzd_apps_store:apps(Doc),
    case kz_json:get_value(Id, Apps) of
        'undefined' ->
            Data = cb_context:req_data(Context),
            AppName = kz_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedApps =
                kz_json:set_value(Id
                                 ,kz_json:set_value(<<"name">>, AppName, Data)
                                 ,Apps
                                 ),
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
            cb_context:set_doc(Context, UpdatedDoc);
        _ ->
            crossbar_util:response('error', <<"Application already installed">>, 400, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Remove app from account
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec prepare_uninstall(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
prepare_uninstall(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = kzd_apps_store:apps(Doc),
    case kz_json:get_value(Id, Apps) of
        'undefined' ->
            crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
        _ ->
            UpdatedApps = kz_json:delete_key(Id, Apps),
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
            cb_context:set_doc(Context, UpdatedDoc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_update(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
prepare_update(Context, Id) ->
    Doc = cb_context:doc(Context),
    Apps = kzd_apps_store:apps(Doc),
    case kz_json:get_value(Id, Apps) of
        'undefined' ->
            crossbar_util:response('error', <<"Application is not installed">>, 400, Context);
        _ ->
            Data = cb_context:req_data(Context),
            AppName = kz_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedApps =
                kz_json:set_value(Id
                                 ,kz_json:set_value(<<"name">>, AppName, Data)
                                 ,Apps
                                 ),
            UpdatedDoc = kzd_apps_store:set_apps(Doc, UpdatedApps),
            cb_context:set_doc(Context, UpdatedDoc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_icon(cb_context:context()) -> cb_context:context().
get_icon(Context) ->
    JObj = cb_context:doc(Context),
    Icon = kz_json:get_value(?ICON, JObj),
    get_attachment(Context, Icon).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_get_screenshot(cb_context:context(), kz_term:ne_binary()) ->
          'error' |
          {'ok', kz_term:ne_binary(), kz_json:object()}.
maybe_get_screenshot(Context, Number) ->
    JObj = cb_context:doc(Context),
    Screenshots = kz_json:get_value(<<"screenshots">>, JObj),
    try lists:nth(kz_term:to_integer(Number)+1, Screenshots) of
        Name ->
            case kz_doc:attachment(JObj, Name) of
                'undefined' -> 'error';
                Attachment ->
                    {'ok', Name, Attachment}
            end
    catch
        _:_ -> 'error'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_screenshot(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
get_screenshot(Context, Number) ->
    case maybe_get_screenshot(Context, Number) of
        'error' ->
            crossbar_util:response_bad_identifier(<<?SCREENSHOT/binary , "/", Number/binary>>
                                                 ,Context
                                                 );
        {'ok', Name, _} ->
            get_attachment(Context, Name)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_apps_store(cb_context:context()) -> cb_context:context().
load_apps_store(Context) ->
    Context1 = crossbar_doc:load(kzd_apps_store:id(), Context, ?TYPE_CHECK_OPTION_ANY),
    case {cb_context:resp_status(Context1)
         ,cb_context:resp_error_code(Context1)
         }
    of
        {'error', 404} ->
            AccountId = cb_context:account_id(Context),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, kz_json:new()}
                               ,{fun cb_context:set_doc/2, kzd_apps_store:new(AccountId)}
                               ]
                              );
        {'success', _} -> Context1;
        {'error', _} -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec get_attachment(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
get_attachment(Context, Id) ->
    JObj = cb_context:doc(Context),
    case kz_doc:attachment(JObj, Id) of
        'undefined' ->
            AppId = kz_doc:id(JObj),
            crossbar_util:response_bad_identifier(AppId, Context);
        Attachment ->
            get_attachment(Context, Id, JObj, Attachment)
    end.

-spec get_attachment(cb_context:context(), kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
          cb_context:context().
get_attachment(Context, Id, JObj, Attachment) ->
    Db = kz_doc:account_db(JObj),
    AppId = kz_doc:id(JObj),
    case kz_datamgr:fetch_attachment(Db, AppId, Id) of
        {'error', R} ->
            Reason = kz_term:to_binary(R),
            lager:error("failed to fetch attachment, ~s in ~s, (account: ~s)", [Id, AppId, Db]),
            cb_context:add_system_error('datastore_fault', kz_json:from_list([{<<"details">>, Reason}]), Context);
        {'ok', AttachBin} ->
            add_attachment(Context, Id, Attachment, AttachBin)
    end.

-spec add_attachment(cb_context:context(), kz_term:ne_binary(), kz_json:object(), binary()) ->
          cb_context:context().
add_attachment(Context, Id, Attachment, AttachBin) ->
    RespHeaders =
        #{<<"content-disposition">> => <<"attachment; filename=", Id/binary>>
         ,<<"content-type">> => kz_json:get_value(<<"content_type">>, Attachment)
         },
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, AttachBin}
                       ,{fun cb_context:add_resp_headers/2, RespHeaders}
                       ]
                      ).
