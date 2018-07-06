%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_service_plans).

-export([init/0
        ,authorize/2
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1 ,content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,post/1 ,post/2, post/3
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"service_plans/crossbar_listing">>).
-define(AVAILABLE, <<"available">>).
-define(CURRENT, <<"current">>).
-define(SYNCHRONIZATION, <<"synchronization">>).
-define(RECONCILIATION, <<"reconciliation">>).
-define(OVERRIDE, <<"override">>).
-define(EDITABLE, <<"editable">>).

-define(ITEM_FIELDS,
        kz_json:from_list(
          [{<<"activation_charge">>, kz_json:new()}
          ,{<<"discounts">>
           ,kz_json:from_list(
              [{<<"maximum">>, kz_json:new()}
              ,{<<"rate">>, kz_json:new()}
              ])
           }
          ,{<<"minimum">>, kz_json:new()}
          ,{<<"rate">>, kz_json:new()}
          ])
       ).

-define(UNDERSCORE_ALL_FIELDS,
        kz_json:set_values([{<<"as">>, kz_json:new()}
                           ,{<<"exceptions">>, kz_json:new()}
                           ]
                          ,?ITEM_FIELDS)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    cb_modules_util:bind(?MODULE
                        ,[{<<"*.authorize.service_plans">>, 'authorize'}
                         ,{<<"*.allowed_methods.service_plans">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.service_plans">>, 'resource_exists'}
                         ,{<<"*.content_types_provided.service_plans">>, 'content_types_provided'}
                         ,{<<"*.validate.service_plans">>, 'validate'}
                         ,{<<"*.execute.post.service_plans">>, 'post'}
                         ,{<<"*.execute.delete.service_plans">>, 'delete'}
                         ]).

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> 'true'.
authorize(Context, _) ->
    is_authorized(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec is_authorized(req_verb(), req_nouns()) -> 'true'.
is_authorized(?HTTP_GET, [{<<"service_plans">>, [?EDITABLE]}]) ->
    'true'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?SYNCHRONIZATION) ->
    [?HTTP_POST];
allowed_methods(?RECONCILIATION) ->
    [?HTTP_POST];
allowed_methods(?CURRENT) ->
    [?HTTP_GET];
allowed_methods(?OVERRIDE) ->
    [?HTTP_POST];
allowed_methods(?AVAILABLE) ->
    [?HTTP_GET];
allowed_methods(?EDITABLE) ->
    [?HTTP_GET];
allowed_methods(_PlanId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?AVAILABLE, _PlanId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /service_plans => []
%%    /service_plans/foo => [<<"foo">>]
%%    /service_plans/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /service_plans mights load a list of service_plan objects
%% /service_plans/123 might load the service_plan object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_service_plan(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CURRENT) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2
                        ,kz_services:public_json(cb_context:account_id(Context))
                        }
                       ]
                      );
validate(Context, ?AVAILABLE) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load_view(?CB_LIST
                          ,[]
                          ,cb_context:set_account_db(Context, ResellerDb)
                          ,fun normalize_view_results/2
                          );
validate(Context, ?SYNCHRONIZATION) ->
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, ?RECONCILIATION) ->
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, ?OVERRIDE) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_util:is_system_admin(AuthAccountId) of
        'true' ->
            crossbar_doc:load(cb_context:account_id(Context)
                             ,cb_context:set_account_db(Context, ?KZ_SERVICES_DB)
                             ,?TYPE_CHECK_OPTION(kzd_services:type())
                             );
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, ?EDITABLE) ->
    summary_editable_fields(Context);
validate(Context, PlanId) ->
    validate_service_plan(Context, PlanId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?AVAILABLE, PlanId) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    Context1 = cb_context:set_account_db(Context, kz_util:format_account_db(ResellerId)),
    crossbar_doc:load(PlanId, Context1, ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

-spec validate_service_plan(cb_context:context(), http_method()) -> cb_context:context().
validate_service_plan(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST
                          ,[]
                          ,Context
                          ,fun normalize_view_results/2
                          );
validate_service_plan(Context, ?HTTP_POST) ->
    maybe_allow_change(Context).

-spec validate_service_plan(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_service_plan(Context, PlanId, ?HTTP_GET) ->
    crossbar_doc:load(PlanId, Context);
validate_service_plan(Context, PlanId, ?HTTP_POST) ->
    maybe_allow_change(Context, PlanId);
validate_service_plan(Context, PlanId, ?HTTP_DELETE) ->
    maybe_allow_change(Context, PlanId).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    Services = pipe_services(cb_context:account_id(Context)
                            ,[fun(Services) -> add_plans(Context, Services) end
                             ,fun(Services) -> delete_plans(Context, Services) end
                             ,fun kz_services:save/1
                             ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?SYNCHRONIZATION) ->
    _ = kz_services:sync(cb_context:account_id(Context)),
    cb_context:set_resp_status(Context, 'success');
post(Context, ?RECONCILIATION) ->
    try kz_services:reconcile(cb_context:account_id(Context)) of
        _ -> cb_context:set_resp_status(Context, 'success')
    catch
        _E:_R ->
            lager:debug("failed to reconcile account services(~s): ~p", [_E, _R]),
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, ?OVERRIDE) ->
    Overrides = kz_json:get_value(<<"overrides">>, cb_context:req_data(Context), kz_json:new()),
    NewDoc =
        kz_json:foldl(fun(PlanId, _JObj, Doc) ->
                              Override = kz_json:get_value(PlanId, Overrides, kz_json:new()),
                              kz_json:set_value([<<"plans">>, PlanId, <<"overrides">>], Override, Doc)
                      end
                     ,cb_context:doc(Context)
                     ,kz_json:get_json_value(<<"plans">>, cb_context:doc(Context))
                     ),

    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1
                                    ,kz_json:get_value(<<"plans">>, NewDoc)
                                    );
        _Status -> Context1
    end;
post(Context, PlanId) ->
    Services = pipe_services(cb_context:account_id(Context)
                            ,[fun(S) -> kz_services:add_service_plan(PlanId, S) end
                             ,fun kz_services:save/1
                             ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, PlanId, ?OVERRIDE) ->
    Doc = cb_context:doc(Context),

    Overrides = kz_json:get_value([<<"plans">>, PlanId, <<"overrides">>], Doc, kz_json:new()),
    Overriden = kz_json:merge([Overrides, cb_context:req_data(Context)]),

    NewDoc = kz_json:set_value([<<"plans">>, PlanId, <<"overrides">>], Overriden, Doc),

    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    case cb_context:resp_status(Context1) of
        'success' ->  cb_context:set_resp_data(Context1, Overriden);
        _Status -> Context1
    end.

%%----------------------------------- ---------------------------------
%% @doc If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, PlanId) ->
    Services = pipe_services(cb_context:account_id(Context)
                            ,[fun(S) -> kz_services:delete_service_plan(PlanId, S) end
                             ,fun kz_services:save/1
                             ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
add_plans(Context, Services) ->
    ReqData = cb_context:req_data(Context),
    lists:foldl(fun kz_services:add_service_plan/2
               ,Services
               ,kz_json:get_list_value(<<"add">>, ReqData, [])
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
delete_plans(Context, Services) ->
    ReqData = cb_context:req_data(Context),
    lists:foldl(fun kz_services:delete_service_plan/2
               ,Services
               ,kz_json:get_value(<<"delete">>, ReqData, [])
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type services_pipe() :: fun((kz_services:services()) -> kz_services:services()).
-spec pipe_services(kz_term:ne_binary(), [services_pipe()]) -> kz_services:services().
pipe_services(AccountId, Routines) ->
    Services = kz_services:fetch(AccountId),
    lists:foldl(fun (F, S) -> F(S) end, Services, Routines).

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

-spec content_types_provided(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
content_types_provided(Context, _) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

-spec content_types_provided(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
content_types_provided(Context, ?AVAILABLE, _) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Check if you have the permission to update or delete service plans
%% @end
%%------------------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> {'ok', kz_term:ne_binary()} | 'false'.
is_allowed(Context) ->
    ResellerId = kz_services:find_reseller_id(cb_context:account_id(Context)),
    AuthAccountId = cb_context:auth_account_id(Context),

    (AuthAccountId =:= ResellerId
     orelse kz_util:is_system_admin(AuthAccountId)
    )
        andalso {'ok', ResellerId}.

%%------------------------------------------------------------------------------
%% @doc Check if you have the permission to update or delete service plans
%% @end
%%------------------------------------------------------------------------------

-spec maybe_allow_change(cb_context:context()) -> cb_context:context().
maybe_allow_change(Context) ->
    case is_allowed(Context) of
        {'ok', ResellerId} ->
            check_plan_ids(Context, ResellerId);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

-spec maybe_allow_change(cb_context:context(), path_token()) -> cb_context:context().
maybe_allow_change(Context, PlanId) ->
    case is_allowed(Context) of
        {'ok', ResellerId} ->
            check_plan_id(Context, PlanId, ResellerId);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec check_plan_ids(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
check_plan_ids(Context, ResellerId) ->
    ReqData = cb_context:req_data(Context),
    AddPlanIds = kz_json:get_value(<<"add">>, ReqData, []),
    check_plan_ids(maybe_forbid_delete(Context), ResellerId, AddPlanIds).

-spec check_plan_ids(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binaries()) -> cb_context:context().
check_plan_ids(Context, _ResellerId, []) ->
    cb_context:set_resp_status(Context, 'success');
check_plan_ids(Context, ResellerId, PlanIds) ->
    lists:foldl(fun(PlanId, Ctxt) ->
                        case cb_context:resp_status(Ctxt) of
                            'success' ->
                                check_plan_id(Ctxt, PlanId, ResellerId);
                            _Status -> Ctxt
                        end
                end
               ,cb_context:set_resp_status(Context, 'success')
               ,PlanIds
               ).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_plan_id(cb_context:context(), path_token(), kz_term:ne_binary()) ->
                           cb_context:context().
check_plan_id(Context, PlanId, ResellerId) ->
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb), ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

-spec maybe_forbid_delete(cb_context:context()) -> cb_context:context().
maybe_forbid_delete(Context) ->
    case kz_json:get_value(<<"delete">>, cb_context:req_data(Context), []) of
        [] -> Context;
        DeletePlansIds ->
            maybe_forbid_delete(DeletePlansIds, Context)
    end.

-spec maybe_forbid_delete(kz_term:ne_binaries(), cb_context:context()) -> cb_context:context().
maybe_forbid_delete(DeletePlansIds, Context) ->
    case kz_services:fetch_services_doc(cb_context:account_id(Context)) of
        {'error', 'not_found'} -> Context;
        {'ok', Services} ->
            ExistingPlansIds = kzd_services:plan_ids(Services),
            case DeletePlansIds -- ExistingPlansIds of
                [] -> Context;
                _ -> cb_context:add_system_error('plan_is_not_assigned', Context)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Returns fixtures of what fields in service plans is customizable
%% @end
%%------------------------------------------------------------------------------
-spec summary_editable_fields(cb_context:context()) -> cb_context:context().
summary_editable_fields(Context) ->
    JObj = read_service_plan_editable(),
    UIApps = kz_json:from_list(get_ui_apps(kapps_util:get_master_account_id())),
    crossbar_doc:handle_json_success(kz_json:set_value(<<"ui_apps">>, UIApps, JObj), Context).

-spec read_service_plan_editable() -> kz_json:object().
read_service_plan_editable() ->
    Path = filename:join([code:priv_dir(?APP), "service_plan_editable_fields.json"]),
    case file:read_file(Path) of
        {'ok', Bin} -> kz_json:decode(Bin);
        {'error', _Reason} ->
            lager:debug("failed to read file ~s: ~p", [Path, _Reason]),
            kz_json:new()
    end.

-spec get_ui_apps({'ok', kz_term:ne_binary()} | {'error', any()}) -> kz_term:proplist().
get_ui_apps({'ok', MasterId}) ->
    case kzd_apps_store:fetch(MasterId) of
        {'ok', JObj} ->
            Apps = kzd_apps_store:apps(JObj),
            Fun = fun(_App, AppJObj, Acc) ->
                          case kzd_app:name(AppJObj) of
                              ?NE_BINARY=Name ->
                                  [{Name, ?ITEM_FIELDS}|Acc];
                              _ -> Acc
                          end
                  end,
            kz_json:foldl(Fun, [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}], Apps);
        {'error', _Reason} ->
            lager:debug("failed to read master's app_store: ~p", [_Reason]),
            [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}]
    end;
get_ui_apps({'error', _Reason}) ->
    lager:debug("failed to get master account_id: ~p", [_Reason]),
    [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}].
