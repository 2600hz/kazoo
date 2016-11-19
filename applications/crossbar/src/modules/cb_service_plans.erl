%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%     Peter Defebvre
%%%     Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_service_plans).

-export([init/0
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
    cb_modules_util:bind(?MODULE
                        ,[{<<"*.allowed_methods.service_plans">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.service_plans">>, 'resource_exists'}
                         ,{<<"*.content_types_provided.service_plans">>, 'content_types_provided'}
                         ,{<<"*.validate.service_plans">>, 'validate'}
                         ,{<<"*.execute.post.service_plans">>, 'post'}
                         ,{<<"*.execute.delete.service_plans">>, 'delete'}
                         ]).

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
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
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
allowed_methods(_PlanId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(?AVAILABLE, _PlanId) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /service_plans => []
%%    /service_plans/foo => [<<"foo">>]
%%    /service_plans/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /service_plans mights load a list of service_plan objects
%% /service_plans/123 might load the service_plan object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_service_plan(Context, cb_context:req_verb(Context)).

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
validate(Context, PlanId) ->
    validate_service_plan(Context, PlanId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?AVAILABLE, PlanId) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb), ?TYPE_CHECK_OPTION(<<"service_plan">>)).

-spec validate_service_plan(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_service_plan(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_service_plan(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST
                          ,[]
                          ,Context
                          ,fun normalize_view_results/2
                          );
validate_service_plan(Context, ?HTTP_POST) ->
    maybe_allow_change(Context).

validate_service_plan(Context, PlanId, ?HTTP_GET) ->
    crossbar_doc:load(PlanId, Context);
validate_service_plan(Context, PlanId, ?HTTP_POST) ->
    maybe_allow_change(Context, PlanId);
validate_service_plan(Context, PlanId, ?HTTP_DELETE) ->
    maybe_allow_change(Context, PlanId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().

post(Context) ->
    Routines = [fun(S) -> add_plans(Context, S) end
               ,fun(S) -> delete_plans(Context, S) end
               ,fun kz_services:save/1
               ],
    Services = lists:foldl(fun apply_fun/2, kz_services:fetch(cb_context:account_id(Context)), Routines),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

post(Context, ?SYNCHRONIZATION) ->
    kz_service_sync:sync(cb_context:account_id(Context)),
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
    Routines = [fun(S) -> kz_services:add_service_plan(PlanId, S) end
               ,fun kz_services:save/1
               ],
    Services = lists:foldl(fun apply_fun/2, kz_services:fetch(cb_context:account_id(Context)), Routines),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

post(Context, PlanId, ?OVERRIDE) ->
    Doc = cb_context:doc(Context),

    Overrides = kz_json:get_value([<<"plans">>, PlanId, <<"overrides">>], Doc, kz_json:new()),
    Overriden = kz_json:merge_recursive([Overrides, cb_context:req_data(Context)]),

    NewDoc = kz_json:set_value([<<"plans">>, PlanId, <<"overrides">>], Overriden, Doc),

    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    case cb_context:resp_status(Context1) of
        'success' ->  cb_context:set_resp_data(Context1, Overriden);
        _Status -> Context1
    end.

%%----------------------------------- ---------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, PlanId) ->
    Routines = [fun(S) -> kz_services:delete_service_plan(PlanId, S) end
               ,fun kz_services:save/1
               ],
    Services = lists:foldl(fun apply_fun/2, kz_services:fetch(cb_context:account_id(Context)), Routines),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, kz_services:service_plan_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
add_plans(Context, Services) ->
    ReqData = cb_context:req_data(Context),
    lists:foldl(
      fun kz_services:add_service_plan/2
               ,Services
               ,kz_json:get_value(<<"add">>, ReqData, [])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
delete_plans(Context, Services) ->
    ReqData = cb_context:req_data(Context),
    lists:foldl(
      fun kz_services:delete_service_plan/2
               ,Services
               ,kz_json:get_value(<<"delete">>, ReqData, [])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apply_fun(fun((kz_services:services()) -> kz_services:services()), kz_services:services()) ->
                       kz_services:services().
apply_fun(F, S) -> F(S).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), ne_binary()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
content_types_provided(Context) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

content_types_provided(Context, _) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

content_types_provided(Context, ?AVAILABLE, _) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> {'ok', ne_binary()} | 'false'.
is_allowed(Context) ->
    ResellerId = kz_services:find_reseller_id(cb_context:account_id(Context)),
    AuthAccountId = cb_context:auth_account_id(Context),
    (AuthAccountId =:= ResellerId
     orelse kz_util:is_system_admin(AuthAccountId)
    )
        andalso {'ok', ResellerId}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec maybe_allow_change(cb_context:context()) -> cb_context:context().
-spec maybe_allow_change(cb_context:context(), path_token()) -> cb_context:context().
maybe_allow_change(Context) ->
    case is_allowed(Context) of
        {'ok', ResellerId} ->
            check_plan_ids(Context, ResellerId);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

maybe_allow_change(Context, PlanId) ->
    case is_allowed(Context) of
        {'ok', ResellerId} ->
            check_plan_id(Context, PlanId, ResellerId);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_plan_ids(cb_context:context(), ne_binary()) -> cb_context:context().
-spec check_plan_ids(cb_context:context(), ne_binary(), ne_binaries()) -> cb_context:context().
check_plan_ids(Context, ResellerId) ->
    ReqData = cb_context:req_data(Context),
    AddPlanIds = kz_json:get_value(<<"add">>, ReqData, []),
    check_plan_ids(maybe_forbid_delete(Context), ResellerId, AddPlanIds).

check_plan_ids(Context, ResellerId, PlanIds) ->
    lists:foldl(
      fun(PlanId, Ctxt) ->
              case cb_context:resp_status(Ctxt) of
                  'success' ->
                      check_plan_id(Ctxt, PlanId, ResellerId);
                  _Status -> Ctxt
              end
      end
               ,cb_context:set_resp_status(Context, 'success')
               ,PlanIds
     ).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_plan_id(cb_context:context(), path_token(), ne_binary()) ->
                           cb_context:context().
check_plan_id(Context, PlanId, ResellerId) ->
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb), ?TYPE_CHECK_OPTION(<<"service_plan">>)).

-spec maybe_forbid_delete(cb_context:context()) -> cb_context:context().
maybe_forbid_delete(Context) ->
    case kz_json:get_value(<<"delete">>, cb_context:req_data(Context), []) of
        [] -> Context;
        DeletePlansIds ->
            maybe_forbid_delete(DeletePlansIds, Context)
    end.

-spec maybe_forbid_delete(ne_binaries(), cb_context:context()) -> cb_context:context().
maybe_forbid_delete(DeletePlansIds, Context) ->
    case kz_services:fetch_services_doc(cb_context:account_id(Context), 'false') of
        {'error', 'not_found'} -> Context;
        {'ok', Services} ->
            ExistingPlansIds = kzd_services:plan_ids(Services),
            case DeletePlansIds -- ExistingPlansIds of
                [] -> Context;
                _ -> cb_context:add_system_error('plan_is_not_assigned', Context)
            end
    end.
