%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"service_plans/crossbar_listing">>).
-define(AVAILABLE, <<"available">>).
-define(CURRENT, <<"current">>).
-define(SYNCHRONIZATION, <<"synchronization">>).
-define(RECONCILIATION, <<"reconciliation">>).

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
    cb_modules_util:bind(
        ?MODULE
        ,[{<<"*.allowed_methods.service_plans">>, 'allowed_methods'}
          ,{<<"*.resource_exists.service_plans">>, 'resource_exists'}
          ,{<<"*.content_types_provided.service_plans">>, 'content_types_provided'}
          ,{<<"*.validate.service_plans">>, 'validate'}
          ,{<<"*.execute.post.service_plans">>, 'post'}
          ,{<<"*.execute.delete.service_plans">>, 'delete'}
         ]
    ).

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
    [?HTTP_GET].
allowed_methods(?SYNCHRONIZATION) ->
    [?HTTP_POST];
allowed_methods(?RECONCILIATION) ->
    [?HTTP_POST];
allowed_methods(?CURRENT) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(?AVAILABLE, _) ->
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
    crossbar_doc:load_view(
      ?CB_LIST
      ,[]
      ,Context
      ,fun normalize_view_results/2
     ).

validate(Context, ?CURRENT) ->
    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_status/2, 'success'}
        ,{fun cb_context:set_resp_data/2
          ,wh_services:public_json(cb_context:account_id(Context))
         }
       ]
     );
validate(Context, ?AVAILABLE) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = wh_services:find_reseller_id(AccountId),
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load_view(
      ?CB_LIST
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
validate(Context, PlanId) ->
    validate_service_plan(Context, PlanId, cb_context:req_verb(Context)).

validate(Context, ?AVAILABLE, PlanId) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = wh_services:find_reseller_id(AccountId),
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb)).

-spec validate_service_plan(cb_context:context(), path_token(), http_method()) -> cb_context:context().
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
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?SYNCHRONIZATION) ->
    wh_service_sync:sync(cb_context:account_id(Context)),
    cb_context:set_resp_status(Context, 'success');
post(Context, ?RECONCILIATION) ->
    try wh_services:reconcile(cb_context:account_id(Context)) of
        _ -> cb_context:set_resp_status(Context, 'success')
    catch
        _E:_R ->
            lager:debug("failed to reconcile account services(~s): ~p", [_E, _R]),
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, PlanId) ->
    Routines = [fun(S) -> wh_services:add_service_plan(PlanId, S) end
                ,fun(S) -> wh_services:save(S) end
               ],
    Services = lists:foldl(fun apply_fun/2, wh_services:fetch(cb_context:account_id(Context)), Routines),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, wh_services:service_plan_json(Services)}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).

%%----------------------------------- ---------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, PlanId) ->
    Routines = [fun(S) -> wh_services:delete_service_plan(PlanId, S) end
                ,fun wh_services:save/1
               ],
    Services = lists:foldl(fun apply_fun/2, wh_services:fetch(cb_context:account_id(Context)), Routines),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, wh_services:service_plan_json(Services)}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).

-spec apply_fun(fun((wh_services:services()) -> wh_services:services()), wh_services:services()) ->
                       wh_services:services().
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
    CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
            ,{'to_csv', ?CSV_CONTENT_TYPES}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

content_types_provided(Context, _) ->
    CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
            ,{'to_csv', ?CSV_CONTENT_TYPES}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

content_types_provided(Context, ?AVAILABLE, _) ->
    CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
            ,{'to_csv', ?CSV_CONTENT_TYPES}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> {'ok', ne_binary()} | 'false'.
is_allowed(Context) ->
    ResellerId = wh_services:find_reseller_id(cb_context:account_id(Context)),
    AuthAccountId = cb_context:auth_account_id(Context),
    (AuthAccountId =:= ResellerId
     orelse wh_util:is_system_admin(AuthAccountId)
    )
        andalso {'ok', ResellerId}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec maybe_allow_change(cb_context:context(), path_token()) -> cb_context:context().
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
-spec check_plan_id(cb_context:context(), path_token(), ne_binary()) ->
                           cb_context:context().
check_plan_id(Context, PlanId, ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    Context1 = crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb)),
    case cb_context:resp_status(Context1) of
        'success' ->
            is_service_plan(Context, PlanId, cb_context:doc(Context1));
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_service_plan(cb_context:context(), path_token(), wh_json:object()) ->
                             cb_context:context().
is_service_plan(Context, PlanId, JObj) ->
    case wh_doc:type(JObj) =:= <<"service_plan">> of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error(
              'bad_identifier'
              ,wh_json:from_list([{<<"cause">>, PlanId}])
              ,Context
             )
    end.
