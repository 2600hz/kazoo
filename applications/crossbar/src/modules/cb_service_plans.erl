%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%     Peter Defebvre
%%%     Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_service_plans).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1 ,content_types_provided/2
         ,validate/1, validate/2
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"service_plans/crossbar_listing">>).

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
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
    AccountId = cb_context:account_id(Context),

    ResellerId = case wh_services:is_reseller(AccountId)
                     andalso AccountId =:= cb_context:auth_account_id(Context)
                 of
                     'true' -> AccountId;
                     'false' -> wh_services:find_reseller_id(AccountId)
                 end,
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),

    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,cb_context:set_account_db(Context, ResellerDb)
                           ,fun normalize_view_results/2
                          ).

validate(Context, <<"current">>) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_status/2, 'success'}
                                 ,{fun cb_context:set_resp_data/2
                                   ,wh_services:public_json(cb_context:account_id(Context))
                                  }
                                ]);
        _Verb ->
            cb_context:add_system_error('bad_identifier', Context)
    end;
validate(Context, <<"synchronization">>) ->
    case cb_context:req_verb(Context) =:= ?HTTP_POST
        andalso is_reseller(Context)
    of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, <<"reconciliation">>) ->
    case cb_context:req_verb(Context) =:= ?HTTP_POST
        andalso is_reseller(Context)
    of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, PlanId) ->
    validate_service_plan(Context, PlanId, cb_context:req_verb(Context)).

-spec validate_service_plan(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_service_plan(Context, PlanId, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = wh_services:find_reseller_id(AccountId),
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb));
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
post(Context, <<"synchronization">>) ->
    wh_service_sync:sync(cb_context:account_id(Context)),
    cb_context:set_resp_status(Context, 'success');
post(Context, <<"reconciliation">>) ->
    try wh_services:reconcile(cb_context:account_id(Context)) of
        _ -> cb_context:set_resp_status(Context, 'success')
    catch
        _E:_R ->
            io:format("failed to reconcile account ~s(~p): ~p~n", [cb_context:account_id(Context), _E, _R]),
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, PlanId) ->
    Routines = [fun(S) -> wh_services:add_service_plan(PlanId, S) end
                ,fun(S) -> wh_services:save(S) end
               ],
    Services = lists:foldl(fun(F, S) -> F(S) end
                           ,wh_services:fetch(cb_context:account_id(Context))
                           ,Routines
                          ),
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
                ,fun(S) -> wh_services:save(S) end
               ],
    Services = lists:foldl(fun(F, S) -> F(S) end
                           ,wh_services:fetch(cb_context:account_id(Context))
                           ,Routines
                          ),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, wh_services:service_plan_json(Services)}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), ne_binary()) -> cb_context:context().
content_types_provided(Context) ->
    CTPs = [{'to_json', [{<<"application">>, <<"json">>}]}
            ,{'to_csv', [{<<"application">>, <<"octet-stream">>}
                         ,{<<"text">>, <<"csv">>}
                        ]}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

content_types_provided(Context, <<"current">>) ->
    CTPs = [{'to_json', [{<<"application">>, <<"json">>}]}
            ,{'to_csv', [{<<"application">>, <<"octet-stream">>}
                         ,{<<"text">>, <<"csv">>}
                        ]}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

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
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(cb_context:context()) -> {'ok', ne_binary()} | 'false'.
is_reseller(Context) ->
    ResellerId = wh_services:find_reseller_id(cb_context:account_id(Context)),
    cb_context:auth_account_id(Context) =:= ResellerId andalso {'ok', ResellerId}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if you have the permission to update or delete service plans
%% @end
%%--------------------------------------------------------------------
-spec maybe_allow_change(cb_context:context(), path_token()) -> cb_context:context().
maybe_allow_change(Context, PlanId) ->
    case is_reseller(Context) of
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
    case wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"service_plan">> of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('bad_identifier', [{'details', PlanId}], Context)
    end.
