%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.service_plans">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.service_plans">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.service_plans">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.service_plans">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.service_plans">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.service_plans">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    %% <<"current">>, PLAN_ID
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
validate(#cb_context{req_verb = ?HTTP_GET, account_id=AccountId, auth_account_id=AuthId}=Context) ->
    ResellerId = case wh_services:is_reseller(AccountId) andalso AccountId =:= AuthId of
                     'true' -> AccountId;
                     'false' -> wh_services:find_reseller_id(AccountId)
                 end,
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,cb_context:set_account_db(Context, ResellerDb)
                           ,fun normalize_view_results/2
                          ).

validate(#cb_context{req_verb = ?HTTP_GET, account_id=AccountId}=Context, <<"current">>) ->
    Services = wh_services:fetch(AccountId),
    Services1 = case wh_services:is_dirty(Services) of
                    'true' -> wh_services:reconcile_only(Services);
                    'false' -> Services
                end,
    Context#cb_context{resp_status='success', resp_data=wh_services:public_json(Services1)};
validate(Context, <<"current">>) ->
    cb_context:add_system_error('bad_identifier', Context);
validate(#cb_context{req_verb = ?HTTP_GET, account_id=AccountId}=Context, PlanId) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb));
validate(#cb_context{req_verb = ?HTTP_POST}=Context, <<"synchronization">>) ->
    case is_reseller(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, <<"reconciliation">>) ->
    case is_reseller(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, PlanId) ->
    maybe_allow_change(Context, PlanId);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, PlanId) ->
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
-spec is_reseller(cb_context:context()) -> {'ok', ne_binary()} | boolean().
is_reseller(Context) ->
    ResellerId = wh_services:find_reseller_id(cb_context:account_id(Context)),
    case cb_context:auth_account_id(Context) =:= ResellerId of
        'true' -> {'ok', ResellerId};
        'false' -> 'false'
    end.

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
-spec check_plan_id(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
check_plan_id(Context, PlanId, ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    case crossbar_doc:load(PlanId, cb_context:set_account_db(Context, ResellerDb)) of
        #cb_context{resp_status='success', doc=JObj} ->
            is_service_plan(Context, PlanId, JObj);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_service_plan(cb_context:context(), path_token(), wh_json:object()) -> cb_context:context().
is_service_plan(Context, PlanId, JObj) ->
    case wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"service_plan">> of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('bad_identifier', [{'details', PlanId}], Context)
    end.
