%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_service_planner).

-export([init/0
        ,authorize/1, authorize/2
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST_SERVICE_PLANES, <<"service_plans/crossbar_listing">>).

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
    _ = crossbar_bindings:bind(<<"*.authorize.service_planner">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.service_planner">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.service_planner">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.service_planner">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.service_planner">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.service_planner">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.service_planner">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.service_planner">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.service_planner">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean() | {'halt', cb_context:context()}.
authorize(Context) -> is_authorize(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean() | {'halt', cb_context:context()}.
authorize(Context, _) -> is_authorize(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec is_authorize(cb_context:context(), req_verb(), req_nouns()) -> boolean() | {'halt', cb_context:context()}.
is_authorize(_Context, ?HTTP_GET, [{<<"service_planner">>, _}]) ->
    'true';
is_authorize(Context, _, [{<<"service_planner">>, _}]) ->
    {'halt', cb_context:add_system_error('forbidden', Context)};
is_authorize(Context, _, _) ->
    AccountId = cb_context:account_id(Context),
    (kz_term:is_not_empty(AccountId)
     andalso AccountId =:= cb_context:auth_account_id(Context)
     andalso kz_services:is_reseller(AccountId)
    )
        orelse cb_context:is_superduper_admin(Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_PlanId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /security => []
%%    /security/foo => [<<"foo">>]
%%    /security/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_PlanId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /service_planner mights load a list of auth objects
%% /service_planner/123 might load the auth object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"service_planner">>, _}] ->
            summary_available_fields(Context);
        _ ->
            validate_planner(Context, cb_context:req_verb(Context))
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PlanId) ->
    validate_planner(Context, PlanId, cb_context:req_verb(Context)).

%% validates /service_planner
-spec validate_planner(cb_context:context(), http_method()) -> cb_context:context().
validate_planner(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST_SERVICE_PLANES, [] ,Context, fun normalize_view_results/2);
validate_planner(Context, ?HTTP_PUT) ->
    create(Context).

%% validates /service_planner/plane_id
-spec validate_planner(cb_context:context(), http_method(), http_method()) -> cb_context:context().
validate_planner(Context, PlanId, ?HTTP_GET) ->
    crossbar_doc:load(PlanId, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type()));
validate_planner(Context, PlanId, ?HTTP_POST) ->
    update(PlanId, Context);
validate_planner(Context, PlanId, ?HTTP_PATCH) ->
    validate_patch(PlanId, Context);
validate_planner(Context, PlanId, ?HTTP_DELETE) ->
    crossbar_doc:load(PlanId, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _PlanId) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _PlanId) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _PlanId) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns fixtures of what fields in service plans is customizable
%% @end
%%--------------------------------------------------------------------
-spec summary_available_fields(cb_context:context()) -> cb_context:context().
summary_available_fields(Context) ->
    JObj = read_service_plan_editable(),
    UIApps = kz_json:from_list(get_ui_apps(kapps_util:get_master_account_id())),
    crossbar_doc:handle_json_success(kz_json:set_value(<<"ui_apps">>, UIApps, JObj), Context).

-spec read_service_plan_editable() -> kz_json:object().
read_service_plan_editable() ->
    Path = filename:join([code:priv_dir('crossbar'), "service_plan_editable_fields.json"]),
    case file:read_file(Path) of
        {'ok', Bin} -> kz_json:decode(Bin);
        {'error', _Reason} ->
            lager:debug("failed to read file ~s: ~p", [Path, _Reason]),
            kz_json:new()
    end.

-spec get_ui_apps({'ok', ne_binary()} | {'error', any()}) -> kz_proplist().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"service_plans">>, Context, OnSuccess).

-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"service_plans">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), kzd_service_plan:type()));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_service_plan:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
