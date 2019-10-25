%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_services).

-export([init/0
        ,authorize/2
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1 ,content_types_provided/2
        ,to_csv/1
        ,validate/1, validate/2, validate/3
        ,post/1 ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"services/plans">>).
-define(AUDIT_LOG_LIST, <<"services/audit_logs_by_creation">>).
-define(SYNCHRONIZATION, <<"synchronization">>).
-define(RECONCILIATION, <<"reconciliation">>).
-define(EDITABLE, <<"editable">>).
-define(AVAILABLE, <<"available">>).
-define(QUOTE, <<"quote">>).
-define(SUMMARY, <<"summary">>).
-define(AUDIT, <<"audit">>).
-define(AUDIT_SUMMARY, <<"audit_summary">>).
-define(OVERRIDES, <<"overrides">>).
-define(TOPUP, <<"topup">>).
-define(MANUAL, <<"manual">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_services_maintenance:db_init(),
    cb_modules_util:bind(?MODULE
                        ,[{<<"*.authorize.services">>, 'authorize'}
                         ,{<<"*.allowed_methods.services">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.services">>, 'resource_exists'}
                         ,{<<"*.content_types_provided.services">>, 'content_types_provided'}
                         ,{<<"*.to_csv.get.services">>, 'to_csv'}
                         ,{<<"*.validate.services">>, 'validate'}
                         ,{<<"*.execute.post.services">>, 'post'}
                         ,{<<"*.execute.patch.services">>, 'patch'}
                         ,{<<"*.execute.delete.services">>, 'delete'}
                         ]).

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> 'true'.
authorize(Context, _Path) ->
    is_authorized(Context
                 ,cb_context:req_verb(Context)
                 ,cb_context:req_nouns(Context)
                 ).

-spec is_authorized(cb_context:context(), req_verb(), req_nouns()) -> 'true'.
is_authorized(_Context, ?HTTP_GET, [{<<"services">>, [?EDITABLE]}]) ->
    'true';
is_authorized(_Context, ?HTTP_GET, [{<<"services">>, [?QUOTE]}]) ->
    'true';
is_authorized(_Context, _, _) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?EDITABLE) ->
    [?HTTP_GET];
allowed_methods(?AVAILABLE) ->
    [?HTTP_GET];
allowed_methods(?SUMMARY) ->
    [?HTTP_GET];
allowed_methods(?AUDIT) ->
    [?HTTP_GET];
allowed_methods(?AUDIT_SUMMARY) ->
    [?HTTP_GET];
allowed_methods(?TOPUP) ->
    [?HTTP_POST];
allowed_methods(?SYNCHRONIZATION) ->
    [?HTTP_POST];
allowed_methods(?RECONCILIATION) ->
    [?HTTP_POST];
allowed_methods(?QUOTE) ->
    [?HTTP_POST];
allowed_methods(?OVERRIDES) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH];
allowed_methods(?MANUAL) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH];
allowed_methods(_PlanId) ->
    [?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?AUDIT, _AuditId) ->
    [?HTTP_GET];
allowed_methods(?AUDIT_SUMMARY, _SourceService) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /services => []
%%    /services/foo => [<<"foo">>]
%%    /services/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?AUDIT, _) -> 'true';
resource_exists(?AUDIT_SUMMARY, _) -> 'true'.

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

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------
-spec to_csv(cb_cowboy_payload()) -> cb_cowboy_payload().
to_csv({Req, Context}) ->
    {Req, to_response(Context, <<"csv">>, cb_context:req_nouns(Context))}.

-spec to_response(cb_context:context(), kz_term:ne_binary(), req_nouns()) ->
                         cb_context:context().
to_response(Context, _, [{<<"services">>, [?SUMMARY]}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    JObj = cb_context:resp_data(Context),
    case kz_json:get_list_value(<<"invoices">>, JObj, []) of
        [] -> Context;
        Invoices ->
            Items = lists:foldl(fun(Invoice, I) ->
                                        kz_json:get_list_value(<<"items">>, Invoice, []) ++ I
                                end
                               ,[]
                               ,Invoices
                               ),
            cb_context:set_resp_data(Context, Items)
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /services might load a list of service_plan objects
%% /services/123 might load the service_plan object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_service_plan(Context, cb_context:req_verb(Context)).

-spec validate_service_plan(cb_context:context(), http_method()) -> cb_context:context().
validate_service_plan(Context, ?HTTP_GET) ->
    pipe_services(Context, [], fun kz_services_plans:assigned/1);
validate_service_plan(Context, ?HTTP_POST) ->
    %% NOTE: allow this account to 'bulk' change their service plan assignments/overrides
    maybe_allow_change(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?EDITABLE) ->
    %% NOTE: show what service plan fields are 'overridable'
    list_editable(Context);
validate(Context, ?AVAILABLE) ->
    %% NOTE: list service plans available to this account for selection
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services_reseller:get_id(AccountId),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    Options =
        case kz_term:is_true(cb_context:req_value(Context, <<"details">>, 'false')) of
            'false' -> [];
            'true' -> ['include_docs']
        end,
    crossbar_doc:load_view(?CB_LIST
                          ,Options
                          ,cb_context:set_account_db(Context, ResellerDb)
                          ,fun normalize_available_view_results/2
                          );
validate(Context, ?SUMMARY) ->
    %% NOTE: summarize this accounts billing details based on the assigned service plans
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2
                        ,kz_services:summary(cb_context:account_id(Context))
                        }
                       ]
                      );
validate(Context, ?AUDIT) ->
    %% NOTE: show the billing audit logs
    load_audit_logs(Context);
validate(Context, ?AUDIT_SUMMARY) ->
    StartKeyFun = fun(Timestamp) -> [audit_summary_range_key(Timestamp)] end,
    EndKeyFun = fun(Timestamp) -> [audit_summary_range_key(Timestamp), <<16#fff0/utf8>>] end,
    ViewOptions = [{'group_level', 2}
                  ,{'mapper', fun normalize_day_summary_by_date/2}
                  ,{'range_start_keymap', StartKeyFun}
                  ,{'range_end_keymap', EndKeyFun}
                  ],
    ViewName = <<"services/day_summary_by_date">>,
    audit_summary(Context, ViewName, ViewOptions);
validate(Context, ?TOPUP) ->
    %% NOTE: top-up the accounts credit
    validate_topup_amount(Context);
validate(Context, ?MANUAL) ->
    %% NOTE: manage the manual quantities
    case is_allowed(Context) of
        {'ok', _} -> validate_manual_quantities(Context, cb_context:req_verb(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate(Context, PlanId) ->
    validate_service_plan(Context, PlanId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?AUDIT, ?MATCH_MODB_PREFIX(Year, Month, _)=AuditId) ->
    %% NOTE: show a billing audit log
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context, AccountDb),
    crossbar_doc:load(AuditId, Context1, ?TYPE_CHECK_OPTION(<<"audit_log">>));
validate(Context, ?AUDIT, AuditId) ->
    ErrorCause = kz_json:from_list([{<<"cause">>, AuditId}]),
    cb_context:add_system_error('bad_identifier', ErrorCause, Context);
validate(Context, ?AUDIT_SUMMARY, SourceService) ->
    RangeKeyFun = fun(Timestamp) -> audit_summary_range_key(SourceService, Timestamp) end,
    ViewOptions = [{'group_level', 2}
                  ,{'mapper', fun normalize_day_summary_by_source/2}
                  ,{'range_start_keymap', RangeKeyFun}
                  ,{'range_end_keymap', RangeKeyFun}
                  ],
    ViewName = <<"services/day_summary_by_source">>,
    audit_summary(Context, ViewName, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit_summary(cb_context:context(), kz_term:ne_binary(), crossbar_view:options()) -> cb_context:context().
audit_summary(Context, ViewName, Options) ->
    ViewOptions = [{'group', 'true'}
                  ,{'reduce', 'true'}
                  ,{'unchunkable', 'true'}
                  ,{'no_filter', 'true'}
                  ,{'should_paginate', 'false'}
                   | maybe_add_max_range(Context, Options)
                  ],
    crossbar_view:load_modb(Context, ViewName, ViewOptions).

-spec maybe_add_max_range(cb_context:context(), crossbar_view:options()) -> crossbar_view:options().
maybe_add_max_range(Context, Options) ->
    case kz_term:safe_cast(cb_context:req_value(Context, <<"created_to">>), 'undefined', fun kz_term:to_integer/1) of
        T when is_integer(T)
               andalso T > 0 ->
            [{'max_range', ?SECONDS_IN_YEAR} | Options];
        _ -> Options
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit_summary_range_key(integer()) -> crossbar_view:range_keymap().
audit_summary_range_key(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    kz_term:to_binary(
      [kz_term:to_binary(Year), "-", kz_date:pad_month(Month), "-", kz_date:pad_day(Day)]
     ).

-spec audit_summary_range_key(kz_term:ne_binary(), integer()) -> crossbar_view:range_keymap().
audit_summary_range_key(SourceService, Timestamp) ->
    [SourceService, audit_summary_range_key(Timestamp)].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_day_summary_by_date(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_day_summary_by_date(JObj, []) ->
    [DateString, SourceService] = kz_json:get_value(<<"key">>, JObj),
    [kz_json:set_value([SourceService, DateString]
                      ,kz_json:get_value(<<"value">>, JObj)
                      ,kz_json:new()
                      )
    ];
normalize_day_summary_by_date(JObj, [Acc]) ->
    [DateString, SourceService] = kz_json:get_value(<<"key">>, JObj),
    [kz_json:set_value([SourceService, DateString]
                      ,kz_json:get_value(<<"value">>, JObj)
                      ,Acc
                      )
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_day_summary_by_source(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_day_summary_by_source(JObj, Acc) ->
    [_SourceService, DateString] = kz_json:get_value(<<"key">>, JObj),
    [kz_json:from_list([{DateString, kz_json:get_value(<<"value">>, JObj)}]) | Acc].

-spec validate_service_plan(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_service_plan(Context, ?SYNCHRONIZATION, ?HTTP_POST) ->
    %% NOTE: sync this accounts billing details with the bookkeeper
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_service_plan(Context, ?RECONCILIATION, ?HTTP_POST) ->
    %% NOTE: sync this accounts quantities with the services db
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_service_plan(Context, ?QUOTE, ?HTTP_POST) ->
    %% NOTE: create a summary without requiring an account
    %% based on the provided service plans.  If cb_context:account_id
    %% is not empty, use the services plans from the sub-reseller
    %% otherwise master.
    pipe_services(Context
                 ,[fun kz_services:remove_plans/1
                  ,fun(S) -> assign_plans(Context, S) end
                  ,fun(S) -> unassign_plans(Context, S) end
                  ,fun(S) -> override_plans(Context, S) end
                  ]
                 ,fun kz_services:summary/1
                 );
validate_service_plan(Context, ?OVERRIDES, ?HTTP_GET) ->
    %% NOTE: get the global overrides for this account
    pipe_services(Context, [], fun kz_services_plans:overrides/1);
validate_service_plan(Context, ?OVERRIDES, ?HTTP_POST) ->
    %% NOTE: update the global overrides for this account
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_service_plan(Context, ?OVERRIDES, ?HTTP_PATCH) ->
    %% NOTE: update the global overrides for this account
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_service_plan(Context, PlanId, ?HTTP_POST) ->
    %% NOTE: conditionally assign service plan and overrides to the account
    maybe_allow_change(Context, PlanId);
validate_service_plan(Context, PlanId, ?HTTP_DELETE) ->
    %% NOTE: conditionally unassign service plan from the account
    maybe_allow_change(Context, PlanId).

-spec validate_topup_amount(cb_context:context()) -> cb_context:context().
validate_topup_amount(Context) ->
    case get_topup_amount(Context) =< 0 of
        'false' -> cb_context:set_resp_status(Context, 'success');
        'true' ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Field is required but missing">>}]
                   ),
            cb_context:add_validation_error(<<"amount">>, <<"required">>, Msg, Context)
    end.

-spec validate_manual_quantities(cb_context:context(), http_method()) -> cb_context:context().
validate_manual_quantities(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    Services = kz_services:fetch(AccountId),
    JObj = kz_services:manual_quantities(Services),
    crossbar_doc:handle_json_success(JObj, Context);
validate_manual_quantities(Context, _HTTPMethod) ->
    case is_allowed(Context) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    pipe_services(Context
                 ,[fun(S) -> assign_plans(Context, S) end
                  ,fun(S) -> unassign_plans(Context, S) end
                  ,fun(S) -> override_plans(Context, S) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services_plans:assigned/1
                 ).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?SYNCHRONIZATION) ->
    try kz_services_bookkeeper:sync(cb_context:account_id(Context)) of
        _ -> cb_context:set_resp_status(Context, 'success')
    catch
        _E:_R ->
            lager:debug("failed to synchronize account services(~s): ~p", [_E, _R]),
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, ?RECONCILIATION) ->
    try kz_services:reconcile(cb_context:account_id(Context)) of
        _ -> cb_context:set_resp_status(Context, 'success')
    catch
        _E:_R ->
            lager:debug("failed to reconcile account services(~s): ~p", [_E, _R]),
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, ?TOPUP) ->
    AccountId = cb_context:account_id(Context),
    Amount = get_topup_amount(Context),
    Audit = crossbar_services:audit_log(Context),
    Trigger = kz_services_topup:manual_trigger(),
    case kz_services_topup:topup(AccountId, Amount, Trigger, Audit) of
        {'ok', 'undefined', Ledger} ->
            JObj = kz_json:from_list(
                     [{<<"ledger">>, kz_ledger:public_json(Ledger)}]
                    ),
            crossbar_doc:handle_json_success(JObj, Context);
        {'ok', Transaction, Ledger} ->
            JObj = kz_json:from_list(
                     [{<<"transaction">>, kz_transaction:public_json(Transaction)}
                     ,{<<"ledger">>, kz_ledger:public_json(Ledger)}
                     ]
                    ),
            crossbar_doc:handle_json_success(JObj, Context);
        {'error', {'transaction_incomplete', Transaction}} ->
            crossbar_services:transaction_to_error(Context, Transaction);
        {'error', _Reason} ->
            %% TODO: need better errors, from the bookkeeper...
            cb_context:add_system_error('unspecified_fault', Context)
    end;
post(Context, ?QUOTE) ->
    Context;
post(Context, ?OVERRIDES) ->
    Overrides = cb_context:req_data(Context),
    pipe_services(Context
                 ,[fun(S) -> override_plans(Context, S, Overrides) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services_plans:overrides/1
                 );
post(Context, ?MANUAL) ->
    Quantities = cb_context:req_data(Context),
    pipe_services(Context
                 ,[fun(S) -> manual_quantities(Context, S, Quantities) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services:manual_quantities/1
                 );
post(Context, PlanId) ->
    Addition = kz_json:set_value(<<"id">>, PlanId, cb_context:req_data(Context)),
    Options = maybe_merge_overrides(Context),
    pipe_services(Context
                 ,[fun(S) ->
                           kz_services_plan:assign(S, Addition, Options)
                   end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services_plans:assigned/1
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, ?OVERRIDES) ->
    Overrides = cb_context:req_data(Context),
    pipe_services(Context
                 ,[fun(S) -> override_plans(Context, S, Overrides) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services_plans:overrides/1
                 );
patch(Context, ?MANUAL) ->
    Quantities = cb_context:req_data(Context),
    pipe_services(Context
                 ,[fun(S) -> manual_quantities(Context, S, Quantities) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services:manual_quantities/1
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list_editable(cb_context:context()) -> cb_context:context().
list_editable(Context) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            JObj = kz_services_plans:editable_fields(),
            crossbar_doc:handle_json_success(JObj, Context);
        AccountId ->
            list_resellers_editable(Context, AccountId)
    end.

-spec list_resellers_editable(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
list_resellers_editable(Context, AccountId) ->
    case kz_services_reseller:is_reseller(AccountId) of
        'true' ->
            JObj = kz_services_plans:editable_fields(AccountId),
            crossbar_doc:handle_json_success(JObj, Context);
        'false' ->
            ResellerId = kz_services_reseller:get_id(AccountId),
            JObj = kz_services_plans:editable_fields(ResellerId),
            crossbar_doc:handle_json_success(JObj, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_audit_logs(cb_context:context()) -> cb_context:context().
load_audit_logs(Context) ->
    Options = [{'mapper', fun normalize_audit_view_results/2}
              ,{'range_start_keymap', []}
              ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
              ],
    crossbar_view:load_modb(Context, ?AUDIT_LOG_LIST, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
override_plans(Context, Services) ->
    case kz_json:get_ne_json_value(<<"overrides">>, cb_context:req_data(Context)) of
        'undefined' -> Services;
        Overrides -> override_plans(Context, Services, Overrides)
    end.

-spec override_plans(cb_context:context(), kz_services:services(), kz_term:api_json()) -> kz_services:services().
override_plans(Context, Services, Overrides) ->
    kz_services_plans:override(Services, Overrides, maybe_merge_overrides(Context)).

-spec maybe_merge_overrides(cb_context:context()) -> kz_term:proplist().
maybe_merge_overrides(Context) ->
    case kz_json:is_true(<<"merge">>, cb_context:req_json(Context), 'false')
        orelse cb_context:req_verb(Context) =:= ?HTTP_PATCH
    of
        'false' -> [];
        'true' -> ['merge']
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec manual_quantities(cb_context:context(), kz_services:services(), kz_term:api_json()) -> kz_services:services().
manual_quantities(Context, Services, Quantities) ->
    kz_services:set_manual_updates(Services, Quantities, maybe_merge_quantities(Context)).

-spec maybe_merge_quantities(cb_context:context()) -> kz_term:proplist().
maybe_merge_quantities(Context) ->
    case kz_json:is_true(<<"merge">>, cb_context:req_json(Context), 'false')
        orelse cb_context:req_verb(Context) =:= ?HTTP_PATCH
    of
        'false' -> [];
        'true' -> ['merge']
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, PlanId) ->
    pipe_services(Context
                 ,[fun(S) -> kz_services_plan:unassign(S, PlanId) end
                  ,fun kz_services:commit/1
                  ]
                 ,fun kz_services_plans:assigned/1
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
assign_plans(Context, Services) ->
    Additions = kz_json:get_first_defined([<<"add">>
                                          ,<<"plans">>
                                          ]
                                         ,cb_context:req_data(Context)
                                         ,[]
                                         ),
    Options = maybe_merge_overrides(Context),
    lists:foldl(fun(Addition, S) ->
                        kz_services_plan:assign(S, Addition, Options)
                end
               ,Services
               ,Additions
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unassign_plans(cb_context:context(), kz_services:services()) -> kz_services:services().
unassign_plans(Context, Services) ->
    ReqData = cb_context:req_data(Context),
    lists:foldl(fun(PlanId, S) -> kz_services_plan:unassign(S, PlanId) end
               ,Services
               ,kz_json:get_value(<<"delete">>, ReqData, [])
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type services_pipe() :: fun((kz_services:services()) -> kz_services:services()).
-type resp_function() :: fun((kz_json:object()) -> kz_json:object()).
-spec pipe_services(cb_context:context(), [services_pipe()], resp_function()) -> cb_context:context().
pipe_services(Context, Routines, RespFunction) ->
    AccountId = cb_context:account_id(Context),
    AuditLog = crossbar_services:audit_log(Context),
    Services = lists:foldl(fun (F, S) -> F(S) end
                          ,kz_services:set_audit_log(kz_services:fetch(AccountId), AuditLog)
                          ,Routines
                          ),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, RespFunction(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_available_view_results(kz_json:object(), kz_json:objects()) ->
                                              kz_json:objects().
normalize_available_view_results(JObj, Acc) ->
    case kz_json:get_ne_json_value(<<"doc">>, JObj) of
        'undefined' -> [kz_json:get_json_value(<<"value">>, JObj)|Acc];
        Doc -> [kz_doc:public_fields(Doc)|Acc]
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_audit_view_results(kz_json:object(), kz_json:objects()) ->
                                          kz_json:objects().
normalize_audit_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Check if you have the permission to update or delete service plans
%% @end
%%------------------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> {'ok', kz_term:ne_binary()} | 'false'.
is_allowed(Context) ->
    ResellerId = kz_services_reseller:get_id(cb_context:account_id(Context)),
    AuthAccountId = cb_context:auth_account_id(Context),
    case AuthAccountId =:= ResellerId
        orelse cb_context:is_superduper_admin(Context)
    of
        'true' -> {'ok', ResellerId};
        'false' -> 'false'
    end.

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
    AddPlanIds = get_plan_ids(Context),
    check_plan_ids(maybe_forbid_delete(Context), ResellerId, AddPlanIds).

-spec get_plan_ids(cb_context:context()) -> kz_term:ne_binaries().
get_plan_ids(Context) ->
    ReqData = cb_context:req_data(Context),
    Items = kz_json:get_value(<<"add">>, ReqData, []),
    get_plan_ids(Items, []).

-spec get_plan_ids(kz_term:ne_binaries() | kz_json:objects(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_plan_ids([], Ids) -> Ids;
get_plan_ids([Item|Items], Ids) ->
    case kz_json:is_json_object(Item) of
        'false' -> get_plan_ids(Items, maybe_add_plan_id(Item, Ids));
        'true' ->
            Id = kz_doc:id(Item),
            get_plan_ids(Items, maybe_add_plan_id(Id, Ids))
    end.

-spec maybe_add_plan_id(kz_term:api_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
maybe_add_plan_id(Id=?NE_BINARY, Ids) -> [Id|Ids];
maybe_add_plan_id(_, Ids) -> Ids.

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
    %% TODO: is there a cleaner way to do this... yes...
    Services = kz_services:fetch(cb_context:account_id(Context)),
    ServiceJObj = kz_services:services_jobj(Services),
    ExistingPlansIds = kzd_services:plan_ids(ServiceJObj),
    case DeletePlansIds -- ExistingPlansIds of
        [] -> Context;
        _ -> cb_context:add_system_error('plan_is_not_assigned', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_topup_amount(cb_context:context()) -> kz_currency:units().
get_topup_amount(Context) ->
    ReqData = cb_context:req_data(Context),
    kz_currency:dollars_to_units(
      kz_json:get_float_value(<<"amount">>, ReqData, 0)
     ).
