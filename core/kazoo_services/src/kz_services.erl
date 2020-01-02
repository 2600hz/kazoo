%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services).

-export([account_id/1]).
-export([services_jobj/1
        ,set_services_jobj/2
        ]).
-export([current_services_jobj/1
        ,set_current_services_jobj/2
        ]).
-export([plans/1
        ,current_plans/1
        ,proposed_plans/1
        ,has_plans/1
        ,plans_foldl/3
        ,hydrate_plans/1
        ,reset_plans/1
        ,remove_plans/1
        ]).
-export([reset_quantities/1]).
-export([account_quantities/1
        ,hydrate_account_quantities/1
        ,reset_account_quantities/1
        ]).
-export([cascade_quantities/1
        ,hydrate_cascade_quantities/1
        ,reset_cascade_quantities/1
        ]).
-export([manual_quantities/1
        ,hydrate_manual_quantities/1
        ,reset_manual_quantities/1
        ]).
-export([has_updates/1
        ,set_updates/4
        ,reset_updates/1
        ]).
-export([account_updates/1
        ,account_current_billables/1
        ,account_proposed_billables/1
        ,set_account_updates/3
        ,reset_account_updates/1
        ]).
-export([cascade_updates/1
        ,cascade_current_billables/1
        ,cascade_proposed_billables/1
        ,set_cascade_updates/3
        ,reset_cascade_updates/1
        ]).
-export([manual_updates/1
        ,set_manual_updates/2
        ,set_manual_updates/3
        ,reset_manual_updates/1
        ]).
-export([invoices/1
        ,invoices_foldl/3
        ,hydrate_invoices/1
        ,reset_invoices/1
        ]).
-export([bookkeeper_type/1
        ,set_bookkeeper_type/2
        ]).
-export([bookkeeper_vendor_id/1
        ,set_bookkeeper_vendor_id/2
        ]).
-export([is_dirty/1]).
-export([is_deleted/1]).
-export([audit_log/1
        ,set_audit_log/2
        ]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1
        ,public_json/2
        ]).
-export([summary/1]).

-export([fetch/1
        ,fetch/2
        ]).
-export([commit_updates/3
        ,commit_updates/4
        ]).
-export([commit/1]).
-export([commit_account/1]).
-export([maybe_save_services_jobj/1]).
-export([save_services_jobj/1]).
-export([delete/1]).
-export([reconcile/1
        ,reconcile/2
        ]).
-export([to_billables/1]).

-export([is_services/1]).

-include("services.hrl").

-define(DONT_CASCADE_MASTER, kapps_config:get_is_false(?CONFIG_CAT, <<"cascade_commits_to_master_account">>, 'true')).

-record(kz_services, {account_id :: kz_term:api_ne_binary()
                     ,account_quantities = 'undefined' :: kz_term:api_object()
                     ,account_updates = kz_json:new() :: kz_json:object()
                     ,account_current_billables = [] :: kz_services_quantities:billables()
                     ,account_proposed_billables = [] :: kz_services_quantities:billables()
                     ,audit_log = kz_json:new() :: kz_json:object()
                     ,cascade_quantities = 'undefined' :: kz_term:api_object()
                     ,cascade_updates = kz_json:new() :: kz_json:object()
                     ,cascade_current_billables = [] :: kz_json:objects()
                     ,cascade_proposed_billables = [] :: kz_json:objects()
                     ,current_services_jobj = kzd_services:new() :: kzd_services:doc()
                     ,dirty = 'false' :: boolean()
                     ,invoices = 'undefined' :: 'undefined' | kz_services_invoices:invoices()
                     ,manual_quantities = 'undefined' :: kz_term:api_object()
                     ,manual_updates = kz_json:new() :: kz_json:object()
                     ,plans = 'undefined' ::  'undefined' | kz_services_plans:plans()
                     ,services_jobj = kzd_services:new() :: kzd_services:doc()
                     }).

-type services() :: #kz_services{}.
-type setter_fun() :: {fun((services(), Value) -> services()), Value}.
-type setter_funs() :: [setter_fun()].
-type plans_foldl() :: fun((services(), kz_term:ne_binary(), kz_json:objects(), Acc) -> Acc).
-type invoices_foldl() :: fun((services(), kz_json:objects(), Acc) -> Acc).

-type fetch_option() :: 'skip_cache' |
                        'hydrate_plans' |
                        'hydrate_account_quantities' |
                        'hydrate_cascade_quantities' |
                        'hydrate_manual_quantities' |
                        'hydrate_invoices' |
                        {'updates', kz_term:ne_binary(), kz_services_quantities:billables(), kz_services_quantities:billables()} |
                        {'audit_log', kz_json:object()}.
-type fetch_options() :: [fetch_option()].

-export_type([services/0
             ,plans_foldl/0
             ,invoices_foldl/0
             ,setter_fun/0
             ,setter_funs/0
             ,fetch_options/0
             ]).

%%------------------------------------------------------------------------------
%% @doc Returns the account id of the {@link services()}.
%% @end
%%------------------------------------------------------------------------------
-spec account_id(services()) -> kz_term:api_ne_binary().
account_id(#kz_services{account_id=AccountId}) ->
    AccountId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec services_jobj(services()) -> kzd_services:doc().
services_jobj(#kz_services{services_jobj=ServicesJObj}) ->
    ServicesJObj.

-spec set_services_jobj(services(), kzd_services:doc()) -> services().
set_services_jobj(#kz_services{}=Services, ServicesJObj) ->
    Services#kz_services{services_jobj=ServicesJObj
                        ,account_id=kz_doc:id(ServicesJObj)
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec current_services_jobj(services()) -> kzd_services:doc().
current_services_jobj(#kz_services{current_services_jobj=CurrentServicesJObj}) ->
    CurrentServicesJObj.

-spec set_current_services_jobj(services(), kzd_services:doc()) -> services().
set_current_services_jobj(#kz_services{}=Services, CurrentServicesJObj) ->
    Services#kz_services{current_services_jobj=CurrentServicesJObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type maybe_fetch_routines() :: [{boolean(), {atom(), any()}}].
-spec current_plans(services()) -> kz_services_plans:plans().
current_plans(Services) ->
    CurrentPlans = kzd_services:plans(current_services_jobj(Services)),
    ProposedPlans = kzd_services:plans(services_jobj(Services)),

    ProposedBillables = account_proposed_billables(Services),
    CurrentBillables = account_current_billables(Services),
    UpdatedObjectPlans = find_object_plans(CurrentBillables, ProposedBillables),

    Routines = [{kz_json:are_equal(CurrentPlans, ProposedPlans)
                ,{'services_jobj', current_services_jobj(Services)}
                }
               ,{kz_term:is_empty(UpdatedObjectPlans)
                ,{'modified_object_plans', UpdatedObjectPlans}
                }
               ],
    maybe_fetch_plans(Services, Routines).

-spec proposed_plans(services()) -> kz_services_plans:plans().
proposed_plans(Services) ->
    CurrentPlans = kzd_services:plans(current_services_jobj(Services)),
    ProposedPlans = kzd_services:plans(services_jobj(Services)),

    ProposedBillables = account_proposed_billables(Services),
    CurrentBillables = account_current_billables(Services),
    UpdatedObjectPlans = find_object_plans(ProposedBillables, CurrentBillables),

    Routines = [{kz_json:are_equal(CurrentPlans, ProposedPlans)
                ,{'services_jobj', services_jobj(Services)}
                }
               ,{kz_term:is_empty(UpdatedObjectPlans)
                ,{'modified_object_plans', UpdatedObjectPlans}
                }
               ],
    maybe_fetch_plans(Services, Routines).

-spec plans(services()) -> kz_services_plans:plans().
plans(#kz_services{plans='undefined'}=Services) ->
    kz_services_plans:fetch(Services);
plans(#kz_services{plans=Plans}) ->
    Plans.

-spec has_plans(services() | kz_term:ne_binary()) -> boolean().
has_plans(?NE_BINARY = AccountId) ->
    has_plans(fetch(AccountId));
has_plans(#kz_services{}=Services) ->
    not kz_services_plans:is_empty(plans(Services)).

-spec has_proposed_object_plans(services()) -> boolean().
has_proposed_object_plans(Services) ->
    ProposedBillables = account_proposed_billables(Services),
    CurrentBillables = account_current_billables(Services),
    kz_term:is_not_empty(find_object_plans(ProposedBillables, CurrentBillables)).

-spec plans_foldl(plans_foldl(), Acc, services()) -> Acc.
plans_foldl(FoldFun, Acc, Services) ->
    ServicesFoldFun = fun(BookkeeperId, PlansList, A) ->
                              FoldFun(Services, BookkeeperId, PlansList, A)
                      end,
    kz_services_plans:foldl(ServicesFoldFun, Acc, plans(Services)).

-spec hydrate_plans(services()) -> services().
hydrate_plans(Services) ->
    Plans = kz_services_plans:fetch(Services),
    Services#kz_services{plans=Plans}.

-spec reset_plans(services()) -> services().
reset_plans(Services) ->
    Services#kz_services{plans='undefined'}.

-spec remove_plans(services()) -> services().
remove_plans(Services) ->
    ServicesJObj =
        kzd_services:set_plans(services_jobj(Services), kz_json:new()),
    reset_plans(set_services_jobj(Services, ServicesJObj)).

-spec find_object_plans(kz_services_quantities:billables(), kz_services_quantities:billables()) -> kz_json:objects().
find_object_plans(JObjsA, JObjsB) when is_list(JObjsA), is_list(JObjsB) ->
    FilterFun = find_object_plans_filter(
                  find_object_plans(JObjsB)
                 ),
    lists:filter(FilterFun, find_object_plans(JObjsA)).

-spec find_object_plans_filter(kz_services_quantities:billables()) -> fun((kz_services_quantities:billable()) -> boolean()).
find_object_plans_filter(ObjectPlans) ->
    Plans = [{kz_doc:id(Plan), Plan}
             || Plan <- ObjectPlans
            ],
    fun(JObj) ->
            case props:get_value(kz_doc:id(JObj), Plans) of
                'undefined' -> 'true';
                Plan ->
                    not kz_json:are_equal(JObj, Plan)
            end
    end.

-spec find_object_plans(kz_services_quantities:billables()) -> kz_json:objects().
find_object_plans(JObjs) when is_list(JObjs) ->
    lists:foldl(fun find_object_plans_fold/2, [], JObjs).

-spec find_object_plans_fold(kz_json:object(), kz_json:objects()) -> kz_json:objects().
find_object_plans_fold(JObj, Plans) ->
    case create_services_object_plans(JObj) of
        'undefined' -> Plans;
        Plan -> [Plan|Plans]
    end.

-spec create_services_object_plans(kz_json:object()) -> kz_term:api_object().
create_services_object_plans(JObj) ->
    case kz_json:get_ne_json_value([<<"service">>, <<"plans">>], JObj) of
        'undefined' -> 'undefined';
        Value ->
            kz_json:from_list(
              [{<<"id">>,  kz_doc:id(JObj, kz_binary:rand_hex(16))}
              ,{<<"key">>, kz_doc:type(JObj)}
              ,{<<"value">>, Value}
              ]
             )
    end.

-spec maybe_fetch_plans(services(), maybe_fetch_routines()) -> kz_services_plans:plans().
maybe_fetch_plans(Services, Routines) ->
    Options = lists:foldl(fun({'false', {Key, Value}}, Map) ->
                                  maps:put(Key, Value, Map);
                             (_, Map) -> Map
                          end
                         ,#{}
                         ,Routines
                         ),
    case kz_term:is_empty(maps:keys(Options)) of
        'true' -> plans(Services);
        'false' -> kz_services_plans:fetch(Services, Options)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset_quantities(services()) -> services().
reset_quantities(Services) ->
    Services#kz_services{account_quantities='undefined'
                        ,cascade_quantities='undefined'
                        ,manual_quantities='undefined'
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_quantities(services()) -> kz_json:object().
account_quantities(#kz_services{account_quantities='undefined'}=Services) ->
    Updates = account_updates(Services),
    Quantities = kzd_services:account_quantities(services_jobj(Services)),
    sum_quantities_updates(Quantities, Updates);
account_quantities(#kz_services{account_quantities=Quantities}) ->
    Quantities.

-spec hydrate_account_quantities(services()) -> services().
hydrate_account_quantities(Services) ->
    Quantities = kz_services_quantities:fetch_account(Services),
    Services#kz_services{account_quantities=Quantities}.

-spec reset_account_quantities(services()) -> services().
reset_account_quantities(Services) ->
    Services#kz_services{account_quantities='undefined'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_quantities(services()) -> kz_json:object().
cascade_quantities(#kz_services{cascade_quantities='undefined'}=Services) ->
    Updates = cascade_updates(Services),
    Quantities = kzd_services:cascade_quantities(services_jobj(Services)),
    sum_quantities_updates(Quantities, Updates);
cascade_quantities(#kz_services{cascade_quantities=Quantities}) ->
    Quantities.

-spec hydrate_cascade_quantities(services()) -> services().
hydrate_cascade_quantities(Services) ->
    Quantities = kz_services_quantities:fetch_cascade(Services),
    Services#kz_services{cascade_quantities=Quantities}.

-spec reset_cascade_quantities(services()) -> services().
reset_cascade_quantities(Services) ->
    Services#kz_services{cascade_quantities='undefined'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec manual_quantities(services()) -> kz_json:object().
manual_quantities(Services) ->
    Updates = manual_updates(Services),
    case kz_term:is_empty(Updates) of
        'true' -> kzd_services:manual_quantities(services_jobj(Services));
        'false' -> Updates
    end.

-spec hydrate_manual_quantities(services()) -> services().
hydrate_manual_quantities(Services) ->
    Quantities = kzd_services:manual_quantities(services_jobj(Services)),
    Services#kz_services{manual_quantities=Quantities}.

-spec reset_manual_quantities(services()) -> services().
reset_manual_quantities(Services) ->
    Services#kz_services{manual_quantities='undefined'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_updates(services()) -> boolean().
has_updates(Services) ->
    not kz_term:is_empty(account_updates(Services))
        orelse not kz_term:is_empty(cascade_updates(Services))
        orelse not kz_term:is_empty(manual_updates(Services)).

-spec set_updates(services(), kz_term:ne_binary(), kz_services_quantities:billables(), kz_services_quantities:billables()) -> services().
set_updates(Services, Account, Current, Proposed) ->
    AccountId = kzs_util:format_account_id(Account),
    case account_id(Services) =:= AccountId of
        'true' -> set_account_updates(Services, Current, Proposed);
        'false' -> set_cascade_updates(Services, Current, Proposed)
    end.

-spec reset_updates(services()) -> services().
reset_updates(Services) ->
    Routines = [fun reset_account_updates/1
               ,fun reset_cascade_updates/1
               ,fun reset_manual_updates/1
               ],
    lists:foldl(fun(F, S) -> F(S) end
               ,Services
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_updates(services()) -> kz_json:object().
account_updates(#kz_services{account_updates=Updates}) ->
    Updates.

-spec account_current_billables(services()) -> kz_services_quantities:billables().
account_current_billables(#kz_services{account_current_billables=Current}) ->
    Current.

-spec account_proposed_billables(services()) -> kz_services_quantities:billables().
account_proposed_billables(#kz_services{account_proposed_billables=Proposed}) ->
    Proposed.

-spec set_account_updates(services(), kz_services_quantities:billables(), kz_services_quantities:billables()) -> services().
set_account_updates(#kz_services{}=Services, Current, Proposed) when is_list(Current), is_list(Proposed) ->
    Updates = kz_services_quantities:calculate_updates(Services, Current, Proposed),
    Services#kz_services{account_updates=Updates
                        ,account_current_billables=Current
                        ,account_proposed_billables=Proposed
                        }.

-spec reset_account_updates(services()) -> services().
reset_account_updates(Services) ->
    Services#kz_services{account_updates=kz_json:new()
                        ,account_current_billables=[]
                        ,account_proposed_billables=[]
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_updates(services()) -> kz_json:object().
cascade_updates(#kz_services{cascade_updates=Updates}) ->
    Updates.

-spec cascade_current_billables(services()) -> kz_json:objects().
cascade_current_billables(#kz_services{cascade_current_billables=Current}) ->
    Current.

-spec cascade_proposed_billables(services()) -> kz_json:objects().
cascade_proposed_billables(#kz_services{cascade_proposed_billables=Proposed}) ->
    Proposed.

-spec set_cascade_updates(services(), kz_services_quantities:billables(), kz_services_quantities:billables()) -> services().
set_cascade_updates(#kz_services{}=Services, Current, Proposed) ->
    Updates = kz_services_quantities:calculate_updates(Services, Current, Proposed),
    Services#kz_services{cascade_updates=Updates
                        ,cascade_current_billables=Current
                        ,cascade_proposed_billables=Proposed
                        }.

-spec reset_cascade_updates(services()) -> services().
reset_cascade_updates(Services) ->
    Services#kz_services{cascade_updates=kz_json:new()
                        ,cascade_current_billables=[]
                        ,cascade_proposed_billables=[]
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec manual_updates(services()) -> kz_json:object().
manual_updates(#kz_services{manual_updates=Updates}) ->
    Updates.

-spec set_manual_updates(services(), kz_json:object()) -> services().
set_manual_updates(Services, Quantities) ->
    set_manual_updates(Services, Quantities, []).

-spec set_manual_updates(services(), kz_json:object(), kz_term:proplist()) -> services().
set_manual_updates(Services, Quantities, Options) ->
    case props:get_is_true('merge', Options, 'false') of
        'false' -> Services#kz_services{manual_updates=Quantities};
        'true' ->
            UpdatedQuantities = kz_json:merge_recursive(
                                  [manual_quantities(Services), Quantities]
                                 ),
            Services#kz_services{manual_updates=UpdatedQuantities}
    end.

-spec reset_manual_updates(services()) -> services().
reset_manual_updates(Services) ->
    Services#kz_services{manual_updates=kz_json:new()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec invoices(services()) -> kz_services_invoices:invoices().
invoices(#kz_services{invoices='undefined'}=Services) ->
    kz_services_invoices:create(Services);
invoices(#kz_services{invoices=Invoices}) ->
    Invoices.

-spec invoices_foldl(invoices_foldl(), Acc, services()) -> Acc.
invoices_foldl(FoldFun, Acc, Services) ->
    ServicesFoldFun = fun(Invoice, A) ->
                              FoldFun(Services, Invoice, A)
                      end,
    kz_services_invoices:foldl(ServicesFoldFun, Acc, invoices(Services)).

-spec hydrate_invoices(services()) -> services().
hydrate_invoices(#kz_services{}=Services) ->
    Invoices = kz_services_invoices:create(Services),
    Services#kz_services{invoices=Invoices}.

-spec reset_invoices(services()) -> services().
reset_invoices(Services) ->
    Services#kz_services{invoices='undefined'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(services()) -> kz_term:ne_binary().
bookkeeper_type(Services) ->
    case kzd_services:bookkeeper_type(services_jobj(Services), 'undefined') of
        'undefined' -> maybe_master_bookkeeper_type(Services);
        BookkeeperType -> BookkeeperType
    end.

-spec maybe_master_bookkeeper_type(services()) -> kz_term:ne_binary().
maybe_master_bookkeeper_type(Services) ->
    ResellerId = kz_services_reseller:get_id(Services),
    case kapps_util:get_master_account_id() of
        {'ok', ResellerId} -> master_bookeeper_type();
        {'ok', _OtherId} -> kzd_services:default_bookkeeper_type()
    end.

-spec master_bookeeper_type() -> kz_term:ne_binary().
master_bookeeper_type() ->
    kapps_config:get_ne_binary(?CONFIG_CAT, <<"master_account_bookkeeper">>, kzd_services:default_bookkeeper_type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_type(services(), kz_term:ne_binary()) -> services().
set_bookkeeper_type(Services, BookkeeperType) ->
    ServicesJObj = kzd_services:set_bookkeeper_type(services_jobj(Services), BookkeeperType),
    set_services_jobj(Services, ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(services()) -> kz_term:ne_binary().
bookkeeper_vendor_id(Services) ->
    case kzd_services:bookkeeper_vendor_id(services_jobj(Services), 'undefined') of
        'undefined' -> kz_services_reseller:get_id(Services);
        BookkeeperVendorId -> BookkeeperVendorId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_vendor_id(services(), kz_term:ne_binary()) -> services().
set_bookkeeper_vendor_id(Services, BookkeeperVendorId) ->
    ServicesJObj = kzd_services:set_bookkeeper_vendor_id(services_jobj(Services), BookkeeperVendorId),
    set_services_jobj(Services, ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_dirty(services()) -> boolean().
is_dirty(#kz_services{dirty=Dirty}) -> Dirty.

-spec set_dirty(services()) -> services().
set_dirty(Services) ->
    Services#kz_services{dirty='true'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_deleted(services()) -> boolean().
is_deleted(Services) ->
    kz_doc:is_soft_deleted(services_jobj(Services)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit_log(services()) -> kz_json:object().
audit_log(#kz_services{audit_log=AuditLog}=Services) ->
    Routines = [fun add_audit_log_account/2
               ,fun add_audit_log_changes_type/2
               ,fun add_audit_log_changes_account/2
               ],
    lists:foldl(fun(F, A) -> F(Services, A) end
               ,AuditLog
               ,Routines
               ).

-spec set_audit_log(services(), kz_json:object()) -> services().
set_audit_log(Services, AuditLog) ->
    Services#kz_services{audit_log=AuditLog}.

-spec set_initial_audit_log(services(), kz_json:object()) -> services().
set_initial_audit_log(Services, AuditLog) ->
    Services#kz_services{audit_log=add_audit_log_changes_account(Services, AuditLog)}.

-spec add_audit_log_account(services(), kz_json:object()) -> kz_json:object().
add_audit_log_account(Services, AuditLog) ->
    AccountId = account_id(Services),
    Props = [{[<<"account">>, <<"id">>], AccountId}
            ,{[<<"account">>, <<"name">>], kzd_accounts:fetch_name(AccountId)}
            ],
    kz_json:set_values(Props, AuditLog).

-spec add_audit_log_changes_type(services(), kz_json:object()) -> kz_json:object().
add_audit_log_changes_type(Services, AuditLog) ->
    add_audit_log_changes_type(Services
                              ,AuditLog
                              ,kz_json:get_ne_binary_value([<<"changes">>, <<"type">>], AuditLog)
                              ).

-spec add_audit_log_changes_type(services(), kz_json:object(), kz_term:api_ne_binary()) ->
                                        kz_json:object().
add_audit_log_changes_type(Services, AuditLog, 'undefined') ->
    lists:foldl(fun maybe_set_change_type/2
               ,AuditLog
               ,[{<<"cascade">>, kz_term:is_not_empty(cascade_updates(Services))}
                ,{<<"account">>, kz_term:is_not_empty(account_updates(Services))}
                ,{<<"manual">>, kz_term:is_not_empty(manual_updates(Services))}
                ]
               );
add_audit_log_changes_type(_Services, AuditLog, _Type) -> AuditLog.

-spec maybe_set_change_type({kz_json:key(), boolean()}, kz_json:object()) -> kz_json:object().
maybe_set_change_type({_Type, 'false'}, AuditLog) -> AuditLog;
maybe_set_change_type({Type, 'true'}, AuditLog) ->
    Types = kz_json:get_list_value([<<"changes">>, <<"type">>], AuditLog, []),
    kz_json:set_value([<<"changes">>, <<"type">>], [Type | Types], AuditLog).

-spec add_audit_log_changes_account(kz_term:ne_binary() | services(), kz_json:object()) -> kz_json:object().
add_audit_log_changes_account(<<AccountId/binary>>, AuditLog) ->
    case kz_json:get_ne_binary_value([<<"changes">>, <<"account_id">>], AuditLog) =:= 'undefined' of
        'false' -> AuditLog;
        'true' ->
            Props = [{[<<"changes">>, <<"account_id">>], AccountId}
                    ,{[<<"changes">>, <<"account_name">>], kzd_accounts:fetch_name(AccountId)}
                    ],
            kz_json:set_values(Props, AuditLog)
    end;
add_audit_log_changes_account(Services, AuditLog) ->
    add_audit_log_changes_account(account_id(Services), AuditLog).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> services().
empty() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            ServicesJObj = empty_services_jobj(MasterAccountId),
            set_services_jobj(#kz_services{}, ServicesJObj);
        _Else -> #kz_services{}
    end.

-spec empty_services_jobj(kz_term:ne_binary()) -> kzd_services:doc().
empty_services_jobj(ResellerId) ->
    Setters = [{fun kzd_services:set_status/2, kzd_services:status_good()}
              ,{fun kzd_services:set_reseller_id/2, ResellerId}
              ,{fun kzd_services:set_account_quantities/2, kz_json:new()}
              ,{fun kzd_services:set_cascade_quantities/2, kz_json:new()}
              ,{fun kzd_services:set_payment_tokens/2, kz_json:new()}
              ],
    kz_doc:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> services().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(services(), setter_funs()) -> services().
setters(Services, Routines) ->
    lists:foldl(fun({Setter, Value}, S) ->
                        Setter(S, Value)
                end
               ,Services
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(services()) -> kz_json:api_json_term().
public_json(Services) ->
    public_json(Services, 'undefined').

-type getter_fun() :: fun((kz_json:object()) -> kz_json:api_json_term()).

-spec public_json(services(), getter_fun() | 'undefined') -> kz_json:api_json_term().
public_json(Services, 'undefined') ->
    kz_doc:public_fields(services_jobj(Services));
public_json(Services, GetterFunction) ->
    GetterFunction(kz_doc:public_fields(services_jobj(Services))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summary(kz_term:ne_binary()|services()) -> kz_json:object().
summary(?NE_BINARY=Account) ->
    FetchOptions = ['hydrate_plans'
                   ,'hydrate_invoices'
                   ],
    summary(fetch(Account, FetchOptions));
summary(Services) ->
    ServicesJObj = services_jobj(Services),
    kz_json:from_list(
      [{<<"plans">>
       ,kz_services_plans:assigned(Services)
       }
      ,{<<"overrides">>
       ,kzd_services:overrides(ServicesJObj)
       }
      ,{<<"invoices">>
       ,kz_services_invoices:public_json(invoices(Services))
       }
      ,{<<"quantities">>
       ,summary_quantities(Services)
       }
      ,{<<"reseller">>
       ,summary_reseller(Services)
       }
      ,{<<"ratedeck">>
       ,kz_services_ratedecks:fetch(Services)
       }
      ,{<<"applications">>
       ,kz_services_applications:fetch(Services)
       }
      ,{<<"limits">>
       ,kz_doc:public_fields(
          kz_services_limits:fetch(Services)
         )
       }
      ,{<<"payment_tokens">>
       ,kz_services_payment_tokens:fetch(Services)
       }
      ,{<<"billing_cycle">>
       ,summary_billing_cycle(Services)
       }
      ,{<<"status">>
       ,summary_status(Services)
       }
      ]
     ).

-spec summary_reseller(services()) -> kz_json:object().
summary_reseller(Services) ->
    kz_json:from_list(
      [{<<"id">>, kz_services_reseller:get_id(Services)}
      ,{<<"is_reseller">>, kz_services_reseller:is_reseller(Services)}
      ]
     ).

-spec summary_quantities(services()) -> kz_json:object().
summary_quantities(Services) ->
    kz_json:from_list(
      [{<<"account">>, account_quantities(Services)}
      ,{<<"cascade">>, cascade_quantities(Services)}
      ,{<<"manual">>, manual_quantities(Services)}
      ]
     ).

-spec summary_status(services()) -> kz_json:object().
summary_status(Services) ->
    case kz_services_standing:acceptable(Services) of
        {'true', Reason} ->
            kz_json:from_list(
              [{<<"acceptable">>, 'true'}
              ,{<<"reason">>, Reason}
              ]
             );
        {'false'
        ,#{reason := Reason
          ,message := Message
          }
        } ->
            kz_json:from_list(
              [{<<"acceptable">>, 'false'}
              ,{<<"reason">>, Reason}
              ,{<<"message">>, Message}
              ]
             )
    end.

-spec summary_billing_cycle(services()) -> kz_json:object().
summary_billing_cycle(_Services) ->
    {{Y, M, _}, _} = calendar:universal_time(),
    NextBillDate =
        calendar:datetime_to_gregorian_seconds(
          {kz_date:normalize({Y, M + 1, 1})
          ,{0, 0, 0}
          }
         ),
    kz_json:from_list(
      [{<<"next">>, NextBillDate}
      ,{<<"period">>, 1}
      ,{<<"unit">>, <<"month">>}
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc Fetch the services doc for a give account from the services database
%% or create a new (in memory) if not present.
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary()) -> services().
fetch(Account) ->
    fetch(Account, []).

-spec fetch(kz_term:api_ne_binary(), fetch_options()) -> services().
fetch('undefined', Options) ->
    handle_fetch_options(empty(), Options);
fetch(Account=?NE_BINARY, Options) ->
    AccountId = kzs_util:format_account_id(Account),
    OpenDocFun = choose_open_doc_fun(Options, AccountId),

    handle_fetched_doc(AccountId, Options, OpenDocFun(?KZ_SERVICES_DB, AccountId)).

-spec handle_fetched_doc(kz_term:ne_binary(), fetch_options(), {'ok', kz_json:object()} | {'error', 'not_found'}) ->
                                services().
handle_fetched_doc(_AccountId, Options, {'ok', ServicesJObj}) ->
    Setters = [{fun set_services_jobj/2, ServicesJObj}
              ,{fun set_current_services_jobj/2, ServicesJObj}
              ],
    handle_fetch_options(setters(Setters), Options);
handle_fetched_doc(AccountId, Options, {'error', 'not_found'}) ->
    %% TODO: hydrate cascade and account quantities
    ServicesJObj = create(AccountId),
    Setters = [{fun set_services_jobj/2, ServicesJObj}
              ,{fun set_current_services_jobj/2, ServicesJObj}
              ],
    handle_fetch_options(setters(Setters), Options).

-type open_doc_fun() :: fun((kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   kz_datamgr:data_error() |
                                   {'error', 'not_found'}
                                       ).
-spec choose_open_doc_fun(fetch_options(), kz_term:ne_binary()) -> open_doc_fun().
choose_open_doc_fun(Options, AccountId) ->
    case props:is_true('skip_cache', Options, 'false') of
        'false' ->
            lager:debug("fetching services doc ~s (with cache)"
                       ,[AccountId]
                       ),
            fun kz_datamgr:open_cache_doc/2;
        'true' ->
            lager:debug("fetching services doc ~s (without cache)"
                       ,[AccountId]
                       ),
            fun kz_datamgr:open_doc/2
    end.

-spec create(kz_term:ne_binary()) -> kz_term:api_object().
create(AccountId) ->
    lager:debug("trying to create new services doc for ~s", [AccountId]),
    create(AccountId, kzd_accounts:fetch(AccountId)).

-spec create(kz_term:ne_binary(), {'error', 'not_found'} | {'ok', kzd_accounts:doc()}) ->
                    kz_term:api_object().
create(_AccountId, {'error', 'not_found'}) ->
    lager:info("failed to find account database for ~s", [_AccountId]),
    'undefined';
create(AccountId, {'ok', AccountJObj}) ->
    ResellerId = kz_services_reseller:find_id(AccountId),
    BaseJObj = kz_doc:update_pvt_parameters(kz_json:new()
                                           ,kzs_util:format_account_db(AccountId)
                                           ,[{'account_id', AccountId}
                                            ,{'crossbar_doc_vsn', 2}
                                            ,{'id', AccountId}
                                            ,{'type', kzd_services:type()}
                                            ]
                                           ),
    Setters = [{fun kzd_services:set_status/2, kzd_services:status_good()}
              ,{fun kzd_services:set_reseller_id/2, ResellerId}
              ,{fun kzd_services:set_tree/2, kzd_accounts:tree(AccountJObj)}
              ,{fun kzd_services:set_account_quantities/2, kz_json:new()}
              ,{fun kzd_services:set_cascade_quantities/2, kz_json:new()}
              ],
    kz_doc:setters(BaseJObj, Setters).

-spec handle_fetch_options(services(), kz_term:proplist()) -> services().
handle_fetch_options(#kz_services{}=Services, []) -> Services;
handle_fetch_options(#kz_services{}=Services, ['hydrate_plans'| Options]) ->
    handle_fetch_options(hydrate_plans(Services), Options);
handle_fetch_options(#kz_services{}=Services, ['hydrate_account_quantities'| Options]) ->
    handle_fetch_options(hydrate_account_quantities(Services), Options);
handle_fetch_options(#kz_services{}=Services, ['hydrate_cascade_quantities'| Options]) ->
    handle_fetch_options(hydrate_cascade_quantities(Services), Options);
handle_fetch_options(#kz_services{}=Services, ['hydrate_manual_quantities'| Options]) ->
    handle_fetch_options(hydrate_manual_quantities(Services), Options);
handle_fetch_options(#kz_services{}=Services, ['hydrate_invoices'|Options]) ->
    handle_fetch_options(hydrate_invoices(Services), Options);
handle_fetch_options(#kz_services{}=Services, [{'updates', Account, Current, Proposed}|Options]) ->
    handle_fetch_options(set_updates(Services, Account, Current, Proposed), Options);
handle_fetch_options(#kz_services{}=Services, [{'audit_log', AuditLog}|Options]) ->
    handle_fetch_options(set_initial_audit_log(Services, AuditLog), Options);
handle_fetch_options(#kz_services{}=Services, ['skip_cache'|Options]) ->
    handle_fetch_options(Services, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type billable_updates() :: 'undefined' | kz_services_quantities:billables() | kz_services_quantities:billable().

-spec commit_updates(kz_term:ne_binary(), billable_updates(), billable_updates()) ->
          services().
commit_updates(Account, Current, Proposed) ->
    commit_updates(Account, Current, Proposed, kz_json:new()).

-spec commit_updates(kz_term:ne_binary(), billable_updates(), billable_updates(), kz_json:object()) ->
          services().
commit_updates(Account, Current, Proposed, AuditLog) ->
    FetchOptions = fetch_options(Account, Current, Proposed, AuditLog),
    Services = fetch(Account, FetchOptions),

    case should_skip_updates(Services) of
        'true' ->
            lager:debug("no possible change, skipping update"),
            Services;
        'false' ->
            commit_updates(Services, FetchOptions)
    end.

-spec fetch_options(kz_term:ne_binary(), billable_updates(), billable_updates(), kz_json:object()) ->
          fetch_options().
fetch_options(Account, Current, Proposed, AuditLog) ->
    [{'updates', Account, to_billables(Current), to_billables(Proposed)}
    ,{'audit_log', add_audit_log_changes_account(Account, AuditLog)}
    ].

-spec to_billables(billable_updates()) -> kz_services_quantities:billables().
to_billables('undefined') -> [];
to_billables(Bs) when is_list(Bs) -> Bs;
to_billables(B) -> [B].

-spec should_skip_updates(services()) -> boolean().
should_skip_updates(Services) ->
    kz_term:is_empty(account_updates(Services))
        andalso kz_term:is_empty(cascade_updates(Services))
        andalso kz_term:is_empty(manual_updates(Services))
        andalso (not has_proposed_object_plans(Services)).

-spec commit_updates(services(), kz_term:proplist()) -> services().
commit_updates(Services, FetchOptions) ->
    UpdatedServices = commit_account(Services),
    case is_dirty(UpdatedServices) of
        'false' -> UpdatedServices;
        'true' ->
            Tree = lists:reverse(kzd_services:tree(services_jobj(Services))),
            CascadeFetchOptions = cascade_commits_fetch_options(FetchOptions),
            _ = cascade_commit_updates(CascadeFetchOptions, Tree),
            UpdatedServices
    end.

-spec cascade_commits_fetch_options(kz_term:proplist()) -> kz_term:proplist().
cascade_commits_fetch_options(FetchOptions) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"hydrate_cascade_commits">>, 'true') of
        'false' -> FetchOptions;
        'true' ->
            ['hydrate_cascade_quantities'
            ,'hydrate_account_quantities'
             | FetchOptions
            ]
    end.

-spec cascade_commit_updates(kz_term:proplist(), kz_term:ne_binaries()) -> 'ok'.
cascade_commit_updates(_FetchOptions, []) -> 'ok';
cascade_commit_updates(FetchOptions, [Account|Accounts]) ->
    case Accounts =:= []
        andalso ?DONT_CASCADE_MASTER
    of
        'true' -> 'ok';
        'false' ->
            #kz_services{} = commit_account(fetch(Account, FetchOptions)),
            cascade_commit_updates(FetchOptions, Accounts)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit(services()) -> services().
commit(#kz_services{}=Services) ->
    UpdatedServices = commit_account(Services),
    case is_dirty(UpdatedServices) of
        'true' ->
            _ = cascade_commit(Services),
            UpdatedServices;
        'false' -> UpdatedServices
    end.

-spec cascade_commit(kz_term:ne_binaries() | services()) -> 'ok'.
cascade_commit([]) -> 'ok';
cascade_commit([Account|Accounts]) ->
    case Accounts =:= []
        andalso ?DONT_CASCADE_MASTER
    of
        'true' -> 'ok';
        'false' ->
            FetchOptions = ['hydrate_account_quantities'
                           ,'hydrate_cascade_quantities'
                           ,'hydrate_plans'
                           ,'hydrate_invoices'
                           ],
            Services = fetch(Account, FetchOptions),
            _ = commit_account(Services),
            cascade_commit(Accounts)
    end;
cascade_commit(Services) ->
    Tree = kzd_services:tree(services_jobj(Services)),
    cascade_commit(Tree).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit_account(kz_term:ne_binary()|services()) -> services().
commit_account(?NE_BINARY=AccountId) ->
    commit_account(fetch(AccountId));
commit_account(Services) ->
    lager:debug("commit any changes to account ~s"
               ,[account_id(Services)]
               ),
    Routines = [fun maybe_hydrate_account_quantities/1
               ,fun maybe_hydrate_cascade_quantities/1
               ,fun hydrate_invoices/1
               ,fun kz_services_bookkeeper:maybe_update/1
               ,fun maybe_save_services_jobj/1
               ],
    lists:foldl(fun(F, S) -> F(S) end
               ,reset_invoices(Services)
               ,Routines
               ).

-spec maybe_hydrate_cascade_quantities(services()) -> services().
maybe_hydrate_cascade_quantities(#kz_services{cascade_quantities='undefined'}=Services) ->
    case kz_term:is_empty(cascade_updates(Services)) of
        'true' -> hydrate_cascade_quantities(Services);
        'false' -> Services
    end;
maybe_hydrate_cascade_quantities(Services) ->
    Services.

-spec maybe_hydrate_account_quantities(services()) -> services().
maybe_hydrate_account_quantities(#kz_services{account_quantities='undefined'}=Services) ->
    case kz_term:is_empty(account_updates(Services)) of
        'false' -> hydrate_account_quantities(Services);
        'true' -> Services
    end;
maybe_hydrate_account_quantities(Services) ->
    Services.

-spec maybe_save_services_jobj(services()) -> services().
maybe_save_services_jobj(Services) ->
    CurrentServicesJObj = current_services_jobj(Services),
    ProposedServices = commit_quantities(Services),
    ProposedServicesJObj = services_jobj(ProposedServices),
    case kz_json:are_equal(kz_doc:public_fields(CurrentServicesJObj)
                          ,kz_doc:public_fields(ProposedServicesJObj)
                          )
    of
        'false' ->
            save_services_jobj(ProposedServices
                              ,ProposedServicesJObj
                              );
        'true' ->
            lager:debug("services document is unchanged"),
            Services
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit_quantities(services()) -> services().
commit_quantities(Services) ->
    Routines = [fun commit_cascade_quantities/1
               ,fun commit_account_quantities/1
               ,fun commit_manual_quantities/1
               ],
    lists:foldl(fun(F, S) -> F(S) end
               ,Services
               ,Routines
               ).

-spec commit_cascade_quantities(services()) -> services().
commit_cascade_quantities(Services) ->
    Quantities = cascade_quantities(Services),
    JObj = services_jobj(Services),
    set_services_jobj(Services
                     ,kzd_services:set_cascade_quantities(JObj, Quantities)
                     ).

-spec commit_account_quantities(services()) -> services().
commit_account_quantities(Services) ->
    Quantities = account_quantities(Services),
    JObj = services_jobj(Services),
    set_services_jobj(Services
                     ,kzd_services:set_account_quantities(JObj, Quantities)
                     ).

-spec commit_manual_quantities(services()) -> services().
commit_manual_quantities(Services) ->
    Quantities = manual_quantities(Services),
    JObj = services_jobj(Services),
    set_services_jobj(Services
                     ,kzd_services:set_manual_quantities(JObj, Quantities)
                     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_services_jobj(services()) -> services().
save_services_jobj(Services) ->
    save_services_jobj(Services, services_jobj(Services)).

-spec save_services_jobj(services(), kzd_services:doc()) -> services().
save_services_jobj(Services, ProposedJObj) ->
    case kz_datamgr:save_doc(?KZ_SERVICES_DB, ProposedJObj) of
        {'ok', UpdatedJObj} ->
            lager:info("updated services document ~s"
                      ,[account_id(Services)]
                      ),
            Setters = [{fun set_services_jobj/2, UpdatedJObj}
                      ,{fun set_current_services_jobj/2, UpdatedJObj}
                      ],
            setters(set_dirty(Services), Setters);
        {'error', _Reason} ->
            lager:info("unable to update services document ~s: ~p"
                      ,[account_id(Services), _Reason]
                      ),
            Services
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binary() | services()) -> services().
delete(?MATCH_ACCOUNT_RAW(AccountId)) ->
    delete(fetch(AccountId));
delete(#kz_services{}=Services) ->
    %% TODO: cancel services with all bookkeepers...
    _ = kz_datamgr:del_doc(?KZ_SERVICES_DB, kz_doc:id(services_jobj(Services))),
    Services.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_term:ne_binary()) -> services().
reconcile(Account) ->
    reconcile(Account, kz_json:new()).

-spec reconcile(kz_term:ne_binary(), kz_json:object()) -> services().
reconcile(Account, AuditLog) ->
    AccountId = kzs_util:format_account_id(Account),
    lager:debug("reconcile ~s", [AccountId]),
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ,{'audit_log', add_audit_log_changes_account(AccountId, AuditLog)}
                   ,'skip_cache'
                   ],
    commit(fetch(AccountId, FetchOptions)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_services(services() | any()) -> boolean().
is_services(#kz_services{}) -> 'true';
is_services(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sum_quantities_updates(kz_json:object(), kz_json:object()) -> kz_json:object().
sum_quantities_updates(Quantities, Updates) ->
    Props = [{[CategoryName, ItemName]
             ,kz_json:get_integer_value([CategoryName, ItemName], Updates, 0)
             }
             || CategoryName <- kz_json:get_keys(Updates),
                ItemName <- kz_json:get_keys(CategoryName, Updates)
            ],
    lists:foldl(fun({Key, UpdateValue}, JObj) ->
                        CurrentValue = kz_json:get_integer_value(Key, JObj, 0),
                        kz_json:set_value(Key, CurrentValue + UpdateValue, JObj)
                end
               ,Quantities
               ,Props
               ).
