%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Hesaam
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_bill_early).

%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([handle_req/1]).

%% Maunal Trgiggerables
-export([send_reminder/1
        ,bill_early/1
        ]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".bill_early">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'handle_req'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_term:ne_binary()) -> 'ok'.
handle_req(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb),

    EarlyDays = kapps_config:get_integer(?MOD_CAT, <<"how_many_early_days">>, 5),
    {DueDate, IsDaysEarlyYet} = is_days_early_yet(EarlyDays),

    ShouldBill = kapps_account_config:get_global(AccountId, ?MOD_CAT, <<"bill_early_enabled">>, 'false'),
    ShouldRemind = kapps_account_config:get_global(AccountId, ?MOD_CAT, <<"reminder_enabled">>, 'true'),

    handle_req(AccountId, DueDate, IsDaysEarlyYet, ShouldBill, ShouldRemind).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bill_early(kz_term:ne_binary()) -> 'ok'.
bill_early(Account) ->
    AccountId = kz_util:format_account_id(Account),

    EarlyDays = kapps_config:get_integer(?MOD_CAT, <<"how_many_early_days">>, 5),
    {DueDate, _} = is_days_early_yet(EarlyDays),
    Services = get_services(AccountId),
    do_bill_early(AccountId, Services, DueDate).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_reminder(kz_term:ne_binary()) -> 'ok'.
send_reminder(Account) ->
    AccountId = kz_util:format_account_id(Account),

    EarlyDays = kapps_config:get_integer(?MOD_CAT, <<"how_many_early_days">>, 5),
    {DueDate, _} = is_days_early_yet(EarlyDays),
    Services = get_services(AccountId),
    do_send_reminder(AccountId, Services, DueDate).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_term:ne_binary(), non_neg_integer(), boolean(), boolean(), boolean()) -> 'ok'.
handle_req(_, _, 'false', _ShouldBill, _ShouldRemind) ->
    'ok';
handle_req(AccountId, DueDate, 'true', 'true', _ShouldRemind) ->
    bill_early(AccountId, DueDate, is_already_ran_account(AccountId));
handle_req(AccountId, DueDate, 'true', 'false', 'true') ->
    send_reminder(AccountId, DueDate, is_already_ran_account(AccountId));
handle_req(_, _, _, _ShouldBill, _ShouldRemind) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_days_early_yet(non_neg_integer()) -> {non_neg_integer(), boolean()}.
is_days_early_yet(EarlyDays) ->
    {Year, Month, Day} = erlang:date(),
    LastDay = calendar:last_day_of_the_month(Year, Month),
    DueTimestamp = calendar:datetime_to_gregorian_seconds({kz_date:normalize({Year, Month, LastDay + 1}), {0, 0, 0}}),
    {DueTimestamp, LastDay - EarlyDays < Day}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_services(kz_term:ne_binary()) -> kz_services:services().
get_services(AccountId) ->
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ],
    kz_services:fetch(AccountId, FetchOptions).

-spec is_already_ran_account(kz_term:ne_binary()) -> boolean().
is_already_ran_account(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            case kzd_accounts:bill_early_task_timestamp(JObj) of
                'undefined' -> 'false';
                DueDate ->
                    %% If DueDate is in future (payment due day) then we already visited
                    %% this account before for the current bill cycle, otherwise this is the first time
                    %% we visited this account for this current bill cycle.
                    kz_time:now_s() =< DueDate
            end;
        {'error', _R} ->
            lager:debug("can't check early bill/reminder was ran for ~s, lets check it tomorrow again"
                       ,[AccountId]
                       ),
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bill_early(kz_term:ne_binary(), non_neg_integer(), boolean()) -> 'ok'.
bill_early(AccountId, DueDate, 'false') ->
    lager:debug("attempting to early billing ~s", [AccountId]),
    Services = get_services(AccountId),
    case kz_services_plans:is_empty(kz_services:plans(Services)) of
        'true' ->
            lager:debug("account ~s has no plan assigned, ignoring", [AccountId]),
            set_bill_early_task_timestamp(AccountId, DueDate);
        'false' ->
            do_bill_early(AccountId, Services, DueDate)
    end;
bill_early(_, _, 'true') ->
    'ok'.

-spec do_bill_early(kz_term:ne_binary(), kz_services:services(), non_neg_integer()) -> 'ok'.
do_bill_early(AccountId, Services, DueDate) ->
    Invoices = kz_services:invoices(Services),
    _ = kz_services_invoices:foldl(fun(Invoice, _Acc) ->
                                           do_bill_early(AccountId, Services, Invoice, DueDate)
                                   end
                                  ,'ok'
                                  ,Invoices
                                  ),
    'ok'.

-spec do_bill_early(kz_term:ne_binary(), kz_services:services(), kz_services_invoice:invoice(), non_neg_integer()) -> 'ok'.
do_bill_early(AccountId, Services, Invoice, DueDate) ->
    lager:debug("trying to sync bookkeeper for ~s", [AccountId]),
    Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
              ,{<<"Bookkeeper-ID">>, kz_services_invoice:bookkeeper_id(Invoice)}
              ,{<<"Bookkeeper-Type">>, kz_services_invoice:bookkeeper_type(Invoice)}
              ,{<<"Vendor-ID">>, kz_services_invoice:bookkeeper_vendor_id(Invoice)}
              ,{<<"Items">>
               ,kz_services_items:public_json(
                  kz_services_invoice:items(Invoice)
                 )
               }
              ,{<<"Call-ID">>, kz_util:get_callid()}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Resp = kz_amqp_worker:call(Request
                              ,fun kapi_bookkeepers:publish_update_req/1
                              ,fun kapi_bookkeepers:update_resp_v/1
                              ,20 * ?MILLISECONDS_IN_SECOND
                              ),
    check_bokkkeeper_response(AccountId, DueDate, Resp).

check_bokkkeeper_response(_AccountId, _, {'error', 'timeout'}) ->
    lager:debug("timeout when running early bill for ~s, trying again tomorrow", [_AccountId]);
check_bokkkeeper_response(AccountId, DueDate, Resp) ->
    case kz_json:get_ne_binary_value(<<"Status">>, Resp, <<"error">>) of
        <<"error">> ->
            lager:debug("failed to run early bill for ~s with reason ~s, trying again tomorrow"
                       ,[AccountId, kz_json:get_ne_binary_value(<<"Reason">>, Resp, <<"unknown">>)]
                       );
        _ ->
            lager:debug("successfully billed early ~s", [AccountId]),
            set_bill_early_task_timestamp(AccountId, DueDate)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_reminder(kz_term:ne_binary(), non_neg_integer(), boolean()) -> 'ok'.
send_reminder(AccountId, DueDate, 'false') ->
    lager:debug("attempting to send bill reminder ~s", [AccountId]),
    Services = get_services(AccountId),
    case kz_services_plans:is_empty(kz_services:plans(Services)) of
        'true' ->
            lager:debug("account ~s has no plan assigned, ignoring", [AccountId]),
            set_bill_early_task_timestamp(AccountId, DueDate);
        'false' ->
            do_send_reminder(AccountId, Services, DueDate)
    end;
send_reminder(_, _, 'true') ->
    'ok'.

-spec do_send_reminder(kz_term:ne_binary(), kz_services:services(), non_neg_integer()) -> 'ok'.
do_send_reminder(AccountId, Services, DueDate) ->
    Invoices = kz_services:invoices(Services),
    _ = kz_services_invoices:foldl(fun(Invoice, _Acc) ->
                                           do_notify_reseller(AccountId, Services, Invoice, DueDate)
                                   end
                                  ,'ok'
                                  ,Invoices
                                  ),
    set_bill_early_task_timestamp(AccountId, DueDate).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec do_notify_reseller(kz_term:ne_binary(), kz_services:services(), kz_services_invoice:invoice(), non_neg_integer()) -> 'ok'.
do_notify_reseller(AccountId, Services, Invoice, DueDate) ->
    lager:debug("sending bill reminder notification to account ~s", [AccountId]),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Due-Date">>, DueDate}
            ,{<<"Items">>
             ,kz_services_items:public_json(
                kz_services_invoice:items(Invoice)
               )
             }
            ,{<<"Timestamp">>, kz_time:now_s()}
             | maybe_add_payment_token(Services, Invoice)
             ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(props:filter_undefined(Props), fun kapi_notifications:publish_bill_reminder/1).

-spec maybe_add_payment_token(kz_services:services(), kz_services_invoice:invoice()) ->
                                     kz_term:proplist().
maybe_add_payment_token(Services, Invoice) ->
    case kz_services_invoice:bookkeeper_type(Invoice) of
        'undefined' -> [];
        Bookkeeper ->
            [{<<"Payment-Token">>, kz_services_payment_tokens:default(Services, Bookkeeper)}]
    end.

-spec set_bill_early_task_timestamp(kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
set_bill_early_task_timestamp(AccountId, DueDate) ->
    Update = [{kzd_accounts:path_bill_early_task_timestamp(), DueDate}],
    _ = kzd_accounts:update(AccountId, Update),
    'ok'.

%%% End of Module.
