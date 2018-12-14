%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Hesaam
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_bill_reminder).

%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([send_reminder/1
        ]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".bill">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    case kapps_config:is_true(?MOD_CAT, <<"reminder_enabled">>, 'false') of
        'true' ->
            _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'send_reminder'),
            'ok';
        'false' ->
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_reminder(kz_term:ne_binary()) -> 'ok'.
send_reminder(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb),

    EarlyDays = kapps_config:get_integer(?MOD_CAT, <<"reminder_early_days">>, 5),
    {DueDate, IsDaysEarlyYet} = is_days_early_yet(EarlyDays),

    case IsDaysEarlyYet
         andalso should_remind_account(AccountId)
    of
        'true' -> do_send_reminder(AccountId, DueDate);
        'false' -> 'ok'
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_days_early_yet(non_neg_integer()) -> {non_neg_integer(), boolean()}.
is_days_early_yet(EarlyDays) ->
    {Year, Month, Day} = erlang:date(),
    LastDay = calendar:last_day_of_the_month(Year, Month),
    {_, _, DueDay} = DueDate = kz_date:normalize({Year, Month, LastDay + 1}),
    DueTimestamp = calendar:datetime_to_gregorian_seconds({DueDate, {0, 0, 0}}),
    {DueTimestamp, DueDay - EarlyDays > Day}.


-spec should_remind_account(kz_term:ne_binary()) -> boolean().
should_remind_account(AccountId) ->
    kapps_account_config:get(AccountId
                            ,?MOD_CAT
                            ,<<"reminder_enabled">>
                            ,kapps_config:is_true(?MOD_CAT, <<"default_account_enabled">>, 'true')
                            ).

-spec do_send_reminder(kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
do_send_reminder(AccountId, DueDate) ->
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ],
    Services = kz_services:fetch(AccountId, FetchOptions),
    Invoices = kz_services:invoices(Services),
    _ = kz_services_invoices:foldl(fun(Invoice, _Acc) ->
                                           notify_reseller(AccountId, Services, Invoice, DueDate)
                                   end
                                  ,'ok'
                                  ,Invoices
                                  ),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec notify_reseller(kz_term:ne_binary(), kz_services:services(), kz_services_invoice:invoice(), non_neg_integer()) -> 'ok'.
notify_reseller(AccountId, Services, Invoice, DueDate) ->
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
            [{<<"Payment-Token">>, kz_services_payment_token:default(Services, Bookkeeper)}]
    end.

%%% End of Module.
