%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_recurring).

-export([early_collect/2
        ,send_early_reminder/2

        ,force_collect/1
        ,force_reminder/1

        ,status_good/0
        ]).

-export([is_current_month_collected/1

        ,is_last_month_collected/1
        ,is_last_month_collected/3

        ,is_collected/3
        ]).

-include("services.hrl").

-define(COLLECT_RECURRING_MARKER_ID, <<"collect_recurring_was_here">>).
-define(REMINDER_MARKER_ID, <<"early_bill_reminder_was_here">>).

-type error_details() :: #{message := kz_term:ne_binary()
                          ,reason := kz_term:ne_binary()
                          }.

-type run_return() :: {'ok', kz_term:ne_binary()} |
                      {'error', error_details()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec early_collect(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> run_return().
early_collect(AccountId, DueTimestamp) ->
    {PrevYear, PrevMonth, _} = get_previous_month(DueTimestamp),
    ShouldCollect = should_process(AccountId, ?COLLECT_RECURRING_MARKER_ID, PrevYear, PrevMonth),
    early_collect(AccountId, PrevYear, PrevMonth, ShouldCollect).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_early_reminder(kz_term:ne_binary(), non_neg_integer()) -> {'ok', kz_term:ne_binary()}.
send_early_reminder(AccountId, DueTimestamp) ->
    {PrevYear, PrevMonth, _} = get_previous_month(DueTimestamp),
    ShouldCollect = should_process(AccountId, ?REMINDER_MARKER_ID, PrevYear, PrevMonth),
    send_early_reminder(AccountId, DueTimestamp, PrevYear, PrevMonth, ShouldCollect).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec force_collect(kz_term:ne_binary()) -> run_return().
force_collect(AccountId) ->
    collect(AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec force_reminder(kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()}.
force_reminder(AccountId) ->
    {Year, Month, _} = erlang:date(),
    LastDay = calendar:last_day_of_the_month(Year, Month),
    DueTimestamp =
        calendar:datetime_to_gregorian_seconds({kz_date:normalize({Year, Month, LastDay + 1})
                                               ,{0, 0, 0}
                                               }),
    send_reminder(AccountId, DueTimestamp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status_good() -> kz_term:ne_binary().
status_good() -> <<"successful">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_current_month_collected(kz_term:ne_binary()) -> boolean().
is_current_month_collected(?MATCH_MODB_SUFFIX_RAW(_, Year, Month) = AccountMODB) ->
    is_collected(AccountMODB, Year, Month);
is_current_month_collected(?MATCH_MODB_SUFFIX_UNENCODED(_, Year, Month) = AccountMODB) ->
    is_collected(AccountMODB, Year, Month);
is_current_month_collected(?MATCH_MODB_SUFFIX_ENCODED(_, Year, Month) = AccountMODB) ->
    is_collected(AccountMODB, Year, Month);
is_current_month_collected(Account) ->
    ?MATCH_MODB_SUFFIX_ENCODED(_, Year, Month) = kz_util:format_account_mod_id(Account),
    is_collected(Account, Year, Month).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_last_month_collected(kz_term:ne_binary()) -> boolean().
is_last_month_collected(?MATCH_MODB_SUFFIX_RAW(_, Year, Month) = AccountMODB) ->
    is_last_month_collected(AccountMODB, Year, Month);
is_last_month_collected(?MATCH_MODB_SUFFIX_UNENCODED(_, Year, Month) = AccountMODB) ->
    is_last_month_collected(AccountMODB, Year, Month);
is_last_month_collected(?MATCH_MODB_SUFFIX_ENCODED(_, Year, Month) = AccountMODB) ->
    is_last_month_collected(AccountMODB, Year, Month);
is_last_month_collected(Account) ->
    MODB = ?MATCH_MODB_SUFFIX_ENCODED(_, _, _) = kz_util:format_account_mod_id(Account),
    is_last_month_collected(MODB).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_last_month_collected(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
is_last_month_collected(Account, Year, Month) ->
    Timestamp = calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}}),
    {PrevYear, PrevMonth, _} = get_previous_month(Timestamp),
    is_collected(Account, PrevYear, PrevMonth).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_collected(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
is_collected(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account),
    is_proccessed(AccountId, ?COLLECT_RECURRING_MARKER_ID, Year, Month).

%%%=============================================================================
%%% Collect Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec early_collect(kz_term:ne_binary(), kz_time:year(), kz_time:month(), boolean()) -> run_return().
early_collect(AccountId, Year, Month, 'false') ->
    case set_marker(AccountId, ?COLLECT_RECURRING_MARKER_ID, Year, Month) of
        {'ok', _} ->
            collect(AccountId);
        {'error', _Reason} ->
            ErrMessage = <<"unabled to set marker to collect recurring charges">>,
            lager:debug("charging account ~s failed: ~s", [AccountId, ErrMessage]),
            {'error', #{message => ErrMessage
                       ,reason => <<"set_marker_fault">>
                       }
            }
    end;
early_collect(_, _, _, 'true') ->
    {'ok', <<"account is already processed">>}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collect(kz_term:ne_binary()) -> run_return().
collect(AccountId) ->
    lager:debug("attempting to collect recurring charges for ~s", [AccountId]),
    Services = kz_services:fetch(AccountId, ['hydrate_plans']),
    Invoices = kz_services:invoices(Services),
    Results = kz_services_invoices:foldl(collect_invoices_fold_fun(Services), [], Invoices),
    handle_collect_bookkeeper_results(Results).

collect_invoices_fold_fun(Services) ->
    fun(Invoice, Results) ->
            Type = kz_services_invoice:bookkeeper_type(Invoice),
            case kzd_services:default_bookkeeper_type() =:= Type of
                'true' ->
                    Result = kz_json:from_list([{<<"Status">>, status_good()}]),
                    [{'ok', Result} | Results];
                'false' ->
                    Result = collect_bookkeeper(Invoice, Services),
                    [Result | Results]
            end
    end.

-spec collect_bookkeeper(kz_services_invoice:invoice(), kz_services:services()) ->
                                kz_amqp_worker:request_return().
collect_bookkeeper(Invoice, Services) ->
    Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
              ,{<<"Bookkeeper-ID">>, kz_services_invoice:bookkeeper_id(Invoice)}
              ,{<<"Bookkeeper-Type">>, kz_services_invoice:bookkeeper_type(Invoice)}
              ,{<<"Vendor-ID">>, kz_services_invoice:bookkeeper_vendor_id(Invoice)}
              ,{<<"Call-ID">>, kz_util:get_callid()}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:call(Request
                       ,fun kapi_bookkeepers:publish_collect_recurring_req/1
                       ,fun kapi_bookkeepers:collect_recurring_resp_v/1
                       ,2 * ?MILLISECONDS_IN_SECOND
                       ).

-spec handle_collect_bookkeeper_results(any()) -> run_return().
handle_collect_bookkeeper_results([]) ->
    {'ok', status_good()};
handle_collect_bookkeeper_results([{'ok', _Result} | Results]) ->
    handle_collect_bookkeeper_results(Results);
handle_collect_bookkeeper_results([_Error | Results]) ->
    lager:debug("unexpected bookkeeper result: ~p", [_Error]),
    handle_collect_bookkeeper_results(Results).

%%%=============================================================================
%%% Reminder Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_early_reminder(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:year(), kz_time:month(), boolean()) ->
                                 {'ok', kz_term:ne_binary()}.
send_early_reminder(AccountId, DueTimestamp, Year, Month, 'false') ->
    lager:debug("attempting to send bill reminder ~s", [AccountId]),
    _ = send_reminder(AccountId, DueTimestamp),
    _ = set_marker(AccountId, ?REMINDER_MARKER_ID, Year, Month),
    {'ok', status_good()};
send_early_reminder(_, _, _, _, 'true') ->
    {'ok', <<"account is already processed">>}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_reminder(kz_term:ne_binary(), non_neg_integer()) -> {'ok', kz_term:ne_binary()}.
send_reminder(AccountId, DueTimestamp) ->
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ,'skip_cache'
                   ],
    Services = kz_services:fetch(AccountId, FetchOptions),
    Invoices = kz_services:invoices(Services),
    _ = kz_services_invoices:foldl(fun(Invoice, _Acc) ->
                                           do_notify_reseller(AccountId, Services, Invoice, DueTimestamp)
                                   end
                                  ,'ok'
                                  ,Invoices
                                  ),
    {'ok', status_good()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec do_notify_reseller(kz_term:ne_binary(), kz_services:services(), kz_services_invoice:invoice(), non_neg_integer()) -> 'ok'.
do_notify_reseller(AccountId, Services, Invoice, DueTimestamp) ->
    lager:debug("sending bill reminder notification to account ~s", [AccountId]),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Due-Date">>, DueTimestamp}
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_previous_month(kz_time:gregorian_seconds()) -> kz_time:date().
get_previous_month(DueTimestamp) ->
    {{DueYear, DueMonth, _}, _} = calendar:gregorian_seconds_to_datetime(DueTimestamp),
    calendar:datetime_to_gregorian_seconds({kz_date:normalize({DueYear, DueMonth - 1,1}), {0, 0, 0}}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_process(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
should_process(AccountId, MarkerId, Year, Month) ->
    is_account_enabled(AccountId)
        andalso is_proccessed(AccountId, MarkerId, Year, Month).

-spec is_account_enabled(kz_term:ne_binary()) -> 'ok'.
is_account_enabled(AccountId) ->
    case kzd_accounts:is_enabled(AccountId) of
        'false' ->
            lager:debug("skipping disabled account ~s", [AccountId]),
            'false';
        'true' ->
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_proccessed(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
is_proccessed(AccountId, MarkerId, Year, Month) ->
    AccountMODB = kz_util:format_account_mod_id(AccountId, Year, Month),
    case kazoo_modb:open_doc(AccountMODB, MarkerId) of
        {'ok', _} ->
            LastDay = calendar:last_day_of_the_month(Year, Month),
            {_NextYear, _NextMonth, _} = kz_date:normalize({Year, Month, LastDay + 1}),
            lager:debug("(early) recurring charges for account ~s already processed for ~s-~s"
                       ,[AccountId, _NextYear, _NextMonth]
                       ),
            'true';
        {'error', 'not_found'} ->
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_marker(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
set_marker(AccountId, MarkerId, Year, Month) ->
    AccountMODB = kz_util:format_account_mod_id(AccountId, Year, Month),
    PvtOptions = [{'account_id', AccountId}
                 ,{'crossbar_doc_vsn', 1}
                 ,{'id', MarkerId}
                 ,{'type', <<"services_recurring_marker">>}
                 ],
    JObj = kz_doc:update_pvt_parameters(kz_json:new(), AccountMODB, PvtOptions),
    lager:debug("attempting to mark recurring marker in modb ~s", [AccountMODB]),
    case kazoo_modb:save_doc(AccountMODB, JObj) of
        {'ok', _}=OK -> OK;
        {'error', _Reason}=Error ->
            lager:error("failed to save bill early/reminder maker for ~s: ", [AccountId, _Reason]),
            Error
    end.
