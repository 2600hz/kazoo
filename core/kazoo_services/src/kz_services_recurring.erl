%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_recurring).

-export([early_collect/2
        ,send_early_reminder/2

        ,force_collect/1, force_collect/2
        ,force_reminder/1, force_reminder/2

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
%% @doc Attempt to collect recurring charges for the due timestamp.
%%
%% This get services for the provided account ID and send collect recurring AMQP
%% messages to all invoice's bookkeeper.
%%
%% `DueTimestamp' is due date for recurring, and will be used to check to get
%% previous month database to check if the money is collected already or not.
%% @end
%%------------------------------------------------------------------------------
-spec early_collect(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> run_return().
early_collect(AccountId, DueTimestamp) ->
    {PrevYear, PrevMonth, _} = get_previous_month(DueTimestamp),
    ShouldCollect = should_process(AccountId, ?COLLECT_RECURRING_MARKER_ID, PrevYear, PrevMonth),
    early_collect(AccountId, DueTimestamp, PrevYear, PrevMonth, ShouldCollect).

%%------------------------------------------------------------------------------
%% @doc Attempt to send a remonder for recurring charges for the due timestamp.
%%
%% `DueTimestamp' is due date for recurring, and will be used to check to get
%% previous month database to check if the notification is send already or not.
%% @end
%%------------------------------------------------------------------------------
-spec send_early_reminder(kz_term:ne_binary(), non_neg_integer()) -> {'ok', kz_term:ne_binary()}.
send_early_reminder(AccountId, DueTimestamp) ->
    {PrevYear, PrevMonth, _} = get_previous_month(DueTimestamp),
    ShouldCollect = should_process(AccountId, ?REMINDER_MARKER_ID, PrevYear, PrevMonth),
    send_early_reminder(AccountId, DueTimestamp, PrevYear, PrevMonth, ShouldCollect).

%%------------------------------------------------------------------------------
%% @doc Collect recurring charges now without checking if it already collect.
%%
%% It doesn't not check and set the marker to see if the it ran before.
%% @end
%%------------------------------------------------------------------------------
-spec force_collect(kz_term:ne_binary()) -> run_return().
force_collect(AccountId) ->
    force_collect(AccountId, kz_time:now_s()).

%%------------------------------------------------------------------------------
%% @doc Collect recurring charges for `DueTimestamp' without checking if
%% it already collect.
%%
%% It doesn't not check and set the marker to see if the it ran before.
%% @end
%%------------------------------------------------------------------------------
-spec force_collect(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> run_return().
force_collect(AccountId, DueTimestamp) ->
    collect(AccountId, DueTimestamp).

%%------------------------------------------------------------------------------
%% @doc Send recurring charges reminder now without checking if it
%% already collect.
%%
%% It doesn't not check and set the marker to see if the it ran before.
%% @end
%%------------------------------------------------------------------------------
-spec force_reminder(kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()}.
force_reminder(AccountId) ->
    force_reminder(AccountId, kz_time:now_s()).

%%------------------------------------------------------------------------------
%% @doc Send recurring charges reminder with `DueTimestamp' without checking if
%% it already collect.
%%
%% It doesn't not check and set the marker to see if the it ran before.
%% @end
%%------------------------------------------------------------------------------
-spec force_reminder(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> {'ok', kz_term:ne_binary()}.
force_reminder(AccountId, DueTimestamp) ->
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
    ?MATCH_MODB_SUFFIX_ENCODED(_, Year, Month) = kzs_util:format_account_mod_id(Account),
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
    MODB = ?MATCH_MODB_SUFFIX_ENCODED(_, _, _) = kzs_util:format_account_mod_id(Account),
    is_last_month_collected(MODB).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_last_month_collected(kz_term:ne_binary(), kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month()) ->
          boolean().
is_last_month_collected(Account, Year, Month) ->
    Timestamp =
        calendar:datetime_to_gregorian_seconds({{kz_term:to_integer(Year), kz_term:to_integer(Month), 1}
                                               ,{0, 0, 0}
                                               }),
    {PrevYear, PrevMonth, _} = get_previous_month(Timestamp),
    is_collected(Account, PrevYear, PrevMonth).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_collected(kz_term:ne_binary(), kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month()) ->
          boolean().
is_collected(Account, Year, Month) ->
    AccountId = kzs_util:format_account_id(Account),
    is_proccessed(AccountId
                 ,?COLLECT_RECURRING_MARKER_ID
                 ,kz_term:to_integer(Year)
                 ,kz_term:to_integer(Month)
                 ).

%%%=============================================================================
%%% Collect Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec early_collect(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:year(), kz_time:month(), boolean()) -> run_return().
early_collect(AccountId, DueTimestamp, Year, Month, 'false') ->
    case set_marker(AccountId, ?COLLECT_RECURRING_MARKER_ID, Year, Month) of
        {'ok', _} ->
            collect(AccountId, DueTimestamp);
        {'error', _Reason} ->
            ErrMessage = <<"unabled to set marker to collect recurring charges">>,
            lager:debug("charging account ~s failed: ~s", [AccountId, ErrMessage]),
            {'error', #{message => ErrMessage
                       ,reason => <<"set_marker_fault">>
                       }
            }
    end;
early_collect(_, _, _, _, 'true') ->
    {'ok', <<"account is already processed">>}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collect(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> run_return().
collect(AccountId, DueTimestamp) ->
    lager:debug("attempting to collect recurring charges for ~s", [AccountId]),
    Services = kz_services:fetch(AccountId, ['hydrate_plans']),
    Invoices = kz_services:invoices(Services),
    Results = kz_services_invoices:foldl(collect_invoices_fold_fun(Services, DueTimestamp), [], Invoices),
    handle_collect_bookkeeper_results(Results).

-type collect_invoice_fold() :: fun((kz_services_invoice:invoice(), kz_amqp_worker:request_return()) -> [kz_amqp_worker:request_return()]).
-spec collect_invoices_fold_fun(kz_services:services(), kz_time:gregorian_seconds()) -> collect_invoice_fold().
collect_invoices_fold_fun(Services, DueTimestamp) ->
    fun(Invoice, Results) ->
            Type = kz_services_invoice:bookkeeper_type(Invoice),
            case kzd_services:default_bookkeeper_type() =:= Type of
                'true' ->
                    Result = kz_json:from_list([{<<"Status">>, status_good()}]),
                    [{'ok', Result} | Results];
                'false' ->
                    Result = collect_bookkeeper(Invoice, Services, DueTimestamp),
                    [Result | Results]
            end
    end.

-spec collect_bookkeeper(kz_services_invoice:invoice(), kz_services:services(), kz_time:gregorian_seconds()) ->
          kz_amqp_worker:request_return().
collect_bookkeeper(Invoice, Services, DueTimestamp) ->
    Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
              ,{<<"Bookkeeper-ID">>, kz_services_invoice:bookkeeper_id(Invoice)}
              ,{<<"Bookkeeper-Type">>, kz_services_invoice:bookkeeper_type(Invoice)}
              ,{<<"Call-ID">>, kz_log:get_callid()}
              ,{<<"Due-Timestamp">>, DueTimestamp}
              ,{<<"Vendor-ID">>, kz_services_invoice:bookkeeper_vendor_id(Invoice)}
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
    kz_date:normalize({DueYear, DueMonth - 1,1}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_process(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> boolean().
should_process(AccountId, MarkerId, Year, Month) ->
    is_account_enabled(AccountId)
        andalso is_proccessed(AccountId, MarkerId, Year, Month).

-spec is_account_enabled(kz_term:ne_binary()) -> boolean().
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
    AccountMODB = kzs_util:format_account_mod_id(AccountId, Year, Month),
    case kazoo_modb:open_doc(AccountMODB, MarkerId) of
        {'ok', JObj} ->
            lager:debug("recurring charges for account ~s for ~b-~b is processed on ~s"
                       ,[AccountId, Year, Month, kz_time:format_datetime(kz_doc:created(JObj))]
                       ),
            'true';
        {'error', 'not_found'} ->
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_marker(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
          {'ok', kz_json:object()} |
          {'error', kazoo_data:data_errors()}.
set_marker(AccountId, MarkerId, Year, Month) ->
    AccountMODB = kzs_util:format_account_mod_id(AccountId, Year, Month),
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
            lager:error("failed to save bill early/reminder maker in ~s: ~p", [AccountMODB, _Reason]),
            Error
    end.
