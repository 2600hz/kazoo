%%%-------------------------------------------------------------------
%%% @Copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(notify_resend_maintenance).

-export([pending/0, pending/1, pending/2
        ,statistics/0
        ,pending_by_type/1
        ,notify_info/1

        ,delete/1, delete_older_than/1, delete_between/2
        ]).

-include("tasks.hrl").

-define(FORMAT_STRING, "| ~-32s | ~-17s | ~-22s | ~-8s | ~-60s |~n").
-define(JOB_TABLE_EDGE,
        io:format("+----------------------------------+-------------------+------------------------+----------+--------------------------------------------------------------+~n", [])
       ).
-define(JOB_TABLE_HEADER_EDGE,
        io:format("+==================================+===================+========================+==========+==============================================================+~n", [])
       ).
-define(JOB_TABLE_HEADER,
        begin
            ?JOB_TABLE_EDGE,
            io:format(?FORMAT_STRING, [<<"Job">>, <<"Modified">>, <<"Type">>, <<"Attempts">>, <<"Failure Reason">>]),
            ?JOB_TABLE_HEADER_EDGE
        end
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Same as pending/2 without showing details and limited to 100 notifications
%% @end
%%--------------------------------------------------------------------
-spec pending() -> 'no_return'.
pending() ->
    pending(<<"100">>, <<"false">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Same as pending/2 without showing details
%% @end
%%--------------------------------------------------------------------
-spec pending(ne_binary()) -> 'no_return'.
pending(ShowCount) ->
    pending(ShowCount, <<"false">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Prints a limited amount of notifications publish pending.
%% Notification that been process is excluded.
%% Options:
%%   ShowCount: amount of notificatiosn to show
%%   Details: Whether or not to show notifications details
%% @end
%%--------------------------------------------------------------------
-spec pending(ne_binary(), ne_binary()) -> 'no_return'.
pending(ShowCount, <<"true">>) ->
    Total = get_total([{group_level, 0}]),
    RunningIds = [kz_doc:id(J) || J <- kz_notify_resend:running()],
    RunningIdsLength = length(RunningIds),
    PendingLength = Total - RunningIdsLength,

    ViewOptions = [{'limit', ShowCount}
                  ,{'reduce', 'false'}
                  ,'include_docs'
                  ],
    _ = case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/pending_range">>, ViewOptions) of
            {ok, JObjs} -> [print_json(kz_json:get_value(<<"doc">>, J)) || J <- JObjs];
            {error, _Reason} -> io:format("Failed getting pending notifications: ~p~n", [_Reason])
        end,
    io:format(" Total: ~b, Running ~b, Pending ~b~n", [Total, RunningIdsLength, PendingLength]),
    'no_return';
pending(ShowCount, _Details) ->
    Total = get_total([{group_level, 0}]),
    RunningIds = [kz_doc:id(J) || J <- kz_notify_resend:running()],
    RunningIdsLength = length(RunningIds),
    PendingLength = Total - RunningIdsLength,
    print_job_table([{'limit', ShowCount}
                    ,{'reduce', 'false'}
                    ], RunningIds),
    io:format(" Total: ~b, Running ~b, Pending ~b~n", [Total, RunningIdsLength, PendingLength]),
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Show Pending notification by type, limited to first 100 ready to
%% process.
%% @end
%%--------------------------------------------------------------------
-spec pending_by_type(ne_binary()) -> 'no_return'.
pending_by_type(Type) ->
    Total = get_total([{startkey, [Type, 0]}
                      ,{endkey, [Type, kz_time:current_tstamp()]}
                      ,{'group_level', 1}
                      ,'reduce'
                      ]),
    ViewOptions = [{startkey, [Type, 0]}
                  ,{endkey, [Type, kz_time:current_tstamp()]}
                  ,{reduce, false}
                  ,{limit, 100}
                  ],
    print_job_table(ViewOptions, [], <<"pending_notify/pending_by_type">>),
    io:format(" Total: ~b~n", [Total]),
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Show details for the notification by Id
%% @end
%%--------------------------------------------------------------------
-spec notify_info(ne_binary()) -> 'no_return'.
notify_info(Id) ->
    _ = case kz_datamgr:open_doc(?KZ_PENDING_NOTIFY_DB, Id) of
            {'ok', JObj} ->
                print_json(JObj);
            {'error', _Reason} ->
                io:format("Error opening ~s: ~p~n", [Id, _Reason])
        end,
    'no_return'.

-spec delete(Id) -> 'no_return'.
delete(Id) ->
    case kz_datamgr:del_doc(?KZ_PENDING_NOTIFY_DB, Id) of
        {'ok', _} -> io:format("notification ~s has been deleted~n", [Id]);
        {'error', _Reason} -> io:format("failed to delete notification ~s: ~p~n", [Id, _Reason])
    end.

delete_older_than(Timestamp) ->
    TsTamp = kz_term:to_integer(Timestamp),
    ViewOptions = [{startkey, [0]}
                  ,{endkey, [TsTamp]}
                  ],
    ToBeDeleted = case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/pending_range">>, ViewOptions) of
                      {ok, []} ->
                          io:format("no notification older than ~b has been found~n", [TsTamp]),
                          [];
                      {ok, JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
                      {error, _Reason} ->
                          io:format("failed to found notifications: ~p~n", [_Reason]),
                          []
                  end,
    bulk_delete(ToBeDeleted),
    'no_return'.

delete_between(StartTimestamp, EndTimestamp) ->
    Start = kz_term:to_integer(StartTimestamp),
    End = kz_term:to_integer(EndTimestamp),
    ViewOptions = [{startkey, [Start]}
                  ,{endkey, [End]}
                  ],
    ToBeDeleted = case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/pending_range">>, ViewOptions) of
                      {ok, []} ->
                          io:format("no notification older than ~b has been found~n", [TsTamp]),
                          [];
                      {ok, JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
                      {error, _Reason} ->
                          io:format("failed to found notifications: ~p~n", [_Reason]),
                          []
                  end,
    bulk_delete(ToBeDeleted),
    'no_return'.

bulk_delete([]) -> 'ok';
bulk_delete(Ids) ->
    case kz_datamgr:del_docs(?KZ_PENDING_NOTIFY_DB, Ids) of
        {'ok', Js} -> io:format("~b notifications has been deleted~n", [length(Js)]);
        {'error', _Reason} -> io:format("failed to delete notifications: ~p~n", [_Reason])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Prints total pending notification for each type
%% @end
%%--------------------------------------------------------------------
-spec statistics() -> 'no_return'.
statistics() ->
    ViewOptions = ['reduce'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/pending_by_type">>, ViewOptions) of
        {'ok', Rs} ->
            Fun = fun(TypeJObj, Acc) ->
                      TypeCount =  kz_json:get_integer_value(<<"value">>, TypeJObj),
                      [Type] = kz_json:get_value(<<"key">>, TypeJObj),
                      io:format("~-26s: ~b~n", [Type, TypeCount]),
                      TypeCount + Acc
                  end,
            Total = lists:foldl(Fun, 0, Rs),
            io:format("Total: ~p~n", [Total]);
        {'error', _Reason} ->
            io:format("Failed to get pending statistics: ~p", [_Reason])
    end,
    io:format("Total running: ~b~n", [length(kz_notify_resend:running())]),
    'no_return'.

-spec get_total(kz_proplist()) -> non_neg_integer().
get_total(ViewOptions) ->
    case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, <<"pending_notify/pending_by_type">>, ViewOptions) of
        {'ok', Rs} ->
            lists:sum([kz_json:get_integer_value(<<"value">>, R, 0) || R <- Rs]);
        {'error', _} -> 0
    end.

%% print_job_table(ViewOptions) ->
%%     print_job_table(ViewOptions, []).

print_job_table(ViewOptions, RunningIds) ->
    print_job_table(ViewOptions, RunningIds, <<"pending_notify/pending_range">>).

print_job_table(ViewOptions, RunningIds, View) ->
    ?JOB_TABLE_HEADER,
    _ = case kz_datamgr:get_results(?KZ_PENDING_NOTIFY_DB, View, ViewOptions)
        of
            {'ok', []} ->
                io:format("| ~-151s |~n", ["No pending notification publish found"]);
            {'ok', JObjs} ->
                [io:format(?FORMAT_STRING, [kz_json:get_value([<<"value">>, <<"id">>], JObj)
                                           ,format_modified_date(kz_json:get_value(<<"value">>, JObj))
                                           ,kz_json:get_value([<<"value">>, <<"type">>], JObj)
                                           ,kz_json:get_ne_binary_value([<<"value">>, <<"attempts">>], JObj)
                                           ,kz_json:get_ne_binary_value([<<"value">>, <<"reason">>], JObj)
                                           ])
                 || JObj <- [J || J <- JObjs, not lists:member(kz_json:get_value([<<"value">>, <<"id">>], J), RunningIds)]
                ];
            {'error', _Reason} ->
                io:format("| Failed getting pending notifications: ~p~n", [_Reason])
        end,
    ?JOB_TABLE_EDGE.

-spec format_modified_date(kz_json:object()) -> ne_binary().
format_modified_date(JObj) ->
    Modified = kz_json:get_integer_value(<<"modified">>, JObj, kz_json:get_integer_value(<<"pvt_modified">>, JObj)),
    RetryAfter = kz_json:get_integer_value(<<"retry_after">>, JObj, 0),
    kz_time:format_datetime(Modified + RetryAfter).

-spec print_json(kz_json:json_term()) -> 'ok'.
print_json(Data) ->
    io:fwrite(io_lib:format("~ts\n", [kz_json:encode(Data, ['pretty'])])).
