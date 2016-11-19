%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_maintenance).

-export([receipts/0
        ,restore_system_templates/0
        ,restore_system_template/1
        ]).

-include("teletype.hrl").

-define(RECEIPT_FORMAT, " ~4.s | ~45.s | ~-45.s | ~-30.s | ~-30.s | ~-20.s~n").

-spec receipts() -> 'ok'.
receipts() ->
    io:format(?RECEIPT_FORMAT, [<<>>, <<"Call or Msg ID">>, <<"Receipt">>, <<"To">>, <<"From">>, <<"Time">>]),
    Receipts = kz_cache:filter_local(?CACHE_NAME, fun filter_receipts/2),
    Sorted = lists:usort(fun sort_receipts/2, Receipts),
    lists:foldl(fun print_receipt/2, 1, Sorted),
    'ok'.

-spec filter_receipts(any(), any()) -> boolean().
filter_receipts({'receipt', _R}, #email_receipt{}) -> 'true';
filter_receipts(_, _) -> 'false'.

-spec sort_receipts({_, email_receipt()}, {_, email_receipt()}) -> boolean().
sort_receipts({_, #email_receipt{timestamp=S1}}, {_, #email_receipt{timestamp=S2}}) ->
    S1 < S2.

-spec print_receipt({{'receipt', ne_binary()}, email_receipt()}, pos_integer()) -> pos_integer().
print_receipt({{'receipt', Receipt}
              ,#email_receipt{to=To
                             ,from=From
                             ,timestamp=GregSecs
                             ,call_id=CallId
                             }}
             ,Count
             ) ->
    io:format(?RECEIPT_FORMAT, [kz_util:to_binary(Count)
                               ,CallId
                               ,receipt_for_printing(Receipt)
                               ,convert_for_printing(To)
                               ,convert_for_printing(From)
                               ,kz_util:pretty_print_datetime(GregSecs)
                               ]),
    Count+1.

-spec convert_for_printing(ne_binary() | ne_binaries()) -> ne_binary().
convert_for_printing(<<_/binary>>=V) -> V;
convert_for_printing([_|_]=Vs) -> kz_util:join_binary(Vs, <<",">>).

-spec receipt_for_printing(ne_binary()) -> ne_binary().
receipt_for_printing(Receipt) ->
    case re:run(Receipt
               ,<<"^2.0.0 Ok: queued as ([[:alnum:]]+).*\$">>
               ,[{'capture', 'all_but_first', 'binary'}]
               )
    of
        {'match', [QueuedReceipt]} ->
            <<"Queued as ", QueuedReceipt/binary>>;
        _ -> default_receipt_printing(Receipt)
    end.

default_receipt_printing(Receipt) ->
    kz_util:strip_binary(Receipt, [$\n, $\r]).

-spec restore_system_templates() -> ok.
restore_system_templates() ->
    lists:foreach(fun restore_system_template/1, list_system_templates()).

-spec restore_system_template(ne_binary()) -> ok.
restore_system_template(<<"skel">>) -> 'ok';
restore_system_template(TemplateId) ->
    DbId = kz_notification:db_id(TemplateId),
    ModId = kz_notification:resp_id(TemplateId),

    io:format("restoring to default version: ~s(~s)~n", [ModId, DbId]),

    {'ok', TemplateDoc} = kz_datamgr:open_doc(?KZ_CONFIG_DB, DbId),
    {'ok', _Deleted} = kz_datamgr:del_doc(?KZ_CONFIG_DB, TemplateDoc),
    io:format("  deleted ~s~n", [TemplateId]),

    Mod = kz_util:to_atom(<<"teletype_", ModId/binary>>, 'true'),
    io:format("  re-initializing template ~s~n", [ModId]),
    Mod:init(),
    io:format("  finished~n").

list_system_templates() ->
    case kz_datamgr:all_docs(?KZ_CONFIG_DB
                            ,[{'startkey', <<"notification.">>}
                             ,{'endkey', <<"notification.zzz">>}
                             ]
                            )
    of
        {'ok', Results} ->
            [kz_json:get_value(<<"key">>, Result) || Result <- Results];
        {'error', _E} ->
            io:format("failed to query existing notifications: ~p~n", [_E]),
            []
    end.
