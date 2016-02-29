%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_maintenance).

-export([receipts/0]).

-include("teletype.hrl").

-define(RECEIPT_FORMAT, " ~4.s | ~45.s | ~-45.s | ~-30.s | ~-30.s | ~-20.s~n").

-spec receipts() -> 'ok'.
receipts() ->
    io:format(?RECEIPT_FORMAT, [<<>>, <<"Call or Msg ID">>, <<"Receipt">>, <<"To">>, <<"From">>, <<"Time">>]),
    Receipts = kzc_cache:filter(?CACHE_NAME, fun filter_receipts/2),
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
    io:format(?RECEIPT_FORMAT, [wh_util:to_binary(Count)
                                ,CallId
                                ,receipt_for_printing(Receipt)
                                ,convert_for_printing(To)
                                ,convert_for_printing(From)
                                ,wh_util:pretty_print_datetime(GregSecs)
                               ]),
    Count+1.

-spec convert_for_printing(ne_binary() | ne_binaries()) -> ne_binary().
convert_for_printing(<<_/binary>>=V) -> V;
convert_for_printing([_|_]=Vs) -> wh_util:join_binary(Vs, <<",">>).

-spec receipt_for_printing(ne_binary()) -> ne_binary().
receipt_for_printing(Receipt) ->
    case re:run(Receipt
                ,<<"^2.0.0 Ok: queued as ([[:alnum:]]+).*$">>
                ,[{'capture', 'all_but_first', 'binary'}]
               )
    of
        {'match', [QueuedReceipt]} ->
            <<"Queued as ", QueuedReceipt/binary>>;
        _ -> default_receipt_printing(Receipt)
    end.

default_receipt_printing(Receipt) ->
    wh_util:strip_binary(Receipt, [$\n, $\r]).
