%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Simple & efficient operations on CSV binaries.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_csv).

%% Public API
-export([count_rows/1
        ,fold/3
        ,take_row/1
        ,split_row/1
        ,pad_row_to/2
        ,associator/3
        ,row_to_iolist/1
        ,json_to_iolist/1
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-ifndef(ZILCH).
-define(ZILCH, 'undefined').
-endif.

-type cell() :: ne_binary() | ?ZILCH.
-type row() :: [cell(), ...].

-export_type([cell/0
             ,row/0
             ,folder/1
             ,fassoc/0
             ,verifier/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return count of rows minus the first one.
%% Returns 0 if a row is longer/smaller than the header row.
%% @end
%%--------------------------------------------------------------------
-spec count_rows(binary()) -> non_neg_integer().
count_rows(<<>>) -> 0;
count_rows(CSV) when is_binary(CSV) ->
    try fold(CSV, fun throw_bad/2, {-1,0}) of
        {_, TotalRows} -> TotalRows
    catch
        'throw':'bad_csv' -> 0
    end.

-spec throw_bad(row(), {integer(), non_neg_integer()}) -> {integer(), non_neg_integer()}.
throw_bad(Header, {-1,0}) ->
    case lists:all(fun is_binary/1, Header) of
        %% Strip header line from total rows count
        'true' ->
            io:format("header: ~p~n", [length(Header)]),
            {length(Header), 0};
        'false' ->
            io:format("bad header: ~p~n", [Header]),
            throw('bad_csv')
    end;
throw_bad(Row, {MaxRow,RowsCounted}) ->
    case length(Row) of
        MaxRow -> {MaxRow, RowsCounted+1};
        _ ->
            io:format("bad row: ~p~n", [Row]),
            throw('bad_csv')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type folder(T) :: fun((row(), T) -> T).
-spec fold(binary(), folder(T), T) -> T when
      T :: any().
fold(CSV, Fun, Acc)
  when is_binary(CSV), is_function(Fun, 2) ->
    case take_row(CSV) of
        'eof' -> Acc;
        {Row, CSVRest} ->
            NewAcc = Fun(Row, Acc),
            fold(CSVRest, Fun, NewAcc)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec take_row(binary()) -> {row(), binary()} |
                            'eof'.
take_row(<<>>) -> 'eof';
take_row(CSV=?NE_BINARY) ->
    case binary:split(CSV, [<<"\r\n">>, <<"\n\r">>, <<"\r\r">>, <<"\n">>, <<"\r">>]) of
        [<<>>|_] -> 'eof';
        [Row] ->
            {split_row(Row), <<>>};
        [Row, CSVRest] ->
            {split_row(Row), CSVRest}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec split_row(ne_binary()) -> row().
split_row(Row=?NE_BINARY) ->
    split_fields(Row, []).

split_fields(<<>>, Fields) ->
    lists:reverse(Fields);
split_fields(<<$\n, _/binary>>, Fields) ->
    lists:reverse(Fields);
split_fields(<<$", Row/binary>>, Fields) ->
    split_field(Row, $", Fields);
split_fields(<<$', Row/binary>>, Fields) ->
    split_field(Row, $', Fields);
split_fields(Row, Fields) ->
    split_field(Row, $,, Fields).

split_field(Row, EndChar, Fields) ->
    split_field(Row, EndChar, Fields, []).

split_field(<<$,, Row/binary>>, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<EndChar, $,, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary([EndChar | lists:reverse([EndChar | FieldSoFar])]),
    split_fields(Row, [Field | Fields]);

split_field(<<>>, $,, Fields, FieldSoFar) ->
    split_fields(<<>>, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<EndChar>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary([EndChar | lists:reverse([EndChar | FieldSoFar])]),
    split_fields(<<>>, [Field | Fields]);

split_field(<<Char:1/binary, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    split_field(Row, EndChar, Fields, [Char | FieldSoFar]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pad_row_to(non_neg_integer(), row()) -> row().
pad_row_to(N, Row)
  when N > length(Row) ->
    Row ++ lists:duplicate(N - length(Row), ?ZILCH);
pad_row_to(_, Row) ->
    Row.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type fassoc_ret() :: {'true', row()} | 'false'.
-type fassoc() :: fun((row()) -> fassoc_ret()).
-type verifier() :: fun((atom(), cell()) -> boolean()).
-spec associator(row(), row(), verifier()) -> fassoc().
associator(CSVHeader, OrderedFields, Verifier) ->
    Max = length(OrderedFields),
    Map = maps:from_list(
            [{find_position(Header, OrderedFields, 1), I}
             || {I,Header} <- lists:zip(lists:seq(1, length(CSVHeader)), CSVHeader)
            ]),
    fun (Row0) ->
            Row = pad_row_to(Max, Row0),
            ReOrdered =
                [ begin
                      Cell = case maps:get(I, Map, 'undefined') of
                                 'undefined' -> ?ZILCH;
                                 J -> lists:nth(J, Row)
                             end,
                      Verifier(lists:nth(I, OrderedFields), Cell)
                          andalso Cell
                  end
                  || I <- lists:seq(1, Max)
                ],
            case lists:any(fun is_boolean/1, ReOrdered) of
                'false' -> {'true', ReOrdered};
                'true' -> 'false'
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec row_to_iolist(row()) -> iodata().
row_to_iolist([Cell]) -> cell_to_binary(Cell);
row_to_iolist(Row=[_|_]) ->
    kz_util:iolist_join($,, [cell_to_binary(Cell) || Cell <- Row]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Converts JSON-represented CSV data to binary.
%% We assume fields for first record are defined in all other records.
%% @end
%%--------------------------------------------------------------------
-spec json_to_iolist(nonempty_list(kz_json:object())) -> iodata().
json_to_iolist(Records)
  when is_list(Records) ->
    Tmp = <<"/tmp/json_", (kz_binary:rand_hex(11))/binary, ".csv">>,
    Fields = kz_json:get_keys(hd(Records)),
    'ok' = file:write_file(Tmp, [kz_util:iolist_join($,, Fields), $\n]),
    lists:foreach(fun (Record) ->
                          Row = [kz_json:get_ne_binary_value(Field, Record, ?ZILCH) || Field <- Fields],
                          _ = file:write_file(Tmp, [row_to_iolist(Row),$\n], ['append'])
                  end
                 ,Records
                 ),
    {'ok', IOData} = file:read_file(Tmp),
    kz_util:delete_file(Tmp),
    IOData.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec find_position(A, [A], I) -> I when
      A :: ne_binary(),
      I :: pos_integer().
find_position(Item, [Item|_], Pos) -> Pos;
find_position(Item, [_|Items], N) ->
    find_position(Item, Items, N+1).

%% @private
-spec cell_to_binary(cell()) -> binary().
cell_to_binary(?ZILCH) -> <<>>;
cell_to_binary(Cell=?NE_BINARY) ->
    binary:replace(Cell, <<$,>>, <<$;>>, ['global']).


%%% End of Module.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

associator_test() ->
    OrderedFields = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"2">>],
    Verify = fun (_Cell) -> 'true' end,
    Verifier = fun (_Field, Cell) -> Verify(Cell) end,
    FAssoc = associator(CSVHeader, OrderedFields, Verifier),
    ?assertEqual({'true', [<<"1">>, <<"2">>, <<"3">>, 'undefined', <<"5">>]}, FAssoc(CSVRow)).

associator_verify_test() ->
    OrderedFields = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"2">>],
    Verify = fun (_Cell) -> 'false' end,
    Verifier = fun (<<"B">>, Cell) -> Verify(Cell); (_Field, _Cell) -> 'true' end,
    FAssoc = associator(CSVHeader, OrderedFields, Verifier),
    ?assertEqual('false', FAssoc(CSVRow)).

take_row_test_() ->
    CSV1 = <<"a\r\nb\nc\nd\n\re\r\r">>,
    CSV2 = <<"b\nc\nd\n\re\r\r">>,
    CSV3 = <<"c\nd\n\re\r\r">>,
    CSV4 = <<"d\n\re\r\r">>,
    CSV5 = <<"e\r\r">>,
    CSV6 = <<>>,
    CSV7 = <<"\r\r">>,
    [?_assertEqual({[<<"a">>], CSV2}, take_row(CSV1))
    ,?_assertEqual({[<<"b">>], CSV3}, take_row(CSV2))
    ,?_assertEqual({[<<"c">>], CSV4}, take_row(CSV3))
    ,?_assertEqual({[<<"d">>], CSV5}, take_row(CSV4))
    ,?_assertEqual({[<<"e">>], CSV6}, take_row(CSV5))
    ,?_assertEqual('eof', take_row(CSV6))
    ,?_assertEqual('eof', take_row(CSV7))
    ,?_assertEqual({[<<"1">>,<<"B">>], <<>>}, take_row(<<"1,B">>))
    ].

pad_row_to_test_() ->
    [?_assertEqual([?ZILCH], pad_row_to(1, []))
    ,?_assertEqual([?ZILCH], pad_row_to(1, [?ZILCH]))
    ,?_assertEqual([?ZILCH, ?ZILCH, ?ZILCH], pad_row_to(3, [?ZILCH]))
    ].

count_rows_test_() ->
    [?_assertEqual(0, count_rows(<<"a,b,\n,1,2,">>))
    ,?_assertEqual(0, count_rows(<<"abc">>))
    ,?_assertEqual(0, count_rows(<<>>))
    ,?_assertEqual(0, count_rows(<<"a,b,c\n1\n2\n3">>))
    ,?_assertEqual(1, count_rows(<<"a,b,c\n1,2,3">>))
    ,?_assertEqual(3, count_rows(<<"a,b,c\n1,2,3\r\n4,5,6\n7,8,9\n">>))
    ].

row_to_iolist_test_() ->
    [?_assertException('error', 'function_clause', row_to_iolist([]))] ++
        [?_assertEqual(Expected, iolist_to_binary(row_to_iolist(Input)))
         || {Expected, Input} <- [{<<"a,b">>, [<<"a">>, <<"b">>]}
                                 ,{<<"a,,b">>, [<<"a">>, ?ZILCH, <<"b">>]}
                                 ,{<<",,b">>, [?ZILCH, ?ZILCH, <<"b">>]}
                                 ,{<<"a,b,">>, [<<"a">>, <<"b">>, ?ZILCH]}
                                 ,{<<"a,b,,,c">>, [<<"a">>, <<"b">>, ?ZILCH, ?ZILCH, <<"c">>]}
                                 ]
        ].

json_to_iolist_test_() ->
    Records1 = [kz_json:from_list([{<<"A">>, <<"a1">>}])
               ,kz_json:from_list([{<<"A">>, <<"42">>}])
               ],
    Records2 = [kz_json:from_list([{<<"field1">>,?ZILCH}, {<<"field deux">>,<<"QUUX">>}])
               ,kz_json:from_list([{<<"field deux">>, ?ZILCH}])
               ,kz_json:from_list([{<<"field1">>, <<"r'bla.+\\n'">>}])
               ],
    Records3 = [kz_json:from_list([{<<"account_id">>,<<"009afc511c97b2ae693c6cc4920988e8">>}, {<<"e164">>,<<"+14157215234">>}, {<<"cnam.outbound">>,<<"me">>}])
               ,kz_json:from_list([{<<"account_id">>,<<>>}, {<<"e164">>,<<"+14157215235">>}, {<<"cnam.outbound">>,<<>>}])
               ],
    [?_assertEqual(<<"A\na1\n42\n">>, json_to_iolist(Records1))
    ,?_assertEqual(<<"field1,field deux\n,QUUX\n,\nr'bla.+\\n',\n">>, json_to_iolist(Records2))
    ,?_assertEqual(<<"account_id,e164,cnam.outbound\n009afc511c97b2ae693c6cc4920988e8,+14157215234,me\n,+14157215235,\n">>, json_to_iolist(Records3))
    ].

split_test() ->
    Rows = [{<<"\"0.1651\",\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",\"0\"">>
            ,[<<"\"0.1651\"">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"ZAMBIA, MOBILE-26094\"">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"26094\"">>, <<"\"0\"">>]
            }
           ,{<<"\"0.1651\",\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",0">>
            ,[<<"\"0.1651\"">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"ZAMBIA, MOBILE-26094\"">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"26094\"">>, <<"0">>]
            }
           ,{<<"0.1651,\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",\"0\"">>
            ,[<<"0.1651">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"ZAMBIA, MOBILE-26094\"">>, <<"\"ZAMBIA, MOBILE\"">>, <<"\"26094\"">>, <<"\"0\"">>]
            }
           ],
    [?assertEqual(Split, split_row(Row)) || {Row, Split} <- Rows].

-endif.
