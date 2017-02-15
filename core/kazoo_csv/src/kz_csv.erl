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
-include_lib("kazoo_csv/include/kazoo_csv.hrl").

-type cell() :: ne_binary() | ?ZILCH.
-type row() :: [cell(),...].
-type csv() :: binary().

-export_type([cell/0
             ,row/0
             ,csv/0
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
        'throw':{'error', 'bad_header_row'} -> 0;
        'throw':{'error', 'bad_csv_row'} -> 0
    end.

-spec throw_bad(row(), {integer(), non_neg_integer()}) -> {integer(), non_neg_integer()}.
throw_bad(Header, {-1,0}) ->
    case lists:all(fun is_binary/1, Header) of
        %% Strip header line from total rows count
        'true' ->
            {length(Header), 0};
        'false' ->
            lager:error("bad header row: ~p", [Header]),
            throw({'error', 'bad_header_row'})
    end;
throw_bad(Row, {MaxRow,RowsCounted}) ->
    case length(Row) of
        MaxRow -> {MaxRow, RowsCounted+1};
        _ ->
            lager:error("bad row length ~p instead of ~p in ~p", [length(Row), MaxRow, Row]),
            throw({'error', 'bad_csv_row'})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type folder(T) :: fun((row(), T) -> T).
-spec fold(csv(), folder(T), T) -> T.
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
    case split_row(CSV) of
        {[], <<>>} -> 'eof';
        {Row, Rest} -> {Row, Rest}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec split_row(ne_binary()) -> {row() | [], binary()}.
split_row(Row=?NE_BINARY) ->
    split_fields(Row, []).

-spec split_fields(binary(), [binary()]) -> {row() | [], binary()}.
split_fields(<<>>, Fields) ->
    {lists:reverse(Fields), <<>>};

split_fields(<<"\n\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r\n", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};

split_fields(<<"\n", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};
split_fields(<<"\r", Rest/binary>>, Fields) ->
    {lists:reverse(Fields), Rest};

split_fields(<<$", Row/binary>>, Fields) ->
    split_field(Row, $", Fields);
split_fields(<<$', Row/binary>>, Fields) ->
    split_field(Row, $', Fields);
split_fields(Row, Fields) ->
    split_field(Row, $,, Fields).

-type field_terminator() :: 34 | 39 | 44. %% $" | $' | $,

-spec split_field(binary(), field_terminator(), row()) ->
                         {row(), binary()}.
-spec split_field(binary(), field_terminator(), row(), [byte()]) ->
                         {row(), binary()}.
split_field(Row, EndChar, Fields) ->
    split_field(Row, EndChar, Fields, []).

split_field(<<$,>>, $,, Fields, FieldSoFar) ->
    split_fields(<<>>, [<<>>, iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<$,, Row/binary>>, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\n\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r\n", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\n", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<"\r", _/binary>>=Row, $,, Fields, FieldSoFar) ->
    split_fields(Row, [iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);

split_field(<<EndChar, EndChar, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    split_field(Row, EndChar, Fields, [EndChar, EndChar | FieldSoFar]);
split_field(<<EndChar, $,, Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(Row, [Field | Fields]);

split_field(<<EndChar, "\r\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r\r", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\r", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r\n", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r\n", Row/binary>>, [Field | Fields]);
split_field(<<EndChar, "\r", Row/binary>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<"\r", Row/binary>>, [Field | Fields]);

split_field(<<>>, $,, Fields, FieldSoFar) ->
    split_fields(<<>>, [<<>>, iolist_to_binary(lists:reverse(FieldSoFar)) | Fields]);
split_field(<<EndChar>>, EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse(FieldSoFar)),
    split_fields(<<>>, [Field | Fields]);

split_field(<<Char:1/binary>>, _EndChar, Fields, FieldSoFar) ->
    Field = iolist_to_binary(lists:reverse([Char | FieldSoFar])),
    split_fields(<<>>, [Field | Fields]);
split_field(<<Char:8, Row/binary>>, EndChar, Fields, FieldSoFar) ->
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
-type fassoc_ret() :: {'ok', map()} | {'error', ne_binary()}.
-type fassoc() :: fun((row()) -> fassoc_ret()).
-type verifier() :: fun((atom(), cell()) -> boolean()).
-spec associator(row(), row(), verifier()) -> fassoc().
associator(CSVHeader, TaskFields, Verifier) ->
    Header = complete_header(TaskFields, CSVHeader),
    Max = length(Header),
    Map = map_io_indices(Header, CSVHeader),
    fun (Row0) ->
            Row = pad_row_to(Max, Row0),
            F = fun (_, ?NE_BINARY=Field) -> Field;
                    (I, MappedRow) when is_map(MappedRow) ->
                        case verify(Verifier, Header, Row, I, Map) of
                            {Key, Cell} -> MappedRow#{Key => Cell};
                            Field -> Field
                        end
                end,
            case lists:foldl(F, #{}, lists:seq(1, Max)) of
                MappedRow when is_map(MappedRow) -> {ok, MappedRow};
                Field -> {error, Field}
            end
    end.

verify(Verifier, Header, Row, I, Map) ->
    Cell = case maps:get(I, Map, undefined) of
               undefined -> ?ZILCH;
               J -> lists:nth(J, Row)
           end,
    Field = lists:nth(I, Header),
    case Verifier(Field, Cell) of
        false -> Field;
        true -> {Field, Cell}
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
-spec find_position(ne_binary(), ne_binaries()) -> pos_integer().
-spec find_position(ne_binary(), ne_binaries(), pos_integer()) -> pos_integer().
find_position(Item, Items) ->
    find_position(Item, Items, 1).
find_position(Item, [Item|_], Pos) -> Pos;
find_position(Item, [_|Items], N) ->
    find_position(Item, Items, N+1).

complete_header(Fields, CSVHeader) ->
    Diff = CSVHeader -- Fields,
    Fields ++ Diff.

%% @private
map_io_indices(Header, CSVHeader) ->
    MapF = fun ({I, Head}, M) ->
                   M#{find_position(Head, Header) => I}
           end,
    IndexToCSVHeader = lists:zip(lists:seq(1, length(CSVHeader)), CSVHeader),
    lists:foldl(MapF, #{}, IndexToCSVHeader).

%% @private
-spec cell_to_binary(cell()) -> binary().
cell_to_binary(?ZILCH) -> <<>>;
cell_to_binary(<<>>) -> <<>>;
cell_to_binary(Cell=?NE_BINARY) ->
    binary:replace(Cell, <<$,>>, <<$;>>, ['global']).


%%% End of Module.
