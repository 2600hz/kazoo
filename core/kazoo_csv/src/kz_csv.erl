%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Simple & efficient operations on CSV binaries.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_csv).

-include("csv.hrl").

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

throw_bad(Header, {-1,0}) ->
    case lists:all(fun is_binary/1, Header) of
        %% Strip header line from total rows count
        'true' -> {length(Header), 0};
        'false' -> throw('bad_csv')
    end;
throw_bad(Row, {MaxRow,RowsCounted}) ->
    case length(Row) of
        MaxRow -> {MaxRow, RowsCounted+1};
        _ -> throw('bad_csv')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type folder(T) :: fun((row(), T) -> T).
-spec fold(CSV, folder(T), T) -> T when
      CSV :: binary(),
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
-spec take_row(CSV) -> {row(), CSV} |
                       'eof' when
      CSV :: binary().
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
    [case Cell of
         <<>> -> ?ZILCH;
         _ -> Cell
     end
     || Cell <- binary:split(Row, <<$,>>, ['global'])
    ].

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
    Tmp = <<"/tmp/json_", (kz_util:rand_hex_binary(11))/binary, ".csv">>,
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
