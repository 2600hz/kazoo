%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Simple and efficient operations on CSV binaries.
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_csv).

%% Public API
-export([count_rows/1
        ,fold/3
        ,take_row/1, take_mapped_row/2
        ,pad_row_to/2
        ,associator/3
        ,verify_mapped_row/2
        ,row_to_iolist/1, mapped_row_to_iolist/2
        ,json_to_iolist/1, json_to_iolist/2

        ,jobjs_to_file/1, jobjs_to_file/2
        ,write_header_to_file/1, write_header_to_file/2
        ]).
-export([from_jobjs/1
        ,from_jobjs/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_csv/include/kazoo_csv.hrl").

-type cell() :: binary() | ?ZILCH.
-type header() :: [kz_term:ne_binary(),...].
-type row() :: [cell(),...].
-type mapped_row() :: #{kz_term:ne_binary() => cell()}.
-type csv() :: binary().

-export_type([cell/0
             ,header/0
             ,row/0, mapped_row/0
             ,csv/0
             ,folder/1
             ,fassoc/0, verifier/0
             ,mapped_row_verifier/0
             ,file_return/0
             ]).

-ifdef(TEST).
-export([take_line/1]).
-export([split_row/1]).
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Return count of rows minus the first one.
%% Returns 0 if a row is longer/smaller than the header row.
%% @end
%%------------------------------------------------------------------------------
-spec count_rows(csv()) -> non_neg_integer().
count_rows(<<>>) -> 0;
count_rows(CSV) when is_binary(CSV) ->
    try fold(CSV, fun throw_bad/2, {-1, 0}) of
        {_, TotalRows} -> TotalRows
    catch
        'throw':{'error', 'bad_header_row'} -> 0;
        'throw':{'error', 'bad_csv_row'} -> 0
    end.

-spec throw_bad(row(), {integer(), non_neg_integer()}) -> {integer(), non_neg_integer()}.
throw_bad(Header, {-1, 0}) ->
    case lists:all(fun kz_term:is_ne_binary/1, Header) of
        %% Strip header line from total rows count
        'true' ->
            {length(Header), 0};
        'false' ->
            lager:error("bad header row: ~p", [Header]),
            throw({'error', 'bad_header_row'})
    end;
throw_bad(Row, {MaxRow, RowsCounted}) ->
    case length(Row) of
        MaxRow -> {MaxRow, RowsCounted + 1};
        _Len ->
            lager:error("bad row length ~p instead of ~p in ~p", [_Len, MaxRow, Row]),
            throw({'error', 'bad_csv_row'})
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type folder(T) :: fun((row(), T) -> T).
-spec fold(csv(), folder(T), T) -> T.
fold(CSV, Fun, Acc)
  when is_binary(CSV),
       is_function(Fun, 2) ->
    case take_row(CSV) of
        'eof' -> Acc;
        {Row, CSVRest} ->
            NewAcc = Fun(Row, Acc),
            fold(CSVRest, Fun, NewAcc)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec take_row(csv()) -> {row(), csv()} | 'eof'.
take_row(<<>>) -> 'eof';
take_row(CSV)
  when is_binary(CSV) ->
    case take_line(CSV) of
        'eof' -> 'eof';
        [Line] ->
            {split_row(Line), <<>>};
        [Line, CSVRest] ->
            {split_row(Line), CSVRest}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec take_mapped_row(row(), csv()) -> {mapped_row(), csv()} | 'eof'.
take_mapped_row(Header, CSV)
  when is_binary(CSV) ->
    case take_row(CSV) of
        'eof' -> 'eof';
        {Row, CSVRest} ->
            MappedRow = maps:from_list(lists:zip(Header, Row)),
            {MappedRow, CSVRest}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pad_row_to(non_neg_integer(), row()) -> row().
pad_row_to(N, Row)
  when N > length(Row) ->
    Row ++ lists:duplicate(N - length(Row), ?ZILCH);
pad_row_to(_, Row) ->
    Row.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type fassoc_ret() :: {'ok', mapped_row()} | {'error', kz_term:ne_binary()}.
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
                MappedRow when is_map(MappedRow) -> {'ok', MappedRow};
                Field -> {'error', Field}
            end
    end.

verify(Verifier, Header, Row, I, Map) ->
    Cell = case maps:get(I, Map, 'undefined') of
               'undefined' -> ?ZILCH;
               J -> lists:nth(J, Row)
           end,
    Field = lists:nth(I, Header),
    case Verifier(Field, Cell) of
        'false' -> Field;
        'true' -> {Field, Cell}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns an unordered list of the name of columns that did not pass validation.
%% @end
%%------------------------------------------------------------------------------
-type mapped_row_verifier() :: fun((kz_term:ne_binary(), cell()) -> boolean()).
-spec verify_mapped_row(mapped_row_verifier(), mapped_row()) -> [] | header().
verify_mapped_row(Pred, MappedRow) when is_function(Pred, 2),
                                        is_map(MappedRow) ->
    F = fun (K, V, Acc) ->
                case Pred(K, V) of
                    'true' -> Acc;
                    'false' -> [K|Acc]
                end
        end,
    maps:fold(F, [], MappedRow).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec row_to_iolist(row()) -> iodata().
row_to_iolist([Cell]) -> [cell_to_binary(Cell), $\n];
row_to_iolist(Row=[_|_]) ->
    [csv_ize(Row), $\n].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec mapped_row_to_iolist(row(), mapped_row()) -> iodata().
mapped_row_to_iolist(HeaderRow, Map) ->
    row_to_iolist([maps:get(Header, Map, ?ZILCH) || Header <- HeaderRow]).

%%------------------------------------------------------------------------------
%% @doc Converts JSON-represented CSV data to binary.
%% We assume fields for first record are defined in all other records.
%% @end
%%------------------------------------------------------------------------------
-spec json_to_iolist(nonempty_list(kz_json:object())) -> iodata().
json_to_iolist(Records) ->
    json_to_iolist(Records, kz_json:get_keys(hd(Records))).

-spec json_to_iolist(nonempty_list(kz_json:object()), header()) -> iodata().
json_to_iolist(Records, Fields)
  when is_list(Records),
       is_list(Fields) ->
    Tmp = <<"/tmp/json_", (kz_binary:rand_hex(11))/binary, ".csv">>,
    'ok' = file:write_file(Tmp, [kz_term:iolist_join($,, Fields), $\n]),
    lists:foreach(fun (Record) ->
                          Row = [kz_json:get_ne_binary_value(Field, Record, ?ZILCH) || Field <- Fields],
                          _ = file:write_file(Tmp, [row_to_iolist(Row)], ['append'])
                  end
                 ,Records
                 ),
    {'ok', IOData} = file:read_file(Tmp),
    kz_util:delete_file(Tmp),
    IOData.

-spec write_header_to_file(file_return()) -> file_return().
write_header_to_file({'undefined', _}=CSVAcc) -> CSVAcc;
write_header_to_file({File, CellOrdering}) ->
    write_header_to_file({File, CellOrdering}, []).

-spec write_header_to_file(file_return(), kz_term:proplist()) -> file_return().
write_header_to_file({File, CellOrdering}, HeaderMap) ->
    HeaderFile = <<File/binary, ".header">>,

    Headings = [begin
                    Heading = kz_binary:join(Cells, <<"_">>),
                    props:get_value(Heading, HeaderMap, Heading)
                end
                || Cells <- CellOrdering
               ],

    Header = [csv_ize(Headings), $\n],
    'ok' = file:write_file(HeaderFile, Header),

    {'ok', _} = kz_os:cmd(<<"cat ", File/binary, " >> ", HeaderFile/binary>>),
    {'ok', _} = file:copy(HeaderFile, File),

    kz_util:delete_file(HeaderFile),

    {File, CellOrdering}.

-type file_return() :: {'undfined' | file:filename_all(), kz_json:paths()}.
-spec jobjs_to_file(kz_json:objects()) -> file_return().
jobjs_to_file([]) -> {'undefined', []};
jobjs_to_file([JObj | _]=JObjs) ->
    CellOrdering = maybe_update_ordering([], kz_json:flatten(JObj)),
    jobjs_to_file(JObjs, CellOrdering).

-spec jobjs_to_file(kz_json:objects(), file_return() | kz_json:paths()) -> file_return().
jobjs_to_file(JObjs, {File, CellOrdering}) ->
    lists:foldl(fun jobj_to_file/2
               ,{File, CellOrdering}
               ,JObjs
               );
jobjs_to_file(JObjs, CellOrdering) ->
    jobjs_to_file(JObjs, {csv_filename(), CellOrdering}).

csv_filename() ->
    <<"/tmp/json_", (kz_binary:rand_hex(11))/binary, ".csv">>.

-spec maybe_convert_cell_to_binary(kz_json:get_key(), kz_json:object()) -> binary().
maybe_convert_cell_to_binary(Path, JObj) ->
    case kz_json:get_value(Path, JObj, ?ZILCH) of
        List when is_list(List) -> list_to_binary(lists:join(",", List));
        Value -> cell_to_binary(Value)
    end.

-spec jobj_to_file(kz_json:object(), file_return()) -> file_return().
jobj_to_file(JObj, {File, CellOrdering}) ->
    FlatJObj = kz_json:flatten(JObj),
    NewOrdering = maybe_update_ordering(CellOrdering, FlatJObj),

    Row = [maybe_convert_cell_to_binary(Path, JObj) || Path <- NewOrdering],
    _ = file:write_file(File, [csv_ize(Row), $\n], ['append']),
    {File, NewOrdering}.

maybe_update_ordering(CellOrdering, FlatJObj) ->
    kz_json:foldl(fun maybe_add_field/3, CellOrdering, FlatJObj).

maybe_add_field(Field, Value, CellOrdering) ->
    IsJsonObject = kz_json:is_json_object(Value),
    case lists:member(Field, CellOrdering) of
        'false' when not IsJsonObject ->
            lager:debug("adding field ~s", [Field]),
            CellOrdering ++ [Field];
        'false' ->
            lager:debug("skipping JSON field ~p: ~p", [Field, Value]),
            CellOrdering;
        'true' -> CellOrdering
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_jobjs(kz_json:objects()) -> iolist().
from_jobjs(JObjs) ->
    from_jobjs(JObjs, []).

-spec from_jobjs(kz_json:objects(), kz_term:proplist()) -> iolist().
from_jobjs(JObjs, Options) ->
    Routines = [fun maybe_transform/2
               ,fun check_integrity/2
               ,fun json_objs_to_csv/2
               ],
    lists:foldl(fun(F, J) -> F(J, Options) end, JObjs, Routines).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec take_line(csv()) -> [csv(),...] | 'eof'.
take_line(<<>>) -> 'eof';
take_line(CSV) ->
    case binary:split(CSV, [<<"\r\n">>, <<"\n\r">>, <<"\r\r">>, <<$\n>>, <<$\r>>]) of
        [<<>>|_] -> 'eof';
        Split -> Split
    end.

-define(ASCII_SINGLE_QUOTE, 39).
-define(ASCII_DOUBLE_QUOTE, 34).
-define(ASCII_COMMA, 44).

-spec split_row(kz_term:ne_binary()) -> row().
split_row(Line) ->
    case lists:foldl(fun consume_char/2
                    ,{[], ?ASCII_COMMA, []}
                    ,binary_to_list(Line)
                    )
    of
        {Acc, ?ASCII_COMMA, []} -> lists:reverse([?ZILCH | Acc]);
        {Acc, ?ASCII_COMMA, CellAcc} -> lists:reverse([from_cell_acc(CellAcc) | Acc]);

        {Acc, ?ASCII_DOUBLE_QUOTE, [?ASCII_DOUBLE_QUOTE]} ->
            lists:reverse([<<>> | Acc]);
        {Acc, ?ASCII_SINGLE_QUOTE, [?ASCII_SINGLE_QUOTE]} ->
            lists:reverse([<<>> | Acc]);
        {Acc, ?ASCII_DOUBLE_QUOTE, [?ASCII_DOUBLE_QUOTE | CellAcc]} ->
            lists:reverse([from_cell_acc(CellAcc) | Acc]);
        {Acc, ?ASCII_SINGLE_QUOTE, [?ASCII_SINGLE_QUOTE | CellAcc]} ->
            lists:reverse([from_cell_acc(CellAcc) | Acc]);
        {Acc, ?ASCII_DOUBLE_QUOTE, CellAcc} ->
            lists:reverse([from_cell_acc(CellAcc) | Acc]);
        {Acc, ?ASCII_SINGLE_QUOTE, CellAcc} ->
            lists:reverse([from_cell_acc(CellAcc) | Acc])
    end.

from_cell_acc(CellAcc) ->
    iolist_to_binary(lists:reverse(CellAcc)).

consume_char(?ASCII_DOUBLE_QUOTE, {Acc, ?ASCII_COMMA, []}) ->
    %% Cell is starting with a quote
    {Acc, ?ASCII_DOUBLE_QUOTE, []};
consume_char(?ASCII_COMMA, {Acc, ?ASCII_DOUBLE_QUOTE, [?ASCII_DOUBLE_QUOTE | CellAcc]}) ->
    %% double-quoted cell is finished
    {[from_cell_acc(CellAcc) | Acc], ?ASCII_COMMA, []};

consume_char(?ASCII_SINGLE_QUOTE, {Acc, ?ASCII_COMMA, []}) ->
    %% Cell is starting with a quote
    {Acc, ?ASCII_SINGLE_QUOTE, []};
consume_char(?ASCII_COMMA, {Acc, ?ASCII_SINGLE_QUOTE, [?ASCII_SINGLE_QUOTE | CellAcc]}) ->
    %% single-quoted cell is finished
    {[from_cell_acc(CellAcc) | Acc], ?ASCII_COMMA, []};

consume_char(?ASCII_COMMA, {Acc, ?ASCII_COMMA, []}) ->
    %% empty cell collected
    {[?ZILCH | Acc], ?ASCII_COMMA, []};
consume_char(?ASCII_COMMA, {Acc, ?ASCII_COMMA, CellAcc}) ->
    %% new cell starting
    {[from_cell_acc(CellAcc) | Acc], ?ASCII_COMMA, []};
consume_char(Char, {Acc, ?ASCII_COMMA, CellAcc}) ->
    {Acc, ?ASCII_COMMA, [Char | CellAcc]};

consume_char(Quoted, {Acc, Quoted, [Quoted | _]=CellAcc}) ->
    %% If we see ''foo'' - normalize to 'foo'
    {Acc, Quoted, CellAcc};
consume_char(Char, {Acc, Quoted, CellAcc}) ->
    {Acc, Quoted, [Char | CellAcc]}.

-spec find_position(kz_term:ne_binary(), kz_term:ne_binaries()) -> pos_integer().
find_position(Item, Items) ->
    find_position(Item, Items, 1).

-spec find_position(kz_term:ne_binary(), kz_term:ne_binaries(), pos_integer()) -> pos_integer().
find_position(Item, [Item|_], Pos) -> Pos;
find_position(Item, [_|Items], N) ->
    find_position(Item, Items, N+1).

complete_header(Fields, CSVHeader) ->
    Diff = CSVHeader -- Fields,
    Fields ++ Diff.

map_io_indices(Header, CSVHeader) ->
    MapF = fun ({I, Head}, M) ->
                   M#{find_position(Head, Header) => I}
           end,
    IndexToCSVHeader = lists:zip(lists:seq(1, length(CSVHeader)), CSVHeader),
    lists:foldl(MapF, #{}, IndexToCSVHeader).

-spec cell_to_binary(cell()) -> binary().
cell_to_binary(?ZILCH) -> <<>>;
cell_to_binary(<<>>) -> <<>>;
cell_to_binary(Cell=?NE_BINARY) ->
    binary:replace(Cell, <<$,>>, <<$;>>, ['global']);
cell_to_binary(Cell) ->
    kz_term:to_binary(Cell).

-spec maybe_transform(kz_json:objects(), kz_term:proplist()) -> kz_json:objects().
maybe_transform(JObjs, Options) ->
    case props:get_value('transform_fun', Options) of
        'undefined' -> JObjs;
        Fun -> [kz_json:map(Fun, JObj) || JObj <- JObjs]
    end.

-spec check_integrity(list(), kz_term:proplist()) -> kz_json:objects().
check_integrity(JObjs, _Options) ->
    Headers = get_headers(JObjs),
    check_integrity(JObjs, Headers, []).

-spec check_integrity(kz_json:objects(), kz_term:ne_binaries(), kz_json:objects()) ->
                             kz_json:objects().
check_integrity([], _, Acc) ->
    lists:reverse(Acc);
check_integrity([JObj|JObjs], Headers, Acc) ->
    NJObj = lists:foldl(fun check_integrity_fold/2, JObj, Headers),
    NJObj1 = kz_json:from_list(lists:keysort(1, kz_json:to_proplist(NJObj))),
    check_integrity(JObjs, Headers, [NJObj1|Acc]).

-spec check_integrity_fold(kz_json:path(), kz_json:object()) ->
                                  kz_json:json_term().
check_integrity_fold(Header, JObj) ->
    case kz_json:get_value(Header, JObj) of
        'undefined' ->
            kz_json:set_value(Header, <<>>, JObj);
        _ -> JObj
    end.

-spec get_headers(kz_json:objects()) -> kz_term:ne_binaries().
get_headers(JObjs) ->
    lists:foldl(fun fold_over_objects/2, [], JObjs).

-spec fold_over_objects(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
fold_over_objects(JObj, Headers) ->
    lists:foldl(fun fold_over_keys/2, Headers, kz_json:get_keys(JObj)).

-spec fold_over_keys(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
fold_over_keys(Key, Hs) ->
    case lists:member(Key, Hs) of
        'false' -> [Key|Hs];
        'true' -> Hs
    end.

-spec create_csv_header(kz_json:objects(), kz_term:proplist()) -> iolist().
create_csv_header(JObjs, Options) ->
    Headers = case props:get_value('header_map', Options) of
                  'undefined' -> get_headers(JObjs);
                  HeaderMap ->
                      lists:map(fun(JObjHeader) -> header_map(JObjHeader, HeaderMap) end
                               ,get_headers(JObjs)
                               )
              end,
    [csv_ize(lists:reverse(Headers)), $\n].

-spec header_map(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary().
header_map(JObjHeader, HeaderMap) ->
    case props:get_value(JObjHeader, HeaderMap) of
        'undefined' -> JObjHeader; % doesn't change Header to HeaderMap's "friendly" version
        FriendlyHeader -> FriendlyHeader
    end.

-spec json_objs_to_csv(kz_json:objects(), kz_term:proplist()) -> iolist().
json_objs_to_csv([], _) -> [];
json_objs_to_csv(JObjs, Options) ->
    case props:is_true('build_headers', Options, 'true') of
        'true' -> [create_csv_header(JObjs, Options), [[json_to_csv(JObj), $\n] || JObj <- JObjs]];
        'false' -> [[json_to_csv(JObj), $\n] || JObj <- JObjs]
    end.

%% wrap cells in quotes
-spec csv_ize(kz_json:path()) -> iolist().
csv_ize([F|Rest]) ->
    [wrap_first_cell(try_to_binary(F))
    ,[wrap_next_cell(try_to_binary(V)) || V <- Rest]
    ].

wrap_first_cell(?ZILCH) ->
    [];
wrap_first_cell(V) ->
    [<<"\"">>, kz_term:to_binary(V), <<"\"">>].

wrap_next_cell(?ZILCH) ->
    [<<",">>];
wrap_next_cell(V) ->
    [<<",\"">>, V, <<"\"">>].

-spec try_to_binary(any()) -> kz_term:api_binary().
try_to_binary('undefined') -> 'undefined';
try_to_binary(Value) ->
    try kz_term:to_binary(Value)
    catch
        _E:_R -> <<>>
    end.

-spec json_to_csv(kz_json:object()) -> iolist().
json_to_csv(JObj) ->
    csv_ize(kz_json:values(JObj)).
