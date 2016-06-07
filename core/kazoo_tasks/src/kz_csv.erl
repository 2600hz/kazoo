%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Simple & efficient operations on CSV binaries.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_csv).

%% Public API
-export([take_row/1
        ,split_row/1
        ,pad_row_to/2
        ,associator/3
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-ifndef(ZILCH).
-define(ZILCH, 'undefined').
-endif.

-type cell() :: ne_binary() | ?ZILCH.
-type row() :: [cell(), ...].

-export_type([row/0
             ,fassoc/0
             ,verifier/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

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
    Row ++ lists:duplicate(N - length(Row), ?ZILCH).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type fassoc_ret() :: {'true', row()} | 'false'.
-type fassoc() :: fun((row()) -> fassoc_ret()).
-type verifier() :: fun((cell()) -> boolean()).
-spec associator(row(), row(), verifier()) -> fassoc().
associator(CSVHeader, OrderedFields, Verifier) ->
    Max = length(OrderedFields),
    Indexed = lists:zip(lists:seq(1, length(CSVHeader)), CSVHeader),
    F =
        fun ({I,Header}, Map) ->
                Map#{find_position(Header, OrderedFields, 1) => I}
        end,
    Map = lists:foldl(F, #{}, Indexed),
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
            case lists:any(fun erlang:is_boolean/1, ReOrdered) of
                'false' -> {'true', ReOrdered};
                'true' -> 'false'
            end
    end.

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


%%% End of Module.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

associator_test() ->
    OrderedFields = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"2">>],
    Verify = fun (_Cell) -> 'true' end,
    Verifier = fun (_Field, Cell) -> Verify(Cell) end,
    FAssoc = ?MODULE:associator(CSVHeader, OrderedFields, Verifier),
    ?assertEqual({'true', [<<"1">>, <<"2">>, <<"3">>, 'undefined', <<"5">>]}, FAssoc(CSVRow)).

associator_verify_test() ->
    OrderedFields = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"2">>],
    Verify = fun (_Cell) -> 'false' end,
    Verifier = fun (<<"B">>, Cell) -> Verify(Cell); (_Field, _Cell) -> 'true' end,
    FAssoc = ?MODULE:associator(CSVHeader, OrderedFields, Verifier),
    ?assertEqual('false', FAssoc(CSVRow)).

rows_test_() ->
    CSV1 = <<"a\r\nb\nc\nd\n\re\r\r">>,
    CSV2 = <<"b\nc\nd\n\re\r\r">>,
    CSV3 = <<"c\nd\n\re\r\r">>,
    CSV4 = <<"d\n\re\r\r">>,
    CSV5 = <<"e\r\r">>,
    CSV6 = <<>>,
    CSV7 = <<"\r\r">>,
    [?_assertEqual({[<<"a">>], CSV2}, ?MODULE:take_row(CSV1))
    ,?_assertEqual({[<<"b">>], CSV3}, ?MODULE:take_row(CSV2))
    ,?_assertEqual({[<<"c">>], CSV4}, ?MODULE:take_row(CSV3))
    ,?_assertEqual({[<<"d">>], CSV5}, ?MODULE:take_row(CSV4))
    ,?_assertEqual({[<<"e">>], CSV6}, ?MODULE:take_row(CSV5))
    ,?_assertEqual('eof', ?MODULE:take_row(CSV6))
    ,?_assertEqual('eof', ?MODULE:take_row(CSV7))
    ,?_assertEqual({[<<"1">>,<<"B">>], <<>>}, ?MODULE:take_row(<<"1,B">>))
    ].

-endif.
