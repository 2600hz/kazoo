%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_csv_test).

-include_lib("kazoo_csv/include/kazoo_csv.hrl").
-include_lib("kazoo/include/kz_types.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(FIELDS, [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>]).


verify_bin1(Cell)
  when 1 =:= byte_size(Cell) -> true;
verify_bin1(_) -> false.

verifier(<<"D">>, _Cell) -> true;
verifier(_Field, Cell) ->
    verify_bin1(Cell).

associator_test() ->
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"2">>],
    FAssoc = kz_csv:associator(CSVHeader, ?FIELDS, fun verifier/2),
    ?assertEqual({ok
                 ,#{<<"A">> => <<"1">>
                   ,<<"B">> => <<"2">>
                   ,<<"C">> => <<"3">>
                   ,<<"D">> => ?ZILCH
                   ,<<"E">> => <<"5">>
                   }
                 }
                ,FAssoc(CSVRow)
                ).

verify_all_1bin(_Field, Cell) ->
    verify_bin1(Cell).

associator_verify_test() ->
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"B">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"42">>],
    FAssoc = kz_csv:associator(CSVHeader, ?FIELDS, fun verify_all_1bin/2),
    ?assertEqual({error,<<"B">>}, FAssoc(CSVRow)).

verify_FIELDS_only(_, undefined) -> true;
verify_FIELDS_only(Field, Value) ->
    case lists:member(Field, ?FIELDS) of
        false -> true;
        true -> 1 =:= byte_size(Value)
    end.

associator_varargs_test() ->
    CSVHeader = [<<"A">>, <<"E">>, <<"C">>, <<"my_field">>],
    CSVRow    = [<<"1">>, <<"5">>, <<"3">>, <<"blip blop">>],
    FAssoc = kz_csv:associator(CSVHeader, ?FIELDS, fun verify_FIELDS_only/2),
    ?assertEqual({ok
                 ,#{<<"A">> => <<"1">>
                   ,<<"B">> => ?ZILCH
                   ,<<"C">> => <<"3">>
                   ,<<"D">> => ?ZILCH
                   ,<<"E">> => <<"5">>
                   ,<<"my_field">> => <<"blip blop">>
                   }
                 }
                ,FAssoc(CSVRow)
                ).

verify_account_id_only(<<"account_id">>, ?MATCH_ACCOUNT_RAW(_)) -> true;
verify_account_id_only(<<"account_id">>, _) -> false;
verify_account_id_only(_Field, _Value) -> true.

associator_varargs2_test_() ->
    Fields = [<<"account_id">>, <<"e164">>, <<"cnam.outbound">>],
    CSVHeader = Fields ++ [<<"opaque.field1.nest1">>],
    CSVRow1 = [<<"bla">>, <<"+14157215235">>, ?ZILCH, ?ZILCH],
    CSVRow2 = [<<"6b71cb72c876b5b1396a335f8f8a2594">>, <<"+14157215234">>, ?ZILCH, <<"val1">>],
    FAssoc = kz_csv:associator(CSVHeader, Fields, fun verify_account_id_only/2),
    [?_assertEqual({error, <<"account_id">>}, FAssoc(CSVRow1))
    ,?_assertEqual({ok
                   ,#{<<"account_id">> => <<"6b71cb72c876b5b1396a335f8f8a2594">>
                     ,<<"e164">> => <<"+14157215234">>
                     ,<<"cnam.outbound">> => ?ZILCH
                     ,<<"opaque.field1.nest1">> => <<"val1">>
                     }
                   }
                  ,FAssoc(CSVRow2)
                  )
    ].

take_row_test_() ->
    CSV1 = <<"a\r\nb\nc\nd\n\re\r\r">>,
    CSV2 = <<"b\nc\nd\n\re\r\r">>,
    CSV3 = <<"c\nd\n\re\r\r">>,
    CSV4 = <<"d\n\re\r\r">>,
    CSV5 = <<"e\r\r">>,
    CSV6 = <<>>,
    CSV7 = <<"\r\r">>,
    [?_assertEqual({[<<"a">>], CSV2}, kz_csv:take_row(CSV1))
    ,?_assertEqual({[<<"b">>], CSV3}, kz_csv:take_row(CSV2))
    ,?_assertEqual({[<<"c">>], CSV4}, kz_csv:take_row(CSV3))
    ,?_assertEqual({[<<"d">>], CSV5}, kz_csv:take_row(CSV4))
    ,?_assertEqual({[<<"e">>], CSV6}, kz_csv:take_row(CSV5))
    ,?_assertEqual(eof, kz_csv:take_row(CSV6))
    ,?_assertEqual(eof, kz_csv:take_row(CSV7))
    ,?_assertEqual({[<<"1">>,<<"B">>], <<>>}, kz_csv:take_row(<<"1,B">>))
    ].

pad_row_to_test_() ->
    [?_assertEqual([?ZILCH], kz_csv:pad_row_to(1, []))
    ,?_assertEqual([?ZILCH], kz_csv:pad_row_to(1, [?ZILCH]))
    ,?_assertEqual([?ZILCH, ?ZILCH, ?ZILCH], kz_csv:pad_row_to(3, [?ZILCH]))
    ].

count_rows_test_() ->
    [?_assertEqual(0, kz_csv:count_rows(<<"a,b,\n,1,2,">>))
    ,?_assertEqual(0, kz_csv:count_rows(<<"abc">>))
    ,?_assertEqual(0, kz_csv:count_rows(<<>>))
    ,?_assertEqual(0, kz_csv:count_rows(<<"a,b,c\n1\n2\n3">>))
    ,?_assertEqual(1, kz_csv:count_rows(<<"a,b,c\n1,2,3">>))
    ,?_assertEqual(3, kz_csv:count_rows(<<"a,b,c\n1,2,3\r\n4,5,6\n7,8,9\n">>))
    ].

row_to_iolist_test_() ->
    [?_assertException(error, function_clause, kz_csv:row_to_iolist([]))
    ] ++
        [?_assertEqual(Expected, iolist_to_binary(kz_csv:row_to_iolist(Input)))
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
    [?_assertEqual(<<"A\na1\n42\n">>, kz_csv:json_to_iolist(Records1))
    ,?_assertEqual(<<"field1,field deux\n,QUUX\n,\nr'bla.+\\n',\n">>, kz_csv:json_to_iolist(Records2))
    ,?_assertEqual(<<"account_id,e164,cnam.outbound\n009afc511c97b2ae693c6cc4920988e8,+14157215234,me\n,+14157215235,\n">>, kz_csv:json_to_iolist(Records3))
    ].
