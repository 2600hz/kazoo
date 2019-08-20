%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_csv_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_csv/include/kazoo_csv.hrl").

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
    ?assertEqual({'ok'
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
    ,?_assertEqual({ok, #{<<"account_id">> => <<"6b71cb72c876b5b1396a335f8f8a2594">>
                         ,<<"e164">> => <<"+14157215234">>
                         ,<<"cnam.outbound">> => ?ZILCH
                         ,<<"opaque.field1.nest1">> => <<"val1">>
                         }
                   }
                  ,FAssoc(CSVRow2)
                  )
    ].

verify_mapped_row_test_() ->
    MappedRow1 = #{<<"account_id">> => <<"bla">>
                  ,<<"e164">> => <<"+14157215235">>
                  ,<<"cnam.outbound">> => ?ZILCH
                  ,<<"opaque.field1.nest1">> => ?ZILCH
                  },
    MappedRow2 = #{<<"account_id">> => <<"6b71cb72c876b5b1396a335f8f8a2594">>
                  ,<<"e164">> => <<"+14157215234">>
                  ,<<"cnam.outbound">> => ?ZILCH
                  ,<<"opaque.field1.nest1">> => <<"val1">>
                  },
    [?_assertEqual([<<"account_id">>]
                  ,kz_csv:verify_mapped_row(fun verify_account_id_only/2, MappedRow1))
    ,?_assertEqual([]
                  ,kz_csv:verify_mapped_row(fun verify_account_id_only/2, MappedRow2))
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
    ,?_assertEqual('eof', kz_csv:take_row(CSV6))
    ,?_assertEqual('eof', kz_csv:take_row(CSV7))
    ,?_assertEqual({[?ZILCH,?ZILCH], <<>>}, kz_csv:take_row(<<",">>))
    ,?_assertEqual({[<<"1">>,<<"B">>], <<>>}, kz_csv:take_row(<<"1,B">>))
    ,?_assertEqual({[<<"a">>, ?ZILCH], <<>>}, kz_csv:take_row(<<"a,\n">>))
    ,?_assertEqual({[?ZILCH, <<"a">>], <<>>}, kz_csv:take_row(<<",a\n">>))
    ,?_assertEqual({[<<"a">>, ?ZILCH], <<"\n">>}, kz_csv:take_row(<<"a,\n\n">>))
    ,?_assertEqual({[?ZILCH, <<>>], <<>>}, kz_csv:take_row(<<",\"\"\n">>))
    ,?_assertEqual({[?ZILCH, <<"bla">>], <<>>}, kz_csv:take_row(<<",\"bla\"\n">>))
    ,?_assertEqual({[?ZILCH,?ZILCH,?ZILCH,?ZILCH], <<>>}, kz_csv:take_row(<<",,,">>))
    ,?_assertEqual({[<<"a">>,?ZILCH], <<>>}, kz_csv:take_row(<<"'a',">>))
    ,?_assertEqual({[?ZILCH,<<"A">>,?ZILCH,<<"B">>,<<"1, 3">>,?ZILCH,<<>>,?ZILCH], <<>>}
                  ,kz_csv:take_row(<<",A,,B,'1, 3',,'',\n">>)
                  )
    ,?_assertEqual({[<<"1">>, <<"{\"type\": \"Point\", \"coordinates\": [102.0, 0.5]}">>], <<>>}
                  ,kz_csv:take_row(<<"1,'{\"type\": \"Point\", \"coordinates\": [102.0, 0.5]}'\n">>)
                  )
    ,?_assertEqual({[<<"1">>, <<"ha \"ha\" ha">>], <<>>}
                  ,kz_csv:take_row(<<"1,'ha \"ha\" ha'\n">>)
                  )
    ,?_assertEqual({[<<"1">>, <<"ha 'ha' ha">>], <<>>}
                  ,kz_csv:take_row(<<"1,'ha ''ha'' ha'\n">>)
                  )
    ].

take_mapped_row_test_() ->
    [F] = [<<"F1">>],
    CSV1 = <<"a\r\nb\nc\nd\n\re\r\r">>,
    CSV2 = <<"b\nc\nd\n\re\r\r">>,
    CSV3 = <<"c\nd\n\re\r\r">>,
    CSV4 = <<"d\n\re\r\r">>,
    CSV5 = <<"e\r\r">>,
    CSV6 = <<>>,
    CSV7 = <<"\r\r">>,
    [?_assertEqual({#{F => <<"a">>}, CSV2}, kz_csv:take_mapped_row([F],CSV1))
    ,?_assertEqual({#{F => <<"b">>}, CSV3}, kz_csv:take_mapped_row([F],CSV2))
    ,?_assertEqual({#{F => <<"c">>}, CSV4}, kz_csv:take_mapped_row([F],CSV3))
    ,?_assertEqual({#{F => <<"d">>}, CSV5}, kz_csv:take_mapped_row([F],CSV4))
    ,?_assertEqual({#{F => <<"e">>}, CSV6}, kz_csv:take_mapped_row([F],CSV5))
    ,?_assertEqual('eof', kz_csv:take_mapped_row([F],CSV6))
    ,?_assertEqual('eof', kz_csv:take_mapped_row([F],CSV7))
    ,?_assertEqual({#{<<"f1">> => <<"B">>, <<"f2">> => <<"1">>}, <<>>}
                  ,kz_csv:take_mapped_row([<<"f2">>,<<"f1">>], <<"1,B">>)
                  )
    ,?_assertEqual({#{<<"f1">> => <<"a">>, <<"f2">> => ?ZILCH}, <<>>}
                  ,kz_csv:take_mapped_row([<<"f1">>,<<"f2">>], <<"a,\n">>)
                  )
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
    ,?_assertMatch(0, kz_csv:count_rows(<<"a,b,c\n1\n2\n3">>))
    ,?_assertEqual(1, kz_csv:count_rows(<<"a,b,c\n1,2,3">>))
    ,?_assertEqual(3, kz_csv:count_rows(<<"a,b,c\n1,2,3\r\n4,5,6\n7,8,9\n">>))
    ,?_assertEqual(3, kz_csv:count_rows(<<"a,b,c\n1,2,3\r\n4,5,6\n\r7,8,9\r\r">>))
    ].

row_to_iolist_test_() ->
    [?_assertException('error', 'function_clause', kz_csv:row_to_iolist([]))
    ]
        ++ [?_assertEqual(Expected, iolist_to_binary(kz_csv:row_to_iolist(Input)))
            || {Expected, Input} <- [{<<"\"a\",\"b\"\n">>, [<<"a">>, <<"b">>]}
                                    ,{<<"\"a\",,\"b\"\n">>, [<<"a">>, ?ZILCH, <<"b">>]}
                                    ,{<<",,\"b\"\n">>, [?ZILCH, ?ZILCH, <<"b">>]}
                                    ,{<<"\"a\",\"b\",\n">>, [<<"a">>, <<"b">>, ?ZILCH]}
                                    ,{<<"\"a\",\"b\",,,\"c\"\n">>, [<<"a">>, <<"b">>, ?ZILCH, ?ZILCH, <<"c">>]}
                                    ]
           ].

mapped_row_to_iolist_test_() ->
    H = [<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>],
    [?_assertException('error', 'function_clause', kz_csv:mapped_row_to_iolist([], #{}))
    ]
        ++ [?_assertEqual(Expected, iolist_to_binary(Got))
            || {N, Expected, Input} <- mapped_row_data(),
               Header <- [lists:sublist(H, 1, N)],
               Got <- [kz_csv:mapped_row_to_iolist(Header, Input)]
           ].

mapped_row_data() ->
    [{5, <<",,,,\n">>, #{}}
    ,{5, <<"\"a\",\"b\",,,\n">>, #{<<"1">> => <<"a">>, <<"2">> => <<"b">>}}
    ,{2, <<"\"a\",\"b\"\n">>, #{<<"1">> => <<"a">>, <<"2">> => <<"b">>}}
    ,{3, <<"\"a\",,\"b\"\n">>, #{<<"1">> => <<"a">>, <<"2">> => ?ZILCH, <<"3">> => <<"b">>}}
    ,{2, <<"\"a\",\n">>, #{<<"1">> => <<"a">>, <<"2">> => ?ZILCH, <<"3">> => <<"b">>}}
    ,{3, <<",,\"b\"\n">>, #{<<"1">> => ?ZILCH, <<"2">> => ?ZILCH, <<"3">> => <<"b">>}}
    ,{2, <<",\n">>, #{<<"1">> => ?ZILCH, <<"2">> => ?ZILCH, <<"3">> => <<"b">>}}
    ,{3, <<"\"a\",\"b\",\n">>, #{<<"1">> => <<"a">>, <<"2">> => <<"b">>, <<"3">> => ?ZILCH}}
    ,{2, <<"\"a\",\"b\"\n">>, #{<<"1">> => <<"a">>, <<"2">> => <<"b">>, <<"3">> => ?ZILCH}}
    ,{5, <<"\"a\",\"b\",,,\"c\"\n">>, #{<<"1">> => <<"a">>
                                       ,<<"2">> => <<"b">>
                                       ,<<"3">> => ?ZILCH
                                       ,<<"4">> => ?ZILCH
                                       ,<<"5">> => <<"c">>
                                       }}
    ,{4, <<"\"a\",\"b\",,\n">>, #{<<"1">> => <<"a">>
                                 ,<<"2">> => <<"b">>
                                 ,<<"3">> => ?ZILCH
                                 ,<<"4">> => ?ZILCH
                                 ,<<"5">> => <<"c">>
                                 }}
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
    ,?_assertEqual(<<"field1,field deux\n,\"QUUX\"\n,\n\"r'bla.+\\n'\",\n">>
                  ,kz_csv:json_to_iolist(Records2, [<<"field1">>,<<"field deux">>])
                  )
    ,?_assertEqual(<<"account_id,e164,cnam.outbound\n\"009afc511c97b2ae693c6cc4920988e8\",\"+14157215234\",\"me\"\n,\"+14157215235\",\n">>, kz_csv:json_to_iolist(Records3))
    ].

split_test_() ->
    [?_assertEqual([<<"0.1651">>, <<"ZAMBIA, MOBILE">>, <<"ZAMBIA, MOBILE-26094">>, <<"ZAMBIA, MOBILE">>, <<"26094">>, <<"0">>]
                  ,kz_csv:split_row(<<"\"0.1651\",\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",\"0\"">>)
                  )
    ,?_assertEqual([<<"0.1651">>, <<"ZAMBIA, MOBILE">>, <<"ZAMBIA, MOBILE-26094">>, <<"ZAMBIA, MOBILE">>, <<"26094">>, <<"0">>]
                  ,kz_csv:split_row(<<"\"0.1651\",\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",0">>)
                  )
    ,?_assertEqual([<<"0.1651">>, <<"ZAMBIA, MOBILE">>, <<"ZAMBIA, MOBILE-26094">>, <<"ZAMBIA, MOBILE">>, <<"26094">>, <<"0">>]
                  ,kz_csv:split_row(<<"0.1651,\"ZAMBIA, MOBILE\",\"ZAMBIA, MOBILE-26094\",\"ZAMBIA, MOBILE\",\"26094\",\"0\"">>)
                  )
    ,?_assertEqual([?ZILCH, ?ZILCH], kz_csv:split_row(<<",">>))
    ,?_assertEqual([<<"test">>,?ZILCH], kz_csv:split_row(<<"test,">>))
    ,?_assertEqual([<<"test">>,?ZILCH,?ZILCH], kz_csv:split_row(<<"test,,">>))
    ,?_assertEqual([<<"test">>,?ZILCH,<<"foo bar">>], kz_csv:split_row(<<"test,,foo bar">>))
    ,?_assertEqual([?ZILCH,<<"test">>,<<>>,<<"foo bar">>], kz_csv:split_row(<<",test,'',foo bar">>))
    ,?_assertEqual([?ZILCH,<<"test">>,<<>>,<<"foo bar">>], kz_csv:split_row(<<",test,\"\",foo bar">>))
    ].

files_test_() ->
    %% Attribution: https://github.com/maxogden/csv-spectrum/tree/master/csvs
    filelib:fold_files("test/", "\\.csv\$", 'false', fun gen_file_tests/2, []).

gen_file_tests(File, Tests) ->
    {'ok', CSV} = file:read_file(File),
    ?debugFmt("~s CSV: ~p~n", [File, CSV]),
    [{File, ?_assert(0 < kz_csv:count_rows(CSV))} | Tests].

variable_json_test() ->
    JSONs = [<<"{\"a\":1}">>
            ,<<"{\"a\":2, \"b\":3}">>
            ,<<"{\"b\":3, \"a\":4}">>
            ,<<"{\"c\":3, \"a\":4}">>
            ],
    JObjs = [kz_json:decode(JSON) || JSON <- JSONs],
    {File, CellOrdering} = kz_csv:jobjs_to_file(JObjs),
    {File, CellOrdering} = kz_csv:write_header_to_file({File, CellOrdering}),

    ?assert(filelib:is_regular(File)),
    ?assertEqual([[<<"a">>], [<<"b">>], [<<"c">>]], CellOrdering),

    {'ok', CSV} = file:read_file(File),
    Expected = <<"\"a\",\"b\",\"c\"\n\"1\"\n\"2\",\"3\"\n\"4\",\"3\"\n\"4\",\"\",\"3\"\n">>,

    ?assertEqual(Expected, CSV),

    kz_util:delete_file(File).

comma_list_json_test() ->
    JSONs = [<<"{\"a\":[\"x\",\"y\"]}">>
            ,<<"{\"a\":[],\"b\":[\"x\",\"y\",\"z\"]}">>
            ,<<"{\"a\":[{\"x\":1},{\"y\":2}]">>
            ],
    JObjs = [kz_json:decode(JSON) || JSON <- JSONs],
    {File, CellOrdering} = kz_csv:jobjs_to_file(JObjs),
    {File, CellOrdering} = kz_csv:write_header_to_file({File, CellOrdering}),

    ?assert(filelib:is_regular(File)),
    ?assertEqual([[<<"a">>], [<<"b">>]], CellOrdering),

    {'ok', CSV} = file:read_file(File),
    Expected = <<"\"a\",\"b\"\n\"x,y\"\n\"\",\"x,y,z\"\n\"\",\"\"\n">>,

    ?assertEqual(Expected, CSV),

    kz_util:delete_file(File).
