%% Basic JSON (RFC-4627) codec tests
%%---------------------------------------------------------------------------
%% Copyright (c) 2007-2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% Copyright (c) 2007-2010 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(test_rfc4627).
-include("rfc4627.hrl").

-export([test_all/0]).

-record(address, {number, street, town, country = <<"England">>}).

test_all() ->
    passed = test_codec(),
    passed = test_records(),
    passed = test_dict(),
    passed = test_unicode(),
    passed = test_equiv(),
    passed = test_eof_detection(),
    passed = test_exclude(),
    passed.

test_codec() ->
    Cases = [
	     {1, "1"},
	     {1, "1,", false},
	     {1.0, "1.0"},
	     {12.3, "1.23e1"},
	     {<<"hi">>, "\"hi\""},
	     {<<>>, "\"\""},
	     {<<"\n">>, "\"\\n\""},
	     {<<"\t">>, "\"\\t\""},
	     {<<"\\">>, "\"\\\\\""},
	     {<<195,133>>, [34,0,16#c5,0,34,0]},
	     {<<127>>, [34,0,16#7f,0,34,0]},
	     {<<194,128>>, [34,0,16#80,0,34,0]},
	     {<<30>>, "\"\\u001e\""},
	     {[1, 2], "[1, 2]"},
	     {[], "[]"},
	     {{obj, [{"a", 1}, {"b", 2}]}, "{\"a\": 1, \"b\": 2}"},
	     {{obj, []}, "{}"},
	     {{obj, []}, "{    \n\n  }"},
	     {true, "true"},
	     {null, "null"},
	     {false, "false"}
	     ],
    lists:foreach(fun test_codec/1, Cases),
    passed.

test_codec({Erl, Json}) ->
    test_codec({Erl, Json, true});
test_codec({Erl, Json, EofExpected}) ->
    %% We can test Erl -> Json -> Erl, but not Json -> Erl ->
    %% Json. However, we can test Json -> Erl.
    {ok, Erl, []} = rfc4627:decode(rfc4627:encode(Erl)),
    {ok, Erl, Rest} = rfc4627:decode(Json),
    {at_eof, EofExpected} = {at_eof, (Rest == [])},
    passed.

%% UTF tests.
test_unicode() ->
    passed = test_unicode_encodings(),
    passed = test_unicode_json(),
    passed.

test_unicode_encodings() ->
    ZWaterBass = [16#0000007A, 16#00006C34, 16#0001D11E],
    U32B = [0, 0, 16#00, 16#7A, 0, 0, 16#6C, 16#34, 0, 1, 16#D1, 16#1E]
	= rfc4627:unicode_encode({'utf-32be', ZWaterBass}),
    U32L = [16#7A, 0, 0, 0, 16#34, 16#6C, 0, 0, 16#1E, 16#D1, 1, 0]
	= rfc4627:unicode_encode({'utf-32le', ZWaterBass}),
    U32B_BOM = [0, 0, 16#FE, 16#FF, 0, 0, 16#00, 16#7A, 0, 0, 16#6C, 16#34, 0, 1, 16#D1, 16#1E]
	= rfc4627:unicode_encode({'utf-32', ZWaterBass}),
    U16L = [16#7A, 16#00, 16#34, 16#6C, 16#34, 16#D8, 16#1E, 16#DD]
	= rfc4627:unicode_encode({'utf-16le', ZWaterBass}),
    U16B = [16#00, 16#7A, 16#6C, 16#34, 16#D8, 16#34, 16#DD, 16#1E]
	= rfc4627:unicode_encode({'utf-16be', ZWaterBass}),
    U16B_BOM = [16#FE, 16#FF, 16#00, 16#7A, 16#6C, 16#34, 16#D8, 16#34, 16#DD, 16#1E]
	= rfc4627:unicode_encode({'utf-16', ZWaterBass}),
    U8 = [16#7A, 16#E6,16#B0,16#B4, 16#F0,16#9D,16#84,16#9E]
	= rfc4627:unicode_encode({'utf-8', ZWaterBass}),
    {'utf-32be', ZWaterBass} = rfc4627:unicode_decode(U32B),
    {'utf-32le', ZWaterBass} = rfc4627:unicode_decode(U32L),
    {'utf-32', ZWaterBass} = rfc4627:unicode_decode(U32B_BOM),
    {'utf-16be', ZWaterBass} = rfc4627:unicode_decode(U16B),
    {'utf-16le', ZWaterBass} = rfc4627:unicode_decode(U16L),
    {'utf-16', ZWaterBass} = rfc4627:unicode_decode(U16B_BOM),
    {'utf-8', ZWaterBass} = rfc4627:unicode_decode(U8),
    {'utf-8', ZWaterBass} = rfc4627:unicode_decode([16#EF,16#BB,16#BF]++U8),
    passed.

test_unicode_json() ->
    U16B = [16#00, 16#7A, 16#6C, 16#34, 16#D8, 16#34, 16#DD, 16#1E],
    U16BQuote = [0, 16#22],
    U32L = [16#7A, 0, 0, 0, 16#34, 16#6C, 0, 0, 16#1E, 16#D1, 1, 0],
    U32LQuote = [16#22, 0, 0, 0],
    U8 = [16#7A, 16#E6,16#B0,16#B4, 16#F0,16#9D,16#84,16#9E],
    U8Bin = list_to_binary(U8),
    {utf8_decode, {ok, U8Bin, ""}} =
	{utf8_decode, rfc4627:decode("\"" ++ U8 ++ "\"")},
    {utf16_decode, {ok, U8Bin, ""}} =
	{utf16_decode, rfc4627:decode(U16BQuote ++ U16B ++ U16BQuote)},
    {utf32_decode, {ok, U8Bin, ""}} =
	{utf32_decode, rfc4627:decode(U32LQuote ++ U32L ++ U32LQuote)},
    {u_escape_decode, {ok, U8Bin, ""}} =
	{u_escape_decode, rfc4627:decode("\"\\u007a\\u6c34\\ud834\\udd1e\"")},
    {u_escape_decode, {ok, U8Bin, ""}} =
	{u_escape_decode, rfc4627:decode("\"z\\u6C34\\uD834\\uDD1E\"")},
    UnicodeKeyed = {obj, [{[16#C5], list_to_binary(xmerl_ucs:to_utf8([16#C5]))}]},
    {utf8_roundtrip, {ok, UnicodeKeyed, ""}} =
	{utf8_roundtrip, rfc4627:decode(rfc4627:encode(UnicodeKeyed))},
    {utf16_roundtrip, {ok, UnicodeKeyed, ""}} =
	{utf16_roundtrip, rfc4627:decode(rfc4627:unicode_encode({'utf-16le', rfc4627:encode_noauto(UnicodeKeyed)}))},
    {utf32_roundtrip, {ok, UnicodeKeyed, ""}} =
	{utf32_roundtrip, rfc4627:decode(rfc4627:unicode_encode({'utf-32be', rfc4627:encode_noauto(UnicodeKeyed)}))},
    passed.

test_records() ->
    A = #address{number = 6, street = <<"Rufus Street">>, town = <<"London">>},
    AEnc = {obj, [{"number", 6},
		  {"street", <<"Rufus Street">>},
		  {"town", <<"London">>},
		  {"country", <<"England">>}]} = ?RFC4627_FROM_RECORD(address, A),
    A = ?RFC4627_TO_RECORD(address, {obj, [{"number", 6},
					   {"street", <<"Rufus Street">>},
					   {"town", <<"London">>}]}),
    {ok, AEnc, []} = rfc4627:decode(rfc4627:encode(AEnc)),
    passed.

test_dict() ->
    Dict = dict:append("c", 2,
                       dict:append("c", 1,
                                   dict:store("b", <<"hello">>,
                                              dict:store("a", 123, dict:new())))),
    {ok, Obj, ""} = rfc4627:decode(rfc4627:encode(Dict)),
    {ok, 123} = rfc4627:get_field(Obj, "a"),
    {ok, <<"hello">>} = rfc4627:get_field(Obj, "b"),
    {ok, [1, 2]} = rfc4627:get_field(Obj, "c"),
    passed.

test_exclude() ->
    Dict = dict:store("c", 2,
                        dict:store("b", <<"hello">>,
                            dict:store("a", 123, dict:new()))),
    {ok, Obj, ""} = rfc4627:decode(rfc4627:encode(Dict)),
    Obj2 = rfc4627:exclude_field(Obj, "a"),
    true = rfc4627:equiv({obj, [{"c", 2}, {"b", <<"hello">>}]}, Obj2),
    Obj3 = rfc4627:exclude_field(Obj2, "b"),
    true = rfc4627:equiv({obj, [{"c", 2}]}, Obj3),
    Obj4 = rfc4627:exclude_field(Obj3, "x"),
    true = rfc4627:equiv({obj, [{"c", 2}]}, Obj4),
    Obj5 = rfc4627:exclude_field(Obj3, "c"),
    true = rfc4627:equiv({obj, []}, Obj5),
    passed.

test_equiv() ->
    true = rfc4627:equiv([1, 2], [1, 2]),
    false = rfc4627:equiv([1, 2], [2, 1]),
    false = rfc4627:equiv([1, 2], [1]),
    false = rfc4627:equiv([1], [1, 2]),
    true = rfc4627:equiv([], []),
    false = rfc4627:equiv([], [1]),
    false = rfc4627:equiv([1], []),

    true = rfc4627:equiv({obj, [{"a", true}, {"b", 123}]}, {obj, [{"a", true}, {"b", 123}]}),
    true = rfc4627:equiv({obj, [{"a", true}, {"b", 123}]}, {obj, [{"b", 123}, {"a", true}]}),
    false = rfc4627:equiv({obj, [{"a", true}, {"b", 124}]}, {obj, [{"a", true}, {"b", 123}]}),
    false = rfc4627:equiv({obj, [{"b", 123}]}, {obj, [{"a", true}, {"b", 123}]}),
    false = rfc4627:equiv({obj, [{"a", true}, {"b", 123}]}, {obj, [{"b", 123}]}),
    true = rfc4627:equiv({obj, []}, {obj, []}),
    false = rfc4627:equiv({obj, []}, {obj, [{"a", true}, {"b", 123}]}),

    true = rfc4627:equiv(<<"ab">>, <<"ab">>),
    false = rfc4627:equiv(<<"ab">>, <<"a">>),
    true = rfc4627:equiv(<<>>, <<>>),

    passed.

test_eof_detection() ->
    {error, unexpected_end_of_input} = rfc4627:decode(""),
    {error, syntax_error} = rfc4627:decode("()"),
    passed.
