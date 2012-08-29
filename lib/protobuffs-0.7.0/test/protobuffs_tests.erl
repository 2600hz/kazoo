%%%-------------------------------------------------------------------
%%% File    : protobuffs_tests.erl
%%% Author  : David AAberg <davabe@hotmail.com>
%%% Description :
%%%
%%% Created :  2 Aug 2010 by David AAberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(protobuffs_tests).

-compile(export_all).

-include("quickcheck_setup.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(DECODE, protobuffs:decode).

-define(ENCODE, protobuffs:encode).

-define(DECODE_PACKED, protobuffs:decode_packed).

-define(ENCODE_PACKED, protobuffs:encode_packed).

asciistring() ->
    list(integer(0,127)).

bytestring() ->
    list(integer(0,255)).

utf8char() ->
    union([integer(0, 36095), integer(57344, 65533),
	   integer(65536, 1114111)]).

utf8string() -> list(utf8char()).

-ifdef(EQC).
eqc_module_test() ->
    ?assertEqual([], eqc:module(?MODULE)).
-endif.

-ifdef(PROPER).
proper_specs_test() ->
    ?assertEqual([],
		 (proper:check_specs(protobuffs, [long_result]))).

proper_module_test() ->
    ?assertEqual([],
		 (proper:module(?MODULE, [long_result]))).
-endif.
%%--------------------------------------------------------------------
%% Encode/Decode int32
%%--------------------------------------------------------------------
prop_int() ->
    ?FORALL({Id, Int}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Int}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Int, int32)), int32))
	    end).

encode_int_test_() ->
    [?_assertMatch(<<8, 150, 1>>, (?ENCODE(1, 150, int32))),
     ?_assertMatch(<<16, 145, 249, 255, 255, 255, 255, 255,
		     255, 255, 1>>,
		   (?ENCODE(2, (-879), int32)))].

decode_int_test_() ->
    [?_assertMatch({{1, 150}, <<>>},
		   (?DECODE(<<8, 150, 1>>, int32))),
     ?_assertMatch({{2, -879}, <<>>},
		   (?DECODE(<<16, 145, 249, 255, 255, 255, 255, 255, 255,
			      255, 1>>,
			    int32)))].

%%--------------------------------------------------------------------
%% Encode/Decode string
%%--------------------------------------------------------------------
prop_string() ->
    ?FORALL({Id, String}, {non_neg_integer(), oneof([asciistring(), utf8string()])},
	    begin
	      {{Id, String}, <<>>} =:=
		(?DECODE((?ENCODE(Id, String, string)), string))
	    end).

encode_string_test_() ->
    [?_assertMatch(<<18, 7, 116, 101, 115, 116, 105, 110,
		     103>>,
		   (?ENCODE(2, "testing", string)))].

decode_string_test_() ->
    [?_assertMatch({{2, "testing"}, <<>>},
		   (?DECODE(<<18, 7, 116, 101, 115, 116, 105, 110, 103>>,
			    string)))].

%%--------------------------------------------------------------------
%% Encode/Decode bool
%%--------------------------------------------------------------------
prop_bool() ->
    ?FORALL({Id, Bool},
	    {non_neg_integer(), oneof([boolean(), 0, 1])},
	    begin
	      Fun = fun (B) when B =:= 1; B =:= true -> true;
			(B) when B =:= 0; B =:= false -> false
		    end,
	      {{Id, Fun(Bool)}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Bool, bool)), bool))
	    end).

enclode_bool_test_() ->
    [?_assertMatch(<<8, 1>>, (?ENCODE(1, true, bool))),
     ?_assertMatch(<<8, 0>>, (?ENCODE(1, false, bool))),
     ?_assertMatch(<<40, 1>>, (?ENCODE(5, 1, bool))),
     ?_assertMatch(<<40, 0>>, (?ENCODE(5, 0, bool)))].

decode_bool_test_() ->
    [?_assertMatch({{1, true}, <<>>},
		   (?DECODE(<<8, 1>>, bool))),
     ?_assertMatch({{1, false}, <<>>},
		   (?DECODE(<<8, 0>>, bool)))].

%%--------------------------------------------------------------------
%% Encode/Decode enum
%%--------------------------------------------------------------------
prop_enum() ->
    ?FORALL({Id, Enum}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Enum}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Enum, enum)), enum))
	    end).

encode_enum_test_() ->
    [?_assertMatch(<<8, 5>>, (?ENCODE(1, 5, enum)))].

decode_enum_test_() ->
    [?_assertMatch({{1, 5}, <<>>},
		   (?DECODE(<<8, 5>>, enum)))].

%%--------------------------------------------------------------------
%% Encode/Decode uint32
%%--------------------------------------------------------------------
prop_uint32() ->
    ?FORALL({Id, Uint32},
	    {non_neg_integer(), non_neg_integer()},
	    begin
	      {{Id, Uint32}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Uint32, uint32)), uint32))
	    end).

encode_uint32_test_() ->
    [?_assertMatch(<<32, 169, 18>>,
		   (?ENCODE(4, 2345, uint32)))].

decode_uint32_test_() ->
    [?_assertMatch({{4, 2345}, <<>>},
		   (?DECODE(<<32, 169, 18>>, uint32)))].

%%--------------------------------------------------------------------
%% Encode/Decode sint32
%%--------------------------------------------------------------------
prop_sint32() ->
    ?FORALL({Id, Sint32}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Sint32}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Sint32, sint32)), sint32))
	    end).

encode_sint32_test_() ->
    [?_assertMatch(<<24, 137, 5>>,
		   (?ENCODE(3, (-325), sint32))),
     ?_assertMatch(<<32, 212, 3>>,
		   (?ENCODE(4, 234, sint32)))].

decode_sint32_test_() ->
    [?_assertMatch({{3, -325}, <<>>},
		   (?DECODE(<<24, 137, 5>>, sint32))),
     ?_assertMatch({{4, 234}, <<>>},
		   (?DECODE(<<32, 212, 3>>, sint32)))].

%%--------------------------------------------------------------------
%% Encode/Decode int64
%%--------------------------------------------------------------------
prop_int64() ->
    ?FORALL({Id, Int64}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Int64}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Int64, int64)), int64))
	    end).

encode_int64_test_() ->
    [?_assertMatch(<<16, 192, 212, 5>>,
		   (?ENCODE(2, 92736, int64)))].

decode_int64_test_() ->
    [?_assertMatch({{2, 92736}, <<>>},
		   (?DECODE(<<16, 192, 212, 5>>, int64)))].

%%--------------------------------------------------------------------
%% Encode/Decode uint64
%%--------------------------------------------------------------------
prop_uint64() ->
    ?FORALL({Id, Uint64},
	    {non_neg_integer(), non_neg_integer()},
	    begin
	      {{Id, Uint64}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Uint64, uint64)), uint64))
	    end).

encode_uint64_test_() ->
    [?_assertMatch(<<40, 182, 141, 51>>,
		   (?ENCODE(5, 837302, uint64)))].

decode_uint64_test_() ->
    [?_assertMatch({{5, 837302}, <<>>},
		   (?DECODE(<<40, 182, 141, 51>>, uint64)))].

%%--------------------------------------------------------------------
%% Encode/Decode sint64
%%--------------------------------------------------------------------
prop_sint64() ->
    ?FORALL({Id, Sint64}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Sint64}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Sint64, sint64)), sint64))
	    end).

encode_sint64_test_() ->
    [?_assertMatch(<<16, 189, 3>>,
		   (?ENCODE(2, (-223), sint64))),
     ?_assertMatch(<<32, 128, 8>>,
		   (?ENCODE(4, 512, sint64)))].

decode_sint64_test_() ->
    [?_assertMatch({{2, -223}, <<>>},
		   (?DECODE(<<16, 189, 3>>, sint64))),
     ?_assertMatch({{4, 512}, <<>>},
		   (?DECODE(<<32, 128, 8>>, sint64)))].

%%--------------------------------------------------------------------
%% Encode/Decode fixed32
%%--------------------------------------------------------------------
prop_fixed32() ->
    ?FORALL({Id, Fixed32},
	    {non_neg_integer(), non_neg_integer()},
	    begin
	      {{Id, Fixed32}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Fixed32, fixed32)), fixed32))
	    end).

encode_fixed32_test_() ->
    [?_assertMatch(<<21, 172, 20, 0, 0>>,
		   (?ENCODE(2, 5292, fixed32)))].

decode_fixed32_test_() ->
    [?_assertMatch({{2, 5292}, <<>>},
		   (?DECODE(<<21, 172, 20, 0, 0>>, fixed32)))].

%%--------------------------------------------------------------------
%% Encode/Decode sfixed32
%%--------------------------------------------------------------------
prop_sfixed32() ->
    ?FORALL({Id, Sfixed32}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Sfixed32}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Sfixed32, sfixed32)), sfixed32))
	    end).

encode_sfixed32_test_() ->
    [?_assertMatch(<<61, 182, 32, 0, 0>>,
		   (?ENCODE(7, 8374, sfixed32)))].

decode_sfixed32_test_() ->
    [?_assertMatch({{7, 8374}, <<>>},
		   (?DECODE(<<61, 182, 32, 0, 0>>, sfixed32)))].

%%--------------------------------------------------------------------
%% Encode/Decode fixed64
%%--------------------------------------------------------------------
prop_fixed64() ->
    ?FORALL({Id, Fixed64},
	    {non_neg_integer(), non_neg_integer()},
	    begin
	      {{Id, Fixed64}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Fixed64, fixed64)), fixed64))
	    end).

encode_fixed64_test_() ->
    [?_assertMatch(<<161, 18, 83, 12, 0, 0, 0, 0, 0, 0>>,
		   (?ENCODE(292, 3155, fixed64)))].

decode_fixed64_test_() ->
    [?_assertMatch({{292, 3155}, <<>>},
		   (?DECODE(<<161, 18, 83, 12, 0, 0, 0, 0, 0, 0>>,
			    fixed64)))].

%%--------------------------------------------------------------------
%% Encode/Decode sfixed64
%%--------------------------------------------------------------------
prop_sfixed64() ->
    ?FORALL({Id, Sfixed64}, {non_neg_integer(), integer()},
	    begin
	      {{Id, Sfixed64}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Sfixed64, sfixed64)), sfixed64))
	    end).

encode_sfixed64_test_() ->
    [?_assertMatch(<<185, 1, 236, 1, 0, 0, 0, 0, 0, 0>>,
		   (?ENCODE(23, 492, sfixed64)))].

decode_sfixed64_test_() ->
    [?_assertMatch({{23, 492}, <<>>},
		   (?DECODE(<<185, 1, 236, 1, 0, 0, 0, 0, 0, 0>>,
			    sfixed64)))].

%%--------------------------------------------------------------------
%% Encode/Decode bytes
%%--------------------------------------------------------------------
prop_bytes() ->
    ?FORALL({Id, Bytes},
	    {non_neg_integer(), oneof([bytestring(), binary()])},
	    begin
	      Fun = fun (B) when is_list(B) ->
			    list_to_binary(B);
			(B) -> B
		    end,
	      {{Id, Fun(Bytes)}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Bytes, bytes)), bytes))
	    end).

encode_bytes_test_() ->
    [?_assertMatch(<<26, 3, 8, 150, 1>>,
		   (?ENCODE(3, <<8, 150, 1>>, bytes))),
     ?_assertMatch(<<34, 4, 84, 101, 115, 116>>,
		   (?ENCODE(4, "Test", bytes))),
     ?_assertMatch(<<34, 0>>, (?ENCODE(4, "", bytes))),
     ?_assertError(badarg, ?ENCODE(4, [256], bytes))].

decode_bytes_test_() ->
    [?_assertMatch({{3, <<8, 150, 1>>}, <<>>},
		   (?DECODE(<<26, 3, 8, 150, 1>>, bytes))),
     ?_assertMatch({{4, <<"Test">>}, <<>>},
		   (?DECODE(<<34, 4, 84, 101, 115, 116>>, bytes))),
     ?_assertMatch({{4, <<>>}, <<>>},
		   (?DECODE(<<34, 0>>, bytes))),
     ?_assertMatch({{4, <<196, 128>>}, <<>>},
		   (?DECODE(<<34, 2, 196, 128>>, bytes)))].

%%--------------------------------------------------------------------
%% Encode/Decode float
%%--------------------------------------------------------------------
prop_float() ->
    ?FORALL({Id, Float},
	    {non_neg_integer(),
	     oneof([float(), integer(), nan, infinity,
		    '-infinity'])},
	    begin
	      Fun = fun (F) when is_float(F) ->
			    <<Return:32/little-float>> = <<F:32/little-float>>,
			    Return;
			(F) when is_integer(F) -> F + 0.0;
			(F) -> F
		    end,
	      {{Id, Fun(Float)}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Float, float)), float))
	    end).

encode_float_test_() ->
    [?_assertMatch(<<165, 5, 0, 0, 106, 67>>,
		   (?ENCODE(84, 234, float))),
     ?_assertMatch(<<29, 123, 148, 105, 68>>,
		   (?ENCODE(3, 9.3432e+2, float))),
     ?_assertMatch(<<45, 0, 0, 192, 255>>,
		   (?ENCODE(5, nan, float))),
     ?_assertMatch(<<69, 0, 0, 128, 127>>,
		   (?ENCODE(8, infinity, float))),
     ?_assertMatch(<<29, 0, 0, 128, 255>>,
		   (?ENCODE(3, '-infinity', float)))].

decode_float_test_() ->
    [?_assertMatch({{84, 2.34e+2}, <<>>},
		   (?DECODE(<<165, 5, 0, 0, 106, 67>>, float))),
     ?_assertMatch({{5, nan}, <<>>},
		   (?DECODE(<<45, 0, 0, 192, 255>>, float))),
     ?_assertMatch({{8, infinity}, <<>>},
		   (?DECODE(<<69, 0, 0, 128, 127>>, float))),
     ?_assertMatch({{3, '-infinity'}, <<>>},
		   (?DECODE(<<29, 0, 0, 128, 255>>, float)))].

%%--------------------------------------------------------------------
%% Encode/Decode double
%%--------------------------------------------------------------------
prop_double() ->
    ?FORALL({Id, Double},
	    {non_neg_integer(),
	     oneof([float(), integer(), nan, infinity,
		    '-infinity'])},
	    begin
	      Fun = fun (D) when is_integer(D) -> D + 0.0;
			(D) -> D
		    end,
	      {{Id, Fun(Double)}, <<>>} =:=
		(?DECODE((?ENCODE(Id, Double, double)), double))
	    end).

encode_double_test_() ->
    [?_assertMatch(<<241, 1, 0, 0, 0, 0, 0, 44, 174, 64>>,
		   (?ENCODE(30, 3862, double))),
     ?_assertMatch(<<17, 0, 0, 0, 0, 0, 40, 138, 64>>,
		   (?ENCODE(2, 8.37e+2, double))),
     ?_assertMatch(<<41, 0, 0, 0, 0, 0, 0, 248, 255>>,
		   (?ENCODE(5, nan, double))),
     ?_assertMatch(<<65, 0, 0, 0, 0, 0, 0, 240, 127>>,
		   (?ENCODE(8, infinity, double))),
     ?_assertMatch(<<25, 0, 0, 0, 0, 0, 0, 240, 255>>,
		   (?ENCODE(3, '-infinity', double)))].

decode_double_test_() ->
    [?_assertMatch({{2, 8.37e+2}, <<>>},
		   (?DECODE(<<17, 0, 0, 0, 0, 0, 40, 138, 64>>, double))),
     ?_assertMatch({{5, nan}, <<>>},
		   (?DECODE(<<41, 0, 0, 0, 0, 0, 0, 248, 255>>, double))),
     ?_assertMatch({{8, infinity}, <<>>},
		   (?DECODE(<<65, 0, 0, 0, 0, 0, 0, 240, 127>>, double))),
     ?_assertMatch({{3, '-infinity'}, <<>>},
		   (?DECODE(<<25, 0, 0, 0, 0, 0, 0, 240, 255>>, double)))].

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated int32
%%--------------------------------------------------------------------
prop_packed_int32() ->
    ?FORALL({Id, Int32s},
	    {non_neg_integer(), non_empty(list(integer()))},
	    begin
	      {{Id, Int32s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Int32s, int32)),
				int32))
	    end).

encode_packed_int32_test_() ->
    [?_assertMatch(<<34, 6, 3, 142, 2, 158, 167, 5>>,
		   (?ENCODE_PACKED(4, [3, 270, 86942], int32))),
     ?_assertMatch(<<>>, (?ENCODE_PACKED(4, [], int32)))].

decode_packed_int32_test_() ->
    [?_assertMatch({{4, [3, 270, 86942]}, <<>>},
		   (?DECODE_PACKED(<<34, 6, 3, 142, 2, 158, 167, 5>>,
				   int32)))].

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated bool
%%--------------------------------------------------------------------
prop_packed_bool() ->
    ?FORALL({Id, Bools},
	    {non_neg_integer(),
	     non_empty(list(oneof([boolean(), 0, 1])))},
	    begin
	      Fun = fun (1) -> true;
			(0) -> false;
			(B) -> B
		    end,
	      {{Id, lists:map(Fun, Bools)}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Bools, bool)),
				bool))
	    end).

prop_packed_enum() ->
    ?FORALL({Id, Enums},
	    {non_neg_integer(), non_empty(list(integer()))},
	    begin
	      {{Id, Enums}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Enums, enum)),
				enum))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated enum
%%--------------------------------------------------------------------
encode_packed_enum_test_() ->
    [?_assertMatch(<<2, 2, 0, 0>>,
		   (?ENCODE_PACKED(0, [0, 0], enum)))].

decode_packed_enum_test_() ->
    [?_assertMatch({{Id, [0, 0]}, <<>>},
		   (?DECODE_PACKED(<<2, 2, 0, 0>>, enum)))].

prop_packed_uint32() ->
    ?FORALL({Id, Uint32s},
	    {non_neg_integer(), non_empty(list(non_neg_integer()))},
	    begin
	      {{Id, Uint32s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Uint32s, uint32)),
				uint32))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated sint32
%%--------------------------------------------------------------------
prop_packed_sint32() ->
    ?FORALL({Id, Sint32s},
	    {non_neg_integer(), non_empty(list(integer()))},
	    begin
	      {{Id, Sint32s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Sint32s, sint32)),
				sint32))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated int64
%%--------------------------------------------------------------------
prop_packed_int64() ->
    ?FORALL({Id, Int64s},
	    {non_neg_integer(), non_empty(list(integer()))},
	    begin
	      {{Id, Int64s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Int64s, int64)),
				int64))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated uint64
%%--------------------------------------------------------------------
prop_packed_uint64() ->
    ?FORALL({Id, Uint64s},
	    {non_neg_integer(), non_empty(list(non_neg_integer()))},
	    begin
	      {{Id, Uint64s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Uint64s, uint64)),
				uint64))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated sint64
%%--------------------------------------------------------------------
prop_packed_sint64() ->
    ?FORALL({Id, Sint64s},
	    {non_neg_integer(), non_empty(list(integer()))},
	    begin
	      {{Id, Sint64s}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Sint64s, sint64)),
				sint64))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated float
%%--------------------------------------------------------------------
prop_packed_float() ->
    ?FORALL({Id, Floats},
	    {non_neg_integer(),
	     non_empty(list(oneof([float(), integer()])))},
	    begin
	      Fun = fun (F) when is_float(F) ->
			    <<Return:32/little-float>> = <<F:32/little-float>>,
			    Return;
			(F) when is_integer(F) -> F + 0.0;
			(F) -> F
		    end,
	      {{Id, lists:map(Fun, Floats)}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Floats, float)),
				float))
	    end).

%%--------------------------------------------------------------------
%% Encode/Decode packed repeated double
%%--------------------------------------------------------------------
prop_packed_double() ->
    ?FORALL({Id, Doubles},
	    {non_neg_integer(),
	     non_empty(list(oneof([float(), integer()])))},
	    begin
	      Fun = fun (D) when is_integer(D) -> D + 0.0;
			(D) -> D
		    end,
	      {{Id, lists:map(Fun, Doubles)}, <<>>} =:=
		(?DECODE_PACKED((?ENCODE_PACKED(Id, Doubles, double)),
				double))
	    end).

%%--------------------------------------------------------------------
%% Skip fields in stream
%%--------------------------------------------------------------------
skip_next_field_test_() ->
    [
     %% Skip a varint with no remainder
     ?_assertEqual({ok,<<>>}, protobuffs:skip_next_field(<<32,0>>)),
     %% Skip a varint
     ?_assertEqual({ok,<<8,1>>}, protobuffs:skip_next_field(<<32,0,8,1>>)),
     %% Skip a string
     ?_assertEqual({ok,<<8,1>>}, protobuffs:skip_next_field(<<18,3,102,111,111,8,1>>)),
     %% Skip a 32-bit
     ?_assertEqual({ok,<<8,1>>}, protobuffs:skip_next_field(<<21,32,0,0,0,8,1>>)),
     %% Skip a 64-bit
     ?_assertEqual({ok,<<8,1>>}, protobuffs:skip_next_field(<<17,32,0,0,0,0,0,0,0,8,1>>))
    ].
