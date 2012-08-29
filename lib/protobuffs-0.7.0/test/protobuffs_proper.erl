%%%-------------------------------------------------------------------
%%% File    : protobuffs_eqc.erl
%%% Author  : David AAberg <david_ab@RB-DAVIDAB01>
%%% Description :
%%%
%%% Created :  5 Aug 2010 by David AAberg <david_ab@RB-DAVIDAB01>
%%%-------------------------------------------------------------------
-module(protobuffs_proper).

-include("quickcheck_setup.hrl").

-compile(export_all).

utf8char() ->
    union([integer(0, 36095), integer(57344, 65533),
	   integer(65536, 1114111)]).

utf8string() -> list(utf8char()).

uint32() -> choose(0, 4294967295).

sint32() -> choose(-2147483648, 2147483647).

uint64() -> choose(0, 18446744073709551615).

sint64() ->
    choose(-9223372036854775808, 9223372036854775807).

value() ->
    oneof([{real(), double}, {real(), float}, {nan, float},
	   {infinity, float}, {'-infinity', float}, {nan, double},
	   {infinity, double}, {'-infinity', double},
	   {uint32(), uint32}, {uint64(), uint64},
	   {sint32(), sint32}, {sint64(), sint64},
	   {uint32(), fixed32}, {uint64(), fixed64},
	   {sint32(), sfixed32}, {sint64(), sfixed64},
	   {sint32(), int32}, {sint64(), int64}, {bool(), bool},
	   {sint32(), enum}, {utf8string(), string},
	   {binary(), bytes}]).

compare_messages(ExpectedMsg, Msg) ->
    lists:foldl(fun ({E, D}, Acc) ->
			compare(E, D) andalso Acc
		end,
		true,
		lists:zip(tuple_to_list(ExpectedMsg),
			  tuple_to_list(Msg))).

compare(A, A) -> true;
compare([A], B) -> compare(A, B);
compare(A, [B]) -> compare(A, B);
compare(A, B) when is_tuple(A), is_tuple(B) ->
    compare(tuple_to_list(A), tuple_to_list(B));
compare([A | RA], [B | RB]) ->
    compare(A, B) andalso compare(RA, RB);
compare(A, B) when is_float(A), is_float(B) ->
    <<A32:32/little-float>> = <<A:32/little-float>>,
    <<B32:32/little-float>> = <<B:32/little-float>>,
    if A =:= B -> true;
       A32 =:= B32 -> true;
       true -> false
    end;
compare(A, B) ->
    error_logger:error_msg("~p =/= ~p~n", [A, B]), false.

prop_protobuffs() ->
    ?FORALL({FieldID, {Value, Type}},
	    {?SUCHTHAT(I, (uint32()), (I =< 1073741823)), value()},
	    begin
	      case Type of
		float when is_float(Value) ->
		    Encoded = protobuffs:encode(FieldID, Value, Type),
		    {{FieldID, Float}, <<>>} = protobuffs:decode(Encoded,
								 Type),
		    <<Value32:32/little-float>> = <<Value:32/little-float>>,
		    Float =:= Value32;
		_Else ->
		    Encoded = protobuffs:encode(FieldID, Value, Type),
		    {{FieldID, Value}, <<>>} ==
		      protobuffs:decode(Encoded, Type)
	      end
	    end).

prop_protobuffs_packed() ->
    ?FORALL({FieldID, {Values, Type}},
	    {?SUCHTHAT(I, (uint32()), (I =< 1073741823)),
	     oneof([{non_empty(list(uint32())), uint32},
		    {non_empty(list(uint64())), uint64},
		    {non_empty(list(sint32())), sint32},
		    {non_empty(list(sint64())), sint64},
		    {non_empty(list(sint32())), int32},
		    {non_empty(list(sint64())), int64},
		    {non_empty(list(bool())), bool},
		    {non_empty(list(real())), double},
		    {non_empty(list(real())), float}])},
	    begin
	      case Type of
		float ->
		    Encoded = protobuffs:encode_packed(FieldID, Values,
						       Type),
		    {{FieldID, DecodedValues}, <<>>} =
			protobuffs:decode_packed(Encoded, Type),
		    lists:all(fun ({Expected, Result}) ->
				      <<Expected32:32/little-float>> =
					  <<Expected:32/little-float>>,
				      Expected32 =:= Result
			      end,
			      lists:zip(Values, DecodedValues));
		_Else ->
		    Encoded = protobuffs:encode_packed(FieldID, Values,
						       Type),
		    Decoded = protobuffs:decode_packed(Encoded, Type),
		    {{FieldID, Values}, <<>>} == Decoded
	      end
	    end).

proper_protobuffs_empty() ->
    ?FORALL({Empty},
	    {{empty, default(undefined, real()),
	      default(undefined, real()),
	      default(undefined, sint32()),
	      default(undefined, sint64()),
	      default(undefined, uint32()),
	      default(undefined, uint64()),
	      default(undefined, sint32()),
	      default(undefined, sint64()),
	      default(undefined, uint32()),
	      default(undefined, uint64()),
	      default(undefined, sint32()),
	      default(undefined, sint64()),
	      default(undefined, bool()),
	      default(undefined, utf8string()),
	      default(undefined, binary()),
	      default(undefined, {empty_emptymessage})}},
	    begin
	      Decoded =
		  empty_pb:decode_empty(empty_pb:encode_empty(Empty)),
	      compare_messages(Empty, Decoded)
	    end).

check_with_default(Expected, Result, undefined, Fun) ->
    Fun(Expected, Result);
check_with_default(undefined, Result, Default, Fun) ->
    Fun(Default, Result);
check_with_default(Expected, Result, _Default, Fun) ->
    Fun(Expected, Result).

proper_protobuffs_hasdefault() ->
    ?FORALL(Withdefault,
	    {withdefault, {real(), 1.0}, {real(), 2.0},
	     {sint32(), 1}, {sint64(), 2}, {uint32(), 3},
	     {uint64(), 4}, {sint32(), 5},
	     {default(undefined, sint64()), 6},
	     {default(undefined, uint32()), 7},
	     {default(undefined, uint64()), 8},
	     {default(undefined, sint32()), 9},
	     {default(undefined, sint64()), 10},
	     {default(undefined, bool()), true},
	     {default(undefined, utf8string()), "test"},
	     {default(undefined, utf8string()), ""}},
	    begin
	      FunGetDefault = fun ({undefined, Val}) -> Val;
				  ({Val, _}) -> Val;
				  (Val) -> Val
			      end,
	      FunGetTestMsg = fun ({Val, _}) -> Val;
				  (Val) -> Val
			      end,
	      Expected = list_to_tuple(lists:map(FunGetDefault,
						 tuple_to_list(Withdefault))),
	      TestMsg = list_to_tuple(lists:map(FunGetTestMsg,
						tuple_to_list(Withdefault))),
	      Decoded =
		  hasdefault_pb:decode_withdefault(hasdefault_pb:encode_withdefault(TestMsg)),
	      compare_messages(Expected, Decoded)
	    end).

location() ->
    Str = utf8string(),
    default(undefined, {location, Str, Str}).

proper_protobuffs_simple() ->
    ?FORALL({Person},
	    {{person, utf8string(), utf8string(), utf8string(),
	      sint32(), location()}},
	    begin
	      Decoded =
		  simple_pb:decode_person(simple_pb:encode_person(Person)),
	      compare_messages(Person, Decoded)
	    end).

phone_type() ->
    Int32 = default(undefined, sint32()),
    {person_phonenumber_phonetype, Int32, Int32, Int32}.

phone_number() ->
    {person_phonenumber, utf8string(),
     default(undefined, phone_type())}.

proper_protobuffs_nested1() ->
    ?FORALL(Person,
	    {person, utf8string(), sint32(),
	     default(undefined, utf8string()), list(phone_number())},
	    begin
	      Decoded =
		  nested1_pb:decode_person(nested1_pb:encode_person(Person)),
	      compare_messages(Person, Decoded)
	    end).

innerAA() ->
    {outer_middleaa_inner, sint64(),
     default(undefined, bool())}.

middleAA() ->
    Inner = innerAA(),
    {outer_middleaa, default(undefined, Inner)}.

innerBB() ->
    {outer_middlebb_inner, sint32(),
     default(undefined, bool())}.

middleBB() ->
    Inner = innerBB(),
    {outer_middlebb, default(undefined, Inner)}.

proper_protobuffs_nested2() ->
    ?FORALL({Middle},
	    {{outer, default(undefined, middleAA()),
	      default(undefined, middleBB())}},
	    begin
	      Decoded =
		  nested2_pb:decode_outer(nested2_pb:encode_outer(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

inner() ->
    {outer_middle_inner, default(undefined, bool())}.

other() -> {outer_other, default(undefined, bool())}.

middle() ->
    Inner = inner(),
    Other = other(),
    {outer_middle, Inner, Other}.

proper_protobuffs_nested3() ->
    ?FORALL({Middle},
	    {default({outer, undefined}, {outer, middle()})},
	    begin
	      Decoded =
		  nested3_pb:decode_outer(nested3_pb:encode_outer(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

proper_protobuffs_nested4() ->
    ?FORALL({Middle},
	    {default({outer, undefined}, {outer, middle()})},
	    begin
	      Decoded =
		  nested4_pb:decode_outer(nested4_pb:encode_outer(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

first_inner() ->
    {first_inner, default(undefined, bool())}.

proper_protobuffs_nested5() ->
    ?FORALL(Inner,
	    (oneof([default({first, undefined},
			    {first, first_inner()}),
		    {second, first_inner()}])),
	    begin
	      case element(1, Inner) of
		first ->
		    Decoded =
			nested5_pb:decode_first(nested5_pb:encode_first(Inner)),
		    compare_messages(Inner, Decoded);
		second ->
		    Decoded =
			nested5_pb:decode_second(nested5_pb:encode_second(Inner)),
		    compare_messages(Inner, Decoded)
	      end
	    end).

enum_value() -> oneof([value1, value2]).

proper_protobuffs_enum() ->
    ?FORALL({Middle},
	    {default({enummsg, undefined},
		     {enummsg, enum_value()})},
	    begin
	      Decoded =
		  enum_pb:decode_enummsg(enum_pb:encode_enummsg(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

enum_outside_value() -> oneof(['FIRST', 'SECOND']).

proper_protobuffs_enum_outside() ->
    ?FORALL({Middle},
	    {default({enumuser, undefined},
		     {enumuser, enum_outside_value()})},
	    begin
	      Decoded =
		  enum_outside_pb:decode_enumuser(enum_outside_pb:encode_enumuser(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

proper_protobuffs_extensions() ->
    ?FORALL({Middle},
	    {default({extendable, dict:new()},
		     {maxtendable, dict:new()})},
	    begin
	      DecodeFunc = list_to_atom("decode_" ++
					  atom_to_list(element(1, Middle))),
	      Decoded =
		  extensions_pb:DecodeFunc(extensions_pb:encode(Middle)),
	      compare_messages(Middle, Decoded)
	    end).

address_phone_number() ->
    {person_phonenumber, utf8string(),
     oneof(['HOME', 'WORK', 'MOBILE'])}.

person() ->
    {person, utf8string(), sint32(),
     default(undefined, utf8string()),
     list(address_phone_number())}.

proper_protobuffs_addressbook() ->
    ?FORALL(Addressbook, {addressbook, list(person())},
	    begin
	      Decoded =
		  addressbook_pb:decode_addressbook(addressbook_pb:encode_addressbook(Addressbook)),
	      compare_messages(Addressbook, Decoded)
	    end).

repeater_location() ->
    {location, utf8string(), utf8string()}.

repeater_person() ->
    {person, utf8string(), utf8string(), utf8string(),
     sint32(), list(utf8string()), list(repeater_location()),
     list(uint32())}.

proper_protobuffs_repeater() ->
    ?FORALL(Repeater, (repeater_person()),
	    begin
	      Decoded =
		  repeater_pb:decode_person(repeater_pb:encode_person(Repeater)),
	      compare_messages(Repeater, Decoded)
	    end).

proper_protobuffs_packed_repeated() ->
    ?FORALL(Repeater, (repeater_person()),
	    begin
	      Decoded =
		  packed_repeated_pb:decode_person(packed_repeated_pb:encode_person(Repeater)),
	      compare_messages(Repeater, Decoded)
	    end).

special_words() ->
    {message, utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string(), utf8string(),
     utf8string(), utf8string(), utf8string()}.

proper_protobuffs_special_words() ->
    ?FORALL({SpecialWords}, {special_words()},
	    begin
	      Decoded =
		  special_words_pb:decode_message(special_words_pb:encode_message(SpecialWords)),
	      compare_messages(SpecialWords, Decoded)
	    end).

proper_protobuffs_import() ->
    ?FORALL({Imported},
	    {default({foo, {imported, utf8string()}},
		     {foo, undefined})},
	    begin
	      Decoded =
		  import_pb:decode_foo(import_pb:encode(Imported)),
	      compare_messages(Imported, Decoded)
	    end).

single() -> {message, uint32()}.

proper_protobuffs_single() ->
    ?FORALL(Single, (single()),
	    begin
	      Decoded =
		  single_pb:decode_message(single_pb:encode_message(Single)),
	      compare_messages(Single, Decoded)
	    end).

proper_protobuffs_extend() ->
    ?FORALL(Extend,
	    (default({extendable,
		      dict:from_list([{126,
				       {optional, sint32(), sint32, []}}])},
		     {extendable, dict:new()})),
	    begin
	      Decoded =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Extend)),
	      compare_messages(Extend, Decoded)
	    end).

proper_protobuffs_extend_degraded() ->
    ?FORALL(Extend,
	    (default({extendable,
		      dict:from_list([{126,
				       {optional, sint32(), sint32, []}}])},
		     {extendable, dict:new()})),
	    begin
	      Encoded = extend_pb:encode_extendable(Extend),
	      DegradedDecoded =
		  extensions_pb:decode_extendable(Encoded),
	      Decoded = extend_pb:decode_extensions(DegradedDecoded),
	      compare_messages(Extend, Decoded)
	    end).

proper_protobuffs_extend_assign() ->
    ?FORALL(Extend, (sint32()),
	    begin
	      Input = {extendable, dict:new()},
	      Expected = {extendable,
			  dict:from_list([{126,
					   {optional, Extend, sint32, none}}])},
	      {ok, Output} = extend_pb:set_extension(Input, bar,
						     Extend),
	      compare_messages(Expected, Output)
	    end).

proper_protobuffs_assign_encode() ->
    ?FORALL(Extend, (sint32()),
	    begin
	      Input = {extendable, dict:new()},
	      Expected = {extendable,
			  dict:from_list([{126,
					   {optional, Extend, sint32, []}}])},
	      {ok, Middle} = extend_pb:set_extension(Input, bar,
						     Extend),
	      Output =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Expected)),
	      compare_messages(Expected, Output)
	    end).

proper_protobuffs_extend_get() ->
    ?FORALL(Extend, (sint32()),
	    begin
	      Input = {extendable, dict:new()},
	      Encodable = {extendable,
			   dict:from_list([{126,
					    {optional, Extend, sint32, []}}])},
	      {ok, Middle} = extend_pb:set_extension(Input, bar,
						     Extend),
	      Decoded =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Middle)),
	      Output = extend_pb:get_extension(Decoded, bar),
	      compare({ok, Extend}, Output)
	    end).

proper_protobuffs_extend_has_enum() ->
    ?FORALL(Extend, (oneof(['FOO', 'BAR'])),
	    begin
	      Input = {extendable, dict:new()},
	      {ok, Encodable} = extend_pb:set_extension(Input, baz,
							Extend),
	      Decoded =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Encodable)),
	      Out = extend_pb:get_extension(Decoded, baz),
	      compare({ok, Extend}, Out)
	    end).

proper_protobuffs_extend_has_message() ->
    ?FORALL(Extend,
	    (default({maxtendable, dict:new()},
		     {maxtendable,
		      dict:from_list([{505,
				       {optional, utf8string(), string,
					[]}}])})),
	    begin
	      Input = {extendable, dict:new()},
	      {ok, Encodable} = extend_pb:set_extension(Input, bin,
							Extend),
	      Decoded =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Encodable)),
	      Out = extend_pb:get_extension(Decoded, bin),
	      compare({ok, Extend}, Out)
	    end).

proper_protobuffs_extend_has_string() ->
    ?FORALL(Extend, (utf8string()),
	    begin
	      Input = {extendable, dict:new()},
	      {ok, Encodable} = extend_pb:set_extension(Input,
							stringy, Extend),
	      Decoded =
		  extend_pb:decode_extendable(extend_pb:encode_extendable(Encodable)),
	      Out = extend_pb:get_extension(Decoded, stringy),
	      compare({ok, Extend}, Out)
	    end).

proper_protobuffs_service() ->
    %Don't handel service tag for the moment testing no errors and that the messages works
    ?FORALL(Service,
	    (oneof([{searchresponse,
		     default(undefined, utf8string())},
		    {searchrequest, default(undefined, utf8string())}])),
	    begin
	      case element(1, Service) of
		searchresponse ->
		    Decoded =
			service_pb:decode_searchresponse(service_pb:encode_searchresponse(Service)),
		    compare_messages(Service, Decoded);
		searchrequest ->
		    Decoded =
			service_pb:decode_searchrequest(service_pb:encode_searchrequest(Service)),
		    compare_messages(Service, Decoded)
	      end
	    end).

proper_protobuffs_exports() ->
    ?FORALL(Exporter,
        (oneof([{exporter, sint32()},
            {export_this_message, sint32()}])),
        begin
            Decoded = case element(1, Exporter) of
                exporter ->
                    Encoded = exports_pb:encode_exporter(Exporter),
                    exports_pb:decode_exporter(Encoded);
                export_this_message ->
                    Encoded = exports_pb:encode_export_this_message(Exporter),
                    exports_pb:decode_export_this_message(Encoded)
            end,
            compare_messages(Exporter, Decoded)
        end).

proper_protobuffs_imports() ->
    ?FORALL(Importer,
        (oneof([
            {importer, {exporter, sint32()}},
            {import_with_underscores, {export_this_message, sint32()}},
            {exporter, sint32()},
            {export_this_message, sint32()}])),
        begin
            Decoded = case element(1, Importer) of
                importer ->
                    Encoded = imports_pb:encode_importer(Importer),
                    imports_pb:decode_importer(Encoded);
                import_with_underscores ->
                    Encoded = imports_pb:encode_import_with_underscores(Importer),
                    imports_pb:decode_import_with_underscores(Encoded);
                exporter ->
                    Encoded = imports_pb:encode_exporter(Importer),
                    imports_pb:decode_exporter(Encoded);
                export_this_message ->
                    Encoded = imports_pb:encode_export_this_message(Importer),
                    imports_pb:decode_export_this_message(Encoded)
            end,
            compare_messages(Importer, Decoded)
        end).
