%%%-------------------------------------------------------------------
%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2011 by David Åberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(protobuffs_parser_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

package_test_() ->
    String = "package \"test.package\";",
    Expected = [{package, "test.package"}],
    parse_test(String, Expected).

import_test_() ->
    String = "import \"test.package\";",
    Expected = [{import, "test.package"}],
    parse_test(String, Expected).

message_test_() ->
    String = "message Test { required string name "
	     "= 1; }",
    Expected = [{message, "Test",
		 [{1, required, "string", "name", none}]}],
    parse_test(String, Expected).

message_default_test_() ->
    String = "message Test { optional float value "
	     "= 1 [default=0.01]; optional string "
	     "stringvalue = 2 [default=\"\"];}",
    Expected = [{message, "Test",
		 [{1, optional, "float", "value", 1.0e-2},
		  {2, optional, "string", "stringvalue", ""}]}],
    parse_test(String, Expected).

packed_test_() ->
    String = "message Test { repeated float values "
	     "= 1 [packed=true]; }",
    Expected = [{message, "Test",
		 [{1, repeated_packed, "float", "values", []}]}],
    parse_test(String, Expected).

enum_test_() ->
    String = "enum MyEnum { VALUE0 = 0; VALUE1 = 1;}",
    Expected = [{enum, "MyEnum",
		 [{'VALUE0', 0}, {'VALUE1', 1}]}],
    parse_test(String, Expected).

enum_negative_test_() ->
    String = "enum MyEnum { VALUE0 = 0; VALUE1 = -1; "
	     "VALUE2 = 2147483648; VALUE3 = -2147483647;}",
    Expected = [{enum, "MyEnum",
		 [{'VALUE0', 0}, {'VALUE1', -1}, {'VALUE2', 2147483648},
		  {'VALUE3', -2147483647}]}],
    parse_test(String, Expected).

service_test_() ->
    String = "service SearchService { rpc Search (SearchReq"
	     "uest) returns (SearchResponse);}",
    Expected = [{service, "SearchService",
		 [{rpc, "Search", "SearchRequest", "SearchResponse"}]}],
    parse_test(String, Expected).

extensions_test_() ->
    String = "message Foo { extensions 100 to 199; }",
    Expected = [{message, "Foo", [{extensions, 100, 199}]}],
    parse_test(String, Expected).

extend_test_() ->
    String = "extend Foo { optional int32 bar = 126; }",
    Expected = [{extend, "Foo",
		 [{126, optional, "int32", "bar", none}]}],
    parse_test(String, Expected).

option_test_() ->
    String = "option message_set_wire_format = true;",
    Expected = [{option, message_set_wire_format, true}],
    parse_test(String, Expected).

inner_option_test_() ->
    String = "message Foo { option message_set_wire_format "
	     "= true;}",
    Expected = [{message, "Foo",
		 [{option, message_set_wire_format, true}]}],
    parse_test(String, Expected).

nested_message_test_() ->
    String = "message Test { required Nested nested "
	     "= 1; message Nested { } }",
    Expected = [{message, "Test",
		 [{1, required, "Nested", "nested", none},
		  {message, "Nested", []}]}],
    parse_test(String, Expected).

parse_test(String, Expected) ->
    {ok, Result, 1} = protobuffs_scanner:string(String),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok, Expected}, Parsed)].
