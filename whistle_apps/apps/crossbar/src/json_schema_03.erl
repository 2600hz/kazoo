%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(json_schema_03).

-export([do_validate/0]).

-include("crossbar.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(logger, [format_log/3]).

do_validate() ->
    {ok, Bin1} = file:read_file("/opt/whistle/data.json"),
    Data = mochijson2:decode(Bin1),
    {ok, Schema} = couch_mgr:open_doc("crossbar%2Fschema", <<"devices">>),
    io:format("------~p", [wh_json:get_value(<<"schema">>, Schema)]),
    validate(Data, wh_json:get_value(<<"schema">>, Schema)).


validate(Instance, {struct, Definitions}) ->
    lists:foldl(fun({Definition, Attributes}, Acc) ->
                        Result = case Definition of
				     <<"minimum">> ->
					 case wh_json:get_value([<<"exclusiveMinimum">>], Definitions, false) of
					     true ->
						 validate_instance(Instance, <<"exclusiveMinimum">>, Attributes);
					     _ ->
						 validate_instance(Instance, <<"minimum">>, Attributes)
					 end;
				     <<"maximum">> ->
					 case wh_json:get_value([<<"exclusiveMaximum">>], Definitions, false) of
					     true ->
						 validate_instance(Instance, <<"exclusiveMaximum">>, Attributes);
					     _ ->
						 validate_instance(Instance, <<"maximum">>, Attributes)
					 end;
				     <<"exclusiveMinimum">> ->
					 Acc;
				     <<"exclusiveMaximum">> ->
					 Acc;
				     <<"_id">> ->
					 Acc;
				     <<"_rev">> ->
					 Acc;
				     SubDefinition={struct, _} ->
					io:format("mooo~p ", [SubDefinition]),
					 validate(Instance, SubDefinition);
				     _ ->
					 io:format("-- ~p->~p~n", [Definition, Attributes]),
					 validate_instance(Instance, Definition, Attributes)
				 end,
                        case Result of
                            false ->
                                format_log(info, "validate_instance(~p, ~p, ~p) result ~p~n", [Instance, Definition, Attributes, Result]);
                            _ ->
                                ok
                        end,
                        Acc and Result
                end, true, Definitions).

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.7
%% required - This attribute indicates if the instance must have a
%%            value, and not be undefined.
%% @end
%%--------------------------------------------------------------------
validate_instance(undefined, <<"required">>, Attribute) ->
    not is_boolean_true(Attribute);
validate_instance(_, <<"required">>, _) ->
    true;

%%--------------------------------------------------------------------
%% @doc
%% No other functions should run if the Instance is undefined
%% @end
%%--------------------------------------------------------------------
validate_instance(undefined, _, _) ->
    true;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.1
%% type - This attribute defines what the primitive type or the schema
%%        of the instance MUST be in order to validate.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"type">>, [{struct, _}=Schema]) ->
    validate(Instance, Schema);
validate_instance(Instance, <<"type">>, [{struct, _}=Schema|T]) ->
    validate(Instance, Schema) orelse validate_instance(Instance, <<"type">>, T);
validate_instance(Instance, <<"type">>, [H|T]) ->
    case validate_instance(Instance, <<"type">>, H) of
        false when T =:= [] ->
            false;
        false ->
            validate_instance(Instance, <<"type">>, T);
        true ->
            true
    end;
validate_instance(Instance, <<"type">>, <<"null">>) ->
    case Instance of
        <<"null">> -> true;
        null -> true;
        _ -> false
    end;
validate_instance(Instance, <<"type">>, <<"string">>) ->
    case Instance of
        Str when is_atom(Str); is_binary(Str) ->
            not validate_instance(Str, <<"type">>, <<"null">>)
                andalso not validate_instance(Str, <<"type">>, <<"boolean">>);
        _ -> false
    end;
validate_instance(Instance, <<"type">>, <<"number">>) ->
    is_number(Instance);
validate_instance(Instance, <<"type">>, <<"integer">>) ->
    is_integer(Instance);
validate_instance(Instance, <<"type">>, <<"boolean">>) ->
    case Instance of
        true -> true;
        <<"true">> -> true;
        false -> true;
        <<"false">> -> true;
        _ -> false
    end;
validate_instance(Instance, <<"type">>, <<"array">>) ->
    is_list(Instance);
validate_instance(Instance, <<"type">>, <<"object">>) ->
    try {struct, _} = Instance, true catch _:_ -> false end;
validate_instance(_, <<"type">>, _) ->
    true;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.2
%% properties - This attribute is an object with property definitions
%%              that define the valid values of instance object property
%%              values.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"properties">>, {struct, Attribute}) ->
    not validate_instance(Instance, <<"type">>, <<"object">>)
        orelse lists:foldr(fun({Name, Schema}, Acc) ->
                                   Acc and validate(wh_json:get_value([Name], Instance), Schema)
                           end, true, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.3
%% patternProperties - This attribute is an object that defines the
%%                     schema for a set of property names of an object
%%                     instance.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.4
%% additionalProperties - This attribute defines a schema for all
%%                        properties that are not explicitly defined in
%%                        an object type definition.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.5
%% items - This attribute defines the allowed items in an instance array,
%%         and MUST be a schema or an array of schemas.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"items">>, {struct, _} = Schema) ->
    not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse lists:foldr(fun(Item, Acc) ->
                                   Acc and validate(Item, Schema)
                           end, true, Instance);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.6
%% additionalItems - This provides a definition for additional items in
%%                   an array instance when tuple definitions of the
%%                   items is provided.
%% @end
%%--------------------------------------------------------------------

%% NOTE: Moved section 5.7 to top of function for programatic reasons

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.8
%% dependencies - This attribute is an object that defines the
%%                requirements of a property on an instance object.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.9
%% minimum - This attribute defines the minimum value of the instance
%%           property when the type of the instance value is a number.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"minimum">>, Attribute)->
    not validate_instance(Instance, <<"type">>, <<"number">>)
        orelse Instance >= Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.10
%% maximum - This attribute defines the maximum value of the instance
%%           property when the type of the instance value is a number.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maximum">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"number">>)
        orelse Instance =< Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.11
%% exclusiveMinimum - his attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "minimum" attribute.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"exclusiveMinimum">>, Instance) when is_number(Instance) ->
    false;
validate_instance(Instance, <<"exclusiveMinimum">>, Attribute) ->
    validate_instance(Instance, <<"minimum">>, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.12
%% exclusiveMaximum - This attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "maximum" attribute.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"exclusiveMaximum">>, Instance) when is_number(Instance) ->
    false;
validate_instance(Instance, <<"exclusiveMaximum">>, Attribute) ->
    validate_instance(Instance, <<"maximum">>, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.13
%% minItems - This attribute defines the minimum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"minItems">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) >= Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.14
%% maxItems - This attribute defines the maximum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maxItems">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) =< Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.15
%% uniqueItems - This attribute indicates that all items in an array
%%               instance MUST be unique (contains no two identical values).
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"uniqueItems">>, _) ->
    not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) =:= length(lists:usort(Instance));

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.16
%% pattern - When the instance value is a string, this provides a
%%           regular expression that a string instance MUST match
%%           in order to be valid.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"pattern">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"string">>)
        orelse case re:compile(to_list(Attribute)) of
                   {ok, RE} -> re:run(to_list(Instance), RE) =/= nomatch;
                   _ -> false
               end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.17
%% minLength - When the instance value is a string, this defines the
%%             minimum length of the string.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"minLength">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"string">>)
        orelse length(to_list(Instance)) >= Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.18
%% maxLength -  When the instance value is a string, this defines
%%              the maximum length of the string.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maxLength">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"string">>)
        orelse length(to_list(Instance)) =< Attribute;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.19
%% enum - This provides an enumeration of all possible values that are
%%        valid for the instance property.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"enum">>, Attribute) ->
    lists:member(Instance, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.20
%% default - This attribute defines the default value of the
%%           instance when the instance is undefined.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.21
%% title - This attribute is a string that provides a short
%%         description of the instance property.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.22
%% description - This attribute is a string that provides a full
%%               description of the of purpose the instance property.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.23
%% format - This property defines the type of data, content type, or
%%           microformat to be expected in the instance property values.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.24
%% divisibleBy - This attribute defines what value the number instance
%%                must be divisible by with no remainder.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"divisibleBy">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, <<"number">>)
        orelse case {Instance, Attribute} of
                   {_, 0} -> false;
                   {0, _} -> true;
                   {I, A} -> trunc(I/A) == I/A
               end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.25
%% disallow - This attribute takes the same values as the "type"
%%            attribute, however if the instance matches the type or if
%%            this value is an array and the instance matches any type
%%            or schema in the array, then this instance is not valid.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"disallow">>, Attribute) ->
    not validate_instance(Instance, <<"type">>, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.26
%% extends - another schema which will provide a base schema which
%%           the current schema will inherit from
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.27
%% id - defines the current URI of this schem
%% @end
%%--------------------------------------------------------------------
validate_instance(_, <<"id">>, _) ->
    true;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.28
%% $ref - defines a URI of a schema that contains the full
%%        representation of this schema
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.29
%% $schema - defines a URI of a JSON Schema that is the schema of
%%           the current schema
%% @end
%%--------------------------------------------------------------------
validate_instance(_, <<"\$schema">>, _) ->
    true;

%%--------------------------------------------------------------------
%% @doc
%% End of validate_instance
%% @end
%%--------------------------------------------------------------------
validate_instance(_,_,_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to test if the json term is the boolean true
%% @end
%%--------------------------------------------------------------------
-spec(is_boolean_true/1 :: (Instance :: json_term()) -> boolean()).
is_boolean_true(Instance) ->
    case Instance of
        <<"true">> ->
            true;
        true ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to convert any json_term to a list
%% @end
%%--------------------------------------------------------------------
-spec(to_list/1 :: (X :: atom() | list() | binary() | integer() | float()) -> list()).
to_list(X) when is_float(X) ->
    mochinum:digits(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

%% EUNIT TESTING
-ifdef(TEST).

-define(NULL, <<"null">>).
-define(TRUE, <<"true">>).
-define(FALSE, <<"false">>).
-define(NEG1, -1).
-define(ZERO, 0).
-define(POS1, 1).
-define(PI, 3.1416).
-define(STR1, <<"foobar">>).
-define(STR2, barfoo).
-define(OBJ1, {struct, [{<<"foo">>, <<"bar">>}]}).
-define(ARR1, []).
-define(ARR2, [?STR1]).
-define(ARR3, [?STR1, ?STR2]).
-define(ARR4, [?STR1, ?STR2, ?PI]).
-define(ARR5, [?NULL, ?NULL, ?PI]).
-define(ARR6, [?STR1, ?STR1, ?PI]).
-define(ARR7, [?ARR3, ?ARR3, ?PI]).
-define(ARR8, [?OBJ1, ?OBJ1, ?PI]).

%% Section 5.1 - type
%%     string Value MUST be a string
type_string_test() ->
    Schema = "{ \"type\": \"string\" }",
    Succeed = [?STR1, ?STR2],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%     number Value MUST be a number, floating point numbers are allowed.
type_number_test() ->
    Schema = "{ \"type\": \"number\" }",
    Succeed = [?NEG1, ?ZERO, ?POS1, ?PI],
    Fail = [?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%     integer Value MUST be an integer, no floating point numbers are allowed
type_integer_test() ->
    Schema = "{ \"type\": \"integer\" }",
    Succeed = [?NEG1, ?ZERO, ?POS1],
    Fail = [?NULL, ?TRUE, ?FALSE, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%     boolean Value MUST be a boolean
type_boolean_test() ->
    Schema = "{ \"type\": \"boolean\" }",
    Succeed = [?TRUE, ?FALSE],
    Fail = [?NULL, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%     object Value MUST be an object
type_object_test() ->
    Schema = "{ \"type\": \"object\" }",
    Succeed = [?OBJ1],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2],
    validate_test(Succeed, Fail, Schema).

%%    array Value MUST be an array
type_array_test() ->
    Schema = "{ \"type\": \"array\" }",
    Succeed = [?ARR1, ?ARR2],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%    null Value MUST be null.
type_null_test() ->
    Schema = "{ \"type\": \"null\" }",
    Succeed = [?NULL],
    Fail = [?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%    any value MAY be of any type including null
type_any_test() ->
    Schema = "{ \"type\": \"any\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [],
    validate_test(Succeed, Fail, Schema).

%%    If the property is not defined or is not in this list, then any type of value is acceptable.
type_unknown_test() ->
    Schema = "{ \"type\": \"foobar\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [],
    validate_test(Succeed, Fail, Schema).

%%    union types An array of two or more simple type definitions
type_simple_union_test() ->
    Schema = "{ \"type\": [\"string\", \"null\"] }",
    Succeed = [?NULL, ?STR1, ?STR2],
    Fail = [?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%    union types An array of type definitions with a nested schema
type_nested_union_test() ->
    Schema = "{ \"type\": [\"string\", { \"type\": \"number\", \"minimum\": -1, \"maximum\": 0}] }",
    Succeed = [?STR1, ?STR2, ?NEG1, ?ZERO],
    Fail = [?NULL, ?TRUE, ?FALSE, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%    union types An array of type definitions with a nested schema
type_complex_union_test() ->
    Schema = "{ \"type\": [{ \"type\": \"number\", \"minimum\": -1, \"maximum\": 0, \"exclusiveMinimum\": true}, \"string\"] }",
    Succeed = [?STR1, ?STR2, ?ZERO],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.2 - properties
%%     object with property definitions that define the valid values of instance object property values

%% Section 5.3 - patternProperties
%%     regular expression pattern name attribute is an object that defines the schema

%% Section 5.4 - additionalProperties
%%     attribute defines a schema for all properties that are not explicitly defined

%% Section 5.5 - items
%%     defines the allowed items in an instance array

%% Section 5.6 - additionalItems
%%      definition for additional items in an array instance when tuple definitions of the items is provided

%% Section 5.7 - required
%%      indicates if the instance must have a value

%% Section 5.8 - dependencies
%%      defines the requirements of a property on an instance object

%% Section 5.9 - minimum
%%     defines the minimum value of the instance property when the type of the instance is a number
minimum_test() ->
    Schema = "{ \"minimum\": 0 }",
    Succeed = [?ZERO, ?POS1, ?PI],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.10 - maximum
%%     defines the maxium value of the instance property when the type of the instance is a number
maximum_test() ->
    Schema = "{ \"maximum\": 0 }",
    Succeed = [?NEG1, ?ZERO],
    Fail = [?NULL, ?TRUE, ?FALSE, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.11 - exclusiveMinimum
%%     indicates if the value of the instance (if the instance is a number) can not equal the number defined by the 'minimum' attribute
exclusive_minimum_test() ->
    Schema = "{ \"minimum\": 0,  \"exclusiveMinimum\": true }",
    Succeed = [?POS1, ?PI],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.12 - exclusiveMaximum
%%     indicates if the value of the instance (if the instance is a number) can not equal the number defined by the 'maximum' attribute
exclusive_maximum_test() ->
    Schema = "{ \"maximum\": 0, \"exclusiveMaximum\": true }",
    Succeed = [?NEG1],
    Fail = [?NULL, ?TRUE, ?FALSE, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.13 - minItems
%%     defines the minimum number of values in an array when the array is the instance value
min_items_test() ->
    Schema = "{ \"minItems\": 2 }",
    Succeed = [?ARR3, ?ARR4],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.14 - maxItems
%%     defines the maximum number of values in an array when the array is the instance value
max_items_test() ->
    Schema = "{ \"maxItems\": 2 }",
    Succeed = [?ARR1, ?ARR2, ?ARR3],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR4, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.15 - uniqueItems
%%     indicates that all items in an array instance MUST be unique (containes no two identical values).
%%      - booleans/numbers/strings/null have the same value
%%      - arrays containes the same number of iteams and each item in the array is equal to teh corresponding item in the other array
%%      - objects contain the same property names, and each property in the object is equal to the corresponding property in the other object
unique_items_test() ->
    Schema = "{ \"uniqueItems\": true }",
    Succeed = [?ARR1, ?ARR2, ?ARR3, ?ARR4],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR5, ?ARR6, ?ARR7, ?ARR8, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.16 - pattern
%%     When the instance value is a string, this provides a regular expression that a string MUST match
pattern_test() ->
    Schema = "{ \"pattern\": \"tle\$\"}",
    Succeed = [chipotle, <<"chipotle">>],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.17 - minLength
%%     When the instance value is a string, this defines the minimum length of the string
min_length_test() ->
    Schema = "{ \"minLength\": 7}",
    Succeed = [longstring, <<"longstring">>],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.18 - maxLength
%%     When the instance value is a string, this defines the maximum length of the string
max_length_test() ->
    Schema = "{ \"maxLength\": 3}",
    Succeed = [foo, <<"bar">>],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.19 - enum
%%     Enumeration of all possible values that are valid for the instance property
enum_test() ->
    Schema = "{ \"enum\": [\"foobar\", 3.1416]}",
    Succeed = [?STR1, ?PI],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.20 - default
%% Section 5.21 - title
%% Section 5.22 - description

%% Section 5.23 - format
%%     defines the type of data, content type, or microformat to be expected

%% Section 5.24 - divisibleBy
%%     defines what value the number instance must be divisible by
divisible_by_test() ->
    Schema = "{ \"divisibleBy\": 3}",
    Succeed = [?ZERO, 3, 15],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%     test the true spirt of this property as per the advocate
divisible_by_float_test() ->
    Schema = "{ \"divisibleBy\": 0.01}",
    Succeed = [?NEG1, ?ZERO, ?POS1, 3.15],
    Fail = [?NULL, ?TRUE, ?FALSE, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    validate_test(Succeed, Fail, Schema).

%% Section 5.25 - disallow
%%     string Value MUST NOT be a string
disallow_string_test() ->
    Schema = "{ \"disallow\": \"string\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2],
    validate_test(Succeed, Fail, Schema).

%%     number Value MUST NOT be a number, including floating point numbers
disallow_number_test() ->
    Schema = "{ \"disallow\": \"number\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?NEG1, ?ZERO, ?POS1, ?PI],
    validate_test(Succeed, Fail, Schema).

%%     integer Value MUST NOT be an integer, does not include floating point numbers
disallow_integer_test() ->
    Schema = "{ \"disallow\": \"integer\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?NEG1, ?ZERO, ?POS1],
    validate_test(Succeed, Fail, Schema).

%%     boolean Value MUST NOT be a boolean
disallow_boolean_test() ->
    Schema = "{ \"disallow\": \"boolean\" }",
    Succeed = [?NULL, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?TRUE, ?FALSE],
    validate_test(Succeed, Fail, Schema).

%%     object Value MUST NOT be an object
disallow_object_test() ->
    Schema = "{ \"disallow\": \"object\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2],
    Fail = [?OBJ1],
    validate_test(Succeed, Fail, Schema).

%%    array Value MUST NOT be an array
disallow_array_test() ->
    Schema = "{ \"disallow\": \"array\" }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?OBJ1],
    Fail = [?ARR1, ?ARR2],
    validate_test(Succeed, Fail, Schema).

%%    null Value MUST NOT be null
disallow_null_test() ->
    Schema = "{ \"disallow\": \"null\" }",
    Succeed = [?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?NULL],
    validate_test(Succeed, Fail, Schema).

%%    union types An array of type definitions with a nested schema
disallow_nested_union_test() ->
    Schema = "{ \"disallow\": [\"string\", { \"type\": \"number\", \"maximum\": 0}] }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2, ?NEG1, ?ZERO],
    validate_test(Succeed, Fail, Schema).

%%    union types An array of type definitions with a nested schema
disallow_complex_union_test() ->
    Schema = "{ \"disallow\": [{ \"type\": \"number\", \"minimum\": -1, \"maximum\": 0, \"exclusiveMinimum\": true}, \"string\"] }",
    Succeed = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2, ?ZERO],
    validate_test(Succeed, Fail, Schema).

%% Section 5.26 - extends
%%     another schema which will provide a base schema which the current schema will inherit from


%% Helper function to run the eunit tests listed above
validate_test(Succeed, Fail, Schema) ->
    S = mochijson2:decode(binary:list_to_bin(Schema)),
    lists:foreach(fun(Elem) ->
%%                          ?debugFmt("validate(~p, ~p) =:= true~n", [Elem, S]),
                          io:format("~p~n", [Elem]),
			  ?assertEqual(true, validate(Elem, S))
		  end, Succeed),
   lists:foreach(fun(Elem) ->
%%                          ?debugFmt("validate(~p, ~p) =:= false~n", [Elem, S]),
                          io:format("~p~n", [Elem]),
			  ?assertEqual(false, validate(Elem, S))
		  end, Fail).
-endif.
