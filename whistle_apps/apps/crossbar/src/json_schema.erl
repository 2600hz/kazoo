%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @author Edouard Swiac <edouard@2600hx.com>
%%%
%%% @copyright (C) 2011, Karl Anderson
%%% @doc 
%%% 
%%% Implementation of JSON Schema spec
%%% http://tools.ietf.org/html/draft-zyp-json-schema-03
%%%
%%% @end
%%% Created : 18 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%% 28 July 2011 - remove dust & refresh code, json schema still v0.3
%%%-------------------------------------------------------------------
-module(json_schema).

-export([do_validate/2]).

-include("crossbar.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(logger, [format_log/3]).

-define(CROSSBAR_SCHEMA_DB, <<"crossbar%2Fschema">>).
-define(TRACE, false). %% trace through the validation steps

%% for testing purpose with particular JSON data from file
-spec(do_validate/2 :: (File :: string() | json_object(), SchemaName :: atom()) -> list()).
do_validate(File, SchemaName) when is_list(File)->
    {ok, Bin1} = file:read_file(File),
    Data = mochijson2:decode(Bin1),
    do_validate(wh_json:get_value(<<"data">>, Data), SchemaName);
%% for crossbar usage
do_validate(Data, SchemaName)  ->
    {ok, Schema} = couch_mgr:open_doc(?CROSSBAR_SCHEMA_DB, wh_util:to_binary(SchemaName)),
    Errors = [X || {T, _}=X <- validate(Data, Schema), T == validation_error],
    case Errors of
	[] -> ok;
	_ -> Errors
    end.

-spec(validate/2 :: (Instance :: term(), json_object()) -> list()).
validate(Instance, {struct, Definitions}) ->
    validate(Instance, {struct, Definitions}, []).

-spec(validate/3 :: (Instance :: term(), json_object(), Messages :: list()) -> list()).
validate(Instance, {struct, Definitions}, Messages) ->
    L = lists:foldl(fun({Definition, Attributes}, Acc) ->
			    Validation = case Definition of
					     <<"_id">>  -> true;
					     <<"_rev">> -> true;
					     _          -> validate_instance(Instance, Definition, Attributes)
					 end,
			    case Validation of
				true                   -> [{ok, []} | Acc];
				_                      -> [Validation | Acc] % {validation_error, _}
			    end
		    end, Messages, Definitions),
    lists:flatten(L). % flattening because of the recursive nature of validate


-spec(trace_validate/3 :: (term(), term(), term()) -> atom()).
trace_validate(Instance, Type, Attribute) ->
    case ?TRACE of
	true ->
	    io:format("~n_+ Validating Instance :: ~p, ~n_+ Type :: ~p, ~n_+ Attribute :: ~p~n", [Instance, Type, Attribute]);
	false ->
	    nothing
    end.
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.7
%% required - This attribute indicates if the instance must have a
%%            value, and not be undefined.
%% @end
%%--------------------------------------------------------------------
-spec(validate_instance/3 :: (Instance :: term(), binary(), term()) -> true | tuple(validation_error, binary())).
validate_instance(Instance, <<"required">>, Attribute) ->
    trace_validate(Instance, <<"required">>, Attribute),
    case is_boolean_true(Attribute) of
	true -> true;
	false -> validation_error(Instance, <<"required but not found">>)
    end;

%%--------------------------------------------------------------------
%% @doc
%% No other functions should run if the Instance is undefined
%% @end
%%--------------------------------------------------------------------
validate_instance(undefined, _, _) ->
    trace_validate(undefined, none, none),
    validation_error(instance_undefined, <<"The instance is undefined">>);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.1
%% type - This attribute defines what the primitive type or the schema
%%        of the instance MUST be in order to validate.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"type">>, [{struct, _}=Schema]) ->
    trace_validate(Instance, <<"type">>, struct),
    validate(Instance, Schema);
validate_instance(Instance, <<"type">>, [{struct, _}=Schema|T]) ->
    trace_validate(Instance, <<"type">>, struct_list),
    validate(Instance, Schema) orelse validate_instance(Instance, <<"type">>, T);
validate_instance(Instance, <<"type">>, [H|T]) ->
    trace_validate(Instance, <<"type">>, list),
    case validate_instance(Instance, <<"type">>, H) of
        false when T =:= [] -> validation_error(Instance, <<"type ", H, " is invalid">>);
        false               -> validate_instance(Instance, <<"type">>, T);
        true                -> true
    end;
validate_instance(Instance, <<"type">>, <<"null">>) ->
    trace_validate(Instance, <<"type">>, <<"null">>),
    case Instance of
        <<"null">> -> true;
        null       -> true;
        _          -> validation_error(Instance, <<"must be null">>)
    end;
validate_instance(Instance, <<"type">>, <<"string">>) ->
    trace_validate(Instance, <<"type">>, <<"string">>),
    case Instance of
        Str when is_atom(Str); is_binary(Str) ->
	    case validate_instance(Str, <<"type">>, <<"null">>) =/= true
                andalso validate_instance(Str, <<"type">>, <<"boolean">>) =/= true of
		true  ->
		    true;
		false ->
		    validation_error(Instance, <<"must be of type string">>)
	    end;
        _ -> validation_error(Instance, <<"must be of type string">>)
    end;
validate_instance(Instance, <<"type">>, <<"number">>) ->
    trace_validate(Instance, <<"type">>, <<"number">>),
    case is_number(Instance) of
	false -> validation_error(Instance, <<"must be of type number">>);
	true  -> true
    end;
validate_instance(Instance, <<"type">>, <<"integer">>) ->
    trace_validate(Instance, <<"type">>, <<"integer">>),
    case is_integer(Instance) of
	false -> validation_error(Instance, <<"must be of type integer">>);
	true  -> true
    end;
validate_instance(Instance, <<"type">>, <<"boolean">>) ->
    trace_validate(Instance, <<"type">>, <<"boolean">>),
    case Instance of
        true         -> true;
        <<"true">>   -> true;
        false        -> true;
        <<"false">>  -> true;
        _            -> validation_error(Instance, <<"must be of type boolean">>)
    end;
validate_instance(Instance, <<"type">>, <<"array">>) ->
    trace_validate(Instance, <<"type">>, <<"array">>),
    case is_list(Instance) of
	true  -> true;
	false -> validation_error(Instance, <<"must be of type array">>)
    end;
validate_instance(Instance, <<"type">>, <<"object">>) ->
    trace_validate(Instance, <<"type">>, <<"object">>),
    try
	{struct, _} = Instance,
	true
    catch
	_:_ ->
	    validation_error(Instance, <<"must be an object">>)
    end;
validate_instance(_, <<"type">>, _) ->
    trace_validate(none, <<"type">>, none),
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
    trace_validate(Instance, <<"properties">>, Attribute),
    not validate_instance(Instance, <<"type">>, <<"object">>) orelse
	lists:map(fun({Name, Schema}) ->
			    validate(wh_json:get_value(Name, Instance), Schema)
		    end, Attribute);

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
%%                        an object type definition
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
    trace_validate(Instance, <<"items">>, none),
    not validate_instance(Instance, <<"type">>, <<"array">>) orelse
	lists:foldr(fun(Item, Acc) ->
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
    case (catch validate_instance(Instance, <<"type">>, <<"number">>)) =/= true
	orelse Instance >= Attribute of
	true -> true;
	false -> validation_error(Instance, <<"must be an integer and/or must have a minimum of ">>, Attribute)
    end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.10
%% maximum - This attribute defines the maximum value of the instance
%%           property when the type of the instance value is a number.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maximum">>, Attribute) ->
    trace_validate(Instance, <<"maximum">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"number">>)
        orelse Instance =< Attribute of
	true -> true;
	false -> validation_error(Instance, <<"must be an integer and/or must have a maximum of">>, Attribute)
    end;
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.11
%% exclusiveMinimum - his attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "minimum" attribute.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"exclusiveMinimum">>, Instance) when is_number(Instance) ->
    trace_validate(Instance, <<"exclusiveMinimum">>, Instance),
    validation_error(Instance, <<"error in exclusiveMinimum">>);
validate_instance(Instance, <<"exclusiveMinimum">>, Attribute) ->
    trace_validate(Instance, <<"exclusiveMinimum">>, Attribute),
    validate_instance(Instance, <<"must be an integer and/or must have an exclusive minimum of">>, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.12
%% exclusiveMaximum - This attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "maximum" attribute.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"exclusiveMaximum">>, Instance) when is_number(Instance) ->
    trace_validate(Instance, <<"exclusiveMaximum">>,Instance),
    validation_error(Instance, <<"error in exclusiveMaximun">>);
validate_instance(Instance, <<"exclusiveMaximum">>, Attribute) ->
    trace_validate(Instance, <<"exclusiveMaximum">>, Attribute),
    validate_instance(Instance, <<"must be an integer and/or must have an exclusive maximum of">>, Attribute);

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.13
%% minItems - This attribute defines the minimum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"minItems">>, Attribute) ->
    trace_validate(Instance, <<"minItems">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) >= Attribute of
	true -> true;
	false -> validation_error(Instance, <<"must be an array to have minItems and/or there are less items than minItems in array">>)
    end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.14
%% maxItems - This attribute defines the maximum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maxItems">>, Attribute) ->
    trace_validate(Instance, <<"maxItems">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) =< Attribute of
	true -> true;
	false -> validation_error(Instance, <<"must be an array to have maxItems and/or there are more items than maxItems in array">>)
    end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.15
%% uniqueItems - This attribute indicates that all items in an array
%%               instance MUST be unique (contains no two identical values).
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"uniqueItems">>, _) ->
    trace_validate(Instance, <<"uniqueItems">>, none),
    case not validate_instance(Instance, <<"type">>, <<"array">>)
        orelse length(Instance) =:= length(lists:usort(Instance)) of
	true -> true;
	false -> validation_error(Instance, <<"items in array must be unique">>)
    end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.16
%% pattern - When the instance value is a string, this provides a
%%           regular expression that a string instance MUST match
%%           in order to be valid.
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.17
%% minLength - When the instance value is a string, this defines the
%%             minimum length of the string.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"minLength">>, Attribute) ->
    trace_validate(Instance, <<"minLength">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"string">>)
        orelse length(to_list(Instance)) >= Attribute of
	true -> true;
	false -> validation_error(Instance, <<"is too short, min. characters allowed:">>, Attribute)
    end;
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.18
%% maxLength -  When the instance value is a string, this defines
%%              the maximum length of the string.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"maxLength">>, Attribute) ->
    trace_validate(Instance, <<"maxLength">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"string">>)
        orelse length(to_list(Instance)) =< Attribute of
	true -> true;
	false -> validation_error(Instance, <<"is too long, max. characters allowed:">>, Attribute)
    end;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.19
%% enum - This provides an enumeration of all possible values that are
%%        valid for the instance property.
%% @end
%%--------------------------------------------------------------------
validate_instance(Instance, <<"enum">>, Attribute) ->
    trace_validate(Instance, <<"enum">>, Attribute),
    case lists:member(Instance, Attribute) of
	true -> true;
	false -> validation_error(Instance, <<"must be member of the array">>)
    end;

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
validate_instance(_, <<"description">>, _) ->
    trace_validate(none, <<"description">>, none),
    true;

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
    trace_validate(Instance, <<"divisibleBy">>, Attribute),
    case not validate_instance(Instance, <<"type">>, <<"number">>)
        orelse case {Instance, Attribute} of
                   {_, 0} -> validation_error(division_0, <<"Division by 0">>);
                   {0, _} -> true;
                   {I, A} -> case trunc(I/A) == I/A of
				 true -> true;
				 false -> validation_error(error, <<"Not divisible">>)
			     end
	       end of
	true -> true;
	false -> validation_error(Instance, <<"must be divisible by">>, Attribute)
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
    trace_validate(Instance, <<"disallow">>, Attribute),
    case not validate_instance(Instance, <<"type">>, Attribute) of
	true -> true;
	false -> validation_error(Instance, <<"this type is not allowed">>)
    end;

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
    trace_validate(none, <<"id">>, none),
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
    trace_validate(none, <<"schema">>, none),
    true;

%%--------------------------------------------------------------------
%% @doc
%% End of validate_instance
%% @end
%%--------------------------------------------------------------------
validate_instance(_,_,_) ->
    trace_validate(none, none, none),
    validation_error(error, <<"This instance is not valid">>).

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


-spec(validation_error/2 :: (term(), binary()) -> tuple(validation_error, binary())).
validation_error({struct, _}, _) ->
    {validation_error, <<"json is invalid">>};
validation_error(Instance, Msg) ->
    {validation_error, <<(wh_util:to_binary(Instance))/binary,
			     " ", (wh_util:to_binary(Msg))/binary>>}.

-spec(validation_error/3 :: (term(), binary(), term()) -> tuple(validation_error, binary())).
validation_error(Instance, Msg, Attribute) ->
    {validation_error, <<(wh_util:to_binary(Instance))/binary,
			     " ", (wh_util:to_binary(Msg))/binary,
                             " ", (wh_util:to_binary(Attribute))/binary>>}.
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
    %Schema = {<<"type">>, <<"string">>},
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
			  Validation = validate(Elem, S),
			  Result = lists:all(fun({T, _}) -> T == ok end, Validation),
			  ?debugFmt("Testing success of ~p => ~p~n", [Validation, Result]),
			  ?assertEqual(true, Result)
		  end, Succeed),
    lists:foreach(fun(Elem) ->
			  Validation = validate(Elem, S),
			  Result = lists:any(fun({T, _}) -> T == validation_error end, Validation),
			  ?debugFmt("Testing failure of ~p => ~p~n", [Validation, Result]),
			  ?assertEqual(true, Result)
		  end, Fail).
-endif.
