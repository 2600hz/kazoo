%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.com>
%%%
%%% @copyright (C) 2011, Edouard Swiac
%%% @doc
%%%
%%% Implementation of JSON Schema spec
%%% http://tools.ietf.org/html/draft-zyp-json-schema-03
%%% http://nico.vahlas.eu/2010/04/23/json-schema-specifying-and-validating-json-data-structures/
%%%
%%% @end
%%% 28 July 2011 - remove dust & refresh code, json schema still v0.3
%%%-------------------------------------------------------------------
-module(crossbar_schema).

-export([do_validate/2]).

-include("crossbar.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CROSSBAR_SCHEMA_DB, <<"crossbar_schemas">>).
-define(TRACE, false). %% trace through the validation steps

-define(VALIDATION_FUN, fun({error, _}) -> false; (?VALID) -> true end).


%% macroing to increase readability
-define(VALID, true).
-define(INVALID(A,B), validation_error(A,B)).
-define(INVALID(A,B,C), validation_error(A,B,C)).
-define(O(A,B), ?LOG(A,B)).

-type validation_result() :: ?VALID | {error, binary()}.
-type validation_results() :: validation_result() | [validation_result(),...].
-type attribute_name() :: binary().
-type attribute_value() :: binary() | json_object().


%%--------------------------------------------------------------------
%% @doc
%% Performs the validation of a JSON structure against a schema stored
%% in DB as a couch doc
%% @end
%%--------------------------------------------------------------------
-spec do_validate/2 :: (File, SchemaName) -> list({error, _}) when
      File :: string() | json_object(),
      SchemaName :: atom().
do_validate(JObj, SchemaName) ->
    case  couch_mgr:open_doc(?CROSSBAR_SCHEMA_DB, wh_util:to_binary(SchemaName)) of
	{ok, Schema} ->
	    R = validate({JObj}, {Schema}),
	    case [M || {error, M} <- lists:flatten(R)] of
		[] -> {ok, []};
		[_|_]=Errors -> {error, Errors}
	    end;
	{error, _} ->
	    {ok, []}
    end.


%%--------------------------------------------------------------------
%% @doc
%% validation method that is recursively applied on the JSON object
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: ({IAttName, IAttVal}, {Schema, SAttName,SAttVal}) -> validation_results() when
      IAttName :: attribute_name(),
      IAttVal :: attribute_value(),
      Schema :: json_object(),
      SAttName :: attribute_name(),
      SAttVal :: attribute_value().

%% undefined property is required if required in schema
validate({InstanceName, undefined}, {Schema}) ->
    case wh_json:get_binary_boolean(<<"required">>, Schema) == <<"true">> of
	true -> ?INVALID(InstanceName, <<"is undefined">>);
	_ -> ?VALID
    end;

%% unfold schema definition until finding property
validate({JObj},  {{struct, Definitions}=Schema}) ->
    [validate({null, JObj}, {Schema, SAttName, SAttVal})  || {SAttName, SAttVal} <- Definitions];

validate({InstanceName, InstanceValue}, {{struct, Schema}}) ->
    [validate({InstanceName, InstanceValue}, {Schema, SAttName, SAttVal}) || {SAttName, SAttVal} <- Schema];

%% couch metadata ignored for our validation impl.
validate(_, {_, <<"_id">>, _}) ->
    %trace(schema, {<<"_id">>, _V}),
    ?VALID;
validate(_, {_, <<"_rev">>, _}) ->
    %trace(schema, {<<"_rev">>, _V}),
    ?VALID;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.27
%% id - defines the current URI of this schem
%% @end
%%--------------------------------------------------------------------
validate(_, {_, <<"id">>, _}) ->
    %trace(schema, {<<"id">>, V}),
    ?VALID;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.28
%% $ref - defines a URI of a schema that contains the full
%%        representation of this schema
%% @end
%%--------------------------------------------------------------------
validate(_, {_, <<"\$ref">>, _V}) ->
    trace(schema, {<<"\$ref">>, _V}),
    ?VALID;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.29
%% $schema - defines a URI of a JSON Schema that is the schema of
%%           the current schema
%% @end
%%--------------------------------------------------------------------
validate(_, {_, <<"\$schema">>, _}) ->
    %trace(schema, {<<"\$schema">>, V}),
    ?VALID;

%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.21
%% title - This attribute is a string that provides a short
%%         description of the instance property.
%% @end
%%--------------------------------------------------------------------
validate(_, {_, <<"description">>, _D}) ->
    %trace(schema, {<<"description">>, _D}),
    ?VALID;

validate({_, {struct, _}}, {_, <<"type">>, <<"object">>}) ->
    ?VALID;





%% PROPERTIES
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.2
%% properties - This attribute is an object with property definitions
%%              that define the valid values of instance object property
%%              values.
%% @end
%%--------------------------------------------------------------------
validate({_, JObj}, {_, <<"properties">>, {struct, Properties}}) ->
    [validate({Property, val(Property, JObj)}, {Schema}) || {Property, Schema} <- Properties];


%% INSTANCE TYPE
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.1
%% type - This attribute defines what the primitive type or the schema
%%        of the instance MUST be in order to validate.
%% @end
%%--------------------------------------------------------------------
%% string
validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"string">>}) ->
    trace({IAttName, IAttVal}, {<<"type">>, <<"string">>}),
    case IAttVal of
        Str when is_atom(Str); is_binary(Str) ->
	    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"null">>}) =/= ?VALID
                andalso validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"boolean">>}) =/= ?VALID of
		true  ->
		    ?VALID;
		false ->
		    ?INVALID(IAttName, <<"must be a string">>)
	    end;
        _ -> ?INVALID(IAttName, <<"must be a string">>)
    end;

%% boolean
validate({IAttName, IAttVal}, {_, <<"type">>, <<"boolean">>}) when is_boolean(IAttVal)->
    trace({IAttName, IAttVal}, {<<"type">>, <<"boolean">>}),
    ?VALID;
validate({IAttName, IAttVal}, {_, <<"type">>, <<"boolean">>}) ->
    trace({IAttName, IAttVal}, {<<"type">>, <<"boolean">>}),
    ?INVALID(IAttName, <<"must be a boolean">>);

%% number
validate({IAttName, IAttVal}, {_, <<"type">>, <<"number">>}) when is_number(IAttVal)->
    trace({IAttName, IAttVal}, {<<"type">>, <<"number">>}),
    ?VALID;
validate({IAttName, IAttVal}, {_, <<"type">>, <<"number">>})->
    trace({IAttName, IAttVal}, {<<"type">>, <<"number">>}),
    ?INVALID(IAttVal, <<"must be a number">>);

%% integer
validate({IAttName, IAttVal}, {_, <<"type">>, <<"integer">>}) when is_integer(IAttVal)->
    trace({IAttName, IAttVal}, {<<"type">>, <<"integer">>}),
    ?VALID;
validate({IAttName, IAttVal}, {_, <<"type">>, <<"integer">>})->
    trace({IAttName, IAttVal}, {<<"type">>, <<"integer">>}),
    ?INVALID(IAttVal, <<"must be an integer">>);

%% array
validate({IAttName, IAttVal}, {_, <<"type">>, <<"array">>})->
    trace({IAttName, IAttVal}, {<<"type">>, <<"array">>}),
    case IAttVal of
	[_|_] -> ?VALID;
	[] -> ?VALID;
	_ -> ?INVALID(IAttName, <<"must be an array">>)
    end;

%% null
validate({IAttName, IAttVal}, {_, <<"type">>, <<"null">>}) ->
    trace({IAttName, IAttVal}, {<<"type">>, <<"null">>}),
    case IAttVal of
        <<"null">> -> ?VALID;
	null       -> ?VALID;
        _          -> ?INVALID(IAttName, <<"must be null">>)
    end;

%% object
validate({IAttName, IAttVal}, {_, <<"type">>, <<"object">>}) ->
    trace({IAttName, IAttVal}, {<<"type">>, <<"object">>}),
    ?VALID;

%% any
validate({IAttName, IAttVal}, {_, <<"type">>, <<"any">>}) ->
    trace({IAttName, IAttVal}, {<<"type">>, <<"any">>}),
    ?VALID;

%% schema as a type
validate({IAttName, IAttVal}, {_, <<"type">>, [{struct, _}]}) ->
    trace({IAttName, IAttVal}, {<<"type">>, struct}),
    ?INVALID(IAttName, <<"type not supported">>);

validate({IAttName, IAttVal}, {_, <<"type">>, [{struct, _}=Schema|T]}) ->
    trace({IAttName, IAttVal}, {<<"type">>, struct_list}),
    case lists:all(?VALIDATION_FUN, validate({IAttName, IAttVal}, {Schema})) of
	true -> ?VALID;
	false -> ?INVALID(IAttName, <<"type ", T/binary, " is invalid">>)
    end;

%% union of type
validate({IAttName, IAttVal},{Schema, <<"type">>, [H]}) ->
    trace({IAttName, IAttVal}, {<<"type">>, H}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, H}) of
	?VALID -> ?VALID;
	_ -> ?INVALID(IAttName, <<"type is invalid">>)
    end;

%% union of type
validate({IAttName, IAttVal}, {Schema, <<"type">>, [H|T]}) ->
    trace({IAttName, IAttVal}, {<<"type">>, H}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, H})  of
        ?VALID ->  ?VALID;
        _ -> validate({IAttName, IAttVal}, {Schema, <<"type">>, T})
    end;

%% any type is considered valid
validate({IAttName, IAttVal}, {_, <<"type">>, _}) ->
    trace({IAttName, IAttVal}, {<<"any type">>, IAttVal}),
    ?VALID;

%% ARRAY ITEMS
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.5
%% items - This attribute defines the allowed items in an instance array,
%%         and MUST be a schema or an array of schemas.
%% does not support tuple typing!
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"items">>, ItemSchema}) ->
    trace({IAttName, IAttVal}, {<<"items">>, <<"items()">>}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"array">>}) of
	?VALID -> [validate({IAttName, AttVal}, {ItemSchema}) || AttVal <- IAttVal];
	_ -> ?INVALID(IAttVal, <<"must be an array to define items">>)
    end;

%% REQUIRED
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.7
%% required - This attribute indicates if the instance must have a
%%            value, and not be undefined.
%% all props are required by default, see UNDEFINED ATTR
%% @end
%%--------------------------------------------------------------------
validate({IAttName, undefined}, {_, <<"required">>, Val}) ->
    trace({IAttName, undefined}, {<<"required">>, Val}),
    case wh_util:to_boolean(Val) of
	true -> ?INVALID(IAttName, <<"is undefined">>);
	false -> ?VALID
    end;
validate({IAttName, IAttValue}, {_, <<"required">>, Val}) ->
    trace({IAttName, IAttValue}, {<<"required">>, Val}),
    ?VALID;

%% attribute defined in the schema that doesn't exist in the instance
%validate({IAttName, undefined}, {Schema}) ->
%    %trace({IAttName, is_undefined}, {SAttName, SAttVal}),
%    case val(<<"required">>, Schema) of
%	false -> ?VALID;
%        _ -> ?INVALID(IAttName, <<"is undefined">>)
%    end;

%% MINIMUM
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.9
%% minimum - This attribute defines the minimum value of the instance
%%           property when the type of the instance value is a number.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"minimum">>, Minimum}) ->
    trace({IAttName, IAttVal}, {<<"minimum">>, Minimum}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"number">>}) of
	?VALID -> case IAttVal >= Minimum of
		      true -> ?VALID;
		      _ -> ?INVALID(IAttName, <<"is lower than">>, Minimum)
		  end;
	_ -> ?INVALID(IAttName, <<"must be an integer to have a minimum">>)
    end;

%% EXCLUSIVE MINIMUM
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.11
%% exclusiveMinimum - his attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "minimum" attribute.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"exclusiveMinimum">>, Boolean}) when is_boolean(Boolean) ->
    trace({IAttName, IAttVal}, {<<"exclusiveMinimum">>, Boolean}),
    SMinimum = val(<<"minimum">>, Schema),
    case validate({IAttName, IAttVal}, {Schema, <<"minimum">>, SMinimum}) of
	?VALID -> case IAttVal == SMinimum and Boolean of
		      true ->?INVALID(IAttName, <<"cannot equal minimum since it's exclusive">>);
		      _ ->  ?VALID
		  end;

	_ -> ?INVALID(IAttName,  <<"cannot equal minimum since it's exclusive">>)
    end;

%% MAXIMUM
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.10
%% maximum - This attribute defines the maximum value of the instance
%%           property when the type of the instance value is a number.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"maximum">>, Maximum}) ->
    trace({IAttName, IAttVal}, {<<"maximum">>, Maximum}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"number">>}) of
	?VALID -> case IAttVal >= Maximum of
		      ?VALID -> ?VALID;
		      _ -> ?INVALID(IAttName, <<"is greater than">>, Maximum)
		  end;
	_ -> ?INVALID(IAttName, <<"must be an integer to have a minimum">>)
    end;

%% EXCLUSIVE MAXIMUM
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.12
%% exclusiveMaximum - This attribute indicates if the value of the
%%                    instance (if the instance is a number) can not
%%                    equal the number defined by the "maximum" attribute.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"exclusiveMaximum">>, Boolean}) when is_boolean(Boolean) ->
    trace({IAttName, IAttVal}, {<<"exclusiveMaximum">>, Boolean}),
    SMax = val(<<"maximum">>, Schema),
    case validate({IAttName, IAttVal}, {Schema, <<"minimum">>, SMax}) of
	?VALID -> case IAttVal == SMax and Boolean of
		      true ->?INVALID(IAttName, <<"cannot equal minimum since it's exclusive">>);
		      _ ->  ?VALID
		  end;

	_ -> ?INVALID(IAttName,  <<"cannot equal maximum since it's exclusive">>)
    end;

%% MINITEMS
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.13
%% minItems - This attribute defines the minimum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"minItems">>, MinItems})  ->
    trace({IAttName, IAttVal}, {<<"minItems">>, MinItems}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"array">>}) of
	?VALID -> case length(IAttVal) >= MinItems of
		      true -> ?VALID;
		      false -> ?INVALID(IAttName, <<"has less items than minItems in array">>)
		  end;
	_ -> ?INVALID(IAttName, <<"must be an array to define minItems">>)
    end;

%% MAXITEMS
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.14
%% maxItems - This attribute defines the maximum number of values in
%%            an array when the array is the instance value.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"maxItems">>, MinItems})  ->
    trace({IAttName, IAttVal}, {<<"maxItems">>, MinItems}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"array">>}) of
	?VALID -> case length(IAttVal) =< MinItems of
		      true -> ?VALID;
		      false -> ?INVALID(IAttName, <<"has more items than minItems in array">>)
		  end;
	_ -> ?INVALID(IAttName, <<"must be an array to define maxItems">>)
    end;

%% UNIQUE ITEMS
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.15
%% uniqueItems - This attribute indicates that all items in an array
%%               instance MUST be unique (contains no two identical values).
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"uniqueItems">>, Boolean})  when is_boolean(Boolean) ->
    trace({IAttName, IAttVal}, {<<"uniqueItems">>, Boolean}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"array">>}) of
        ?VALID -> case length(IAttVal) =:= length(lists:usort(IAttVal)) of
		      true ->  ?VALID;
		      false ->  ?INVALID(IAttName, "items are not unique")
		  end;
	_ -> ?INVALID(IAttName, <<"items in array must be unique">>)
    end;

%% MIN_LENGTH
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.17
%% minLength - When the instance value is a string, this defines the
%%             minimum length of the string.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"minLength">>, MinLength})  ->
    trace({IAttName, IAttVal}, {<<"minLength">>, MinLength}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"string">>}) of
        ?VALID -> case length(wh_util:to_list(IAttVal)) >= MinLength of
		      true -> ?VALID;
		      false -> ?INVALID(IAttName, <<"is too short, min. characters allowed:">>, MinLength)
		  end;
	_ -> ?INVALID(IAttName, <<"must be a string to define a minimum length">>)
    end;

%% MAX_LENGTH
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.18
%% maxLength -  When the instance value is a string, this defines
%%              the maximum length of the string.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"maxLength">>, MinLength})  ->
    trace({IAttName, IAttVal}, {<<"maxLength">>, MinLength}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"string">>}) of
        ?VALID -> case length(wh_util:to_list(IAttVal)) =< MinLength of
		      true -> ?VALID;
		      false -> ?INVALID(IAttName, <<"is too short, min. characters allowed:">>, MinLength)
		  end;
	_ -> ?INVALID(IAttName, <<"must be a string to define a minimum length">>)
    end;

%% ENUM
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.19
%% enum - This provides an enumeration of all possible values that are
%%        valid for the instance property.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {_, <<"enum">>, Enum})  ->
    trace({IAttName, IAttVal}, {<<"enum">>, Enum}),
    case lists:foldl(fun(E, Acc) -> lists:member(IAttVal, E) and Acc end, true, IAttVal) of
	true -> ?VALID;
	false -> ?INVALID(IAttName, <<"must contain value from enum: ">>, Enum)
    end;

%% DIVISIBLE BY
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.24
%% divisibleBy - This attribute defines what value the number instance
%%                must be divisible by with no remainder.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"divisibleBy">>, DivisibleBy})  ->
    trace({IAttName, IAttVal}, {<<"divisibleBy">>, DivisibleBy}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, <<"number">>}) of
        ?VALID -> case {IAttVal, DivisibleBy} of
		      {_, 0} -> ?INVALID(IAttVal, <<"Division by 0">>);
		      {0, _} -> ?VALID;
		      {I, A} -> case trunc(I/A) == I/A of
				    true -> ?VALID;
				    false -> ?INVALID(IAttName, <<" is not divisible by ">>, DivisibleBy)
				end
		  end;
	_ -> ?INVALID(IAttName, <<"must be a number to define a divisibleBy schema">>)
    end;

%% DISALLOW
%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.25
%% disallow - This attribute takes the same values as the "type"
%%            attribute, however if the instance matches the type or if
%%            this value is an array and the instance matches any type
%%            or schema in the array, then this instance is not valid.
%% @end
%%--------------------------------------------------------------------
validate({IAttName, IAttVal}, {Schema, <<"disallow">>, Disallow})  ->
    trace({IAttName, IAttVal}, {<<"disallow">>, Disallow}),
    case validate({IAttName, IAttVal}, {Schema, <<"type">>, Disallow}) of
	?VALID -> ?INVALID(IAttVal, <<"type is not allowed">>);
	_ -> ?VALID
    end;


validate({_Instance, IAttName, IAttVal}, {_Schema, SAttName, SAttVal}) ->
    trace({IAttName, IAttVal}, {SAttName, SAttVal}),
    ?INVALID(IAttName, <<" : unexpected error with value">>).





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
%% Implementation of draft-zyp-json-schema-03 section 5.6
%% additionalItems - This provides a definition for additional items in
%%                   an array instance when tuple definitions of the
%%                   items is provided.
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% Implementation of draft-zyp-json-schema-03 section 5.8
%% dependencies - This attribute is an object that defines the
%%                requirements of a property on an instance object.
%% @end
%%--------------------------------------------------------------------

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
%% Implementation of draft-zyp-json-schema-03 section 5.20
%% default - This attribute defines the default value of the
%%           instance when the instance is undefined.
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
%% Implementation of draft-zyp-json-schema-03 section 5.26
%% extends - another schema which will provide a base schema which
%%           the current schema will inherit from
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% End of validate
%% @end
%%--------------------------------------------------------------------

%% quick alias for wh_json:get_value
-spec val/2 :: (Property, JObj) -> binary() | json_object() when
      Property :: binary(),
      JObj :: json_object().
val(Property, JObj) ->
    wh_json:get_value(Property, JObj, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Logging utilities
%% @end
%%--------------------------------------------------------------------
-spec trace/2 :: ({InstanceKey, InstanceValue}, {SchemaKey, SchemaValue})  -> 'ok' | 'false' when
      InstanceKey :: attribute_name(),
      InstanceValue :: attribute_value(),
      SchemaKey :: attribute_name(),
      SchemaValue :: attribute_value();
		 (schema, {SchemaKey, SchemaValue}) ->  'ok' | 'false' when
      SchemaKey :: attribute_name(),
      SchemaValue :: attribute_value();
		 (instance, {InstanceKey, InstanceValue}) -> 'ok' | 'false' when
      InstanceKey :: attribute_name(),
      InstanceValue :: attribute_value().
trace({IK, IV}, {SK, SV}) ->
    ?TRACE andalso begin
			?O("[TRACE] instance { ~p : ~p } || schema { ~p : ~p }~n", [IK,IV,SK,SV])
		    end;
trace(schema, {SK, SV}) ->
    ?TRACE andalso begin
		       ?O("[TRACE] schema { ~p : ~p }~n", [SK,SV])
		   end;
trace(instance, {IK, IV}) ->
    ?TRACE andalso begin
		       ?O("[TRACE] instance { ~p : ~p }~n", [IK,IV])
		   end.


%%--------------------------------------------------------------------
%% @doc
%% Formats a validation error message
%% @end
%%--------------------------------------------------------------------
-spec validation_error/2 :: (Instance, Message) -> validation_result() when
      Instance :: term(),
      Message :: binary().
validation_error({struct, _}, _) ->
    {error, <<"json is invalid">>};
validation_error(Instance, Msg) ->
    {error, <<(wh_util:to_binary(Instance))/binary,
			     " ", (wh_util:to_binary(Msg))/binary>>}.

-spec validation_error/3 :: (Instance, Message, Attribute) -> validation_result() when
      Instance :: term(),
      Message :: binary(),
      Attribute :: term().
validation_error(Instance, Msg, Attribute) ->
    {error, <<(wh_util:to_binary(Instance))/binary,
			     " ", (wh_util:to_binary(Msg))/binary ,
                             " ", (wh_util:to_binary(Attribute))/binary>>}.

%%--------------------------------------------------------------------
%% @doc
%% Unit testing  \o/
%% @end
%%--------------------------------------------------------------------
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
    {struct, S} = mochijson2:decode(binary:list_to_bin(Schema)),
    lists:foreach(fun(Elem) ->
			  Validation = [validate({test, Elem}, {S, AttName, AttValue}) || {AttName, AttValue} <- S],
			  Result = lists:all(?VALIDATION_FUN, Validation),
			  ?debugFmt("~p: ~p: Testing success of ~p => ~p~n", [S, Elem, Validation, Result]),
			  ?assertEqual(true, Result)
		  end, Succeed),
    lists:foreach(fun(Elem) ->
			  Validation = [validate({test, Elem}, {S, AttName, AttValue}) || {AttName, AttValue} <- S],
			  Result = lists:any(?VALIDATION_FUN, Validation),
			  ?debugFmt("~p: ~p: Testing failure of ~p => ~p~n", [S, Elem, Validation, Result]),
			  ?assertEqual(false, Result)
		  end, Fail).
-endif.
