%%%=============================================================================
%% Copyright 2013 Klarna AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_schema_validator).

%% API
-export([ validate/3
        , get_schema_id/1
        , is_json_object/1
        , default_error_handler/3
        ]).

%% Constant definitions for Json schema keywords
-define(TYPE,                 <<"type">>).
-define(PROPERTIES,           <<"properties">>).
-define(PATTERNPROPERTIES,    <<"patternProperties">>).
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(MINPROPERTIES,        <<"minProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).
-define(EXTENDS,              <<"extends">>).
-define(ID,                   <<"id">>).
-define(_REF,                 <<"$ref">>).                 % NOT IMPLEMENTED YET

%% Constant definitions for Json types
-define(ANY,                  <<"any">>).
-define(ARRAY,                <<"array">>).
-define(BOOLEAN,              <<"boolean">>).
-define(INTEGER,              <<"integer">>).
-define(NULL,                 <<"null">>).
-define(NUMBER,               <<"number">>).
-define(OBJECT,               <<"object">>).
-define(STRING,               <<"string">>).

%%
-define(data_invalid,                'data_invalid').
-define(schema_invalid,              'schema_invalid').

-define(missing_id_field,            'missing_id_field').
-define(missing_required_property,   'missing_required_property').
-define(missing_dependency,          'missing_dependency').
-define(no_match,                    'no_match').
-define(no_extra_properties_allowed, 'no_extra_properties_allowed').
-define(no_extra_items_allowed,      'no_extra_items_allowed').
-define(not_enought_items,           'not_enought_items').
-define(not_allowed,                 'not_allowed').
-define(not_unique,                  'not_unique').
-define(not_in_range,                'not_in_range').
-define(not_minimum,                 'not_minimum').
-define(not_maximum,                 'not_maximum').
-define(not_in_enum,                 'not_in_enum').
-define(not_divisible,               'not_divisible').
-define(wrong_type,                  'wrong_type').
-define(wrong_type_items,            'wrong_type_items').
-define(wrong_type_dependency,       'wrong_type_dependency').
-define(wrong_size,                  'wrong_size').
-define(wrong_min_items,             'wrong_min_items').
-define(wrong_max_items,             'wrong_max_items').
-define(wrong_min_properties,        'wrong_min_properties').
-define(wrong_length,                'wrong_length').
-define(wrong_min_length,            'wrong_min_length').
-define(wrong_max_length,            'wrong_max_length').
-define(wrong_format,                'wrong_format').

%%
-define(not_found, not_found).

%% Internal datastructures
-record( state
       , { original_schema :: jesse:json_term()
         , current_schema  :: jesse:json_term()
         , current_path    :: [binary()] %% current path in reversed order
         , allowed_errors  :: non_neg_integer() | 'infinity'
         , error_list      :: list()
         , error_handler   :: fun((#state{}) -> list() | no_return())
         }
       ).


%%% API
%% @doc Validates json `Data' against `Schema' with `Options'.
%% If the given json is valid, then it is returned to the caller as is,
%% otherwise an exception will be thrown.
-spec validate( JsonSchema :: jesse:json_term()
              , Data       :: jesse:json_term()
              , Options    :: [{Key :: atom(), Data :: any()}]
              ) -> {ok, jesse:json_term()}
                 | no_return().
validate(JsonSchema, Value, Options) ->
  State    = new_state(JsonSchema, Options),
  NewState = check_value(Value, unwrap(JsonSchema), State),
  {result(NewState), Value}.

result(State) ->
  case State#state.error_list of
    [] -> ok;
    _  -> throw(State#state.error_list)
  end.

%% @doc Returns value of "id" field from json object `Schema', assuming that
%% the given json object has such a field, otherwise an exception
%% will be thrown.
-spec get_schema_id(Schema :: jesse:json_term()) -> string().
get_schema_id(Schema) ->
  case get_value(?ID, Schema) of
    ?not_found -> throw({schema_invalid, Schema, missing_id_field});
    Id         -> erlang:binary_to_list(Id)
  end.

%% @doc A naive check if the given data is a json object.
%% Supports two main formats of json representation:
%% 1) mochijson2 format (`{struct, proplist()}')
%% 2) jiffy format (`{proplist()}')
%% 3) jsx format (`[{binary() | atom(), any()}]')
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(any()) -> boolean().
is_json_object({struct, Value}) when is_list(Value) -> true;
is_json_object({Value}) when is_list(Value)         -> true;
%% handle `jsx' empty objects
is_json_object([{}])                                -> true;
%% very naive check. checks only the first element.
is_json_object([{Key, _Value} | _])
  when is_binary(Key) orelse is_atom(Key)
       andalso Key =/= struct                       -> true;
is_json_object(_)                                   -> false.

%%% Internal functions

%% @doc Adds Property to the current path and checks the value
%% using check_value/3
%% @private
check_value(Property, Value, Attrs, State) ->
  %% Add Property to path
  State1 = add_to_path(State, Property),
  State2 = check_value(Value, Attrs, State1),
  %% Reset path again
  remove_last_from_path(State2).

%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
%% @private
check_value(Value, [{?TYPE, Type} | Attrs], State) ->
  NewState = check_type(Value, Type, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PROPERTIES, Properties} | Attrs], State) ->
  NewState = case is_json_object(Value) of
               true  -> check_properties( Value
                                        , unwrap(Properties)
                                        , State
                                        );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?PATTERNPROPERTIES, PatternProperties} | Attrs]
           , State
           ) ->
  NewState = case is_json_object(Value) of
               true  -> check_pattern_properties( Value
                                                , PatternProperties
                                                , State
                                                );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?ADDITIONALPROPERTIES, AdditionalProperties} | Attrs]
           , State
           ) ->
  NewState = case is_json_object(Value) of
               true  -> check_additional_properties( Value
                                                   , AdditionalProperties
                                                   , State
                                                   );
               false -> State
       end,
  check_value(Value, Attrs, NewState);

check_value( Value
             , [{?MINPROPERTIES, MinProperties} | Attrs]
             , State
             ) ->
  NewState = case is_json_object(Value) of
               'true'  -> check_min_properties(Value, MinProperties, State);
               'false' -> State
             end,
  check_value(Value, Attrs, NewState);

check_value(Value, [{?ITEMS, Items} | Attrs], State) ->
  NewState = case is_array(Value) of
               true  -> check_items(Value, Items, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(Value, [{?REQUIRED, _Required} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?DEPENDENCIES, Dependencies} | Attrs], State) ->
  NewState = case is_json_object(Value) of
               true  -> check_dependencies(Value, Dependencies, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINIMUM, Minimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 ExclusiveMinimum = get_value( ?EXCLUSIVEMINIMUM
                                             , get_current_schema(State)
                                             ),
                 check_minimum(Value, Minimum, ExclusiveMinimum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXIMUM, Maximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 ExclusiveMaximum = get_value( ?EXCLUSIVEMAXIMUM
                                             , get_current_schema(State)
                                             ),
                 check_maximum(Value, Maximum, ExclusiveMaximum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMINIMUM, _ExclusiveMinimum} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMAXIMUM, _ExclusiveMaximum} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?MINITEMS, MinItems} | Attrs], State) ->
  NewState = case is_array(Value) of
               true  -> check_min_items(Value, MinItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], State) ->
  NewState = case is_array(Value) of
               true  -> check_max_items(Value, MaxItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], State) ->
  NewState = case is_array(Value) of
               true  -> check_unique_items(Value, Uniqueitems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PATTERN, Pattern} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_pattern(Value, Pattern, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINLENGTH, MinLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_min_length(Value, MinLength, State);
               false -> State
  end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXLENGTH, MaxLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_max_length(Value, MaxLength, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ENUM, Enum} | Attrs], State) ->
  NewState = check_enum(Value, Enum, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?FORMAT, Format} | Attrs], State) ->
  NewState = check_format(Value, Format, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DIVISIBLEBY, DivisibleBy} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_divisible_by(Value, DivisibleBy, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DISALLOW, Disallow} | Attrs], State) ->
  NewState = check_disallow(Value, Disallow, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?EXTENDS, Extends} | Attrs], State) ->
  NewState = check_extends(Value, Extends, State),
  check_value(Value, Attrs, NewState);
check_value(_Value, [], State) ->
  State;
check_value(Value, [_Attr | Attrs], State) ->
  check_value(Value, Attrs, State).

%% @doc 5.1.  type
%%
%% This attribute defines what the primitive type or the schema of the
%% instance MUST be in order to validate.  This attribute can take one
%% of two forms:
%% <dl>
%% <dt>Simple Types</dt>
%%  <dd>A string indicating a primitive or simple type. The
%%    following are acceptable string values:
%%    <dl>
%%    <dt>string</dt>  <dd>Value MUST be a string.</dd>
%%
%%    <dt>number</dt>  <dd>Value MUST be a number, floating point numbers are
%%       allowed.</dd>
%%
%%    <dt>integer</dt>  <dd>Value MUST be an integer, no floating point numbers
%%       are allowed.  This is a subset of the number type.</dd>
%%
%%    <dt>boolean</dt>  <dd>Value MUST be a boolean.</dd>
%%
%%    <dt>object</dt>  <dd>Value MUST be an object.</dd>
%%
%%    <dt>array</dt>  <dd>Value MUST be an array.</dd>
%%
%%    <dt>null</dt>  <dd>Value MUST be null.  Note this is mainly for purpose of
%%       being able use union types to define nullability.  If this type
%%       is not included in a union, null values are not allowed (the
%%       primitives listed above do not allow nulls on their own).</dd>
%%
%%    <dt>any</dt>  <dd>Value MAY be of any type including null.</dd>
%%
%%    If the property is not defined or is not in this list,
%%    then any type of value is acceptable.  Other type values MAY be used for
%%    custom purposes, but minimal validators of the specification
%%    implementation can allow any instance value on unknown type
%%    values.
%%    </dl>
%%  </dd>
%% <dt>Union Types</dt>
%%  <dd>An array of two or more simple type definitions.  Each
%%     item in the array MUST be a simple type definition or a schema.
%%     The instance value is valid if it is of the same type as one of
%%     the simple type definitions, or valid by one of the schemas, in
%%     the array.</dd>
%% </dl>
%%  For example, a schema that defines if an instance can be a string or
%%  a number would be:
%%
%%  {"type":["string","number"]}
%% @private
check_type(Value, Type, State) ->
  case is_type_valid(Value, Type) of
    true  -> State;
    false -> wrong_type(Value, State)
  end.

is_type_valid(Value, ?STRING)   -> is_binary(Value);
is_type_valid(Value, ?NUMBER)   -> try_converting(Value, fun wh_util:to_number/1, fun erlang:is_number/1);
is_type_valid(Value, ?INTEGER)  -> try_converting(Value, fun wh_util:to_integer/1, fun erlang:is_integer/1);
is_type_valid(Value, ?BOOLEAN)  -> wh_util:is_boolean(Value);
is_type_valid(Value, ?OBJECT)   -> is_json_object(Value);
is_type_valid(Value, ?ARRAY)    -> is_array(Value);
is_type_valid(Value, ?NULL)     -> is_null(Value);
is_type_valid(_Value, ?ANY)     -> 'true';
is_type_valid(Value, UnionType) ->
  case is_array(UnionType) of
    true  -> check_union_type(Value, UnionType);
    false -> true
  end.

try_converting(Value, CastFun, ValidatorFun) ->
  try CastFun(Value) of
      Casted -> ValidatorFun(Casted)
  catch
    _E:_R -> 'false'
  end.

%% @private
check_union_type(Value, UnionType) ->
  lists:any( fun(Type) ->
                 try
                   case is_json_object(Type) of
                     true  ->
                       %% case when there's a schema in the array,
                       %% then we need to validate against
                       %% that schema
                       NewState = new_state(Type, []),
                       _ = check_value(Value, unwrap(Type), NewState),
                       true;
                     false ->
                       is_type_valid(Value, Type)
                   end
                 catch
                   throw:[{?data_invalid, _, _, _, _} | _] -> false;
                   throw:[{?schema_invalid, _, _} | _]  -> false
                 end
             end
           , UnionType
           ).

%% @private
wrong_type(Value, State) ->
  handle_data_invalid(?wrong_type, Value, State).

%% @doc 5.2.  properties
%%
%% This attribute is an object with property definitions that define the
%% valid values of instance object property values.  When the instance
%% value is an object, the property values of the instance object MUST
%% conform to the property definitions in this object.  In this object,
%% each property definition's value MUST be a schema, and the property's
%% name MUST be the name of the instance property that it defines.  The
%% instance property value MUST be valid according to the schema from
%% the property definition.  Properties are considered unordered, the
%% order of the instance properties MAY be in any order.
%% @private
check_properties(Value, Properties, State) ->
  TmpState
    = lists:foldl( fun({PropertyName, PropertySchema}, CurrentState) ->
                       case get_value(PropertyName, Value) of
                         ?not_found ->
%% @doc 5.7.  required
%%
%% This attribute indicates if the instance must have a value, and not
%% be undefined.  This is false by default, making the instance
%% optional.
%% @end
                           case get_value(?REQUIRED, PropertySchema) of
                             true ->
                               handle_data_invalid( {?missing_required_property
                                                     , PropertyName}
                                                   , Value
                                                   , CurrentState);
                             _    ->
                               CurrentState
                           end;
                         Property ->
                           NewState = set_current_schema( CurrentState
                                                        , PropertySchema
                                                        ),
                           check_value( PropertyName
                                      , Property
                                      , unwrap(PropertySchema)
                                      , NewState
                                      )
                       end
                   end
                 , State
                 , Properties
                 ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc 5.3.  patternProperties
%%
%% This attribute is an object that defines the schema for a set of
%% property names of an object instance.  The name of each property of
%% this attribute's object is a regular expression pattern in the ECMA
%% 262/Perl 5 format, while the value is a schema.  If the pattern
%% matches the name of a property on the instance object, the value of
%% the instance's property MUST be valid against the pattern name's
%% schema value.
%% @private
check_pattern_properties(Value, PatternProperties, State) ->
  P1P2 = [{P1, P2} || P1 <- unwrap(Value), P2  <- unwrap(PatternProperties)],
  TmpState = lists:foldl( fun({Property, Pattern}, CurrentState) ->
                              check_match(Property, Pattern, CurrentState)
                          end
                        , State
                        , P1P2
                        ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @private
check_match({PropertyName, PropertyValue}, {Pattern, Schema}, State) ->
  case re:run(PropertyName, Pattern, [{capture, none}]) of
    match   ->
      check_value( PropertyName
                 , PropertyValue
                 , unwrap(Schema)
                 , set_current_schema(State, Schema)
                 );
    nomatch ->
      State
  end.

%% @doc 5.4.  additionalProperties
%%
%% This attribute defines a schema for all properties that are not
%% explicitly defined in an object type definition.  If specified,
%% the value MUST be a schema or a boolean.  If false is provided,
%% no additional properties are allowed beyond the properties defined in
%% the schema.  The default value is an empty schema which allows any
%% value for additional properties.
%% @private
check_additional_properties(Value, false, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      lists:foldl( fun({Property, _}, State1) ->
                       State2
                         = handle_data_invalid( ?no_extra_properties_allowed
                                              , Value
                                              , add_to_path(State1, Property)
                                              ),
                       remove_last_from_path(State2)
                   end
                 , State
                 , Extras
                 )
  end;
check_additional_properties(_Value, true, State) ->
  State;
check_additional_properties(Value, AdditionalProperties, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      TmpState
        = lists:foldl( fun({ExtraName, Extra}, CurrentState) ->
                           NewState = set_current_schema( CurrentState
                                                        , AdditionalProperties
                                                        ),
                           check_value( ExtraName
                                      , Extra
                                      , unwrap(AdditionalProperties)
                                      , NewState
                                      )
                       end
                     , State
                     , Extras
                     ),
      set_current_schema(TmpState, JsonSchema)
  end.

%% @doc Returns the additional properties as a list of pairs containing the name
%% and the value of all properties not covered by Properties
%% or PatternProperties.
%% @private
get_additional_properties(Value, Properties, PatternProperties) ->
  ValuePropertiesNames  = [Name || {Name, _} <- unwrap(Value)],
  SchemaPropertiesNames = [Name || {Name, _} <- unwrap(Properties)],
  Patterns    = [Pattern || {Pattern, _} <- unwrap(PatternProperties)],
  ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
  ExtraNames  = lists:foldl( fun(Pattern, ExtraAcc) ->
                                 filter_extra_names(Pattern, ExtraAcc)
                             end
                           , ExtraNames0
                           , Patterns
                           ),
  lists:map(fun(Name) -> {Name, get_value(Name, Value)} end, ExtraNames).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case re:run(ExtraName, Pattern, [{capture, none}]) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc 5.5.  items
%%
%% This attribute defines the allowed items in an instance array,
%% and MUST be a schema or an array of schemas.  The default value is an
%% empty schema which allows any value for items in the instance array.
%%
%% When this attribute value is a schema and the instance value is an
%% array, then all the items in the array MUST be valid according to the
%% schema.
%%
%% When this attribute value is an array of schemas and the instance
%% value is an array, each position in the instance array MUST conform
%% to the schema in the corresponding position for this array.  This
%% called tuple typing.  When tuple typing is used, additional items are
%% allowed, disallowed, or constrained by the "additionalItems"
%% (Section 5.6) attribute using the same rules as
%% "additionalProperties" (Section 5.4) for objects.
%% @private
check_items(Value, Items, State) ->
  case is_json_object(Items) of
    true ->
      {_, TmpState} = lists:foldl( fun(Item, {Index, CurrentState}) ->
                                       { Index + 1
                                       , check_value( Index
                                                    , Item
                                                    , unwrap(Items)
                                                    , CurrentState
                                                    )
                                       }
                                   end
                                 , {0, set_current_schema(State, Items)}
                                 , Value
                                 ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Items) ->
      check_items_array(Value, Items, State);
    _ ->
      handle_schema_invalid({?wrong_type_items, Items}, State)
  end.

%% @private
check_items_array(Value, Items, State) ->
  JsonSchema = get_current_schema(State),
  case length(Value) - length(Items) of
    0 ->
      check_items_fun(lists:zip(Value, Items), State);
    NExtra when NExtra > 0 ->
%% @doc 5.6.  additionalItems
%%
%% This provides a definition for additional items in an array instance
%% when tuple definitions of the items is provided.  This can be false
%% to indicate additional items in the array are not allowed, or it can
%% be a schema that defines the schema of the additional items.
%% @end
      case get_value(?ADDITIONALITEMS, JsonSchema) of
        ?not_found -> State;
        true       -> State;
        false      ->
          handle_data_invalid(?no_extra_items_allowed, Value, State);
        AdditionalItems ->
          ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
          Tuples = lists:zip(Value, lists:append(Items, ExtraSchemas)),
          check_items_fun(Tuples, State)
      end;
    NExtra when NExtra < 0 ->
      handle_data_invalid(?not_enought_items, Value, State)
  end.

%% @private
check_items_fun(Tuples, State) ->
  {_, TmpState} = lists:foldl( fun({Item, Schema}, {Index, CurrentState}) ->
                                 NewState = set_current_schema( CurrentState
                                                              , Schema
                                                              ),
                                 { Index + 1
                                 , check_value(Index, Item, unwrap(Schema),
                                               NewState)
                                 }
                               end
                             , {0, State}
                             , Tuples
                             ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc 5.8.  dependencies
%%
%% This attribute is an object that defines the requirements of a
%% property on an instance object.  If an object instance has a property
%% with the same name as a property in this attribute's object, then the
%% instance must be valid against the attribute's property value
%% (hereafter referred to as the "dependency value").
%%
%% The dependency value can take one of two forms:
%% <dl>
%% <dt>Simple Dependency</dt>  <dd>If the dependency value is a string,
%%    then the instance object MUST have a property with the same name as the
%%    dependency value.  If the dependency value is an array of strings,
%%    then the instance object MUST have a property with the same name
%%    as each string in the dependency value's array.</dd>
%%
%% <dt>Schema Dependency</dt>  <dd>If the dependency value is a schema, then the
%%    instance object MUST be valid against the schema.</dd>
%% </dl>
%% @private
check_dependencies(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, DependencyValue}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> check_dependency_value( Value
                                                         , DependencyValue
                                                         , CurrentState
                                                         )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @private
check_dependency_value(Value, Dependency, State) when is_binary(Dependency) ->
  case get_value(Dependency, Value) of
    ?not_found ->
      handle_data_invalid({?missing_dependency, Dependency}, Value, State);
    _          ->
      State
  end;
check_dependency_value(Value, Dependency, State) ->
  case is_json_object(Dependency) of
    true ->
      TmpState = check_value( Value
                            , unwrap(Dependency)
                            , set_current_schema(State, Dependency)
                            ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Dependency) ->
      check_dependency_array(Value, Dependency, State);
    _ ->
      handle_schema_invalid({?wrong_type_dependency, Dependency}, State)
  end.

%% @private
check_dependency_array(Value, Dependency, State) ->
  lists:foldl( fun(PropertyName, CurrentState) ->
                   check_dependency_value(Value, PropertyName, CurrentState)
               end
             , State
             , Dependency
             ).

%% @doc 5.9.  minimum
%%
%% This attribute defines the minimum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_minimum(Value, Minimum, ExclusiveMinimum, State) ->
%% @doc 5.11.  exclusiveMinimum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "minimum" attribute.  This is false by default, meaning the instance
%% value can be greater then or equal to the minimum value.
%% @end
  Result = case ExclusiveMinimum of
             true -> Value > Minimum;
             _    -> Value >= Minimum
           end,
  case Result of
    true  -> State;
    false ->
      handle_data_invalid(?not_minimum, Value, State)
  end.

%%% @doc 5.10.  maximum
%%
%% This attribute defines the maximum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_maximum(Value, Maximum, ExclusiveMaximum, State) ->
%% @doc 5.12.  exclusiveMaximum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "maximum" attribute.  This is false by default, meaning the instance
%% value can be less then or equal to the maximum value.
%% @end
  Result = case ExclusiveMaximum of
             true -> Value < Maximum;
             _    -> Value =< Maximum
           end,
  case Result of
    true  -> State;
    false ->
      handle_data_invalid(?not_maximum, Value, State)
  end.

%% @doc 5.13.  minItems
%%
%% This attribute defines the minimum number of values in an array when
%% the array is the instance value.
%% @private
check_min_items(Value, MinItems, State) when length(Value) >= MinItems ->
  State;
check_min_items(Value, _MinItems, State) ->
  handle_data_invalid(?wrong_min_items, Value, State).

%% @doc 5.4.2 minProperties
check_min_properties(Value, MinProperties, State) ->
  case length(wh_json:get_keys(Value)) >= MinProperties of
    'true' -> State;
    'false' -> handle_data_invalid(?wrong_min_properties, Value, State)
  end.

%% @doc 5.14.  maxItems
%%
%% This attribute defines the maximum number of values in an array when
%% the array is the instance value.
%% @private
check_max_items(Value, MaxItems, State) when length(Value) =< MaxItems ->
  State;
check_max_items(Value, _MaxItems, State) ->
  handle_data_invalid(?wrong_max_items, Value, State).

%% @doc 5.15.  uniqueItems
%%
%% This attribute indicates that all items in an array instance MUST be
%% unique (contains no two identical values).
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
%% @private
check_unique_items([], 'true', State) ->
  State;
check_unique_items(Value, true, State) ->
  try
    lists:foldl( fun(_Item, []) ->
                     ok;
                    (Item, RestItems) ->
                     lists:foreach( fun(ItemFromRest) ->
                                        case is_equal(Item, ItemFromRest) of
                                          true  ->
                                            throw({?not_unique, Item});
                                          false -> ok
                                        end
                                    end
                                  , RestItems
                                  ),
                     tl(RestItems)
                 end
               , tl(Value)
               , Value
               ),
    State
  catch
    throw:ErrorInfo -> handle_data_invalid(ErrorInfo, Value, State)
  end.

%% @doc 5.16.  pattern
%% When the instance value is a string, this provides a regular
%% expression that a string instance MUST match in order to be valid.
%% Regular expressions SHOULD follow the regular expression
%% specification from ECMA 262/Perl 5
%% @private
check_pattern(Value, Pattern, State) ->
  case re:run(Value, Pattern, [{capture, none}]) of
    match   -> State;
    nomatch ->
      handle_data_invalid(?no_match, Value, State)
  end.

%% @doc 5.17.  minLength
%%
%% When the instance value is a string, this defines the minimum length
%% of the string.
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_min_length, Value, State)
  end.

%% @doc 5.18.  maxLength
%%
%% When the instance value is a string, this defines the maximum length
%% of the string.
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_max_length, Value, State)
  end.

%% @doc 5.19.  enum
%%
%% This provides an enumeration of all possible values that are valid
%% for the instance property.  This MUST be an array, and each item in
%% the array represents a possible value for the instance value.  If
%% this attribute is defined, the instance value MUST be one of the
%% values in the array in order for the schema to be valid.  Comparison
%% of enum values uses the same algorithm as defined in "uniqueItems"
%% (Section 5.15).
%% @private
check_enum(Value, Enum, State) ->
  IsValid = lists:any( fun(ExpectedValue) ->
                           is_equal(Value, ExpectedValue)
                       end
                     , Enum
                     ),
  case IsValid of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_enum, Value, State)
  end.

%% TODO:
check_format(_Value, _Format, State) ->
%% 'date-time': /^\d{4}-(?:0[0-9]{1}|1[0-2]{1})-[0-9]{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/,
%% 'date': /^\d{4}-(?:0[0-9]{1}|1[0-2]{1})-[0-9]{2}$/,
%% 'time': /^\d{2}:\d{2}:\d{2}$/,

%% 'email': /^(?:[\w\!\#\$\%\&\'\*\+\-\/\=\?\^\`\{\|\}\~]+\.)*[\w\!\#\$\%\&\'\*\+\-\/\=\?\^\`\{\|\}\~]+@(?:(?:(?:[a-zA-Z0-9](?:[a-zA-Z0-9\-](?!\.)){0,61}[a-zA-Z0-9]?\.)+[a-zA-Z0-9](?:[a-zA-Z0-9\-](?!$)){0,61}[a-zA-Z0-9]?)|(?:\[(?:(?:[01]?\d{1,2}|2[0-4]\d|25[0-5])\.){3}(?:[01]?\d{1,2}|2[0-4]\d|25[0-5])\]))$/,
%% 'ip-address': /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/,
%% 'ipv6': /^\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\s*$/,
%% 'uri': /^[a-zA-Z][a-zA-Z0-9+-.]*:[^\s]*$/,

%% 'color': /(#?([0-9A-Fa-f]{3,6})\b)|(aqua)|(black)|(blue)|(fuchsia)|(gray)|(green)|(lime)|(maroon)|(navy)|(olive)|(orange)|(purple)|(red)|(silver)|(teal)|(white)|(yellow)|(rgb\(\s*\b([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b\s*,\s*\b([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b\s*,\s*\b([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b\s*\))|(rgb\(\s*(\d?\d%|100%)+\s*,\s*(\d?\d%|100%)+\s*,\s*(\d?\d%|100%)+\s*\))/,

%% 'host-name': /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/,

%% 'alpha': /^[a-zA-Z]+$/,
%% 'alphanumeric': /^[a-zA-Z0-9]+$/,
%% 'utc-millisec': function (input) {
%%   return (typeof input === 'string') && parseFloat(input) === parseInt(input, 10) && !isNaN(input);
%% },
%% 'regex': function (input) {
%%   var result = true;
%%   try {
%%     new RegExp(input);
%%   } catch (e) {
%%     result = false;
%%   }
%%   return result;
%% },
%% 'style': /\s*(.+?):\s*([^;]+);?/g,
%% 'phone': /^\+(?:[0-9] ?){6,14}[0-9]$/
  State.

%% @doc 5.24.  divisibleBy
%%
%% This attribute defines what value the number instance must be
%% divisible by with no remainder (the result of the division must be an
%% integer.)  The value of this attribute SHOULD NOT be 0.
%% @private
check_divisible_by(Value, 0, State) ->
  handle_data_invalid(?not_divisible, Value, State);
check_divisible_by(Value, DivisibleBy, State) ->
  Result = (Value / DivisibleBy - trunc(Value / DivisibleBy)) * DivisibleBy,
  case Result of
    0.0 ->
      State;
    _   ->
      handle_data_invalid(?not_divisible, Value, State)
  end.

%% @doc 5.25.  disallow
%%
%% This attribute takes the same values as the "type" attribute, however
%% if the instance matches the type or if this value is an array and the
%% instance matches any type or schema in the array, then this instance
%% is not valid.
%% @private
check_disallow(Value, Disallow, State) ->
  try check_type(Value, Disallow, new_state(Disallow, [])) of
      _ ->
      handle_data_invalid(?not_allowed, Value, State)
  catch
    throw:[{?data_invalid, _, _, _, _} | _] -> State
  end.

%% @doc 5.26.  extends
%%
%% The value of this property MUST be another schema which will provide
%% a base schema which the current schema will inherit from.  The
%% inheritance rules are such that any instance that is valid according
%% to the current schema MUST be valid according to the referenced
%% schema.  This MAY also be an array, in which case, the instance MUST
%% be valid for all the schemas in the array.  A schema that extends
%% another schema MAY define additional attributes, constrain existing
%% attributes, or add other constraints.
%% @private
check_extends(Value, Extends, State) ->
  case is_json_object(Extends) of
    true  ->
      check_value(Value, unwrap(Extends), set_current_schema(State, Extends));
    false ->
      case is_list(Extends) of
        true  -> check_extends_array(Value, Extends, State);
        false -> State %% TODO: implement handling of $ref
      end
  end.

%% @private
check_extends_array(Value, Extends, State) ->
  lists:foldl( fun(SchemaKey, CurrentState) ->
                   check_extends(Value, SchemaKey, CurrentState)
               end
             , State
             , Extends
             ).

%%=============================================================================
%% @doc Returns `true' if given values (instance) are equal, otherwise `false'
%% is returned.
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
%% @private
is_equal(Value1, Value2) ->
  case is_json_object(Value1) andalso is_json_object(Value2) of
    true  -> compare_objects(Value1, Value2);
    false -> case is_list(Value1) andalso is_list(Value2) of
               true  -> compare_lists(Value1, Value2);
               false -> Value1 =:= Value2
             end
  end.

%% @private
compare_lists(Value1, Value2) ->
  case length(Value1) =:= length(Value2) of
    true  -> compare_elements(Value1, Value2);
    false -> false
  end.

%% @private
compare_elements(Value1, Value2) ->
  lists:all( fun({Element1, Element2}) ->
                 is_equal(Element1, Element2)
             end
           , lists:zip(Value1, Value2)
           ).

%% @private
compare_objects(Value1, Value2) ->
  case length(unwrap(Value1)) =:= length(unwrap(Value2)) of
    true  -> compare_properties(Value1, Value2);
    false -> false
  end.

%% @private
compare_properties(Value1, Value2) ->
  lists:all( fun({PropertyName1, PropertyValue1}) ->
                 case get_value(PropertyName1, Value2) of
                   ?not_found     -> false;
                   PropertyValue2 -> is_equal(PropertyValue1, PropertyValue2)
                 end
             end
           , unwrap(Value1)
           ).

%%=============================================================================
%% @private
new_state(JsonSchema, Options) ->
  ErrorHandler  = props:get_value( error_handler
                                     , Options
                                     , fun default_error_handler/3
                                     ),
  AllowedErrors = props:get_value( allowed_errors
                                     , Options
                                     , 0
                                     ),
  #state{ current_schema  = JsonSchema
        , current_path    = []
        , original_schema = JsonSchema
        , allowed_errors  = AllowedErrors
        , error_list      = []
        , error_handler   = ErrorHandler
        }.

%% @private
get_current_schema(#state{current_schema = CurrentSchema}) ->
  CurrentSchema.

%% @private
set_current_schema(State, NewSchema) ->
  State#state{current_schema = NewSchema}.

%% get_original_schema(#state{original_schema = OriginalSchema}) ->
%%   OriginalSchema.

%% @private
get_error_handler(#state{error_handler = ErrorHandler}) ->
  ErrorHandler.

%% @private
get_error_list(#state{error_list = ErrorList}) ->
  ErrorList.

%% @private
set_error_list(State, ErrorList) ->
  State#state{error_list = ErrorList}.

%% @private
get_allowed_errors(#state{allowed_errors = AllowedErrors}) ->
  AllowedErrors.

%% @private
add_to_path(State, Property) ->
  CurrentPath = State#state.current_path,
  State#state{current_path = [Property | CurrentPath]}.

%% @private
remove_last_from_path(State = #state{current_path = [_Property | Path]}) ->
  State#state{current_path = Path}.

%% @private
handle_data_invalid(Info, Value, State) ->
  Error = { ?data_invalid
          , State#state.current_schema
          , Info
          , Value
          , lists:reverse(State#state.current_path)
          },
  handle_error(Error, State).

%% @private
handle_schema_invalid(Info, State) ->
  handle_error({?schema_invalid, State#state.current_schema, Info}, State).

%% @private
handle_error(Error, State) ->
  ErrorHandler  = get_error_handler(State),
  ErrorList     = get_error_list(State),
  AllowedErrors = get_allowed_errors(State),
  set_error_list(State, ErrorHandler(Error, ErrorList, AllowedErrors)).

%% @private
default_error_handler(Error, ErrorList, AllowedErrors) ->
  case AllowedErrors > length(ErrorList) orelse AllowedErrors =:= 'infinity' of
    true  -> [Error | ErrorList];
    false -> throw([Error | ErrorList])
  end.

%%=============================================================================
%% @private
get_value(Key, Schema) ->
  jesse_json_path:value(Key, Schema, ?not_found).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%% @private
empty_if_not_found(?not_found) -> [];
empty_if_not_found(Value)      -> Value.

%%=============================================================================
%% @doc This check is needed since objects in `jsx' are lists (props)
%% @private
is_array(Value) when is_list(Value) -> not is_json_object(Value);
is_array(_)                         -> false.

is_null(null)   -> true;
is_null(_Value) -> false.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
