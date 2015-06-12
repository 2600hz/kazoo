%%%=============================================================================
%% Copyright 2014 Klarna AB
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

-module(jesse_validator_draft3).

%% API
-export([ check_value/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%%% API
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
-spec check_value( Value      :: any()
                 , JsonSchema :: jesse:json_term()
                 , State      :: jesse_state:state()
                 ) -> jesse_state:state() | no_return().
check_value(Value, [{?TYPE, Type} | Attrs], State) ->
  NewState = check_type(Value, Type, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PROPERTIES, Properties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
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
  NewState = case jesse_lib:is_json_object(Value) of
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
  NewState = case jesse_lib:is_json_object(Value) of
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
  NewState = case jesse_lib:is_json_object(Value) of
               'true'  -> check_min_properties(Value, MinProperties, State);
               'false' -> State
             end,
  check_value(Value, Attrs, NewState);

check_value(Value, [{?ITEMS, Items} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
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
  NewState = case jesse_lib:is_json_object(Value) of
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
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_min_items(Value, MinItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_max_items(Value, MaxItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
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
check_value(Value, [{?_REF, RefSchemaURI} | Attrs], State) ->
  NewState = check_ref(Value, RefSchemaURI, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [_Attr | Attrs], State) ->
  check_value(Value, Attrs, State).

%%% Internal functions
%% @doc Adds Property to the current path and checks the value
%% using jesse_schema_validator:validate_with_state/3.
%% @private
check_value(Property, Value, Attrs, State) ->
  %% Add Property to path
  State1 = jesse_state:add_to_path(State, Property),
  State2 = jesse_schema_validator:validate_with_state(Attrs, Value, State1),
  %% Reset path again
  jesse_state:remove_last_from_path(State2).

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
  case is_type_valid(Value, Type, State) of
    true  -> State;
    false -> wrong_type(Value, State)
  end.

%% @private
is_type_valid(Value, ?STRING, _State)  -> is_binary(Value);
is_type_valid(Value, ?NUMBER, _State)  -> try_converting(Value, fun wh_util:to_number/1, fun erlang:is_number/1);
is_type_valid(Value, ?INTEGER, _State) -> try_converting(Value, fun wh_util:to_integer/1, fun erlang:is_integer/1);
is_type_valid(Value, ?BOOLEAN, _State) -> wh_util:is_boolean(Value);
is_type_valid(Value, ?OBJECT, _State)  -> jesse_lib:is_json_object(Value);
is_type_valid(Value, ?ARRAY, _State)   -> jesse_lib:is_array(Value);
is_type_valid(Value, ?NULL, _State)    -> jesse_lib:is_null(Value);
is_type_valid(_Value, ?ANY, _State)    -> true;
is_type_valid(Value, UnionType, State) ->
  case jesse_lib:is_array(UnionType) of
    true  -> check_union_type(Value, UnionType, State);
    false -> true
  end.

try_converting(Value, CastFun, ValidatorFun) ->
  try CastFun(Value) of
      Casted ->
       ValidatorFun(Casted)
  catch
    _E:_R ->
       'false'
  end.

%% @private
check_union_type(Value, UnionType, State) ->
  lists:any( fun(Type) ->
                 try
                   case jesse_lib:is_json_object(Type) of
                     true  ->
                       %% case when there's a schema in the array,
                       %% then we need to validate against that schema
                       NewState = jesse_state:new(Type, []),
                       _ = jesse_schema_validator:validate_with_state( Type
                                                                     , Value
                                                                     , NewState
                                                                     ),
                       true;
                     false ->
                       is_type_valid(Value, Type, State)
                   end
                 catch
                   %% FIXME: don't like to have these error related
                   %% macros here.
                   throw:[{?data_invalid, _, _, _, _} | _] -> false;
                   throw:[{?schema_invalid, _, _} | _]     -> false
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
                                                   , jesse_state:set_current_schema(CurrentState, PropertySchema));
                             _    ->
                               CurrentState
                           end;
                         Property ->
                           NewState = set_current_schema( CurrentState
                                                        , PropertySchema
                                                        ),
                           check_value( PropertyName
                                      , Property
                                      , PropertySchema
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
                 , Schema
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
                                      , AdditionalProperties
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
  case jesse_lib:is_json_object(Items) of
    true ->
      {_, TmpState} = lists:foldl( fun(Item, {Index, CurrentState}) ->
                                       { Index + 1
                                       , check_value( Index
                                                    , Item
                                                    , Items
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
                                 , check_value(Index, Item, Schema, NewState)
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
                                                         , DependencyName
                                                         , DependencyValue
                                                         , CurrentState
                                                         )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @private
check_dependency_value(Value, _DependencyName, Dependency, State)
  when is_binary(Dependency) ->
  case get_value(Dependency, Value) of
    ?not_found ->
      handle_data_invalid({?missing_dependency, Dependency}, Value, State);
    _          ->
      State
  end;
check_dependency_value(Value, DependencyName, Dependency, State) ->
  case jesse_lib:is_json_object(Dependency) of
    true ->
      TmpState = check_value( DependencyName
                            , Value
                            , Dependency
                            , set_current_schema(State, Dependency)
                            ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Dependency) ->
      check_dependency_array(Value, DependencyName, Dependency, State);
    _ ->
      handle_schema_invalid({?wrong_type_dependency, Dependency}, State)
  end.

%% @private
check_dependency_array(Value, DependencyName, Dependency, State) ->
  lists:foldl( fun(PropertyName, CurrentState) ->
                   check_dependency_value( Value
                                         , DependencyName
                                         , PropertyName
                                         , CurrentState
                                         )
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
check_unique_items([], true, State) ->
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

check_format(_Value, _Format, State) ->
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
  try check_type(Value, Disallow, jesse_state:new(Disallow, [])) of
    _ -> handle_data_invalid(?not_allowed, Value, State)
  catch
    %% FIXME: don't like to have these error related macros
    %% here.
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
  case jesse_lib:is_json_object(Extends) of
    true  ->
      check_value(extends, Value, Extends, set_current_schema(State, Extends));
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

check_ref(Value, <<"#", LocalPath/binary>> = RefSchemaURI, State) ->
  Keys = binary:split(LocalPath, <<"/">>, ['global']),
  OriginalSchema = jesse_state:get_original_schema(State),

  case get_local_schema(Keys, OriginalSchema) of
    ?not_found ->
      handle_schema_invalid({'schema_unsupported', RefSchemaURI}, State);
    RefSchema ->
      do_ref_schema(Value, RefSchema, State)
  end;
check_ref(Value, RefSchemaURI, State) ->
  case jesse_state:find_schema(State, RefSchemaURI) of
    ?not_found ->
      handle_schema_invalid({'schema_unsupported', RefSchemaURI}, State);
    RefSchema ->
      do_ref_schema(Value, RefSchema, State)
  end.

get_local_schema([<<>> | Keys], Schema) ->
  get_local_schema(Keys, Schema);
get_local_schema([Key|Keys], Schema) ->
  SubSchema = get_value(Key, Schema),
  case jesse_lib:is_json_object(SubSchema) of
    true -> get_local_schema(Keys, SubSchema);
    false -> ?not_found
  end;
get_local_schema([], Schema) -> Schema.

do_ref_schema(Value, RefSchema, State) ->
  TmpState = check_value(Value, unwrap(RefSchema), set_current_schema(State, RefSchema)),
  set_current_schema(TmpState, get_current_schema(State)).

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
  case jesse_lib:is_json_object(Value1)
    andalso jesse_lib:is_json_object(Value2) of
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
%% Wrappers
%% @private
get_value(Key, Schema) ->
  jesse_json_path:value(Key, Schema, ?not_found).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%% @private
handle_data_invalid(Info, Value, State) ->
  jesse_error:handle_data_invalid(Info, Value, State).

%% @private
handle_schema_invalid(Info, State) ->
  jesse_error:handle_schema_invalid(Info, State).

%% @private
get_current_schema(State) ->
  jesse_state:get_current_schema(State).

%% @private
set_current_schema(State, NewSchema) ->
  jesse_state:set_current_schema(State, NewSchema).

%% @private
empty_if_not_found(Value) ->
  jesse_lib:empty_if_not_found(Value).

%% @private
add_to_path(State, Property) ->
  jesse_state:add_to_path(State, Property).

%% @private
remove_last_from_path(State) ->
  jesse_state:remove_last_from_path(State).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
