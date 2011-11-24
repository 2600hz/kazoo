%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Takes a JSON object and a JSON schema, and validates the object against
%%% the schema.
%%%
%%% Based on http://tools.ietf.org/html/draft-zyp-json-schema-03
%%% @end
%%% Created : 23 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_json_validator).

-export([is_valid/2]).

-compile([export_all]).

-include_lib("whistle/include/wh_types.hrl").

-type error_result() :: {ne_binary(), ne_binary()}.
-type error_results() :: [error_result(),...].
-type results() :: 'true' | error_results().

-define(SIMPLE_TYPES, [<<"string">>,<<"number">>,<<"integer">>,<<"boolean">>,<<"object">>
			   ,<<"array">>,<<"null">>,<<"any">>]).

%% Simplistic regex to match ISO-8601 string (in UTC, so TZ is Z for zulu)
%% Matches YYYY-MM-DDThh:mm:ssZ explicitly
-define(ISO_8601_REGEX, <<"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$">>).

%% match alphanumeric, '.', '+', and '_' 1 or more times, the '@', then alphanumeric, '-', and '.' 1 or more times,
%% ending with a '.', and then 1 or more alphanumeric, '_', or '-'
-define(EMAIL_REGEX, <<"^[[\:alnum\:].+_]{1,}[@][[\:alnum\:]-.]{1,}([.]([[\:alnum\:]_-]{1,}))$">>).
-define(HOSTNAME_REGEX, <<"^[[\:alnum\:]-.]{1,}([.]([[\:alnum\:]_-]{1,}))$">>).

%% Return true or [{JObjKey, ErrorMsg}]
-spec is_valid/2 :: (json_object(), json_object()) -> results().
is_valid(JObj, Schema) ->
    case is_valid_type(JObj, Schema) andalso is_valid_properties(JObj, Schema) of
	true -> true;
	false -> [{<<"root">>, <<"json object is not of a valid type">>}];
	Errors -> Errors
    end.

-spec is_valid_type/2 :: (json_object(), json_object()) -> results().
is_valid_type(JObj, Schema) ->
    case wh_json:get_value(<<"type">>, Schema, <<"any">>) of
	Union when is_list(Union) ->
	    case lists:any(fun(<<"any">>) -> true; (Type) -> wh_json:get_value(<<"type">>, JObj) =:= Type end, Union) of
		true -> true;
		false -> [{<<"type">>, list_to_binary([<<"json object is not one of type: ">>, wh_util:binary_join(Union, <<", ">>)])}]
	    end;
	<<"any">> -> true;
	Type ->
	    case wh_json:get_value(<<"type">>, JObj) =:= Type of
		true -> true;
		false ->  [{<<"type">>, list_to_binary([<<"json object is not of type: ">>, Type])}]
	    end
    end.

%% JObj = { ...,"name":"Mal Reynolds",...}
%% 
%% Schema = {
%%   ...
%%   "properties": {
%%     "name": {"type":"string"}
%%   },
%%   ...
%% }
-spec is_valid_properties/2 :: (json_object(), json_object()) -> results().
is_valid_properties(JObj, Schema) ->
    PropertiesJObj = wh_json:get_value(<<"properties">>, Schema, wh_json:new()),
    Properties = wh_json:to_proplist(PropertiesJObj),

    case [Failed  || {Key, AttributesJObj} <- Properties, (Failed=are_valid_attributes(JObj, Key, AttributesJObj)) =/= true] of
	[] -> true;
	Failed -> lists:flatten(Failed) %% compress nested proplists to one main proplist
    end.

%% JObj = {..., "name":"Mal Reynolds",...}
%% Key = "name"
%% AttributesJObj = {"type":"string"}
-spec are_valid_attributes/3 :: (json_object(), binary(), json_object()) -> results().
are_valid_attributes(JObj, Key, AttributesJObj) ->
    case wh_json:get_value(Key, JObj) of
	undefined ->
	    case is_required_attribute(AttributesJObj) of
		true -> [{Key, <<"Field is required but missing">>}];
		false -> true
	    end;
	Val ->
	    Attributes = wh_json:to_proplist(AttributesJObj),
	    case [Failed
		  || {AttributeKey, AttributeVal} <- Attributes,
		     (Failed=is_valid_attribute(AttributeKey, AttributeVal, Key, may_use_default(Val, AttributesJObj), AttributesJObj)) =/= true
		 ] of
		[] -> true;
		Failed -> Failed
	    end
    end.

%% 5.20 - If the value from JObj is undefined (the key doesn't exist), use the default defined in the schema (if exists)
may_use_default(undefined, AttributesJObj) ->
    case wh_json:get_value(<<"default">>, AttributesJObj) of
	undefined -> undefined;
	Default -> Default
    end;
may_use_default(Val, _) -> Val.


%% Section 5 of the spec
%% AttributeKey, AttributeVal, Key, Val, AttributesJObj
%% Some Attributes:
%% "type" : string | number | integer | float | boolean | array | object | null | any | user-defined
%%          The last two (any and user-defined) are automatically considered valid
%%
-spec is_valid_attribute/5 :: (ne_binary(), ne_binary() | json_object() | number()
			       ,ne_binary(), binary() | list() | json_object() | 'undefined' | number()
			       ,json_object()) -> results().
%% 5.1
is_valid_attribute(<<"type">>, Type, Key, Val, _AttrsJObj) ->
    is_valid_type(Key, Val, Type);

%% 5.2
is_valid_attribute(<<"properties">>, PropertiesSchema, Key, ValJObj, _AttrsJObj) ->
    case is_valid_properties(ValJObj, PropertiesSchema) of
	true -> true;
	Errors -> [ {Key, <<"failed to validate all the fields in this object">>} | Errors]
    end;

%% 5.3
is_valid_attribute(<<"patternProperties">>, _ItemsJObj, _Key, _ValJObj, _AttrsJObj) ->
    true; %% ignored for the moment

%% 5.4
is_valid_attribute(<<"additionalProperties">>, _ItemsJObj, _Key, _ValJObj, _AttrsJObj) ->
    true; %% ignored for the moment

%% 5.5
is_valid_attribute(<<"items">>, ItemsJObj, Key, ValJObj, _AttrsJObj) ->
    case are_valid_items(ValJObj, ItemsJObj) of
	true -> true;
	Errors -> [{Key, <<"failed to validate all items">>} | Errors]
    end;

%% 5.6
is_valid_attribute(<<"additionalItems">>, _ItemsJObj, _Key, _ValJObj, _AttrsJObj) ->
    true; %% ignored for the moment

%% 5.7
is_valid_attribute(<<"required">>, IsRequired, Key, Val, _AttrsJObj) ->
    case wh_util:is_true(IsRequired, false) of
	false -> true;
	true ->
	    case Val =:= undefined of
		false -> true;
		true -> {Key, <<"Field is required but missing">>}
	    end
    end;

%% 5.8
is_valid_attribute(<<"dependencies">>, _DepsJObj, _Key, _Val, _AttrsJObj) ->
    true; %% ignored for the moment

%% 5.9
is_valid_attribute(<<"minimum">>, Min, Key, Val, AttrsJObj) ->
    Exclusive = wh_json:is_true(<<"exclusiveMinimum">>, AttrsJObj, false), %% 5.11
    try {wh_util:to_number(Val), wh_util:to_number(Min), Exclusive} of
	{ValN, MinN, false} when ValN >= MinN -> true;
	{ValN, MinN, true} when ValN > MinN -> true;
	_ -> {Key, list_to_binary([<<"Value must be at least ">>, wh_util:to_binary(Min)])}
    catch
	error:badarg ->
	    {Key, <<"Either the value or the schema minimum for this key is not a number">>}
    end;

%% 5.10
is_valid_attribute(<<"maximum">>, Max, Key, Val, AttrsJObj) ->
    Exclusive = wh_json:is_true(<<"exclusiveMaximum">>, AttrsJObj, false), %% 5.12
    try {wh_util:to_number(Val), wh_util:to_number(Max), Exclusive} of
	{ValN, MaxN, false} when ValN =< MaxN -> true;
	{ValN, MaxN, true} when ValN < MaxN -> true;
	_ -> {Key, list_to_binary([<<"Value must be at most ">>, wh_util:to_binary(Max)])}
    catch
	error:badarg ->
	    {Key, <<"Either the value or the schema maximum for this key is not a number">>}
    end;

%% 5.13
is_valid_attribute(<<"minItems">>, Min, Key, Vals, _AttrsJObj) ->
    try {is_list(Vals), wh_util:to_integer(Min)} of
	{true, Int} when length(Vals) >= Int -> true;
	{false, _} -> {Key, <<"This is not a list">>};
	{true, _Int} -> {Key, list_to_binary([<<"The list is not at least ">>, wh_util:to_binary(Min), <<" items">>])}
    catch
	error:badarg ->
	    {Key, <<"Schema value for minItems not a number">>}
    end;

%% 5.14
is_valid_attribute(<<"maxItems">>, Max, Key, Vals, _AttrsJObj) ->
    try {is_list(Vals), wh_util:to_integer(Max)} of
	{true, Int} when length(Vals) =< Int -> true;
	{false, _} -> {Key, <<"This is not a list">>};
	{true, _Int} -> {Key, list_to_binary([<<"The list is more than ">>, wh_util:to_binary(Max), <<" items">>])}
    catch
	error:badarg ->
	    {Key, <<"Schema value for minItems not a number">>}
    end;

%% 5.15
is_valid_attribute(<<"uniqueItems">>, TryUnique, Key, Vals, _AttrsJObj) ->
    case {wh_util:is_true(TryUnique), is_list(Vals)} of
	{true, true} ->
	    case are_unique_items(Vals) of
		true -> true;
		Errors -> [{Key, <<"list of items are not unique">>} | Errors]
	    end;
	{false, _} -> {Key, <<"uniqueItems set improperly (should be set to 'true'">>};
	{true, false} -> {Key, <<"Expected a list; was disappoint">>}
    end;

%% 5.16
is_valid_attribute(<<"pattern">>, Pattern, Key, Val, _AttrsJObj) ->
    is_valid_pattern(Key, Val, Pattern);

%% 5.17
is_valid_attribute(<<"minLength">>, Min, Key, Val, _AttrsJObj) ->
    try {wh_util:to_integer(Min), is_binary(Val)} of
	{Int, true} when erlang:byte_size(Val) >= Int -> true;
	{_, false} -> {Key, <<"Value is not a string">>};
	{_Int, true} -> {Key, list_to_binary([<<"String must be at least ">>, wh_util:to_binary(Min), <<" characters">>])}
    catch
	error:badarg -> {Key, <<"Schema's minLength not an integer or value wasn't a string">>}
    end;

%% 5.18
is_valid_attribute(<<"maxLength">>, Max, Key, Val, _AttrsJObj) ->
    try {wh_util:to_integer(Max), is_binary(Val)} of
	{Int, true} when erlang:byte_size(Val) =< Int -> true;
	{_, false} -> {Key, <<"Value is not a string">>};
	{_Int, true} -> {Key, list_to_binary([<<"String must be at least ">>, wh_util:to_binary(Max), <<" characters">>])}
    catch
	error:badarg -> {Key, <<"Schema's maxLength not an integer or value wasn't a string">>}
    end;

%% 5.19
is_valid_attribute(<<"enum">>, Enums, Key, Val, _AttrsJObj) ->
    case is_list(Enums) of
	true ->
	    try lists:any(fun(Enum) -> are_same_items(Enum, Val) end) of
		false -> {Key, <<"value not found in enumerated list of values">>}
	    catch
		throw:{duplicate_found,_} -> true
	    end;
	false ->
	    {Key, <<"Schema enum is not a list of values">>}
    end;

%% 5.23
is_valid_attribute(<<"format">>, Format, Key, Val, _AttrsJObj) ->
    is_valid_format(Format, Key, Val);

%% 5.24
is_valid_attribute(<<"divisibleBy">>, DivBy, Key, Val, _AttrsJObj) ->
    try wh_util:to_number(DivBy) of
	0 -> {Key, <<"Trying to divide by 0">>};
	0.0 -> {Key, <<"Trying to divide by 0">>};
	Denominator ->
	    Numerator = wh_util:to_number(Val),
	    case Numerator rem Denominator of
		0 -> true;
		_ -> {Key, list_to_binary([<<"Value not divisible by ">>, wh_util:to_binary(DivBy)])}
	    end
    catch
	error:badarg -> {Key, <<"Either the numerator or the denominator in the schema is not a number">>}
    end;

%% 5.21, 5.22,  and unknown/unhandled attributes
is_valid_attribute(_,_,_,_,_) ->
    true. %% ignorable attribute, like 'title'

-spec are_unique_items/1 :: (list()) -> results().
are_unique_items(Vals) ->
    try lists:usort(Vals, fun are_same_items/2) of
	_ -> true
    catch
	throw:{duplicate_found, A} ->
	    [{<<"duplicate found">>, wh_util:to_binary(A)}]
    end.

%% will throw an exception if A and B are identical
-spec are_same_items/2 :: (term(), term()) -> false.
are_same_items(A, A) -> throw({duplicate_found, A});
are_same_items(A, B) ->
    Funs = [ fun are_null/2
	     ,fun are_boolean/2
	     ,fun are_numbers/2
	     %% ,fun are_strings/2 %% handled by first clause (A =:= A)
	     ,fun are_arrays/2
	     ,fun are_objects/2
	   ],
    lists:foldl(fun(F, _) -> F(A, B) end, false, Funs).

are_null(<<"null">>, <<"null">>=A) ->
    throw({duplicate_found, A});
are_null(null, null) ->
    throw({duplicate_found, <<"null">>});
are_null(_, _) ->
    false.

are_boolean(A, B) ->
    try {wh_util:to_boolean(A), wh_util:to_boolean(B)} of
	{C, C} -> throw({duplicate_found, C});
	_ -> false
    catch
	error:function_clause ->
	    false
    end.

are_numbers(A, B) ->
    try {wh_util:to_number(A), wh_util:to_number(B)} of
	{N, N} -> throw({duplicate_found, A});
	_ -> false
    catch
	error:badarg -> false
    end.

%% contains the same number of items, and each item in
%% the array is equal to the corresponding item in the other array
are_arrays([], []) -> throw({duplicate_found, <<"arrays are identical">>});
are_arrays([], [_|_]) -> false;
are_arrays([_|_], []) -> false;
are_arrays([A|As], [A|Bs]) ->
    are_arrays(As, Bs);
are_arrays(_, _) ->
    false.

%% contains the same property names, and each property
%% in the object is equal to the corresponding property in the other
%% object.
are_objects(A, B) ->
    %% Forall keys in A, A(Key) =:= B(Key), and vice versa
    %% if either is false, they aren't identical objects
    case lists:all(fun(AK) -> wh_json:get_value(AK, B) =:= wh_json:get_value(AK, A) end, wh_json:get_keys(A)) andalso
	lists:all(fun(BK) -> wh_json:get_value(BK, A) =:= wh_json:get_value(BK, B) end, wh_json:get_keys(B)) of
	true -> throw({duplicate_found, <<"objects match">>});
	false -> false
    end.

-spec is_valid_type/3 :: (ne_binary(), binary() | list() | number(), ne_binary()) -> 'true' | error_result().
is_valid_type(Key, Val, <<"string">>) ->
    case is_binary(Val) of
	true -> true;
	false -> {Key, <<"is not a string">>}
    end;
is_valid_type(Key, Val, <<"number">>) ->
    try wh_util:to_number(Val) of
	_ -> true
    catch
	error:badarg ->
	    {Key, <<"is not a number">>}
    end;
is_valid_type(Key, Val, <<"integer">>) ->
    try wh_util:to_integer(Val) of
	_ -> true
    catch
	error:badarg ->
	    {Key, <<"is not an integer">>}
    end;
is_valid_type(Key, Val, <<"float">>) ->
    try wh_util:to_float(Val) of
	_ -> true
    catch
	error:badarg ->
	    {Key, <<"is not an float">>}
    end;
is_valid_type(Key, Val, <<"boolean">>) ->
    try wh_util:to_boolean(Val) of
	_ -> true
    catch
	error:function_clause ->
	    {Key, <<"is not a boolean">>}
    end;
is_valid_type(Key, Val, <<"array">>) ->
    case is_list(Val) of
	true -> true;
	false -> {Key, <<"is not an array of values">>}
    end;
is_valid_type(Key, Val, <<"object">>) ->
    case wh_json:is_json_object(Val) of
	true -> true;
	false -> {Key, <<"is not another json object">>}
    end;
is_valid_type(Key, Val, <<"null">>) ->
    case Val of
	<<"null">> -> true;
	null -> true;
	_ -> {Key, <<"is not null">>}
    end;
%% any type ('any' or user-defined) that get's here is considered valid
is_valid_type(_,_,_) ->
    true.

-spec is_valid_pattern/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'true' | error_result().
is_valid_pattern(Key, Val, Pattern) ->
    case re:compile(Pattern) of
	{error, {ErrString, Position}} ->
	    {Key, list_to_binary([<<"Error compiling pattern '">>, Pattern, <<"': ">>, ErrString, <<" at ">>, wh_util:to_binary(Position)])};
	{ok, MP} ->
	    case re:run(Val, MP) of
		nomatch -> {Key, list_to_binary([<<"Failed to match pattern '">>, Pattern, <<"'">>])};
		_ -> true
	    end
    end.

-spec is_valid_format/3 :: (ne_binary(), ne_binary(), ne_binary() | number()) -> 'true' | error_result().
is_valid_format(<<"date-time">>, Key, Val) ->
    %% ISO 8601 (YYYY-MM-DDThh:mm:ssZ) in UTC
    case re:run(Val, ?ISO_8601_REGEX) of
	nomatch -> {Key, <<"Failed to parse ISO-8601 datetime (in UTC)">>};
	{match, _} -> true
    end;
is_valid_format(<<"date">>, Key, Val) ->
    %% Match YYYY-MM-DD
    case re:run(Val, <<"^\\d{4}-\\d{2}-\\d{2}$">>) of
	nomatch -> {Key, <<"Failed to parse date (expected 'YYYY-MM-DD')">>};
	{match, _} -> true
    end;
is_valid_format(<<"time">>, Key, Val) ->
    %% Match hh:mm:ss
    case re:run(Val, <<"^\\d{2}:\\d{2}:\\d{2}$">>) of
	nomatch -> {Key, <<"Failed to parse time (expected 'hh:mm:ss')">>};
	{match, _} -> true
    end;
is_valid_format(<<"utc-millisec">>, Key, Val) ->
    %% Not really sure what is expected in Val, other than a number
    try wh_util:to_number(Val) of
	_ -> true
    catch
	error:badarg -> {Key, <<"Value doesn't appear to be a number nor a utc-millisec value">>}
    end;
is_valid_format(<<"regex">>, Key, Val) ->
    case re:compile(Val) of
	{error, {ErrString, Position}} ->
	    {Key, list_to_binary([<<"Error compiling pattern '">>, Val, <<"': ">>, ErrString, <<" at ">>, wh_util:to_binary(Position)])};
	{ok, _} -> true
    end;
is_valid_format(<<"color">>, _, _) ->
    %% supposed to be valid CSS 2.1 colors
    true;
is_valid_format(<<"style">>, _, _) ->
    %% supposed to be valid CSS 2.1 style definition
    true;
is_valid_format(<<"phone">>, Key, Val) ->
    case wh_util:is_e164(Val) of
	true -> true;
	false -> {Key, <<"Phone number not in E.164 format">>}
    end;
is_valid_format(<<"uri">>, Key, Val) ->
    try mochiweb_util:urlsplit(wh_util:to_list(Val)) of
	{_Scheme, _Netloc, _Path, _Query, _Fragment} -> true;
	_ -> {Key, <<"Failed to parse URI">>}
    catch
	error:function_clause -> {Key, <<"Failed to parse URI">>}
    end;
is_valid_format(<<"email">>, Key, Val) ->
    case re:run(Val, ?EMAIL_REGEX, [{capture, first, binary}]) of
	{match, [Val]} -> true;
	{match, _} -> {Key, <<"Failed to match entire email">>};
	nomatch -> {Key, <<"Failed to validate email address">>}
    end;
is_valid_format(<<"ip-address">>, Key, Val) ->
    case wh_util:is_ipv4(Val) of
	true -> true;
	false -> {Key, <<"Failed to validate IPv4">>}
    end;
is_valid_format(<<"ipv6">>, Key, Val) ->
    case wh_util:is_ipv6(Val) of
	true -> true;
	false -> {Key, <<"Failed to validate IPv6">>}
    end;
is_valid_format(<<"host-name">>, Key, Val) ->
    case re:run(Val, ?HOSTNAME_REGEX, [{capture, first, binary}]) of
	{match, [Val]} -> true;
	{match, _} -> {Key, <<"Failed to match entire hostname">>};
	nomatch -> {Key, <<"Failed to validate hostname">>}
    end;
is_valid_format(_,_,_) ->
    true.

%% Items: [ json_term(),...]
%% SchemaItemsJObj: {"type":"some_type", properties:{"prop1":{"attr1key":"attr1val",...},...},...}
%%  or could be a list of schemas
-spec are_valid_items/2 :: (list(), json_object() | json_objects()) -> results().
are_valid_items(Items, SchemaItemsJObj) ->
    ItemType = wh_json:get_value(<<"type">>, SchemaItemsJObj, <<"any">>),
    ItemSchemaJObj = wh_json:get_value(<<"items">>, SchemaItemsJObj, wh_json:new()),
    true.


-spec is_required_attribute/1 :: (json_object()) -> boolean().
is_required_attribute(AttributesJObj) ->
    wh_json:is_true(<<"required">>, AttributesJObj, false).

-spec is_optional_attribute/1 :: (json_object()) -> boolean().
is_optional_attribute(AttributesJObj) ->
    wh_json:is_true(<<"optional">>, AttributesJObj, true).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

-define(SCHEMA_PERSON, wh_json:from_list([{<<"type">>, <<"person">>}
					  ,{<<"properties">>, wh_json:from_list([{<<"name">>, wh_json:from_list([{<<"type">>, <<"string">>}
														])}
										 ,{<<"age">>, wh_json:from_list([{<<"type">>, <<"integer">>}
														 ,{<<"maximum">>, 125}
														])}
										])}
					 ])).

-define(SCHEMA_CAT, wh_json:from_list([{<<"type">>, <<"cat">>}
				       ,{<<"properties">>, wh_json:from_list([{<<"name">>, wh_json:from_list([{<<"type">>, <<"string">>}
													     ])}
									      ,{<<"breed">>, wh_json:from_list([{<<"type">>, <<"string">>}
													       ])}
									     ])}
					 ])).

-define(SCHEMA_ALL, wh_json:from_list([{<<"type">>, <<"any">>}
				       ,{<<"properties">>, wh_json:from_list([{<<"name">>, wh_json:from_list([{<<"type">>, <<"string">>}
													     ])}
									     ])}
					 ])).

-define(SCHEMA_BIZARRE, wh_json:from_list([{<<"type">>, [<<"bizarro">>]}
				       ,{<<"properties">>, wh_json:from_list([{<<"flame_length">>, wh_json:from_list([{<<"type">>, <<"integer">>}
														     ])}
									     ])}
					  ])).

-define(SCHEMA_PERSON_OR_CAT, wh_json:from_list([{<<"type">>, [<<"person">>, <<"cat">>]}
						 ,{<<"properties">>, wh_json:from_list([{<<"name">>, wh_json:from_list([{<<"type">>, <<"string">>}
														       ])}
											,{<<"breed">>, wh_json:from_list([{<<"type">>, <<"string">>}
															 ])}
											,{<<"age">>, wh_json:from_list([{<<"type">>, <<"integer">>}
															,{<<"maximum">>, 125}
														       ])}
										       ])}
						])).


-define(JSON_MAL, wh_json:from_list([{<<"type">>, <<"person">>}
					,{<<"name">>, <<"Malcolm Reynolds">>}
					,{<<"age">>, 42}
				       ])).
-define(JSON_GARFIELD, wh_json:from_list([{<<"type">>, <<"cat">>}
					  ,{<<"name">>, <<"Garfield">>}
					  ,{<<"breed">>, <<"tabby">>}
					 ])).

is_valid_type_test() ->
    Mal = ?JSON_MAL,
    Garfield = ?JSON_GARFIELD,
    Person = ?SCHEMA_PERSON,
    Cat = ?SCHEMA_CAT,
    Any = ?SCHEMA_PERSON_OR_CAT,
    All = ?SCHEMA_ALL,
    Bizarre = ?SCHEMA_BIZARRE,

    ?assertEqual(true, is_valid_type(Mal, Person)),
    ?assertEqual(true, is_tuple(is_valid_type(Mal, Cat))),
    ?assertEqual(true, is_valid_type(Garfield, Cat)),
    ?assertEqual(true, is_tuple(is_valid_type(Garfield, Person))),
    ?assertEqual(true, is_valid_type(Garfield, Any)),
    ?assertEqual(true, is_valid_type(Mal, Any)),

    ?assertEqual(true, is_valid_type(Mal, All)),
    ?assertEqual(true, is_valid_type(Garfield, All)),
    ?assertEqual(true, is_tuple(is_valid_type(Garfield, Bizarre))).
	

%% {
%%   "name":"Product",
%%   "properties":{
%%     "id":{
%%       "type":"number",
%%       "description":"Product identifier",
%%       "required":true
%%     },
%%     "name":{
%%       "description":"Name of the product",
%%       "type":"string",
%%       "required":true
%%     },
%%     "price":{
%%       "required":true,
%%       "type": "number",
%%       "minimum":0,
%%       "required":true
%%     },
%%     "tags":{
%%       "type":"array",
%%       "items":{
%%         "type":"string"
%%       }
%%     }
%%   },
%%   "links":[
%%     {
%%       "rel":"full",
%%       "href":"{id}"
%%     },
%%     {
%%       "rel":"comments",
%%       "href":"comments/?id={id}"
%%     }
%%   ]
%% }

-endif.
