%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Takes a JSON object and a JSON schema, and validates the object against
%%% the schema.
%%%
%%% Based on http://tools.ietf.org/html/draft-zyp-json-schema-03
%%%
%%% is_valid_object(JObj, Schema) -> 'true' | [ {JObjKey, ErrorMsg}, ...].
%%% is_valid_attribute(Value, Schema) -> 'true' | [ {Key, ErrorMsg}, ...].
%%%   If the Value is a simple type, Key will be <<"root">>;
%%%   otherwise it will be the sub-key from within Value
%%%
%%% IMPORTANT:
%%% JSON Strings are only Erlang binaries. Support for atoms as JSON strings
%%% is not included.
%%% Atoms matched against are 'true', 'false', and 'null' (which are different
%%% from <<"true">>, <<"false">>, and <<"null">>. Please ensure the decoding of
%%% your JSON string converts appropriately.
%%% wh_json:decode(<<"{\"foo\":\"true\"}">>) -> [{<<"foo">>, <<"true">>}]
%%% wh_json:decode(<<"{\"foo\":true}">>) -> [{<<"foo">>, true}]
%%%
%%% @end
%%% Created : 23 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_json_validator).

-export([is_valid_object/2, is_valid_attribute/3]).

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

%% Return true or [{JObjKey, ErrorMsg},...]
-spec is_valid_object/2 :: (json_object(), json_object()) -> results().
is_valid_object(JObj, Schema) ->
    case is_valid_type(JObj, Schema) of
	true ->
	    case is_valid_properties(JObj, Schema) of
		true -> true;
		Errors -> Errors
	    end;
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
-spec is_valid_attribute/3 :: (ne_binary(), term(), json_object()) -> results().
-spec is_valid_attribute/5 :: (ne_binary(), ne_binary() | json_object() | number()
			       ,ne_binary(), binary() | list() | json_object() | 'undefined' | number()
			       ,json_object()) -> results().

is_valid_attribute(Key, Val, AttrsJObj) ->
    case [ Failed || {AttrKey, AttrVal} <- wh_json:to_proplist(AttrsJObj),
		     (Failed = is_valid_attribute(AttrKey, AttrVal, Key, Val, AttrsJObj)) =/= true
	 ] of
	[] -> true;
	Errors -> lists:flatten(Errors)
    end.

%% 5.1
is_valid_attribute(<<"type">>, Types, Key, Val, _AttrsJObj) when is_list(Types) ->
    is_one_of_valid_types(Types, Key, Val);
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
	    {Key, <<"Either the value or the schema minimum for this key is not a number">>};
	error:function_clause ->
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
	    {Key, <<"Either the value or the schema maximum for this key is not a number">>};
	error:function_clause ->
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
		Error -> [{Key, <<"list of items are not unique">>}, Error]
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
	    try lists:any(fun(Enum) -> are_same_items(Enum, Val) end, Enums) of
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
    try {erlang:abs(wh_util:to_number(DivBy)), erlang:abs(wh_util:to_number(Val))} of
	{0, _} -> {Key, <<"Trying to divide by 0">>};
	{0.0, _} -> {Key, <<"Trying to divide by 0">>};
	{Denominator, Numerator} when is_integer(Denominator) andalso is_integer(Numerator) ->
	    case Numerator rem Denominator of
		0 -> true;
		_ -> {Key, list_to_binary([<<"Value not divisible by ">>, wh_util:to_binary(DivBy)])}
	    end;
	{Denominator, Numerator} ->
	    Res = Numerator / Denominator,
	    %% 1 / 0.1 = 10.0 which is evenly divisible
	    %% trunc(10.0) == 10 (notice == NOT =:=)
	    case trunc(Res) == Res of
		true -> true;
		false -> {Key, list_to_binary([<<"Value not divisible by ">>, wh_util:to_binary(DivBy)])}
	    end
    catch
	error:badarg -> {Key, <<"Either the numerator or the denominator in the schema is not a number">>};
	error:function_clause -> {Key, <<"Either the numerator or the denominator in the schema is not a number">>}
    end;

%% 5.25
is_valid_attribute(<<"disallow">>, Types, Key, Val, _AttrsJObj) when is_list(Types) ->
    case is_one_of_disallowed_types(Types, Key, Val) of
	false -> true;
	E -> E
    end;
is_valid_attribute(<<"disallow">>, Type, Key, Val, _AttrsJObj) ->
    case is_disallowed_type(Key, Val, Type) of
	false -> true;
	E -> E
    end;

%% 5.26
is_valid_attribute(<<"extends">>, _ExtendJObj, _Key, _Val, _AttrsJObj) ->
    %% Not currently supported
    true;

%% 5.27
is_valid_attribute(<<"id">>, _ID, _Key, _Val, _AttrsJObj) ->
    %% ignored, as this is used to identify the schema for reference elsewhere
    true;

%% 5.28
is_valid_attribute(<<"$ref">>, _Uri, _Key, _Val, _AttrsJObj) ->
    %% ignored until we start to follow the ref URIs
    true;

%% 5.29
is_valid_attribute(<<"$schema">>, _Uri, _Key, _Val, _AttrsJObj) ->
    %% ignored, this URI points to a JSON Schema to validate the AttrsJObj schema against
    true;

%% 5.21, 5.22,  and unknown/unhandled attributes
is_valid_attribute(_,_,_,_,_) ->
    true. %% ignorable attribute, like 'title'

-spec are_unique_items/1 :: (list()) -> 'true' | error_result().
are_unique_items(Vals) ->
    try lists:usort(fun are_same_items/2, Vals) of
	_E -> io:format("~p~n", [_E]), true
    catch
	throw:{duplicate_found, A} ->
	    {<<"duplicate found">>, wh_util:to_binary(A)};
	error:function_clause ->
	    %% some one failed (likely wh_json:get_keys/1)
	    %% which means they probably weren't equal
	    io:format("~p~n", [erlang:get_stacktrace()]),
	    true
    end.

%% will throw an exception if A and B are identical
-spec are_same_items/2 :: (term(), term()) -> false.
%% are_same_items(A, A) -> throw({duplicate_found, A});
are_same_items(A, B) ->
    Funs = [ fun are_null/2
	     ,fun are_boolean/2
	     ,fun are_numbers/2
	     ,fun are_strings/2 %% handled by first clause (A =:= A)
	     ,fun are_arrays/2
	     ,fun are_objects/2
	   ],
    lists:foldl(fun(F, _) -> F(A, B) end, false, Funs).

are_null(null, null) ->
    throw({duplicate_found, <<"null">>});
are_null(_, _) ->
    false.

are_boolean(A, B) when (is_binary(A) orelse is_atom(A)) andalso
		       (is_binary(B) orelse is_atom(B)) ->
    try {wh_util:to_boolean(A), wh_util:to_boolean(B)} of
	{C, C} -> throw({duplicate_found, C});
	_ -> false
    catch
	error:function_clause ->
	    false
    end;
are_boolean(_, _) ->
    false.

are_numbers(A, B) ->
    try {wh_util:to_number(A), wh_util:to_number(B)} of
	{N, N} -> throw({duplicate_found, A});
	_ -> false
    catch
	error:badarg -> false;
	error:function_clause -> false
    end.

are_strings(A, B) when is_binary(A) andalso is_binary(B) ->
    case A =:= B of
	true -> throw({duplicate_found, A});
	false -> false
    end;
are_strings(_, _) ->
    false.

%% contains the same number of items, and each item in
%% the array is equal to the corresponding item in the other array
are_arrays([], []) -> throw({duplicate_found, <<"arrays are identical">>});
are_arrays([], [_|_]) -> false;
are_arrays([_|_], []) -> false;
are_arrays([A|As], [A|Bs]) ->
    are_arrays(As, Bs);
are_arrays(_,_) ->
    false.

%% contains the same property names, and each property
%% in the object is equal to the corresponding property in the other
%% object.
are_objects(A, B) when is_tuple(A) andalso is_tuple(B) ->
    %% Forall keys in A, A(Key) =:= B(Key), and vice versa
    %% if either is false, they aren't identical objects
    case lists:all(fun(AK) -> wh_json:get_value(AK, B) =:= wh_json:get_value(AK, A) end, wh_json:get_keys(A)) andalso
	lists:all(fun(BK) -> wh_json:get_value(BK, A) =:= wh_json:get_value(BK, B) end, wh_json:get_keys(B)) of
	true -> throw({duplicate_found, <<"objects match">>});
	false -> false
    end;
are_objects(_, _) ->
    false.

-spec is_one_of_valid_types/3 :: ([ne_binary(),...] | [], binary() | list() | number(), ne_binary() | [ne_binary(),...]) -> 'true' | error_result().
-spec is_one_of_disallowed_types/3 :: ([ne_binary(),...] | [], binary() | list() | number(), ne_binary() | [ne_binary(),...]) -> 'false' | error_result().

-spec is_valid_type/3 :: (ne_binary(), binary() | list() | number(), ne_binary() | [ne_binary(),...]) -> 'true' | error_result().
-spec is_disallowed_type/3 :: (ne_binary(), binary() | list() | number(), ne_binary() | [ne_binary(),...] | json_object()) -> 'false' | error_result().

is_one_of_valid_types([], Key, _) ->
    {Key, <<"Value did not match one of the necessary types">>};
is_one_of_valid_types([Type|Types], Key, Val) ->
    case is_valid_type(Key, Val, Type) of
	true -> true;
	_ -> is_one_of_valid_types(Types, Key, Val)
    end.

is_one_of_disallowed_types([], _, _) ->
    false;
is_one_of_disallowed_types([Type|Types], Key, Val) ->
    case is_disallowed_type(Key, Val, Type) of
	false -> is_one_of_disallowed_types(Types, Key, Val);
	{_,_}=E -> E
    end.

is_valid_type(_Key, _Val, <<"any">>) -> true;
is_valid_type(Key, Val, Type) when is_binary(Type) ->
    case check_valid_type(Val, Type, true) of
	true -> true;
	false -> {Key, list_to_binary([<<"Value is not of type: ">>, Type])}
    end;
is_valid_type(Key, Val, TypeSchema) ->
    are_valid_attributes(wh_json:set_value(Key, Val, wh_json:new()), Key, TypeSchema).

is_disallowed_type(Key, _Val, <<"any">>) -> {Key, <<"Value disallowed because schema disallows all">>};
is_disallowed_type(Key, Val, Type) when is_binary(Type) ->
    case check_valid_type(Val, Type, false) of
	true -> false;
	false -> {Key, list_to_binary([<<"Value is of disallowed type: ">>, Type])}
    end;
is_disallowed_type(Key, Val, TypeSchema) ->
    case are_valid_attributes(wh_json:set_value(Key, Val, wh_json:new()), Key, TypeSchema) of
	true -> {Key, list_to_binary([<<"Value is of disallowed type: ">>, wh_json:get_value(<<"type">>, TypeSchema, <<"any">>)])};
	_ -> false
    end.

%% If we are testing a value to be of a type, ShouldBe is true; meaning we expect the value to be of the type.
%% If ShouldBe is false (as when calling is_disallowed_type/3), then we expect the value to not be of the type.
-spec check_valid_type/3 :: (binary() | list() | number(), ne_binary(), boolean()) -> 'true' | error_result().
check_valid_type(Val, <<"string">>, ShouldBe) when is_binary(Val) ->
     ShouldBe;
check_valid_type(_, <<"string">>, ShouldBe) ->
    not ShouldBe;

check_valid_type(Val, <<"number">>, ShouldBe) ->
    try wh_util:to_number(Val) of
	_ -> ShouldBe
    catch
	error:badarg -> not ShouldBe;
	error:function_clause -> not ShouldBe
    end;
check_valid_type(Val, <<"integer">>, ShouldBe) ->
    try wh_util:to_integer(Val, strict) of
	_ -> ShouldBe
    catch
	error:badarg -> not ShouldBe;
	error:function_clause -> not ShouldBe
    end;
check_valid_type(Val, <<"float">>, ShouldBe) ->
    try wh_util:to_float(Val) of
	_ -> ShouldBe
    catch
	error:badarg -> not ShouldBe;
	error:function_clause -> not ShouldBe
    end;
check_valid_type(Val, <<"boolean">>, ShouldBe) ->
    try wh_util:to_boolean(Val) of
	_ -> ShouldBe
    catch
	error:function_clause -> not ShouldBe
    end;
check_valid_type(Val, <<"array">>, ShouldBe) ->
    case is_list(Val) of
	true -> ShouldBe;
	false -> not ShouldBe
    end;
check_valid_type(Val, <<"object">>, ShouldBe) ->
    case wh_json:is_json_object(Val) of
	true -> ShouldBe;
	false -> not ShouldBe
    end;
check_valid_type(Val, <<"null">>, ShouldBe) ->
    case Val of
	null -> ShouldBe;
	_ -> not ShouldBe
    end;
%% any type ('any' or user-defined) that get's here is considered passing
check_valid_type(_,_,_) ->
    true.

-spec is_valid_pattern/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'true' | error_result().
is_valid_pattern(Key, Val, Pattern) when is_binary(Val) ->
    case re:compile(Pattern) of
	{error, {ErrString, Position}} ->
	    {Key, list_to_binary([<<"Error compiling pattern '">>, Pattern, <<"': ">>, ErrString, <<" at ">>, wh_util:to_binary(Position)])};
	{ok, MP} ->
	    case re:run(Val, MP) of
		nomatch -> {Key, list_to_binary([<<"Failed to match pattern '">>, Pattern, <<"'">>])};
		_ -> true
	    end
    end;
is_valid_pattern(Key, _, _) ->
    {Key, <<"Value is not a string">>}.

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
	error:badarg -> {Key, <<"Value doesn't appear to be a number nor a utc-millisec value">>};
	error:function_clause -> {Key, <<"Value doesn't appear to be a number nor a utc-millisec value">>}
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

-define(NULL, null).
-define(TRUE, true).
-define(FALSE, false).
-define(NEG1, -1).
-define(ZERO, 0).
-define(POS1, 1).
-define(PI, 3.1416).
-define(STR1, <<"foobar">>).
-define(STR2, <<"barfoo">>).
-define(OBJ1, wh_json:from_list([{<<"foo">>, <<"bar">>}])).
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
    Succeed = [<<"chipotle">>],
    Fail = [?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],

    validate_test(Succeed, Fail, Schema).

%% Section 5.17 - minLength
%%     When the instance value is a string, this defines the minimum length of the string
min_length_test() ->
    Schema = "{ \"minLength\": 7}",
    Succeed = [<<"longstring">>],
    Fail = [longstring, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],

    validate_test(Succeed, Fail, Schema).

%% Section 5.18 - maxLength
%%     When the instance value is a string, this defines the maximum length of the string
max_length_test() ->
    Schema = "{ \"maxLength\": 3}",
    Succeed = [<<"bar">>],
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
    SJObj = wh_json:decode(wh_util:to_binary(Schema)),
    S = wh_json:to_proplist(SJObj),

    [ begin
	  Validation = [Failed || {AttName, AttValue} <- S, (Failed = is_valid_attribute(AttName, AttValue, <<"eunit">>, Elem, SJObj)) =/= true],
	  case lists:flatten(Validation) of
	      [] ->
		  ?assert(true);
	      [_|_]=FailCity ->
		  ?debugFmt("Failed at least one test (unexpected): ~p~nFor el: ~p and ~s~n", [FailCity, Elem, Schema]),
		  ?assert(true)
	  end
      end || Elem <- Succeed],
    [ begin
	  Validation = [Failed || {AttName, AttValue} <- S, (Failed = is_valid_attribute(AttName, AttValue, <<"eunit">>, Elem, SJObj)) =/= true],
	  case lists:flatten(Validation) of
	      [] ->
		  ?debugFmt("Passed all tests (unexpected) elem ~p, schema ~s~n", [Elem, Schema]),
		  ?assert(false);
	      [_|_] ->
		  ?assert(true)
	  end
      end || Elem <- Fail].
-endif.
