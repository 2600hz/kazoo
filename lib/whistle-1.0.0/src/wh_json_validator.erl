%%%-------------------------------------------------------------------
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
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_json_validator).

-export([is_valid/2, is_valid_attribute/3]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-type error_key() :: ne_binary() | [ne_binary(),...].
-type error_tuple() :: {error_key(), ne_binary()}.
-type error_proplist() :: [error_tuple(),...] | [].

-type pass() :: {'pass', wh_json:json_object()}.
-type fail() :: {'fail', {error_key(), ne_binary()} | wh_json:json_object() | error_proplist()}.

-type error_acc() :: [] | [{[ne_binary(),...], ne_binary()},...].
-type jkey_acc() :: wh_json:json_proplist_key().

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
-spec is_valid/2 :: (wh_json:json_object(), ne_binary() | wh_json:json_object()) -> pass() | fail().
is_valid(JObj, Schema) when is_binary(Schema) ->
    %% TODO: cache the schema?
    case couch_mgr:open_cache_doc(?WH_SCHEMA_DB, Schema) of
        {ok, SchemaJObj} ->
            is_valid(JObj, SchemaJObj);
        {error, R} ->
            lager:debug("unable to find ~s schema, assuming it passed: ~p", [Schema, R]),
            {pass, JObj}
    end;
is_valid(JObj, Schema) ->
    case are_valid_properties(JObj, Schema) of
        {pass, _}=Ok -> 
            lager:debug("json validated against ~s schema", [wh_json:get_value(<<"_id">>, Schema)]),
            Ok;
        {fail, Errors} ->
            lager:debug("json failed validation against ~s schema", [wh_json:get_value(<<"_id">>, Schema)]),
            {fail, Errors}
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
-spec are_valid_properties/2 :: (wh_json:json_object(), wh_json:json_object()) -> pass() | fail().
-spec are_valid_properties/3 :: (wh_json:json_object(), [] | [ne_binary(),...], wh_json:json_object()) -> pass() | fail().
-spec are_valid_properties/4 :: (wh_json:json_object(), jkey_acc(), error_acc(), wh_json:json_proplist()) -> pass() | fail().

are_valid_properties(JObj, Schema) ->
    PropertiesJObj = wh_json:get_value(<<"properties">>, Schema, wh_json:new()),
    are_valid_properties(JObj, [], PropertiesJObj).

are_valid_properties(JObj, Path, PropertiesJObj) ->
    Properties = wh_json:to_proplist(PropertiesJObj),
    are_valid_properties(JObj, Path, [], Properties).

are_valid_properties(JObj, _, [], []) ->
    {pass, JObj};
are_valid_properties(_, _, Errors, []) ->
    %% compress nested proplists to one main proplist
    {fail, lists:flatten(Errors)};
are_valid_properties(JObj, Path, Errors, [{Property, AttributesJObj}|T]) ->
    Key = Path ++ [Property],
    %% extend json schema to optionally use the default if the provided
    %% value is 'empty'
    ValueFun = case wh_json:is_false(<<"empty">>, AttributesJObj) of
                  true -> fun wh_json:get_ne_value/2;
                  false -> fun wh_json:get_value/2
              end,
    JObj1 = case ValueFun(Key, JObj) of
                undefined ->
                    %% Try to ensure the default value is properly interpreted,
                    %% for example if simply using get_value on temporal routes
                    %% the default integer is interpreted as a float.
                    DefaultFun = case wh_json:get_value(<<"type">>, AttributesJObj) of
                                     <<"string">> -> fun wh_json:get_binary_value/2;
                                     <<"number">> -> fun wh_json:get_number_value/2;
                                     <<"integer">> -> fun wh_json:get_integer_value/2;
                                     <<"float">> -> fun wh_json:get_float_value/2;
                                     <<"boolean">> -> fun wh_json:is_true/2;
                                     _ -> fun wh_json:get_value/2
                                 end,
                    case DefaultFun(<<"default">>, AttributesJObj) of
                        undefined ->
                            JObj;
                        Default ->
                            wh_json:set_value(Key, Default, JObj)
                    end;
                _Else ->
                    JObj
            end,
    case are_valid_attributes(JObj1, Key, AttributesJObj) of
        {pass, JObj2} -> 
            are_valid_properties(JObj2, Path, Errors, T);
        {fail, Error} -> 
            are_valid_properties(JObj1, Path, [Error|Errors], T)
    end.

%% JObj = {..., "name":"Mal Reynolds",...}
%% Key = "name"
%% AttributesJObj = {"type":"string"}
-spec are_valid_attributes/3 :: (wh_json:json_object(), jkey_acc(), wh_json:json_object()) -> pass() | fail().
-spec are_valid_attributes/5 :: (wh_json:json_object(), jkey_acc(), wh_json:json_object(), error_acc(), proplist()) -> pass() | fail().

are_valid_attributes(JObj, Key, AttributesJObj) ->
    %% 5.7 - testing here for required saves lots of work....
    case {wh_json:is_true(<<"required">>, AttributesJObj, false), wh_json:get_value(Key, JObj)} of
        {true, undefined} -> {fail, {Key, <<"required:Field is required but missing">>}};
        {false, undefined} -> {pass, JObj};
        {_, _} ->
            Attributes = wh_json:to_proplist(AttributesJObj),
            are_valid_attributes(JObj, Key, AttributesJObj, [], Attributes)
    end.

are_valid_attributes(JObj, _, _, [], []) ->
    {pass, JObj};
are_valid_attributes(_, _, _, Errors, []) ->
    {fail, Errors};
%% 5.2 is preformed here as a one-off to thread the resulting json object back into the validation
are_valid_attributes(JObj, Key, AttributesJObj, Errors, [{<<"properties">>, AttributeVal}|T]) ->
    case are_valid_properties(JObj, Key, AttributeVal) of
        {pass, JObj1} -> are_valid_attributes(JObj1, Key, AttributesJObj, Errors, T);
        {fail, Error} -> are_valid_attributes(JObj, Key, AttributesJObj, [Error|Errors], T)
    end;
are_valid_attributes(JObj, Key, AttributesJObj, Errors, [{AttributeKey, AttributeVal}|T]) ->
    case is_valid_attribute({AttributeKey, AttributeVal, AttributesJObj}, JObj, Key) of
        {pass, JObj1} -> are_valid_attributes(JObj1, Key, AttributesJObj, Errors, T);
        {fail, Error} -> are_valid_attributes(JObj, Key, AttributesJObj, [Error|Errors], T)
    end.
                
%% Section 5 of the spec
%% AttributeKey, AttributeVal, Key, Val, AttributesJObj
%% Some Attributes:
%% "type" : string | number | integer | float | boolean | array | object | null | any | user-defined
%%          The last two (any and user-defined) are automatically considered valid
%%
%% {'pass', wh_json:json_object()} | {'fail', {jkey_acc(), ne_binary()}}
-spec is_valid_attribute/3 :: ({wh_json:json_string(), wh_json:json_term(), wh_json:json_object()}, wh_json:json_object(), jkey_acc()) -> pass() | fail().

%% 5.1
is_valid_attribute({<<"type">>, [], _}, _, Key) ->
    {fail, {Key, <<"type:Value did not match one of the necessary types">>}};
is_valid_attribute({<<"type">>, [Type|Types], AttrJObj}, JObj, Key) ->
    case is_valid_attribute({<<"type">>, Type, wh_json:new()}, JObj, Key) of
        {pass, _}=P -> P;
        {fail, _} -> is_valid_attribute({<<"type">>, Types, AttrJObj}, JObj, Key)
    end;
is_valid_attribute({<<"type">>, Type, _}, JObj, Key) when is_binary(Type) ->
    Instance = wh_json:get_value(Key, JObj),
    case check_valid_type(Instance, Type, true) of
        true -> 
            {pass, JObj};
        false -> 
            {fail, {Key, list_to_binary([<<"type:Value is not of type ">>, Type])}}
    end;
is_valid_attribute({<<"type">>, TypeSchema, _}, JObj, Key) ->
    are_valid_attributes(JObj, Key, TypeSchema);

%% 5.3
is_valid_attribute({<<"patternProperties">>, _, _}, JObj, _Key) ->
    {pass, JObj}; %% ignored for the moment

%% 5.4
is_valid_attribute({<<"additionalProperties">>, _, _}, JObj, _Key) ->
    {pass, JObj}; %% ignored for the moment

%% 5.5
is_valid_attribute({<<"items">>, Items, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    case {check_valid_type(Instance, <<"array">>), are_valid_items(Instance, Items)} of
        {false, _} -> 
            {pass, JObj};
        {true, true} -> 
            {pass, JObj};
        {true, Error} ->
            {fail, {Key, list_to_binary([<<"items:">>, Error])}}
    end;    

%% 5.6
is_valid_attribute({<<"additionalItems">>, _, _}, JObj, _Key) ->
    {pass, JObj}; %% ignored for the moment

%% 5.7
is_valid_attribute({<<"required">>, IsRequired, _}, JObj, Key) ->
    case wh_util:is_true(IsRequired) andalso wh_json:get_value(Key, JObj) =:= undefined of
        false -> 
            {pass, JObj};
        true -> 
            {fail, {Key, <<"required:Field is required but missing">>}}
    end;

%% 5.8
is_valid_attribute({<<"dependencies">>, _, _}, JObj, _Key) ->
    {pass, JObj}; %% ignored for the moment

%% 5.9 / 5.11
is_valid_attribute({<<"minimum">>, Min, AttrsJObj}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"number">>), wh_util:to_number(Min)} of
        {false, _} ->
            {pass, JObj};
        {true, Int} ->
            Num = wh_util:to_number(Instance),
            case wh_json:is_true(<<"exclusiveMinimum">>, AttrsJObj) of
                true when (Num > Int) -> {pass, JObj};
                false when (Num >= Int) -> {pass, JObj};
                _ ->
                    {fail, {Key, list_to_binary([<<"minimum:Value must be at least ">>, wh_util:to_binary(Int)])}}
            end
    catch
        error:badarg ->
            {fail, {Key, <<"minimum:Either the value or the schema minimum for this key is not a number">>}};
        error:function_clause ->
            {fail, {Key, <<"minimum:Either the value or the schema minimum for this key is not a number">>}}
    end;

%% 5.10 / 5.12
is_valid_attribute({<<"maximum">>, Max, AttrsJObj}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"number">>), wh_util:to_number(Max)} of
        {false, _} ->
            {pass, JObj};
        {true, Int} -> 
            Num = wh_util:to_number(Instance),
            case wh_json:is_true(<<"exclusiveMaximum">>, AttrsJObj) of
                true when (Num < Int) -> {pass, JObj};
                false when (Num =< Int) -> {pass, JObj};
                _ ->
                    {fail, {Key, list_to_binary([<<"maximum:Value must be at most ">>, wh_util:to_binary(Int)])}}
            end
    catch
        error:badarg ->
            {fail, {Key, <<"maximum:Either the value or the schema maximum for this key is not a number">>}};
        error:function_clause ->
            {fail, {Key, <<"maximum:Either the value or the schema maximum for this key is not a number">>}}
    end;

%% 5.13
is_valid_attribute({<<"minItems">>, Min, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"array">>), wh_util:to_integer(Min)} of
        {false, _} -> 
            {pass, JObj};
        {true, Int} when length(Instance) >= Int -> 
            {pass, JObj};
        {true, _} -> 
            {fail, {Key, list_to_binary([<<"minItems:The list is not at least ">>, wh_util:to_binary(Min), <<" items">>])}}
    catch
        error:badarg ->
            {fail, {Key, <<"minItems:Schema value for minItems not a number">>}}
    end;

%% 5.14
is_valid_attribute({<<"maxItems">>, Max, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"array">>), wh_util:to_integer(Max)} of
        {false, _} -> 
            {pass, JObj};
        {true, Int} when length(Instance) =< Int -> 
            {pass, JObj};
        {true, _Int} -> 
            {fail, {Key, list_to_binary([<<"maxItems:The list is more than ">>, wh_util:to_binary(Max), <<" items">>])}}
    catch
        error:badarg ->
            {fail, {Key, <<"maxItems:Schema value for minItems not a number">>}}
    end;

%% 5.15
is_valid_attribute({<<"uniqueItems">>, TryUnique, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    true = wh_util:is_true(TryUnique),
    case {check_valid_type(Instance, <<"array">>), are_unique_items(Instance, TryUnique)} of
        {false, _} -> 
            {pass, JObj};
        {true, true} ->
            {pass, JObj};
        {true, Error} ->
            {fail, {Key, list_to_binary([<<"uniqueItems:">>, Error])}}
    end;

%% 5.16
is_valid_attribute({<<"pattern">>, Pattern, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    case {check_valid_type(Instance, <<"string">>), is_valid_pattern(Instance, Pattern)} of
        {false, _} -> 
            {pass, JObj};
        {true, true} ->
            {pass, JObj};
        {true, Error} -> 
            {fail, {Key, list_to_binary([<<"pattern:">>, Error])}}
    end;

%% 5.17
is_valid_attribute({<<"minLength">>, Min, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"string">>), wh_util:to_integer(Min)} of
        {false, _} -> 
            {pass, JObj};
        {true, Int} when erlang:byte_size(Instance) >= Int ->
            {pass, JObj};
        {true, _} -> 
            {fail, {Key, list_to_binary([<<"minLength:String must be at least ">>, wh_util:to_binary(Min), <<" characters">>])}}
    catch
        error:badarg -> 
            {fail, {Key, <<"minLength:Schema's minLength not an integer or value wasn't a string">>}}
    end;

%% 5.18
is_valid_attribute({<<"maxLength">>, Max, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {check_valid_type(Instance, <<"string">>), wh_util:to_integer(Max)} of
        {false, _} -> 
            {pass, JObj};
        {true, Int} when erlang:byte_size(Instance) =< Int ->
            {pass, JObj};
        {true, _} -> 
            {fail, {Key, list_to_binary([<<"maxLength:String must not be more than ">>, wh_util:to_binary(Max), <<" characters">>])}}
    catch
        error:badarg -> 
            {fail, {Key, <<"maxLength:Schema's maxLength not an integer or value wasn't a string">>}}
    end;

%% 5.19
is_valid_attribute({<<"enum">>, Enums, _}, JObj, Key) ->
    Instance =  wh_json:get_value(Key, JObj),
    I = case check_valid_type(Instance, <<"array">>) of
            true -> Instance;
            false -> [Instance]
        end,            
    case lists:all(fun(Elem) -> 
                           try [are_same_items(Enum, Elem) || Enum <- Enums] of
                               [] -> true;
                               _ -> false
                           catch
                               throw:{duplicate_found, _} -> true
                           end
                   end, I) of
        true ->
            {pass, JObj};
        false ->
            {fail, {Key, <<"enum:Value not found in enumerated list of values">>}}
    end;

%% 5.23
is_valid_attribute({<<"format">>, Format, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    case is_valid_format(Format, Instance) of
        true ->
            {pass, JObj};
        Error ->
            {fail, {Key, Error}}
    end;

%% 5.24
is_valid_attribute({<<"divisibleBy">>, DivBy, _}, JObj, Key) ->
    Instance = wh_json:get_value(Key, JObj),
    try {erlang:abs(wh_util:to_number(DivBy)), erlang:abs(wh_util:to_number(Instance))} of
        {0, _} -> {Key, <<"divisibleBy:Trying to divide by 0">>};
        {0.0, _} -> {Key, <<"divisibleBy:Trying to divide by 0">>};
        {Denominator, Numerator} when is_integer(Denominator) andalso is_integer(Numerator) ->
            case Numerator rem Denominator of
                0 -> 
                    {pass, JObj};
                _ -> 
                    {fail, {Key, list_to_binary([<<"divisibleBy:Value not divisible by ">>, wh_util:to_binary(DivBy)])}}
            end;
        {Denominator, Numerator} ->
            Res = Numerator / Denominator,
            %% 1 / 0.1 = 10.0 which is evenly divisible
            %% trunc(10.0) == 10 (notice == NOT =:=)
            case trunc(Res) == Res of
                true -> 
                    {pass, JObj};
                false -> 
                    {fail, {Key, list_to_binary([<<"divisibleBy: Value not divisible by ">>, wh_util:to_binary(DivBy)])}}
            end
    catch
        error:badarg -> 
            {fail, {Key, <<"divisibleBy:Either the numerator or the denominator in the schema is not a number">>}};
        error:function_clause -> 
            {fail, {Key, <<"divisibleBy:Either the numerator or the denominator in the schema is not a number">>}}
    end;

%% 5.25
is_valid_attribute({<<"disallow">>, [], _}, JObj, _) ->
    {pass, JObj};
is_valid_attribute({<<"disallow">>, [Disallow|Disallows], AttrJObj}, JObj, Key) ->
    case is_valid_attribute({<<"disallow">>, Disallow, wh_json:new()}, JObj, Key) of
        {pass, _} -> is_valid_attribute({<<"disallow">>, Disallows, AttrJObj}, JObj, Key);
        {fail, _}=E -> E 
    end;
is_valid_attribute({<<"disallow">>, Disallow, _}, JObj, Key) when is_binary(Disallow) ->
    Instance = wh_json:get_value(Key, JObj),
    case check_valid_type(Instance, Disallow, true) of
        true ->
            {fail, {Key, list_to_binary([<<"disallow:Value is of disallowed type ">>, Disallow])}};
        false -> 
            {pass, JObj}
    end;
is_valid_attribute({<<"disallow">>, DisallowSchema, _}, JObj, Key) ->
    case are_valid_attributes(JObj, Key, DisallowSchema) of
        {pass, _} -> {fail, {Key, <<"disallow:Value matched one of the disallowed types">>}};
        {fail, _}-> {pass, JObj}
    end;

%% 5.26
is_valid_attribute({<<"extends">>, _, _}, JObj, _) ->
    %% Not currently supported
    {pass, JObj};

%% 5.27
is_valid_attribute({<<"id">>, _, _}, JObj, _) ->
    %% ignored, as this is used to identify the schema for reference elsewhere
    {pass, JObj};

%% 5.28
is_valid_attribute({<<"$ref">>, _, _}, JObj, _) ->
    %% ignored until we start to follow the ref URIs
    {pass, JObj};

%% 5.29
is_valid_attribute({<<"$schema">>, _, _}, JObj, _) ->
    %% ignored, this URI points to a JSON Schema to validate the AttrsJObj schema against
    {pass, JObj};

%% 5.21, 5.22,  and unknown/unhandled attributes
is_valid_attribute(_, JObj, _) ->
    {pass, JObj}. %% ignorable attribute, like 'title'

%%
%%% Helper functions
%%
-spec are_unique_items/1 :: (list()) -> 'true' | ne_binary().
-spec are_unique_items/2 :: (list(), 'undefined' | ne_binary()) -> 'true' | ne_binary().

are_unique_items(Instance) ->
    try lists:usort(fun are_same_items/2, Instance) of
        _E -> true
    catch
        throw:{duplicate_found, A} ->
            list_to_binary([<<"Duplicate found">>, wh_util:to_binary(A)]);
        error:function_clause ->
            %% some one failed (likely wh_json:get_keys/1)
            %% which means they probably weren't equal
            true
    end.

are_unique_items(Instance, TryUnique) ->
    case wh_util:is_true(TryUnique) of
        true -> are_unique_items(Instance);
        false -> <<"Set improperly (should be set to 'true'">>
    end.

%% will throw an exception if A and B are identical
-spec are_same_items/2 :: (term(), term()) -> 'false'.
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

%% If we are testing a value to be of a type, ShouldBe is true; meaning we expect the value to be of the type.
%% If ShouldBe is false (as when calling is_disallowed_type/3), then we expect the value to not be of the type.
-spec check_valid_type/2 :: (binary() | list() | number(), ne_binary()) -> boolean().
-spec check_valid_type/3 :: (binary() | list() | number(), ne_binary(), boolean()) -> boolean().

check_valid_type(Instance, Type) ->
    check_valid_type(Instance, Type, true).

check_valid_type(_, <<"any">>, ShouldBe) ->
     ShouldBe;
check_valid_type(<<_/binary>>, <<"string">>, ShouldBe) ->
     ShouldBe;
check_valid_type(_, <<"string">>, ShouldBe) ->
    not ShouldBe;
check_valid_type(Instance, <<"number">>, ShouldBe) ->
    try wh_util:to_number(Instance) of
        _ -> ShouldBe
    catch
        error:badarg -> not ShouldBe;
        error:function_clause -> not ShouldBe
    end;
check_valid_type(Instance, <<"integer">>, ShouldBe) ->
    try wh_util:to_integer(Instance, strict) of
        _ -> ShouldBe
    catch
        error:badarg -> not ShouldBe;
        error:function_clause -> not ShouldBe
    end;
check_valid_type(Instance, <<"float">>, ShouldBe) ->
    try wh_util:to_float(Instance) of
        _ -> ShouldBe
    catch
        error:badarg -> not ShouldBe;
        error:function_clause -> not ShouldBe
    end;
check_valid_type(Instance, <<"boolean">>, ShouldBe) ->
    try wh_util:to_boolean(Instance) of
        _ -> ShouldBe
    catch
        error:function_clause -> not ShouldBe
    end;
check_valid_type(Instance, <<"array">>, ShouldBe) ->
    case is_list(Instance) of
        true -> ShouldBe;
        false -> not ShouldBe
    end;
check_valid_type(Instance, <<"object">>, ShouldBe) ->
    case wh_json:is_json_object(Instance) of
        true -> ShouldBe;
        false -> not ShouldBe
    end;
check_valid_type(Instance, <<"null">>, ShouldBe) ->
    case Instance of
        null -> ShouldBe;
        _ -> not ShouldBe
    end;
%% any type ('any' or user-defined) that get's here is considered passing
check_valid_type(_, _, ShouldBe) ->
    ShouldBe.

-spec is_valid_pattern/2 :: (ne_binary(), ne_binary()) -> 'true' | ne_binary().
is_valid_pattern(Instance, Pattern) when is_binary(Instance) ->
    case re:compile(Pattern) of
        {error, {ErrString, Position}} ->
            list_to_binary([<<"Error compiling pattern '">>
                                ,Pattern, <<"': ">>, ErrString
                                ,<<" at ">>, wh_util:to_binary(Position)
                           ]);
        {ok, MP} ->
            case re:run(Instance, MP) of
                nomatch -> 
                    list_to_binary([<<"Failed to match pattern '">>, Pattern, <<"'">>]);
                _ -> 
                    true
            end
    end;
is_valid_pattern(_, _) ->
    true.

-spec is_valid_format/2 :: (ne_binary(), ne_binary() | number()) -> 'true' | ne_binary().
is_valid_format(<<"date-time">>, Instance) ->
    %% ISO 8601 (YYYY-MM-DDThh:mm:ssZ) in UTC
    case re:run(Instance, ?ISO_8601_REGEX) of
        {match, _} -> true;
        nomatch -> <<"date-time:Failed to parse ISO-8601 datetime (in UTC)">>
    end;
is_valid_format(<<"date">>, Instance) ->
    %% Match YYYY-MM-DD
    case re:run(Instance, <<"^\\d{4}-\\d{2}-\\d{2}$">>) of
        {match, _} -> true;
        nomatch -> <<"date:Failed to parse date (expected 'YYYY-MM-DD')">>
    end;
is_valid_format(<<"time">>, Instance) ->
    %% Match hh:mm:ss
    case re:run(Instance, <<"^\\d{2}:\\d{2}:\\d{2}$">>) of
        {match, _} -> true;
        nomatch -> <<"time:Failed to parse time (expected 'hh:mm:ss')">>
    end;
is_valid_format(<<"utc-millisec">>, Instance) ->
    %% Not really sure what is expected in Instance, other than a number
    try wh_util:to_number(Instance) of
        _ -> true
    catch
        error:badarg -> <<"utc-millisec:Instance doesn't appear to be a number nor a utc-millisec value">>;
        error:function_clause -> <<"utc-millisec:Instance doesn't appear to be a number nor a utc-millisec value">>
    end;
is_valid_format(<<"regex">>, Instance) ->
    case re:compile(Instance) of
        {ok, _} -> true;
        {error, {ErrString, Position}} ->
            list_to_binary([<<"regex:Error compiling pattern '">>, Instance, <<"': ">>, ErrString, <<" at ">>, wh_util:to_binary(Position)])
    end;
is_valid_format(<<"color">>, _) ->
    %% supposed to be valid CSS 2.1 colors
    true;
is_valid_format(<<"style">>, _) ->
    %% supposed to be valid CSS 2.1 style definition
    true;
is_valid_format(<<"phone">>, Instance) ->
    case wnm_util:is_e164(Instance) of
        true -> true;
        false -> <<"phone:Phone number not in E.164 format">>
    end;
is_valid_format(<<"uri">>, Instance) ->
    try mochiweb_util:urlsplit(wh_util:to_list(Instance)) of
        {_Scheme, _Netloc, _Path, _Query, _Fragment} -> true;
        _ -> <<"uri:Failed to parse URI">>
    catch
        error:function_clause -> <<"uri:Failed to parse URI">>
    end;
is_valid_format(<<"email">>, Instance) ->
    case re:run(Instance, ?EMAIL_REGEX, [{capture, first, binary}]) of
        {match, [Instance]} -> true;
        {match, _} -> <<"email:Failed to match entire email">>;
        nomatch -> <<"email:Failed to validate email address">>
    end;
is_valid_format(<<"ip-address">>, Instance) ->
    case wh_util:is_ipv4(Instance) of
        true -> true;
        false -> <<"ip-address:Failed to validate IPv4">>
    end;
is_valid_format(<<"ipv6">>, Instance) ->
    case wh_util:is_ipv6(Instance) of
        true -> true;
        false -> <<"ipv6:Failed to validate IPv6">>
    end;
is_valid_format(<<"host-name">>, Instance) ->
    case re:run(Instance, ?HOSTNAME_REGEX, [{capture, first, binary}]) of
        {match, [Instance]} -> true;
        {match, _} -> <<"host-name:Failed to match entire hostname">>;
        nomatch -> <<"host-name:Failed to validate hostname">>
    end;
is_valid_format(_,_) ->
    true.

%% Items: [ json_term(),...]
%% SchemaItemsJObj: {"type":"some_type", properties:{"prop1":{"attr1key":"attr1val",...},...},...}
%%  or could be a list of schemas
-spec are_valid_items/2 :: (list(), wh_json:json_object() | wh_json:json_objects()) -> 'true'. %attribute_results().
are_valid_items(_Items, _SchemaItemsJObj) ->
    _ItemType = wh_json:get_value(<<"type">>, _SchemaItemsJObj, <<"any">>),
    _ItemSchemaJObj = wh_json:get_value(<<"items">>, _SchemaItemsJObj, wh_json:new()),
    true.

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
    Succeed = [?ZERO, ?POS1, ?PI, ?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?NEG1],

    validate_test(Succeed, Fail, Schema).

%% Section 5.10 - maximum
%%     defines the maxium value of the instance property when the type of the instance is a number
maximum_test() ->
    Schema = "{ \"maximum\": 0 }",
    Succeed = [?NEG1, ?ZERO, ?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?POS1, ?PI],

    validate_test(Succeed, Fail, Schema).

%% Section 5.11 - exclusiveMinimum
%%     indicates if the value of the instance (if the instance is a number) can not equal the number defined by the 'minimum' attribute
exclusive_minimum_test() ->
    Schema = "{ \"minimum\": 0,  \"exclusiveMinimum\": true }",
    Succeed = [?POS1, ?PI, ?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?ZERO, ?NEG1],

    validate_test(Succeed, Fail, Schema).

%% Section 5.12 - exclusiveMaximum
%%     indicates if the value of the instance (if the instance is a number) can not equal the number defined by the 'maximum' attribute
exclusive_maximum_test() ->
    Schema = "{ \"maximum\": 0, \"exclusiveMaximum\": true }",
    Succeed = [?NEG1, ?NULL, ?TRUE, ?FALSE, ?STR1, ?STR2, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?ZERO, ?POS1, ?PI],

    validate_test(Succeed, Fail, Schema).

%% Section 5.13 - minItems
%%     defines the minimum number of values in an array when the array is the instance value
min_items_test() ->
    Schema = "{ \"minItems\": 2 }",
    Succeed = [?ARR3, ?ARR4, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?OBJ1],
    Fail = [?ARR1, ?ARR2],

    validate_test(Succeed, Fail, Schema).

%% Section 5.14 - maxItems
%%     defines the maximum number of values in an array when the array is the instance value
max_items_test() ->
    Schema = "{ \"maxItems\": 2 }",
    Succeed = [?ARR1, ?ARR2, ?ARR3, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?OBJ1],
    Fail = [?ARR4],

    validate_test(Succeed, Fail, Schema).

%% Section 5.15 - uniqueItems
%%     indicates that all items in an array instance MUST be unique (containes no two identical values).
%%      - booleans/numbers/strings/null have the same value
%%      - arrays containes the same number of iteams and each item in the array is equal to teh corresponding item in the other array
%%      - objects contain the same property names, and each property in the object is equal to the corresponding property in the other object
unique_items_test() ->
    Schema = "{ \"uniqueItems\": true }",
    Succeed = [?ARR1, ?ARR2, ?ARR3, ?ARR4, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?STR1, ?STR2, ?OBJ1],
    Fail = [?ARR5, ?ARR6, ?ARR7, ?ARR8],

    validate_test(Succeed, Fail, Schema).

%% Section 5.16 - pattern
%%     When the instance value is a string, this provides a regular expression that a string MUST match
pattern_test() ->
    Schema = "{ \"pattern\": \"tle\$\"}",
    Succeed = [<<"chipotle">>, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2],

    validate_test(Succeed, Fail, Schema).

%% Section 5.17 - minLength
%%     When the instance value is a string, this defines the minimum length of the string
min_length_test() ->
    Schema = "{ \"minLength\": 7}",
    Succeed = [<<"longstring">>, longstring, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2],

    validate_test(Succeed, Fail, Schema).

%% Section 5.18 - maxLength
%%     When the instance value is a string, this defines the maximum length of the string
max_length_test() ->
    Schema = "{ \"maxLength\": 3}",
    Succeed = [<<"bar">>, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?PI, ?ARR1, ?ARR2, ?OBJ1],
    Fail = [?STR1, ?STR2],

    validate_test(Succeed, Fail, Schema).

%% Section 5.19 - enum
%%     Enumeration of all possible values that are valid for the instance property
enum_test() ->
    Schema = "{ \"enum\": [\"foobar\", \"barfoo\"]}",
    Succeed = [?STR1, ?STR2, ?ARR1, ?ARR2, ?ARR3],
    Fail = [?PI, ?NULL, ?TRUE, ?FALSE, ?NEG1, ?ZERO, ?POS1, ?OBJ1, ?ARR4, ?ARR5, ?ARR6, ?ARR7, ?ARR8],

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

    _ = [ begin
              Validation = [Result || {AttName, AttValue} <- S
                                          ,(begin
                                                Result = is_valid_attribute({AttName, AttValue, SJObj}
                                                                            ,wh_json:set_value(<<"eunit">>, Elem, wh_json:new())
                                                                            ,[<<"eunit">>]),
                                                not passed(Result)
                                            end)],
              Results = lists:flatten(Validation),
              ?assertEqual(true, Results =:= [])
          end || Elem <- Succeed],
    _ = [ begin
              Validation = [Result || {AttName, AttValue} <- S
                                          ,(begin 
                                                Result = is_valid_attribute({AttName, AttValue, SJObj}
                                                                            ,wh_json:set_value(<<"eunit">>, Elem, wh_json:new())
                                                                            ,<<"eunit">>),
                                                not passed(Result)
                                            end)],
              Results = lists:flatten(Validation),
              ?assertEqual(true, Results =/= [])
          end || Elem <- Fail].

passed({pass, _}) -> true;
passed({fail, _}) -> false.

-endif.
