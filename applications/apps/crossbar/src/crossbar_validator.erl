%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_validator).

-include("crossbar.hrl").

-export([validate/2, check/3]).
-export([required/1]).
-export([not_empty/1, is_type/2, is_format/2]).
-export([numeric_min/2, numeric_max/2, numeric_between/3]).
-export([width/2, width/3]).

-import(logger, [format_log/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Runs the check function on all fields defined in definitions, on
%% data
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (Schema :: couch_schema(), Data :: proplist()) -> [] | list(json_object())).
validate(Schema, Data) ->
    lists:foldl(fun({Path, Rules}, Results) ->
			check(Rules, Path, Data) ++ Results
		end, [], Schema).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Runs the validation rules on the data at the path, returns a list
%% of json objects (in mochiweb notation) of failed validators and
%% the parameters provided to each
%% @end
%%--------------------------------------------------------------------
-spec(check/3 :: (Rules :: validator_rules(), Path :: [binary()] | binary(), Data :: proplist()) -> [] | list(json_object())).
check(Rules, Path, Data) ->
    Value = whapps_json:get_value(Path, {struct, Data}),
    lists:foldl(fun({Validator, Params}, Results) ->
			case apply(?MODULE, Validator, [Value] ++ Params) of
			    true ->
				Results;
			    false ->
				[{struct, [{<<"path">>, Path}
					   ,{<<"validator">>, Validator}
					   ,{<<"parameters">>, Params}]
				 } | Results]
			end
		end, [], Rules).
                
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the value is not undefined
%% @end
%%--------------------------------------------------------------------
-spec(required/1 :: (Value :: term()) -> boolean()).
required(Value) ->
    if
        Value =:= undefined -> false;
        true -> true
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the value is not considered empty for its type
%% @end
%%--------------------------------------------------------------------
-spec(not_empty/1 :: (Value :: term()) -> boolean()).
not_empty(Value) ->
    if
        Value =:= undefined -> false;
        is_atom(Value) -> true;
        is_number(Value) -> true;
        is_list(Value) -> length(Value) /= 0;
        is_binary(Value) -> Value =/= <<"">>;
        true -> false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the value is of the type specified
%% @end
%%--------------------------------------------------------------------
-spec(is_type/2 :: (Value :: term(), Type :: number|float|integer|string|boolean|array|object) -> boolean()).
is_type(undefined, _Type) ->
        false;
is_type(Value, Type) ->
    case Type of
        number -> is_number(Value);
        float -> is_float(Value);
        integer -> is_integer(Value);
        string -> is_atom(Value) orelse is_format(Value, line);
        boolean ->
            if
                is_boolean(Value) -> true;
                Value =:= <<"true">> -> true;
                Value =:= <<"false">> -> true;
                true -> false
            end;
        array -> is_list(Value); 
        object -> try {struct, _} = Value, true catch _:_ -> false end;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the value is in the format specified
%% Most of this function was drawn from: http://www.trapexit.org/List_Generators
%% @end
%%--------------------------------------------------------------------
-spec(is_format/2 :: (Value :: term(), Format :: ascii|graph|print|line|space|blank|punct|word|alpha|numeric|hex|phrase|alpha_numeric) -> boolean()).
is_format(Value, Format) when is_atom(Value) ->
    Value =/= undefined andalso is_format(atom_to_list(Value), Format);
is_format(Value, Format) when is_binary(Value) ->
    is_format(binary_to_list(Value), Format);
is_format(Value, Format) when is_list(Value) ->
    case Format of
        %% ASCII characters ([\x00-\x7F])
        ascii ->
                Chars = lists:seq(0, 127),
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Visible characters (i.e. anything except spaces, control
        %% characters, etc.) ([\x21-\x7E])
        graph ->
                Chars = lists:seq(33, 126),
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);
            
        %% Visible characters and spaces (i.e. anything except control
        %% characters, etc.) ([\x20-\x7E])
        print ->
                Chars = lists:seq(32, 126) ++ [$\t],
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Visible characters, spaces, and select control characters
        line ->
                Chars = lists:seq(32, 126) ++ [$ , $\t, $\r, $\n],
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% All whitespace characters, including line breaks ([ \t\r\n\v\f])
        space ->
               Chars = [$ , $\t, $\r, $\n, $\v, $\f],
               lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Space and tab ([ \t])
        blank ->
               Chars = [$ , $\t],
               lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Punctuation and symbols ([!"#$%&'()*+,\-./:;<=>?@[\\\]^_`{|}~])
        punct ->
                Chars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}\~",
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Word characters (letters, numbers and underscores) ([A-Za-z0-9_])
        word ->
               Chars = lists:seq($a, $z) ++ lists:seq($A, $Z)  ++ lists:seq($0, $9) ++ "_",
               lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);
           
        %% Alphabetic characters ([a-zA-Z]) 
        alpha ->
                Chars = lists:seq($a, $z) ++ lists:seq($A, $Z),
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Digits ([0-9])
        numeric ->
                Chars = lists:seq($0, $9),
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Hexadecimal digits ([A-Fa-f0-9])
        hex ->
                Chars = lists:seq($A, $F) ++ lists:seq($a, $f) ++ lists:seq($0, $9),
                lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Letters, numbers, underscores and whitspace
        phrase ->
                Chars = [$ , $\t],
                is_format(Value, word) orelse lists:all(fun(Elem) -> lists:member(Elem, Chars) end, Value);

        %% Alphanumeric characters ([a-zA-Z0-9]).
        alpha_numeric ->
                is_format(Value, alpha) orelse is_format(Value, numeric);

        _ -> false
    end;
is_format(_Value, _Format) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the numerical value is less than or equal to the limit
%% @end
%%--------------------------------------------------------------------
-spec(numeric_min/2 :: (Value :: term(), Limit :: number()) -> boolean()).
numeric_min(Value, Limit) ->
    if
        not is_number(Value) -> false;
        not is_number(Limit) -> false;
        Value >= Limit -> true;
        true -> false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the numerical value is greater than or equal to the limit
%% @end
%%--------------------------------------------------------------------
-spec(numeric_max/2 :: (Value :: term(), Limit :: number()) -> boolean()).
numeric_max(Value, Limit) ->
    if
        not is_number(Value) -> false;
        not is_number(Limit) -> false;
        Value =< Limit -> true;
        true -> false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the numerical is within the given range, inclusive
%% @end
%%--------------------------------------------------------------------
-spec(numeric_between/3 :: (Value :: term(), Min :: number(), Max :: number()) -> boolean()).
numeric_between(Value, Min, Max) ->
    Min =< Max andalso numeric_min(Value, Min) andalso numeric_max(Value, Max).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the length (of a string or array) is greater than
%% or equal to the min
%% @end
%%--------------------------------------------------------------------
-spec(width/2 :: (Value :: term(), Min :: integer()) -> boolean()).
width(Value, Min) when is_atom(Value) ->
    Value =/= undefined andalso width(atom_to_list(Value), Min);
width(Value, Min) when is_binary(Value) ->
    width(binary_to_list(Value), Min);
width(Value, Min) when is_list(Value) ->
    length(Value) >= Min;
width(_Value, _Min) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the length (of a string or array) is within the
%% size range, inclusive
%% @end
%%--------------------------------------------------------------------
-spec(width/3 :: (Value :: term(), Min :: integer(), Max :: integer()) -> boolean()).
width(Value, Min, Max) when is_atom(Value) ->
    Value =/= undefined andalso width(atom_to_list(Value), Min, Max);
width(Value, Min, Max) when is_binary(Value) ->
    width(binary_to_list(Value), Min, Max);
width(Value, Min, Max) when is_list(Value) ->
    length(Value) >= Min andalso length(Value) =< Max;
width(_Value, _Min, _Max) ->
    false.








%% EUNIT TESTING
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%% required
required_test() ->
    Succeed = [<<"foo">>, <<"">>, foo, 5, false, true, [], ["foo", "bar"]],
    Fail = [undefined],
    validate(Succeed, Fail, fun required/1).

%% not_empty
not_empty_test() ->
    Succeed = [<<"foo">>, foo, 5, false, true, ["foo", "bar"]],
    Fail = [undefined, <<"">>, "", []],
    validate(Succeed, Fail, fun not_empty/1).

%% is_type
is_type_number_test() ->
    Succeed = [-10, 0, 10, 2.71828183, 3.14159265],
    Fail = [undefined, string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, [], [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, number) end).

is_type_float_test() ->
    Succeed = [2.71828183, 3.14159265],
    Fail = [undefined, -10, 0, 10, string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, [], [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, float) end).

is_type_integer_test() ->
    Succeed = [-10, -1, 0, 1, 10],
    Fail = [undefined, 2.71828183, 3.14159265, string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, [], [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, integer) end).

is_type_string_test() ->
    Succeed = [string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, []],
    Fail = [undefined, -10, -1, 0, 1, 10, 2.71828183, 3.14159265, <<"bell", 7>>, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, string) end).

is_type_boolean_test() ->
    Succeed = [<<"true">>, <<"false">>, true, false],
    Fail = [undefined, -10, -1, 0, 1, 10, 2.71828183, 3.14159265, string, <<"">>, <<"Test">>, [], [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, boolean) end).

is_type_array_test() ->
    Succeed = [[], [foo, <<"bar">>]],
    Fail = [undefined, -10, -1, 0, 1, 10, 2.71828183, 3.14159265, string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_type(E, array) end).

is_type_object_test() ->
    Succeed = [{struct, [{<<"key">>, []}]}],
    Fail = [undefined, -10, -1, 0, 1, 10, 2.71828183, 3.14159265, string, <<"">>, <<"Test">>, <<"true">>, <<"false">>, true, false, [], [foo, <<"bar">>]],
    validate(Succeed, Fail, fun(E) -> is_type(E, object) end).

%% is_format
is_format_ascii_test() ->
    Succeed = lists:seq(0, 127),
    Fail = [undefined, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_format([E], ascii) end).

is_format_graph_test() ->
    Succeed = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_format([E], graph) end).

is_format_print_test() ->
    Succeed = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" ++ [$ , $\t],
    Fail = [undefined, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_format([E], print) end).

is_format_line_test() ->
    Succeed = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" ++ [$ , $\t, $\r, $\n],
    Fail = [undefined, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> is_format([E], line) end).

is_format_space_test() ->
    Succeed = [$ , $\t, $\r, $\n, $\v, $\f],
    Fail = [undefined, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], space) end).

is_format_blank_test() ->
    Succeed = [$ , $\t],
    Fail = [undefined, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], blank) end).

is_format_punct_test() ->
    Succeed = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}\~",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    validate(Succeed, Fail, fun(E) -> is_format([E], punct) end).

is_format_word_test() ->
    Succeed = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], word) end).

is_format_alpha_test() ->
    Succeed = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], alpha) end).

is_format_numeric_test() ->
    Succeed = "0123456789",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], numeric) end).

is_format_hex_test() ->
    Succeed = "0123456789ABCDEFabcdef",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./:;<=>?@GHIJKLMNOPQRSTUVWXYZ[\\]^_`ghijklmnopqrstuvwxyz{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], hex) end).

is_format_phrase_test() ->
    Succeed = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_" ++ [$ , $\t],
    Fail = [undefined, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], phrase) end).

is_format_alpha_numeric_test() ->
    Succeed = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    Fail = [undefined, $ , $\t, $\r, $\n, $\v, $\f, -10, -1, 3.14159265, 128, 200, {struct, [{<<"key">>, []}]}] ++ "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
    validate(Succeed, Fail, fun(E) -> is_format([E], alpha_numeric) end).

%% numeric_min
numeric_min_test() ->
    Succeed = [0, 0.5, 1, 20, 30, 100],
    Fail = [undefined, -100, -30, -20, -10, -0.01, string, <<"">>, <<"Test">>, false, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> numeric_min(E, 0) end).

%% numeric_max
numeric_max_test() ->
    Succeed = [-100, -30, -20, -10, -0.1, 0],
    Fail = [undefined, 0.5, 1, 20, 30, 100, string, <<"">>, <<"Test">>, false, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed, Fail, fun(E) -> numeric_max(E, 0) end).

%% numeric_between
numeric_between_test() ->
    Succeed = [0, 0.375, 1, 5, 6, 10],
    Fail = [undefined, -10, -1, 10.1, 20, 30, 50, string, <<"">>, <<"Test">>, false, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    ?assertEqual(false, numeric_between(10, 10, 0)),
    validate(Succeed, Fail, fun(E) -> numeric_between(E, 0, 10) end).

%% width
width_test() ->
    Succeed1 = [test, "test", <<"test">>],
    Fail1 = [undefined, 0.5, 1, 20, 30, 100, foo, <<"">>, <<"bar">>, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed1, Fail1, fun(E) -> width(E, 4) end),
    Succeed2 = [test, "test", "test2", <<"test2">>],
    Fail2 = [undefined, 0.5, 1, 20, 30, 100, foo, <<"">>, <<"bar">>, test_this, <<"Hello World!">>, [foo, <<"bar">>], {struct, [{<<"key">>, []}]}],
    validate(Succeed2, Fail2, fun(E) -> width(E, 4, 5) end).

validate(Succeed, Fail, Func) ->
    lists:foreach(fun(Elem) ->
                          io:format("~p~n", [Elem]),
			  ?assertEqual(true, Func(Elem))
		  end, Succeed),
    lists:foreach(fun(Elem) ->
                          io:format("~p~n", [Elem]),
			  ?assertEqual(false, Func(Elem))
		  end, Fail).
-endif.