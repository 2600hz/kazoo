%% JSON - RFC 4627 - for Erlang
%%---------------------------------------------------------------------------
%% @author Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% @author LShift Ltd. <query@lshift.net>
%% @copyright 2007-2010, 2011, 2012 Tony Garnock-Jones and 2007-2010 LShift Ltd.
%% @license
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%%
%% @reference <a href="http://www.ietf.org/rfc/rfc4627.txt">RFC
%% 4627</a>, the JSON RFC
%%
%% @reference <a href="http://www.json.org/">JSON in general</a>
%%
%% @reference Joe Armstrong's <a
%% href="http://erlang.org/pipermail/erlang-questions/2005-November/017805.html">
%% message</a> describing the basis of the JSON data type mapping that
%% this module uses
%%
%% @doc An implementation of RFC 4627 (JSON, the JavaScript Object Notation) for Erlang.
%%
%% The basic API is comprised of the {@link encode/1} and {@link decode/1} functions.
%%
%% == Data Type Mapping ==
%%
%% The data type mapping I've implemented is as per Joe Armstrong's
%% message [http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html] - see {@link json()}.
%%
%% == Unicode ==
%%
%% When serializing a string, if characters are found with codepoint
%% >127, we rely on the unicode encoder to build the proper byte
%% sequence for transmission. We still use the \uXXXX escape for
%% control characters (other than the RFC-specified specially
%% recognised ones).
%%
%% {@link decode/1} will autodetect the unicode encoding used, and any
%% strings returned in the result as binaries will contain UTF-8
%% encoded byte sequences for codepoints >127. Object keys containing
%% codepoints >127 will be returned as lists of codepoints, rather
%% than being UTF-8 encoded. If you have already transformed the text
%% to parse into a list of unicode codepoints, perhaps by your own use
%% of {@link unicode_decode/1}, then use {@link decode_noauto/1} to
%% avoid redundant and erroneous double-unicode-decoding.
%%
%% Similarly, {@link encode/1} produces text that is already UTF-8
%% encoded. To get raw codepoints, use {@link encode_noauto/1} and
%% {@link encode_noauto/2}. You can use {@link unicode_encode/1} to
%% UTF-encode the results, if that's appropriate for your application.
%%
%% == Differences to the specification ==
%%
%% I'm lenient in the following ways during parsing:
%%
%% <ul>
%%  <li>repeated commas in arrays and objects collapse to a single comma</li>
%%  <li>any character =&lt;32 is considered whitespace</li>
%%  <li>leading zeros for numbers are accepted</li>
%%  <li>we don't restrict the toplevel token to only object or array -
%%      any JSON value can be used at toplevel</li>
%% </ul>

%% @type json() = jsonobj() | jsonarray() | jsonnum() | jsonstr() | true | false | null. An Erlang representation of a general JSON value.
%% @type jsonobj() = {obj, [{jsonkey(), json()}]}. A JSON "object" or "struct".
%% @type jsonkey() = string(). A field-name within a JSON "object".
%% @type jsonarray() = [json()]. A JSON array value.
%% @type jsonnum() = integer() | float(). A JSON numeric value.
%% @type jsonstr() = binary(). A JSON string value.
%% @type byte() = integer(). An integer >=0 and =&lt;255.

-module(rfc4627).

-export([mime_type/0, encode/1, decode/1]).
-export([encode_noauto/1, encode_noauto/2, decode_noauto/1]).
-export([unicode_decode/1, unicode_encode/1]).
-export([from_record/3, to_record/3]).
-export([hex_digit/1, digit_hex/1]).
-export([get_field/2, get_field/3, set_field/3, exclude_field/2]).
-export([equiv/2]).

%% @spec () -> string()
%% @doc Returns the IANA-registered MIME type for JSON data.
mime_type() ->
    "application/json".

%% @spec (json()) -> [byte()]
%%
%% @doc Encodes the JSON value supplied, first into Unicode
%% codepoints, and then into UTF-8.
%%
%% The resulting string is a list of byte values that should be
%% interpreted as UTF-8 encoded text.
%%
%% During encoding, atoms and binaries are accepted as keys of JSON
%% objects (type {@link jsonkey()}) as well as the usual strings
%% (lists of character codepoints).
encode(X) ->
    unicode_encode({'utf-8', encode_noauto(X)}).

%% @spec (json()) -> string()
%%
%% @doc Encodes the JSON value supplied into raw Unicode codepoints.
%%
%% The resulting string may contain codepoints with value >=128. You
%% can use {@link unicode_encode/1} to UTF-encode the results, if
%% that's appropriate for your application.
%%
%% During encoding, atoms and binaries are accepted as keys of JSON
%% objects (type {@link jsonkey()}) as well as the usual strings
%% (lists of character codepoints).
encode_noauto(X) ->
    lists:reverse(encode_noauto(X, [])).

%% @spec (json(), string()) -> string()
%%
%% @doc As {@link encode_noauto/1}, but prepends <i>reversed</i> text
%% to the supplied accumulator string.
encode_noauto(true, Acc) ->
    "eurt" ++ Acc;
encode_noauto(false, Acc) ->
    "eslaf" ++ Acc;
encode_noauto(null, Acc) ->
    "llun" ++ Acc;
encode_noauto(Str, Acc) when is_binary(Str) ->
    Codepoints = xmerl_ucs:from_utf8(Str),
    quote_and_encode_string(Codepoints, Acc);
encode_noauto(Str, Acc) when is_atom(Str) ->
    quote_and_encode_string(atom_to_list(Str), Acc);
encode_noauto(Num, Acc) when is_number(Num) ->
    encode_number(Num, Acc);
encode_noauto({obj, Fields}, Acc) ->
    "}" ++ encode_object(Fields, "{" ++ Acc);
encode_noauto(Dict, Acc) when element(1, Dict) =:= dict ->
    "}" ++ encode_object(dict:to_list(Dict), "{" ++ Acc);
encode_noauto(Arr, Acc) when is_list(Arr) ->
    "]" ++ encode_array(Arr, "[" ++ Acc).

encode_object([], Acc) ->
    Acc;
encode_object([{Key, Value}], Acc) ->
    encode_field(Key, Value, Acc);
encode_object([{Key, Value} | Rest], Acc) ->
    encode_object(Rest, "," ++ encode_field(Key, Value, Acc)).

encode_field(Key, Value, Acc) when is_binary(Key) ->
    Codepoints = xmerl_ucs:from_utf8(Key),
    encode_noauto(Value, ":" ++ quote_and_encode_string(Codepoints, Acc));
encode_field(Key, Value, Acc) when is_atom(Key) ->
    encode_noauto(Value, ":" ++ quote_and_encode_string(atom_to_list(Key), Acc));
encode_field(Key, Value, Acc) when is_list(Key) ->
    encode_noauto(Value, ":" ++ quote_and_encode_string(Key, Acc)).

encode_array([], Acc) ->
    Acc;
encode_array([X], Acc) ->
    encode_noauto(X, Acc);
encode_array([X | Rest], Acc) ->
    encode_array(Rest, "," ++ encode_noauto(X, Acc)).

quote_and_encode_string(Str, Acc) ->
    "\"" ++ encode_string(Str, "\"" ++ Acc).

encode_string([], Acc) ->
    Acc;
encode_string([$" | Rest], Acc) ->
    encode_string(Rest, [$", $\\ | Acc]);
encode_string([$\\ | Rest], Acc) ->
    encode_string(Rest, [$\\, $\\ | Acc]);
encode_string([X | Rest], Acc) when X < 32 orelse X > 127 ->
    encode_string(Rest, encode_general_char(X, Acc));
encode_string([X | Rest], Acc) ->
    encode_string(Rest, [X | Acc]).

encode_general_char(8, Acc) -> [$b, $\\ | Acc];
encode_general_char(9, Acc) -> [$t, $\\ | Acc];
encode_general_char(10, Acc) -> [$n, $\\ | Acc];
encode_general_char(12, Acc) -> [$f, $\\ | Acc];
encode_general_char(13, Acc) -> [$r, $\\ | Acc];
encode_general_char(X, Acc) when X > 127 -> [X | Acc];
encode_general_char(X, Acc) ->
    %% FIXME currently this branch never runs.
    %% We could make it configurable, maybe?
    Utf16Bytes = xmerl_ucs:to_utf16be(X),
    encode_utf16be_chars(Utf16Bytes, Acc).

encode_utf16be_chars([], Acc) ->
    Acc;
encode_utf16be_chars([B1, B2 | Rest], Acc) ->
    encode_utf16be_chars(Rest, [hex_digit((B2) band 16#F),
				hex_digit((B2 bsr 4) band 16#F),
				hex_digit((B1) band 16#F),
				hex_digit((B1 bsr 4) band 16#F),
				$u,
				$\\ | Acc]).

%% @spec (Nibble::integer()) -> char()
%% @doc Returns the character code corresponding to Nibble.
%%
%% Nibble must be >=0 and =&lt;15.
hex_digit(N) when is_integer(N), N >= 0, N =< 9 -> $0 + N;
hex_digit(N) when is_integer(N), N >= 10, N =< 15 -> $A + N - 10.

encode_number(Num, Acc) when is_integer(Num) ->
    lists:reverse(integer_to_list(Num), Acc);
encode_number(Num, Acc) when is_float(Num) ->
    lists:reverse(float_to_list(Num), Acc).

%% @spec (Input::(binary() | [byte()])) -> ({ok, json(), Remainder} | {error, Reason})
%% where Remainder = string()
%%       Reason = any()
%%
%% @doc Decodes a JSON value from an input binary or string of
%% Unicode-encoded text.
%%
%% Given a binary, converts it to a list of bytes. Given a
%% list/string, interprets it as a list of bytes.
%%
%% Uses {@link unicode_decode/1} on its input, which results in a list
%% of codepoints, and then decodes a JSON value from that list of
%% codepoints.
%%
%% Returns either `{ok, Result, Remainder}', where Remainder is the
%% remaining portion of the input that was not consumed in the process
%% of decoding Result, or `{error, Reason}'.
decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode(Bytes) ->
    {_Charset, Codepoints} = unicode_decode(Bytes),
    decode_noauto(Codepoints).

%% @spec (Input::string()) -> ({ok, json(), string()} | {error, any()})
%%
%% @doc As {@link decode/1}, but does not perform Unicode decoding on its input.
%%
%% Expects a list of codepoints - an ordinary Erlang string - rather
%% than a list of Unicode-encoded bytes.
decode_noauto(Bin) when is_binary(Bin) ->
    decode_noauto(binary_to_list(Bin));
decode_noauto(Chars) ->
    case catch parse(skipws(Chars)) of
	{'EXIT', Reason} ->
	    %% Reason is usually far too much information, but helps
	    %% if needing to debug this module.
	    {error, Reason};
	{Value, Remaining} ->
	    {ok, Value, skipws(Remaining)}
    end.

%% @spec ([byte()]) -> [char()]
%%
%% @doc Autodetects and decodes using the Unicode encoding of its input.
%%
%% From RFC4627, section 3, "Encoding":
%%
%% <blockquote>
%%    JSON text SHALL be encoded in Unicode.  The default encoding is
%%    UTF-8.
%%
%%    Since the first two characters of a JSON text will always be ASCII
%%    characters [RFC0020], it is possible to determine whether an octet
%%    stream is UTF-8, UTF-16 (BE or LE), or UTF-32 (BE or LE) by looking
%%    at the pattern of nulls in the first four octets.
%%
%%            00 00 00 xx  UTF-32BE
%%            00 xx 00 xx  UTF-16BE
%%            xx 00 00 00  UTF-32LE
%%            xx 00 xx 00  UTF-16LE
%%            xx xx xx xx  UTF-8
%% </blockquote>
%%
%% Interestingly, the BOM (byte-order mark) is not mentioned. We
%% support it here by using it to detect our encoding, discarding it
%% if present, even though RFC4627 explicitly notes that the first two
%% characters of a JSON text will be ASCII.
%%
%% If a BOM ([http://unicode.org/faq/utf_bom.html]) is present, we use
%% that; if not, we use RFC4627's rules (as above). Note that UTF-32
%% is the same as UCS-4 for our purposes (but see also
%% [http://unicode.org/reports/tr19/tr19-9.html]). Note that UTF-16 is
%% not the same as UCS-2!
%%
%% Note that I'm using xmerl's UCS/UTF support here. There's another
%% UTF-8 codec in asn1rt, which works on binaries instead of lists.
%%
unicode_decode([0,0,254,255|C]) -> {'utf-32', xmerl_ucs:from_ucs4be(C)};
unicode_decode([255,254,0,0|C]) -> {'utf-32', xmerl_ucs:from_ucs4le(C)};
unicode_decode([254,255|C]) -> {'utf-16', xmerl_ucs:from_utf16be(C)};
unicode_decode([239,187,191|C]) -> {'utf-8', xmerl_ucs:from_utf8(C)};
unicode_decode(C=[0,0,_,_|_]) -> {'utf-32be', xmerl_ucs:from_ucs4be(C)};
unicode_decode(C=[_,_,0,0|_]) -> {'utf-32le', xmerl_ucs:from_ucs4le(C)};
unicode_decode(C=[0,_|_]) -> {'utf-16be', xmerl_ucs:from_utf16be(C)};
unicode_decode(C=[_,0|_]) -> {'utf-16le', xmerl_ucs:from_utf16le(C)};
unicode_decode(C=_) -> {'utf-8', xmerl_ucs:from_utf8(C)}.

%% @spec (EncodingAndCharacters::{Encoding, [char()]}) -> [byte()]
%% where Encoding = 'utf-32' | 'utf-32be' | 'utf-32le' | 'utf-16' |
%%                  'utf-16be' | 'utf-16le' | 'utf-8'
%%
%% @doc Encodes the given characters to bytes, using the given Unicode encoding.
%%
%% For convenience, we supply a partial inverse of unicode_decode; If
%% a BOM is requested, we more-or-less arbitrarily pick the big-endian
%% variant of the encoding, since big-endian is network-order. We
%% don't support UTF-8 with BOM here.
unicode_encode({'utf-32', C}) -> [0,0,254,255|xmerl_ucs:to_ucs4be(C)];
unicode_encode({'utf-32be', C}) -> xmerl_ucs:to_ucs4be(C);
unicode_encode({'utf-32le', C}) -> xmerl_ucs:to_ucs4le(C);
unicode_encode({'utf-16', C}) -> [254,255|xmerl_ucs:to_utf16be(C)];
unicode_encode({'utf-16be', C}) -> xmerl_ucs:to_utf16be(C);
unicode_encode({'utf-16le', C}) -> xmerl_ucs:to_utf16le(C);
unicode_encode({'utf-8', C}) -> xmerl_ucs:to_utf8(C).

parse([$" | Rest]) -> %% " emacs balancing
    {Codepoints, Rest1} = parse_string(Rest, []),
    {list_to_binary(xmerl_ucs:to_utf8(Codepoints)), Rest1};
parse("true" ++ Rest) -> {true, Rest};
parse("false" ++ Rest) -> {false, Rest};
parse("null" ++ Rest) -> {null, Rest};
parse([${ | Rest]) -> parse_object(skipws(Rest), []);
parse([$[ | Rest]) -> parse_array(skipws(Rest), []);
parse([]) -> exit(unexpected_end_of_input);
parse(Chars) -> parse_number(Chars, []).

skipws([X | Rest]) when X =< 32 ->
    skipws(Rest);
skipws(Chars) ->
    Chars.

parse_string(Chars, Acc) ->
    case parse_codepoint(Chars) of
	{done, Rest} ->
	    {lists:reverse(Acc), Rest};
	{ok, Codepoint, Rest} ->
	    parse_string(Rest, [Codepoint | Acc])
    end.

parse_codepoint([$" | Rest]) -> %% " emacs balancing
    {done, Rest};
parse_codepoint([$\\, Key | Rest]) ->
    parse_general_char(Key, Rest);
parse_codepoint([X | Rest]) ->
    {ok, X, Rest}.

parse_general_char($b, Rest) -> {ok, 8, Rest};
parse_general_char($t, Rest) -> {ok, 9, Rest};
parse_general_char($n, Rest) -> {ok, 10, Rest};
parse_general_char($f, Rest) -> {ok, 12, Rest};
parse_general_char($r, Rest) -> {ok, 13, Rest};
parse_general_char($/, Rest) -> {ok, $/, Rest};
parse_general_char($\\, Rest) -> {ok, $\\, Rest};
parse_general_char($", Rest) -> {ok, $", Rest};
parse_general_char($u, [D0, D1, D2, D3 | Rest]) ->
    Codepoint =
	(digit_hex(D0) bsl 12) +
	(digit_hex(D1) bsl 8) +
	(digit_hex(D2) bsl 4) +
	(digit_hex(D3)),
    if
	Codepoint >= 16#D800 andalso Codepoint < 16#DC00 ->
	    % High half of surrogate pair
	    case parse_codepoint(Rest) of
		{low_surrogate_pair, Codepoint2, Rest1} ->
		    [FinalCodepoint] =
			xmerl_ucs:from_utf16be(<<Codepoint:16/big-unsigned-integer,
						Codepoint2:16/big-unsigned-integer>>),
		    {ok, FinalCodepoint, Rest1};
		_ ->
		    exit(incorrect_usage_of_surrogate_pair)
	    end;
	Codepoint >= 16#DC00 andalso Codepoint < 16#E000 ->
	    {low_surrogate_pair, Codepoint, Rest};
	true ->
	    {ok, Codepoint, Rest}
    end.

%% @spec (Hexchar::char()) -> integer()
%% @doc Returns the number corresponding to Hexchar.
%%
%% Hexchar must be one of the characters `$0' through `$9', `$A'
%% through `$F' or `$a' through `$f'.
digit_hex(C) when is_integer(C), C >= $0, C =< $9 -> C - $0;
digit_hex(C) when is_integer(C), C >= $A, C =< $F -> C - $A + 10;
digit_hex(C) when is_integer(C), C >= $a, C =< $f -> C - $a + 10.

finish_number(Acc, Rest) ->
    Str = lists:reverse(Acc),
    {case catch list_to_integer(Str) of
	 {'EXIT', _} -> list_to_float(Str);
	 Value -> Value
     end, Rest}.

parse_number([$- | Rest], Acc) ->
    parse_number1(Rest, [$- | Acc]);
parse_number(Rest = [C | _], Acc) ->
    case is_digit(C) of
	true -> parse_number1(Rest, Acc);
	false -> exit(syntax_error)
    end.

parse_number1(Rest, Acc) ->
    {Acc1, Rest1} = parse_int_part(Rest, Acc),
    case Rest1 of
	[] -> finish_number(Acc1, []);
	[$. | More] ->
            {Acc2, Rest2} = parse_int_part(More, [$. | Acc1]),
            parse_exp(Rest2, Acc2, false);
        _ ->
            parse_exp(Rest1, Acc1, true)
    end.

parse_int_part(Chars = [_Ch | _Rest], Acc) ->
    parse_int_part0(Chars, Acc).

parse_int_part0([], Acc) ->
    {Acc, []};
parse_int_part0([Ch | Rest], Acc) ->
    case is_digit(Ch) of
	true -> parse_int_part0(Rest, [Ch | Acc]);
	false -> {Acc, [Ch | Rest]}
    end.

parse_exp([$e | Rest], Acc, NeedFrac) ->
    parse_exp1(Rest, Acc, NeedFrac);
parse_exp([$E | Rest], Acc, NeedFrac) ->
    parse_exp1(Rest, Acc, NeedFrac);
parse_exp(Rest, Acc, _NeedFrac) ->
    finish_number(Acc, Rest).

parse_exp1(Rest, Acc, NeedFrac) ->
    {Acc1, Rest1} = parse_signed_int_part(Rest, if
						    NeedFrac -> [$e, $0, $. | Acc];
						    true -> [$e | Acc]
						end),
    finish_number(Acc1, Rest1).

parse_signed_int_part([$+ | Rest], Acc) ->
    parse_int_part(Rest, [$+ | Acc]);
parse_signed_int_part([$- | Rest], Acc) ->
    parse_int_part(Rest, [$- | Acc]);
parse_signed_int_part(Rest, Acc) ->
    parse_int_part(Rest, Acc).

is_digit(N) when is_integer(N) -> N >= $0 andalso N =< $9;
is_digit(_) -> false.

parse_object([$} | Rest], Acc) ->
    {{obj, lists:reverse(Acc)}, Rest};
parse_object([$, | Rest], Acc) ->
    parse_object(skipws(Rest), Acc);
parse_object([$" | Rest], Acc) -> %% " emacs balancing
    {KeyCodepoints, Rest1} = parse_string(Rest, []),
    [$: | Rest2] = skipws(Rest1),
    {Value, Rest3} = parse(skipws(Rest2)),
    parse_object(skipws(Rest3), [{KeyCodepoints, Value} | Acc]).

parse_array([$] | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_array([$, | Rest], Acc) ->
    parse_array(skipws(Rest), Acc);
parse_array(Chars, Acc) ->
    {Value, Rest} = parse(Chars),
    parse_array(skipws(Rest), [Value | Acc]).

%% @spec (Record, atom(), [any()]) -> jsonobj()
%% where Record = tuple()
%%
%% @doc Used by the `?RFC4627_FROM_RECORD' macro in `rfc4627.hrl'.
%%
%% Given a record type definiton of ``-record(myrecord, {field1,
%% field})'', and a value ``V = #myrecord{}'', the code
%% ``?RFC4627_FROM_RECORD(myrecord, V)'' will return a JSON "object"
%% with fields corresponding to the fields of the record. The macro
%% expands to a call to the `from_record' function.
from_record(R, _RecordName, Fields) ->
    {obj, encode_record_fields(R, 2, Fields)}.

encode_record_fields(_R, _Index, []) ->
    [];
encode_record_fields(R, Index, [Field | Rest]) ->
    case element(Index, R) of
	undefined ->
	    encode_record_fields(R, Index + 1, Rest);
	Value ->
	    [{atom_to_list(Field), Value} | encode_record_fields(R, Index + 1, Rest)]
    end.

%% @spec (JsonObject::jsonobj(), DefaultValue::Record, [atom()]) -> Record
%% where Record = tuple()
%%
%% @doc Used by the `?RFC4627_TO_RECORD' macro in `rfc4627.hrl'.
%%
%% Given a record type definiton of ``-record(myrecord, {field1,
%% field})'', and a JSON "object" ``J = {obj, [{"field1", 123},
%% {"field2", 234}]}'', the code ``?RFC4627_TO_RECORD(myrecord, J)''
%% will return a record ``#myrecord{field1 = 123, field2 = 234}''.
%% The macro expands to a call to the `to_record' function.
to_record({obj, Values}, Fallback, Fields) ->
    list_to_tuple([element(1, Fallback) | decode_record_fields(Values, Fallback, 2, Fields)]).

decode_record_fields(_Values, _Fallback, _Index, []) ->
    [];
decode_record_fields(Values, Fallback, Index, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     element(Index, Fallback)
     end | decode_record_fields(Values, Fallback, Index + 1, Rest)].

%% @spec (JsonObject::jsonobj(), atom()) -> jsonobj()
%% @doc Exclude a named field from a JSON "object".
exclude_field({obj, Props}, Key) ->
    {obj, lists:keydelete(Key, 1, Props)}.

%% @spec (JsonObject::jsonobj(), atom()) -> {ok, json()} | not_found
%% @doc Retrieves the value of a named field of a JSON "object".
get_field({obj, Props}, Key) ->
    case lists:keysearch(Key, 1, Props) of
	{value, {_K, Val}} ->
	    {ok, Val};
	false ->
	    not_found
    end.

%% @spec (jsonobj(), atom(), json()) -> json()
%% @doc Retrieves the value of a named field of a JSON "object", or a
%% default value if no such field is present.
get_field(Obj, Key, DefaultValue) ->
    case get_field(Obj, Key) of
	{ok, Val} ->
	    Val;
	not_found ->
	    DefaultValue
    end.

%% @spec (JsonObject::jsonobj(), atom(), json()) -> jsonobj()
%% @doc Adds or replaces a named field with the given value.
%%
%% Returns a JSON "object" that contains the new field value as well
%% as all the unmodified fields from the first argument.
set_field({obj, Props}, Key, NewValue) ->
    {obj, [{Key, NewValue} | lists:keydelete(Key, 1, Props)]}.

%% @spec (A::json(), B::json()) -> bool()
%% @doc Tests equivalence of JSON terms.
%%
%% After Bob Ippolito's `equiv' predicate in mochijson.
equiv({obj, Props1}, {obj, Props2}) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    equiv_sorted_plists(L1, L2);
equiv(A, B) when is_list(A) andalso is_list(B) ->
    equiv_arrays(A, B);
equiv(A, B) ->
    A == B.

equiv_sorted_plists([], []) -> true;
equiv_sorted_plists([], _) -> false;
equiv_sorted_plists(_, []) -> false;
equiv_sorted_plists([{K1, V1} | R1], [{K2, V2} | R2]) ->
    K1 == K2 andalso equiv(V1, V2) andalso equiv_sorted_plists(R1, R2).

equiv_arrays([], []) -> true;
equiv_arrays([], _) -> false;
equiv_arrays(_, []) -> false;
equiv_arrays([V1 | R1], [V2 | R2]) ->
    equiv(V1, V2) andalso equiv_arrays(R1, R2).
