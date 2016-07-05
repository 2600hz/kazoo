%% @author root
%% @doc @todo Add description to kz_aws_http.


-module(kz_aws_http).

-export([make_query_string/1, make_query_string/2, value_to_string/1, url_encode/1, url_encode_loose/1]).

encode_query_term(Key, [], no_assignment) ->
    [Key];
encode_query_term(Key, [], empty_assignment) ->
    [Key, "="];
encode_query_term(Key, Value, _) ->
    [Key, "=", url_encode(value_to_string(Value))].

%% encode an empty value query string differently based on the
%% argument provided, this is based on the fact that S3 and SQS
%% sign url differently, S3 requires that empty arguments have no
%% '=' (/?acl) while SQS requires it (/?QueuePrefix=)
%% default behaviour is adding '='
make_query_string(Params) ->
    make_query_string(Params, empty_assignment).

make_query_string(Params, EmptyQueryOpt) ->
    string:join([encode_query_term(Key, Value, EmptyQueryOpt) || {Key, Value} <-
                                                                     Params, Value =/= none, Value =/= undefined], "&").

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> unicode:characters_to_binary(String).

url_encode(Binary) when is_binary(Binary) ->
    url_encode(unicode:characters_to_list(Binary));
url_encode(String) ->
    url_encode(String, []).
url_encode([], Accum) ->
    lists:reverse(Accum);
url_encode([Char|String], Accum)
  when Char >= $A, Char =< $Z;
       Char >= $a, Char =< $z;
       Char >= $0, Char =< $9;
       Char =:= $-; Char =:= $_;
       Char =:= $.; Char =:= $~ ->
    url_encode(String, [Char|Accum]);
url_encode([Char|String], Accum) ->
    url_encode(String, utf8_encode_char(Char) ++ Accum).

url_encode_loose(Binary) when is_binary(Binary) ->
    url_encode_loose(binary_to_list(Binary));
url_encode_loose(String) ->
    url_encode_loose(String, []).
url_encode_loose([], Accum) ->
    lists:reverse(Accum);
url_encode_loose([Char|String], Accum)
  when Char >= $A, Char =< $Z;
       Char >= $a, Char =< $z;
       Char >= $0, Char =< $9;
       Char =:= $-; Char =:= $_;
       Char =:= $.; Char =:= $~;
       Char =:= $/; Char =:= $: ->
    url_encode_loose(String, [Char|Accum]);
url_encode_loose([Char|String], Accum)
  when Char >=0, Char =< 255 ->
    url_encode_loose(String, [hex_char(Char rem 16), hex_char(Char div 16), $% | Accum]).

utf8_encode_char(Char) when Char > 16#7FFF, Char =< 16#7FFFF ->
    encode_char(Char band 16#3F + 16#80)
        ++ encode_char((16#3F band (Char bsr 6)) + 16#80)
        ++ encode_char((16#3F band (Char bsr 12)) + 16#80)
        ++ encode_char((Char bsr 18) + 16#F0);
utf8_encode_char(Char) when Char > 16#7FF, Char =< 16#7FFF ->
    encode_char(Char band 16#3F + 16#80)
        ++ encode_char((16#3F band (Char bsr 6)) + 16#80)
        ++ encode_char((Char bsr 12) + 16#E0);
utf8_encode_char(Char) when Char > 16#7F, Char =< 16#7FF ->
    encode_char(Char band 16#3F + 16#80)
        ++ encode_char((Char bsr 6) + 16#C0);
utf8_encode_char(Char) when Char =< 16#7F ->
    encode_char(Char).

encode_char(Char) ->
    [hex_char(Char rem 16), hex_char(Char div 16), $%].

hex_char(C) when C < 10 -> $0 + C;
hex_char(C) when C < 16 -> $A + C - 10.
