%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(base32).

%% API
-export([encode/1
        ,encode/2
        ,decode/1
        ,decode/2
        ]).

encode(Bin) when is_binary(Bin) -> encode(Bin, []);
encode(List) when is_list(List) -> encode(list_to_binary(List), []).

encode(Bin, Opts) when is_binary(Bin) andalso is_list(Opts) ->
    Hex = proplists:get_bool(hex, Opts),
    Lower = proplists:get_bool(lower, Opts),
    Fun = case Hex of
              true -> fun(I) -> hex_enc(Lower, I) end;
              false -> fun(I) -> std_enc(Lower,  I) end
          end,
    {Encoded0, Rest} = encode_body(Fun, Bin),
    {Encoded1, PadBy} = encode_rest(Fun, Rest),
    Padding = case proplists:get_bool(nopad, Opts) of
                  true -> <<>>;
                  false -> list_to_binary(lists:duplicate(PadBy, $=))
              end,
    <<Encoded0/binary, Encoded1/binary, Padding/binary>>;
encode(List, Opts) when is_list(List) andalso is_list(Opts) ->
    encode(list_to_binary(List), Opts).

encode_body(Fun, Bin) ->
    Offset = 5 * (byte_size(Bin) div 5),
    <<Body:Offset/binary, Rest/binary>> = Bin,
    {<< <<(Fun(I))>> || <<I:5>> <= Body>>, Rest}.

encode_rest(Fun, Bin) ->
    Whole = bit_size(Bin) div 5,
    Offset = 5 * Whole,
    <<Body:Offset/bits, Rest/bits>> = Bin,
    Body0 = << <<(Fun(I))>> || <<I:5>> <= Body>>,
    {Body1, Pad} = case Rest of
                       <<I:3>> -> {<<(Fun(I bsl 2))>>, 6};
                       <<I:1>> -> {<<(Fun(I bsl 4))>>, 4};
                       <<I:4>> -> {<<(Fun(I bsl 1))>>, 3};
                       <<I:2>> -> {<<(Fun(I bsl 3))>>, 1};
                       <<>> -> {<<>>, 0}
                   end,
    {<<Body0/binary, Body1/binary>>, Pad}.

std_enc(_, I) when is_integer(I) andalso I >= 26 andalso I =< 31 -> I + 24;
std_enc(Lower, I) when is_integer(I) andalso I >= 0 andalso I =< 25 ->
    case Lower of
        true -> I + $a;
        false -> I + $A
    end.

hex_enc(_, I) when is_integer(I) andalso I >= 0 andalso I =< 9 -> I + 48;
hex_enc(Lower, I) when is_integer(I) andalso I >= 10 andalso I =< 31 ->
    case Lower of
        true -> I + 87;
        false -> I + 55
    end.

decode(Bin) when is_binary(Bin) -> decode(Bin, []);
decode(List) when is_list(List) -> decode(list_to_binary(List), []).

decode(Bin, Opts) when is_binary(Bin) andalso is_list(Opts) ->
    Fun = case proplists:get_bool(hex, Opts) of
              true -> fun hex_dec/1;
              false -> fun std_dec/1
          end,
    decode(Fun, Bin, <<>>);
decode(List, Opts) when is_list(List) andalso is_list(Opts) ->
    decode(list_to_binary(List), Opts).

decode(Fun, <<X, "======">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 2):3>>;
decode(Fun, <<X, "====">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 4):1>>;
decode(Fun, <<X, "===">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 1):4>>;
decode(Fun, <<X, "=">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 3):2>>;
decode(Fun, <<X, Rest/binary>>, Bits) ->
    decode(Fun, Rest, <<Bits/bits, (Fun(X)):5>>);
decode(_Fun, <<>>, Bin) -> Bin.

std_dec(I) when I >= $2 andalso I =< $7 -> I - 24;
std_dec(I) when I >= $a andalso I =< $z -> I - $a;
std_dec(I) when I >= $A andalso I =< $Z -> I - $A.

hex_dec(I) when I >= $0 andalso I =< $9 -> I - 48;
hex_dec(I) when I >= $a andalso I =< $z -> I - 87;
hex_dec(I) when I >= $A andalso I =< $Z -> I - 55.