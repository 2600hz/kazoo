%%%-------------------------------------------------------------------
%%% @copyright (C) 2016 2600HZ
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cb_otp_util).
-include("crossbar.hrl").

-export([encode_base32/1
        ,encode_base32/2
        ,decode_base32/1
        ,decode_base32/2
        ,sha_mac/2
        ,timestamp/0
        ,create_qr_code/1
       ]).

encode_base32(Bin) when is_binary(Bin) -> encode_base32(Bin, []);
encode_base32(List) when is_list(List) -> encode_base32(list_to_binary(List), []).

encode_base32(Bin, Opts) when is_binary(Bin) andalso is_list(Opts) ->
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
encode_base32(List, Opts) when is_list(List) andalso is_list(Opts) ->
    encode_base32(list_to_binary(List), Opts).

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

decode_base32(Bin) when is_binary(Bin) -> decode_base32(Bin, []);
decode_base32(List) when is_list(List) -> decode_base32(list_to_binary(List), []).

decode_base32(Bin, Opts) when is_binary(Bin) andalso is_list(Opts) ->
    Fun = case proplists:get_bool(hex, Opts) of
              true -> fun hex_dec/1;
              false -> fun std_dec/1
          end,
    decode_base32(Fun, Bin, <<>>);
decode_base32(List, Opts) when is_list(List) andalso is_list(Opts) ->
    decode_base32(list_to_binary(List), Opts).

decode_base32(Fun, <<X, "======">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 2):3>>;
decode_base32(Fun, <<X, "====">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 4):1>>;
decode_base32(Fun, <<X, "===">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 1):4>>;
decode_base32(Fun, <<X, "=">>, Bits) ->
    <<Bits/bits, (Fun(X) bsr 3):2>>;
decode_base32(Fun, <<X, Rest/binary>>, Bits) ->
    decode_base32(Fun, Rest, <<Bits/bits, (Fun(X)):5>>);
decode_base32(_Fun, <<>>, Bin) -> Bin.

std_dec(I) when I >= $2 andalso I =< $7 -> I - 24;
std_dec(I) when I >= $a andalso I =< $z -> I - $a;
std_dec(I) when I >= $A andalso I =< $Z -> I - $A.

hex_dec(I) when I >= $0 andalso I =< $9 -> I - 48;
hex_dec(I) when I >= $a andalso I =< $z -> I - 87;
hex_dec(I) when I >= $A andalso I =< $Z -> I - 55.

sha_mac(K, S) ->
    try
        crypto:hmac(sha, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

-spec timestamp() -> integer().
timestamp() ->
    {Mega, Secs, _} = os:timestamp(),
    Time = Mega*1000000 + Secs,
    Time div 30.

-spec create_qr_code(api_binary()) -> kz_proplist().
create_qr_code('undefined') -> {'error','undefined'};
create_qr_code(Secret) ->
    lager:debug("create qr code for ~b", [Secret]),
    CHL = kz_util:to_binary(Secret),
    Url = <<"https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=otpauth://totp/OTP?secret=", CHL/binary, "&choe=UTF-8">>,
    case kz_http:get(kz_util:to_list(Url)) of
        {'ok', 200, _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s: ~s", [Url, RespBody]),
            {CHL, base64:encode(RespBody)};
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            {'error','undefined'}
    end.
