%%%-------------------------------------------------------------------
%%% @copyright (C) 2016 2600HZ
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cb_otp).
-include("crossbar.hrl").

%% API
-export([generate_hotp/2
        ,generate_totp/1
        ,generate_random_otp/0
        ,generate_secret/0
        ,generate_secret_and_qr_code/0
       ]).

%%---------------------------------------------------------------------
%% @doc
%% Generate a secret token - changing the value of <20 will give error
%% as the hmac method by default uses 20 as minimum value
%% @end
%%---------------------------------------------------------------------
-spec generate_secret() -> ne_binary().
generate_secret() ->
    Key = crypto:rand_bytes(20),
    cb_otp_util:encode_base32(Key,['nopad']).

%%---------------------------------------------------------------------
%% @doc
%% Generate a secret token and qr code
%% @end
%%---------------------------------------------------------------------
-spec generate_secret_and_qr_code() -> kz_proplist().
generate_secret_and_qr_code() ->
    cb_otp_util:create_qr_code(generate_secret()).

%%---------------------------------------------------------------------
%% @doc
%% generate a one time token based on a secret and a
%% interval number.
%% @end
%%---------------------------------------------------------------------
-spec generate_hotp(ne_binary(), integer()) -> integer().
generate_hotp(Secret, Interval) ->
    generate_otp(Secret, Interval).

%%---------------------------------------------------------------------
%% @doc
%% generate a one time token based on a secret time.
%% @end
%%---------------------------------------------------------------------
-spec generate_totp(ne_binary()) -> integers().
generate_totp(Secret) when is_binary(Secret)->
    Timestamp = cb_otp_util:timestamp(),
    Strict = kapps_config:get_is_true(?CONFIG_CAT, <<"otp_strict_validation">>, 'false'),
    case Strict of
        'true' ->
            [generate_otp(Secret, Timestamp)];
        'false' ->
            [generate_otp(Secret, Timestamp-1)
            ,generate_otp(Secret, Timestamp)
            ,generate_otp(Secret, Timestamp-1)]
    end.

%%---------------------------------------------------------------------
%% @doc
%% generate a random 6 digit one time token.
%% useful for sms validating and getting a random six digit number
%% @end
%%---------------------------------------------------------------------
-spec generate_random_otp() -> integer().
generate_random_otp() ->
    Timestamp = cb_otp_util:timestamp(),
    generate_otp(generate_secret(), Timestamp).

%%---------------------------------------------------------------------
%% @doc
%% generate a 6 digit one time token.
%% @end
%%---------------------------------------------------------------------
-spec generate_otp(ne_binary(), integer()) -> integer().
generate_otp(Secret, Interval) when is_binary(Secret) and
                                    is_integer(Interval) ->
    Key = cb_otp_util:decode_base32(Secret),
    Digest = cb_otp_util:sha_mac(Key, <<Interval:64/integer>>),
    <<_:19/bytes,_:4/bits,Offset:4>> = Digest,
    <<_:Offset/bytes,_:1/bits,P:31,_/binary>> = Digest,
    P rem 1000000.
