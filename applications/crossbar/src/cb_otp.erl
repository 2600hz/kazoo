%%%-------------------------------------------------------------------
%%% @copyright (C) 2016 2600HZ
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cb_otp).
-include("crossbar.hrl").

%% API
-export([generate_secret/0
        ,filter_secret/1
        ,validate_totp/2
        ,generate_hotp/2
        ,generate_totp/1
        ,generate_random_otp/0
       ]).

%%---------------------------------------------------------------------
%% @doc
%% Validates the token received from user against the Secret
%% and current time interval
%% @end
%%---------------------------------------------------------------------
-spec validate_totp(ne_binary(), ne_binary()) -> boolean().
validate_totp(Secret, OTP) ->
    Tokens = generate_totp(Secret),
    validate_token(Tokens, OTP).

%%---------------------------------------------------------------------
%% @doc
%% Removes the secret from the json object
%% @end
%%---------------------------------------------------------------------
-spec filter_secret(kz_json:object()) -> kz_json:object().
filter_secret(JObj) ->
    kz_json:filter(fun({K, _}) -> (not is_secret(K)) end, JObj).

-spec is_secret(key()) -> boolean().
is_secret(<<"otp_secret">>) -> 'true';
is_secret(_) -> false.

%%---------------------------------------------------------------------
%% @doc
%% matches the token with the one provided by user
%% and return boolean value accordingly
%% @end
%%---------------------------------------------------------------------
-spec validate_token(integers(), ne_binary()) -> boolean().
validate_token([], _OTP) ->
    lager:debug("OTP did not match with the provided token"),
    'false';
validate_token([Token|Tokens], OTP) ->
    case kz_util:to_binary(Token) of
        OTP ->
            lager:debug("Token matched with provided OTP"),
            'true';
        _ -> validate_token(Tokens, OTP)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Generate a secret token - changing the value of <20 will give error
%% as the hmac method by default uses 20 as minimum value
%% @end
%%---------------------------------------------------------------------
-spec generate_secret() -> ne_binary().
generate_secret() ->
    Key = crypto:rand_bytes(20),
    base32:encode(Key,['nopad']).

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
%% generate a one time token based on a secret and current time.
%% The fn also checks for if strict validation is required which
%% will only check for current 30 sec token otherwise match it
%% with past, now and future 30 secs to account for time lag
%% @end
%%---------------------------------------------------------------------
-spec generate_totp(ne_binary()) -> integers().
generate_totp(Secret) when is_binary(Secret)->
    Timestamp = timestamp(),
    Strict = kapps_config:get_is_true(?CONFIG_CAT, <<"otp_strict_validation">>, 'false'),
    case Strict of
        'true' ->
            [generate_otp(Secret, Timestamp)];
        'false' ->
            [generate_otp(Secret, Timestamp-1)
            ,generate_otp(Secret, Timestamp)
            ,generate_otp(Secret, Timestamp+1)]
    end.

%%---------------------------------------------------------------------
%% @doc
%% generate a random 6 digit one time token.
%% useful for sms validating and getting a random six digit number
%% @end
%%---------------------------------------------------------------------
-spec generate_random_otp() -> integer().
generate_random_otp() ->
    Timestamp = timestamp(),
    generate_otp(generate_secret(), Timestamp).

%%---------------------------------------------------------------------
%% @doc
%% generate a 6 digit one time token.
%% @end
%%---------------------------------------------------------------------
-spec generate_otp(ne_binary(), integer()) -> integer().
generate_otp(Secret, Interval) when is_binary(Secret) and
                                    is_integer(Interval) ->
    Key = base32:decode(Secret),
    Digest = sha_mac(Key, <<Interval:64/integer>>),
    <<_:19/bytes,_:4/bits,Offset:4>> = Digest,
    <<_:Offset/bytes,_:1/bits,P:31,_/binary>> = Digest,
    P rem 1000000.

%%---------------------------------------------------------------------
%% @doc
%% generate timestamp using the current time and rounding it off to 30s.
%% @end
%%---------------------------------------------------------------------
-spec timestamp() -> integer().
timestamp() ->
    {Mega, Secs, _} = os:timestamp(),
    Time = Mega*1000000 + Secs,
    Time div 30.

-spec sha_mac(ne_binary(), non_neg_integer()) -> ne_binary().
sha_mac(K, S) ->
    try
        crypto:hmac(sha, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.