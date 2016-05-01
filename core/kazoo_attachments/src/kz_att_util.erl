%% @author root
%% @doc @todo Add description to kz_att_util.


-module(kz_att_util).

-export([sha_mac/2, sha256_mac/2,
         md5/1, sha256/1]).

sha_mac(K, S) ->
    try
        crypto:hmac(sha, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

sha256_mac(K, S) ->
    try
        crypto:hmac(sha256, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha256, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

sha256(V) ->
    crypto:hash(sha256, V).

md5(V) ->
    crypto:hash(md5, V).
