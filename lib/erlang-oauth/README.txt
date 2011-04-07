An Erlang OAuth implementation.

Quick start (client usage):

  $ make
  ...
  $ erl -pa ebin -s crypto -s inets
  ...
  1> Consumer = {"key", "secret", hmac_sha1}.
  ...
  2> RequestTokenURL = "http://term.ie/oauth/example/request_token.php".
  ...
  3> {ok, ResponseR} = oauth:get(RequestTokenURL, [], Consumer, "", "").
  ...
  4> ParamsR = oauth_http:response_params(ResponseR).
  ...
  5> TokenR = oauth:token(ParamsR).
  ...
  6> TokenSecretR = oauth:token_secret(ParamsR).
  ...
  7> AccessTokenURL = "http://term.ie/oauth/example/access_token.php".
  ...
  8> {ok, ResponseA} = oauth:get(AccessTokenURL, [], Consumer, TokenR, TokenSecretR).
  ...


Thanks to Jason Davies, Paul Bonser, and Roberto Aloi for their patches.

The percent encoding/decoding implementations are based on those found in
the ibrowse library, written by Chandrashekhar Mullaparthi.

Example client/server code is at http://github.com/tim/erlang-oauth-examples.

Unit tests are at http://github.com/tim/erlang-oauth-tests.

Erlang R12B-5 or greater is required for RSA-SHA1.
