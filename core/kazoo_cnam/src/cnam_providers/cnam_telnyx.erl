%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications
%%% @doc CNAM lookup using Telnyx
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cnam_telnyx).

-export([request/2]).

-include("cnam.hrl").

-define(DEFAULT_URL, <<"https://data.telnyx.com/cnam/v1/caller-information?tn={{phone_number}}">>).

-define(HTTP_ACCEPT_HEADER, "application/json").
-define(HTTP_CONTENT_TYPE, "application/json").

-spec request(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
request(Number, JObj) ->
    Url = kz_term:to_list(get_http_url(JObj)),
    case kz_http:get(Url, get_http_headers(), get_http_options()) of
        {'ok', 404, _, _} ->
            lager:debug("cnam lookup for ~s returned 404", [Number]),
            'undefined';
        {'ok', Status, _, <<>>} ->
            lager:debug("cnam lookup for ~s returned as ~p and empty body", [Number, Status]),
            'undefined';
        {'ok', 200=Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            handle_ok_resp(ResponseBody);
        {'ok', Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            'undefined';
        {'error', _R} ->
            lager:debug("cnam lookup for ~s failed: ~p", [Number, _R]),
            'undefined'
    end.

-spec get_http_url(kz_json:object()) -> kz_term:ne_binary().
get_http_url(JObj) ->
    Template = kapps_config:get_binary(?CNAM_CONFIG_CAT, <<"http_url">>, ?DEFAULT_URL),
    {'ok', SrcUrl} = cnam:render(JObj, Template),
    iolist_to_binary(SrcUrl).

-spec get_http_headers() -> [{nonempty_string(), nonempty_string()}].
get_http_headers() ->
    Headers = [{"Accept", ?HTTP_ACCEPT_HEADER}
              ,{"User-Agent", ?HTTP_USER_AGENT}
              ,{"Content-Type", ?HTTP_CONTENT_TYPE}
              ],
    maybe_enable_auth(Headers).

-spec get_http_options() -> kz_term:proplist().
get_http_options() ->
    [{'connect_timeout', ?HTTP_CONNECT_TIMEOUT_MS}
    ,{'timeout', 1500}
    ].

-spec maybe_enable_auth([{nonempty_string(), nonempty_string()}]) ->
          [{nonempty_string(), nonempty_string()}].
maybe_enable_auth(Props) ->
    Token = kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_token_auth_token">>, <<>>),
    case kz_term:is_empty(Token) of
        'true' -> Props;
        'false' -> [{"Authorization", lists:flatten(["Token ", Token])} | Props]
    end.

-spec handle_ok_resp(kz_term:ne_binary()) -> kz_term:api_binary().
handle_ok_resp(ResponseBody) ->
    JObj = kz_json:decode(ResponseBody),
    case kz_json:get_binary_value(<<"callerInformation">>, JObj) of
        'undefined' -> 'undefined';
        CallerInformation -> maybe_truncate(CallerInformation)
    end.

-spec maybe_truncate(binary()) -> binary().
maybe_truncate(CallerInformation) when size(CallerInformation) > 18 ->
    kz_binary:truncate_right(CallerInformation, 18);
maybe_truncate(CallerInformation) -> CallerInformation.
