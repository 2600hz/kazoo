%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc CNAM lookup using OpenCNAM
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_cnam_opencnam).

-export([request/2]).

-include("stepswitch.hrl").

-define(DEFAULT_METHOD, <<"get">>).
-define(DEFAULT_CONTENT, <<>>).
-define(DEFAULT_URL, <<"https://api.opencnam.com/v2/phone/{{phone_number}}">>).
-define(DEFAULT_ACCEPT_HDR, <<"text/pbx,text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>).
-define(DEFAULT_CONTENT_TYPE_HDR, <<"application/json">>).

-define(HTTP_ACCEPT_HEADER
       ,kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_accept_header">>, ?DEFAULT_ACCEPT_HDR)
       ).
-define(HTTP_CONTENT_TYPE
       ,kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_content_type_header">>, ?DEFAULT_CONTENT_TYPE_HDR)
       ).

-spec request(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
request(Number, JObj) ->
    Url = kz_term:to_list(get_http_url(JObj)),
    case kz_http:req(get_http_method()
                    ,Url
                    ,get_http_headers()
                    ,get_http_body(JObj)
                    ,get_http_options(Url)
                    )
    of
        {'ok', 404, _, _} ->
            lager:debug("cnam lookup for ~s returned 404", [Number]),
            'undefined';
        {'ok', Status, _, <<>>} ->
            lager:debug("cnam lookup for ~s returned as ~p and empty body", [Number, Status]),
            'undefined';
        {'ok', Status, _, ResponseBody} when size(ResponseBody) > 18 ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            kz_binary:truncate_right(ResponseBody, 18);
        {'ok', Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            ResponseBody;
        {'error', _R} ->
            lager:debug("cnam lookup for ~s failed: ~p", [Number, _R]),
            'undefined'
    end.

-spec get_http_url(kz_json:object()) -> kz_term:ne_binary().
get_http_url(JObj) ->
    Template = kapps_config:get_binary(?CNAM_CONFIG_CAT, <<"http_url">>, ?DEFAULT_URL),
    {'ok', SrcUrl} = stepswitch_cnam:render(JObj, Template),
    Url = iolist_to_binary(SrcUrl),

    case binary:match(Template, <<"opencnam">>) of
        'nomatch' -> Url;
        _Else ->
            case kz_http_util:urlsplit(Url) of
                {_Scheme, _Host, _Path, <<>>, _Segment} ->
                    <<Url/binary, "?ref=2600hz&format=pbx">>;
                {Scheme, Host, Path, QS, Segment} ->
                    kz_http_util:urlunsplit({Scheme, Host, Path, <<QS/binary, "&ref=2600hz&format=pbx">>, Segment})
            end
    end.

-spec get_http_body(kz_json:object()) -> list().
get_http_body(JObj) ->
    Template = kapps_config:get_binary(?CNAM_CONFIG_CAT, <<"http_body">>, ?DEFAULT_CONTENT),
    case kz_term:is_empty(Template) of
        'true' -> [];
        'false' ->
            {'ok', Body} = stepswitch_cnam:render(JObj, Template),
            lists:flatten(Body)
    end.

-spec get_http_headers() -> [{nonempty_string(), nonempty_string()}].
get_http_headers() ->
    Headers = [{"Accept", ?HTTP_ACCEPT_HEADER}
              ,{"User-Agent", ?HTTP_USER_AGENT}
              ,{"Content-Type", ?HTTP_CONTENT_TYPE}
              ],
    maybe_enable_auth(Headers).

-spec get_http_options(nonempty_string()) -> kz_term:proplist().
get_http_options(Url) ->
    Defaults = [{'connect_timeout', ?HTTP_CONNECT_TIMEOUT_MS}
               ,{'timeout', 1500}
               ],
    maybe_enable_ssl(Url, Defaults).

-spec maybe_enable_ssl(nonempty_string(), kz_term:proplist()) -> kz_term:proplist().
maybe_enable_ssl("https://" ++ _, Props) ->
    [{'ssl', [{'verify', 'verify_none'}]}|Props];
maybe_enable_ssl(_Url, Props) -> Props.

-spec maybe_enable_auth([{nonempty_string(), nonempty_string()}]) ->
                               [{nonempty_string(), nonempty_string()}].
maybe_enable_auth(Props) ->
    Username = kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_basic_auth_username">>, <<>>),
    Password = kapps_config:get_string(?CNAM_CONFIG_CAT, <<"http_basic_auth_password">>, <<>>),
    case kz_term:is_empty(Username)
        orelse kz_term:is_empty(Password)
    of
        'true' -> Props;
        'false' -> [basic_auth(Username, Password) | Props]
    end.

-spec basic_auth(nonempty_string(), nonempty_string()) ->
                        {nonempty_string(), nonempty_string()}.
basic_auth(Username, Password) ->
    Encoded = base64:encode_to_string(Username ++ [$: | Password]),
    {"Authorization", lists:flatten(["Basic ", Encoded])}.

-spec get_http_method() -> 'get' | 'put' | 'post'.
get_http_method() ->
    case kapps_config:get_binary(?CNAM_CONFIG_CAT, <<"http_method">>, ?DEFAULT_METHOD) of
        <<"post">> -> 'post';
        <<"put">> -> 'put';
        _Else -> 'get'
    end.
