%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc HTTP helper functions for Kazoo
%%% @author Mark Magnusson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_http_util).

-export([urldecode/1, urldecode/2
        ,urlencode/1, urlencode/2
        ,uri/2, resolve_uri/2
        ,parse_query_string/1
        ,urlsplit/1
        ,urlunsplit/1
        ,json_to_querystring/1
        ,props_to_querystring/1
        ,http_code_to_status_line/1
        ,get_resp_header/2, get_resp_header/3
        ,encode_multipart/1, encode_multipart/2
        ,create_boundary/0
        ]).

-ifdef(TEST).
-export([resolve_uri_path/2]).
-endif.

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-spec urldecode(kz_term:text()) -> kz_term:text().
urldecode(URL) -> urldecode(URL, 'rfc').

%%------------------------------------------------------------------------------
%% @doc 'legacy' vs 'rfc'.
%%
%% 'legacy' treats '+' as a space (in addition to %20)
%% 'rfc' only treats %20 as a space.
%% @end
%%------------------------------------------------------------------------------
-spec urldecode(kz_term:text(), 'rfc' | 'legacy') -> kz_term:text().
urldecode(URL, 'rfc') ->
    urldecode_percent20(URL);
urldecode(URL, 'legacy') ->
    urldecode_plus(kz_term:to_binary(URL)).

urldecode_percent20(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:decode(kz_term:to_list(Binary)));
urldecode_percent20(String) when is_list(String) ->
    http_uri:decode(String);
urldecode_percent20(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:decode(kz_term:to_list(Atom)), 'true').

-spec urlencode(kz_term:text()) -> kz_term:text().
urlencode(URL) -> urlencode(URL, 'rfc').

-spec urlencode(kz_term:text(), 'rfc' | 'legacy') -> kz_term:text().
urlencode(URL, 'rfc') ->
    urlencode_percent20(URL);
urlencode(URL, 'legacy') ->
    urlencode_plus(kz_term:to_binary(URL)).

-spec urlencode_percent20(kz_term:text()) -> kz_term:text().
urlencode_percent20(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:encode(kz_term:to_list(Binary)));
urlencode_percent20(String) when is_list(String) ->
    http_uri:encode(String);
urlencode_percent20(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:encode(kz_term:to_list(Atom)), 'true');
urlencode_percent20(Any) ->
    urlencode_percent20(kz_term:to_binary(Any)).

%%------------------------------------------------------------------------------
%% @doc URL decodes a URL encoded string.
%% @end
%%------------------------------------------------------------------------------
-spec urldecode_plus(binary()) -> binary().
urldecode_plus(Source) ->
    urldecode_plus(Source, <<>>).

-spec urldecode_plus(binary(), binary()) -> binary().
urldecode_plus(<<>>, Acc) ->
    Acc;

urldecode_plus(<<$+, R/binary>>, Acc) ->
    urldecode_plus(R, <<Acc/binary, " ">>);

urldecode_plus(<<$%, H, L, R/binary>>, Acc) ->
    Code  = <<H, L>>,
    Ascii = list_to_integer(binary_to_list(Code), 16),

    urldecode_plus(R, <<Acc/binary, Ascii>>);

urldecode_plus(<<H, R/binary>>, Acc) ->
    urldecode_plus(R, <<Acc/binary, H>>).

%%------------------------------------------------------------------------------
%% @doc URL encodes a string.
%% @end
%%------------------------------------------------------------------------------
-spec urlencode_plus(binary()) -> binary().
urlencode_plus(Source) when is_binary(Source) ->
    urlencode_plus(Source, <<>>).

-spec urlencode_plus(binary(), binary()) -> binary().
urlencode_plus(<<>>, Acc) ->
    Acc;

urlencode_plus(<<$\s, R/binary>>, Acc) ->
    urlencode_plus(R, <<Acc/binary, $+>>);

urlencode_plus(<<C, R/binary>>, Acc) ->
    case C of
        $. -> urlencode_plus(R, <<Acc/binary, C>>);
        $- -> urlencode_plus(R, <<Acc/binary, C>>);
        $~ -> urlencode_plus(R, <<Acc/binary, C>>);
        $_ -> urlencode_plus(R, <<Acc/binary, C>>);
        C when C >= $0
               andalso C=< $9 ->
            urlencode_plus(R, <<Acc/binary, C>>);
        C when C >= $a
               andalso C=< $z ->
            urlencode_plus(R, <<Acc/binary, C>>);
        C when C >= $A
               andalso C=< $Z ->
            urlencode_plus(R, <<Acc/binary, C>>);
        _NotSafe ->
            SafeChar = encode_char(C),
            urlencode_plus(R, <<Acc/binary, "%", SafeChar/binary>>)
    end.

%%------------------------------------------------------------------------------
%% @doc Converts a single character to its base-16 %-encoded form.
%% @end
%%------------------------------------------------------------------------------
-spec encode_char(integer()) -> binary().
encode_char(Char) ->
    case integer_to_list(Char, 16) of
        Val when length(Val) < 2 -> list_to_binary(["0", Val]);
        ProperLen                -> list_to_binary(ProperLen)
    end.

%%------------------------------------------------------------------------------
%% @doc Parses a query string and returns a list of key-value pairs.
%% If the input string contains a `?' then everything after the `?' will
%% be treated as the query string, otherwise the entire input is treated
%% as the query string. The return key-value pairs will be URL decoded.
%% @end
%%------------------------------------------------------------------------------
-spec parse_query_string(binary()) -> [{binary(), binary()}].
parse_query_string(Source) ->
    parse_query_string('key', Source, <<>>, <<>>, []).

-spec parse_query_string(atom(), binary(), binary(), binary(), list()) -> list().
parse_query_string(_State, <<>>, <<>>, _ValAcc, RetAcc) ->
    RetAcc;

parse_query_string(_State, <<>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc, 'legacy'),
    Val = urldecode(ValAcc, 'legacy'),

    RetAcc ++ [{Key, Val}];

parse_query_string(_State, <<$?, R/binary>>, _KeyAcc, _ValAcc, _RetAcc) ->
    parse_query_string('key', R, <<>>, <<>>, []);

parse_query_string('key', <<$=, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    parse_query_string('val', R, KeyAcc, <<>>, RetAcc);

parse_query_string('key', <<$;, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc, 'legacy'),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, <<>>}]);

parse_query_string('key', <<$&, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc, 'legacy'),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, <<>>}]);

parse_query_string('val', <<$;, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc, 'legacy'),
    Val = urldecode(ValAcc, 'legacy'),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, Val}]);

parse_query_string('val', <<$&, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc, 'legacy'),
    Val = urldecode(ValAcc, 'legacy'),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, Val}]);

parse_query_string('key', <<C, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    parse_query_string('key', R, <<KeyAcc/binary, C>>, ValAcc, RetAcc);

parse_query_string('val', <<C, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    parse_query_string('val', R, KeyAcc, <<ValAcc/binary, C>>, RetAcc).

%%------------------------------------------------------------------------------
%% @doc Splits a URL into scheme, location, path, query, and fragment parts.
%% @end
%%------------------------------------------------------------------------------
-spec urlsplit(binary()) -> {binary(), binary(), binary(), binary(), binary()}.
urlsplit(Source) ->
    {Scheme, Url1}      = urlsplit_s(Source),
    {Location, Url2}    = urlsplit_l(Url1),
    {Path, Query, Frag} = urlsplit_p(Url2, <<>>),

    {Scheme, Location, Path, Query, Frag}.

%%------------------------------------------------------------------------------
%% @doc Splits out the scheme portion of the URL (if present).
%% @end
%%------------------------------------------------------------------------------
-spec urlsplit_s(binary()) -> {binary(), binary()}.
urlsplit_s(Source) ->
    case urlsplit_s(Source, <<>>) of
        'no_scheme' -> {<<>>, Source};
        ValidScheme -> ValidScheme
    end.

-spec urlsplit_s(binary(), binary()) -> {binary(), binary()} | 'no_scheme'.
urlsplit_s(<<>>, _Acc) ->
    'no_scheme';

urlsplit_s(<<C, R/binary>>, Acc) ->
    case C of
        $: -> {Acc, R};
        $+ -> urlsplit_s(R, <<Acc/binary, C>>);
        $- -> urlsplit_s(R, <<Acc/binary, C>>);
        $. -> urlsplit_s(R, <<Acc/binary, C>>);
        C when C >= $a
               andalso C =< $z ->
            urlsplit_s(R, <<Acc/binary, C>>);
        C when C >= $A
               andalso C =< $Z ->
            urlsplit_s(R, <<Acc/binary, C>>);
        C when C >= $0
               andalso C =< $9 ->
            urlsplit_s(R, <<Acc/binary, C>>);
        _NoScheme -> 'no_scheme'
    end.

%%------------------------------------------------------------------------------
%% @doc Splits out the location portion of the URL.
%% @end
%%------------------------------------------------------------------------------
-spec urlsplit_l(binary()) -> {binary(), binary()}.
urlsplit_l(<<"//", R/binary>>) ->
    urlsplit_l(R, <<>>);

urlsplit_l(Source) ->
    {<<>>, Source}.

-spec urlsplit_l(binary(), binary()) -> {binary(), binary()}.
urlsplit_l(<<>>, Acc) ->
    {Acc, <<>>};

urlsplit_l(<<$/, _I/binary>> = R, Acc) ->
    {Acc, R};

urlsplit_l(<<$?, _I/binary>> = R, Acc) ->
    {Acc, R};

urlsplit_l(<<$#, _I/binary>> = R, Acc) ->
    {Acc, R};

urlsplit_l(<<C, R/binary>>, Acc) ->
    urlsplit_l(R, <<Acc/binary, C>>).

%%------------------------------------------------------------------------------
%% @doc Splits and returns the path, query string, and fragment portions
%% of the URL.
%% @end
%%------------------------------------------------------------------------------
-spec urlsplit_p(binary(), binary()) -> {binary(), binary(), binary()}.
urlsplit_p(<<>>, Acc) ->
    {Acc, <<>>, <<>>};

urlsplit_p(<<$?, R/binary>>, Acc) ->
    {Query, Frag} = urlsplit_q(R, <<>>),
    {Acc, Query, Frag};

urlsplit_p(<<$#, R/binary>>, Acc) ->
    {Acc, <<>>, R};

urlsplit_p(<<C, R/binary>>, Acc) ->
    urlsplit_p(R, <<Acc/binary, C>>).

%%------------------------------------------------------------------------------
%% @doc Splits the query string and fragment parts of the URL.
%% @end
%%------------------------------------------------------------------------------
-spec urlsplit_q(binary(), binary()) -> {binary(), binary()}.
urlsplit_q(<<>>, Acc) ->
    {Acc, <<>>};

urlsplit_q(<<$#, R/binary>>, Acc) ->
    {Acc, R};

urlsplit_q(<<C, R/binary>>, Acc) ->
    urlsplit_q(R, <<Acc/binary, C>>).

%%------------------------------------------------------------------------------
%% @doc Joins the elements of a URL together.
%% @end
%%------------------------------------------------------------------------------
-spec urlunsplit({binary(), binary(), binary(), binary(), binary()}) -> binary().
urlunsplit({S, N, P, Q, F}) ->
    Us = case S of <<>> -> <<>>; _ -> [S, "://"] end,
    Uq = case Q of <<>> -> <<>>; _ -> [$?, Q] end,
    Uf = case F of <<>> -> <<>>; _ -> [$#, F] end,

    iolist_to_binary([Us, N, P, Uq, Uf]).

%%------------------------------------------------------------------------------
%% @doc Convert JSON object to encoded query string.
%%
%% Example: take JObj
%% `{key1: val1, key2: [v2_1, v2_2], key3: {k3_1: v3_1}}'
%% will returns:
%% `key=val&key2[]=v2_1&key2[]=v2_2&key3[key3_1]=v3_1'
%% @end
%%------------------------------------------------------------------------------
-spec json_to_querystring(object()) -> iodata().
json_to_querystring(JObj) -> json_to_querystring(JObj, <<>>).

%% if Prefix is empty, don't wrap keys in array tags, otherwise Prefix[key]=value
-spec json_to_querystring(object(), iodata()) -> iodata().
json_to_querystring(JObj, Prefix) ->
    {Vs, Ks} = kz_json:get_values(JObj),
    fold_kvs(Ks, Vs, Prefix, []).

-spec props_to_querystring(kz_term:proplist()) -> iodata().
props_to_querystring(Props) ->
    props_to_querystring(Props, <<>>).

-spec props_to_querystring(kz_term:proplist(), binary() | kz_term:ne_binaries()) -> iodata().
props_to_querystring(Props, Prefix) ->
    {Vs, Ks} = props:get_values_and_keys(Props),
    fold_kvs([kz_term:to_binary(K) || K <- Ks], Vs, Prefix, []).

%% foreach key/value pair, encode the key/value with the prefix and prepend the &
%% if the last key/value pair, encode the key/value with the prefix, prepend to accumulator
%% and reverse the list (putting the key/value at the end of the list)
-spec fold_kvs(keys(), json_terms(), binary() | iolist(), iodata()) -> iodata().
fold_kvs([], [], _, Acc) -> Acc;
fold_kvs([K], [V], Prefix, Acc) -> lists:reverse([encode_kv(Prefix, K, V) | Acc]);
fold_kvs([K|Ks], [V|Vs], Prefix, Acc) ->
    fold_kvs(Ks, Vs, Prefix, [<<"&">>, encode_kv(Prefix, K, V) | Acc]).

-spec encode_kv(iolist() | binary(), key(), json_term() | json_terms()) -> iodata().
%% If a list of values, use the []= as a separator between the key and each value
encode_kv(Prefix, K, Vs) when is_list(Vs) ->
    encode_kv(Prefix, kz_term:to_binary(K), Vs, <<"[]=">>, []);
%% if the value is a "simple" value, just encode it (url-encoded)
encode_kv(Prefix, K, V) when is_binary(V);
                             is_number(V) ->
    encode_kv(Prefix, K, <<"=">>, urlencode(V, 'legacy'));
encode_kv(Prefix, K, 'true') ->
    encode_kv(Prefix, K, <<"=">>, <<"true">>);
encode_kv(Prefix, K, 'false') ->
    encode_kv(Prefix, K, <<"=">>, <<"false">>);

%% key:{k1:v1, k2:v2} => key[k1]=v1&key[k2]=v2
%% if no prefix is present, use just key to prefix the key/value pairs in the jobj
encode_kv(<<>>, K, ?JSON_WRAPPER(_)=JObj) -> json_to_querystring(JObj, [K]);
%% if a prefix is defined, nest the key in square brackets
encode_kv(Prefix, K, ?JSON_WRAPPER(_)=JObj) -> json_to_querystring(JObj, [Prefix, <<"[">>, K, <<"]">>]).

-spec encode_kv(iolist() | binary(), key(), kz_term:ne_binary(), string() | binary()) -> iodata().
encode_kv(<<>>, K, Sep, V) -> [kz_term:to_binary(K), Sep, kz_term:to_binary(V)];
encode_kv(Prefix, K, Sep, V) -> [Prefix, <<"[">>, kz_term:to_binary(K), <<"]">>, Sep, kz_term:to_binary(V)].

-spec encode_kv(iolist() | binary(), key(), [string()], kz_term:ne_binary(), iolist()) -> iodata().
encode_kv(Prefix, K, [V], Sep, Acc) ->
    lists:reverse([encode_kv(Prefix, K, Sep, urlencode(V, 'legacy')) | Acc]);
encode_kv(Prefix, K, [V|Vs], Sep, Acc) ->
    encode_kv(Prefix, K, Vs, Sep, [ <<"&">>, encode_kv(Prefix, K, Sep, urlencode(V, 'legacy')) | Acc]);
encode_kv(_, _, [], _, Acc) -> lists:reverse(Acc).

%%------------------------------------------------------------------------------
%% @doc Converts HTTP status code to reason.
%% @end
%%------------------------------------------------------------------------------
-spec http_code_to_status_line(atom() | pos_integer()) -> kz_term:ne_binary().
%% 4×× Client Error
http_code_to_status_line(400) -> <<"Bad Request">>;
http_code_to_status_line(401) -> <<"Unauthorized">>;
http_code_to_status_line(402) -> <<"Payment Required">>;
http_code_to_status_line(403) -> <<"Forbidden">>;
http_code_to_status_line(404) -> <<"Not Found">>;
http_code_to_status_line(405) -> <<"Method Not Allowed">>;
http_code_to_status_line(406) -> <<"Not Acceptable">>;
http_code_to_status_line(407) -> <<"Proxy Authentication Required">>;
http_code_to_status_line(408) -> <<"Request Timeout">>;
http_code_to_status_line(409) -> <<"Conflict">>;
http_code_to_status_line(410) -> <<"Gone">>;
http_code_to_status_line(411) -> <<"Length Required">>;
http_code_to_status_line(412) -> <<"Precondition Failed">>;
http_code_to_status_line(413) -> <<"Payload Too Large">>;
http_code_to_status_line(414) -> <<"Request-URI Too Long">>;
http_code_to_status_line(415) -> <<"Unsupported Media Type">>;
http_code_to_status_line(416) -> <<"Requested Range Not Satisfiable">>;
http_code_to_status_line(417) -> <<"Expectation Failed">>;
http_code_to_status_line(418) -> <<"I'm a teapot">>;
http_code_to_status_line(421) -> <<"Misdirected Request">>;
http_code_to_status_line(422) -> <<"Unprocessable Entity">>;
http_code_to_status_line(423) -> <<"Locked">>;
http_code_to_status_line(424) -> <<"Failed Dependency">>;
http_code_to_status_line(426) -> <<"Upgrade Required">>;
http_code_to_status_line(428) -> <<"Precondition Required">>;
http_code_to_status_line(429) -> <<"Too Many Requests">>;
http_code_to_status_line(431) -> <<"Request Header Fields Too Large">>;
http_code_to_status_line(444) -> <<"Connection Closed Without Response">>;
http_code_to_status_line(451) -> <<"Unavailable For Legal Reasons">>;
http_code_to_status_line(499) -> <<"Client Closed Request">>;
%% 5×× Server Error
http_code_to_status_line(500) -> <<"Internal Server Error">>;
http_code_to_status_line(501) -> <<"Not Implemented">>;
http_code_to_status_line(502) -> <<"Bad Gateway">>;
http_code_to_status_line(503) -> <<"Service Unavailable">>;
http_code_to_status_line(504) -> <<"Gateway Timeout">>;
http_code_to_status_line(505) -> <<"HTTP Version Not Supported">>;
http_code_to_status_line(506) -> <<"Variant Also Negotiates">>;
http_code_to_status_line(507) -> <<"Insufficient Storage">>;
http_code_to_status_line(508) -> <<"Loop Detected">>;
http_code_to_status_line(510) -> <<"Not Extended">>;
http_code_to_status_line(511) -> <<"Network Authentication Required">>;
http_code_to_status_line(599) -> <<"Network Connect Timeout Error">>.

-spec get_resp_header(kz_http:field(), kz_http:headers()) -> kz_http:value() | 'undefined'.
get_resp_header(RespHeader, RespHeaders) ->
    get_resp_header(RespHeader, RespHeaders, 'undefined').

-spec get_resp_header(kz_http:field(), kz_http:headers(), Default) -> kz_http:value() | Default.
get_resp_header(RespHeader, RespHeaders, Default) ->
    case lists:keyfind(RespHeader, 1, RespHeaders) of
        {_, Value} -> Value;
        'false' -> Default
    end.

-type part() :: {binary(), kz_term:proplist()}.
-type parts() :: [part()].

-spec encode_multipart(parts()) -> binary().
encode_multipart(Parts) ->
    encode_multipart(Parts, create_boundary()).

-spec encode_multipart(parts(), binary()) -> binary().
encode_multipart(Parts, Boundary) ->
    encode_multipart(Parts, Boundary, <<>>).

-spec encode_multipart(parts(), binary(), binary()) -> binary().
encode_multipart([], Boundary, Encoded) ->
    Close = <<"\r\n--" , Boundary/binary, "--">>,
    <<Encoded/binary, Close/binary>>;
encode_multipart([{Body, Headers} | Parts], Boundary, Encoded) ->
    Delimiter = <<"\r\n--" ,Boundary/binary, "\r\n">>,
    H = encode_multipart_headers(Headers),
    Acc = <<Encoded/binary, Delimiter/binary, H/binary, Body/binary>>,
    encode_multipart(Parts, Boundary, Acc).

-spec encode_multipart_headers(kz_term:proplist()) -> binary().
encode_multipart_headers(Headers) ->
    encode_multipart_headers(Headers, <<>>).

-spec encode_multipart_headers(kz_term:proplist(), binary()) -> binary().
encode_multipart_headers([], Encoded) -> <<Encoded/binary, "\r\n">>;
encode_multipart_headers([{K, V} | Headers], Encoded) ->
    Acc = <<Encoded/binary, K/binary, ": ", V/binary, "\r\n">>,
    encode_multipart_headers(Headers, Acc).

-spec create_boundary() -> kz_term:ne_binary().
create_boundary() ->
    cow_multipart:boundary().

-spec resolve_uri(nonempty_string() | kz_term:ne_binary(), nonempty_string() | kz_term:api_ne_binary()) -> kz_term:ne_binary().
resolve_uri(Raw, 'undefined') -> kz_term:to_binary(Raw);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(<<_/binary>> = RawPath, <<_/binary>> = Relative) ->
    Path = resolve_uri_path(RawPath, Relative),
    kz_binary:join(Path, <<"/">>);
resolve_uri(RawPath, Relative) ->
    resolve_uri(kz_term:to_binary(RawPath), kz_term:to_binary(Relative)).

-spec resolve_uri_path(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binaries().
resolve_uri_path(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, ['global'])),
    UrlTokens = binary:split(Relative, <<"/">>, ['global']),
    lists:reverse(
      lists:foldl(fun resolve_uri_fold/2, PathTokensRev, UrlTokens)
     ).

-spec resolve_uri_fold(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
resolve_uri_fold(<<"..">>, []) -> [];
resolve_uri_fold(<<"..">>, [_ | PathTokens]) -> PathTokens;
resolve_uri_fold(<<".">>, PathTokens) -> PathTokens;
resolve_uri_fold(<<>>, PathTokens) -> PathTokens;
resolve_uri_fold(Segment, [<<>>|DirTokens]) -> [Segment|DirTokens];
resolve_uri_fold(Segment, [LastToken|DirTokens]=PathTokens) ->
    case filename:extension(LastToken) of
        <<>> ->
            %% no extension, append Segment to Tokens
            [Segment | PathTokens];
        _Ext ->
            %% Extension found, append Segment to DirTokens
            [Segment|DirTokens]
    end.

-spec uri(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binary().
uri(BaseUrl, Tokens) ->
    [Pro, Url] = binary:split(BaseUrl, <<"://">>),
    Uri = filename:join([Url | Tokens]),
    <<Pro/binary, "://", Uri/binary>>.
