%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc HTTP helper functions for Kazoo
%%%
%%% @contributors
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(kz_http_util).

-export([urldecode/1
        ,urlencode/1
        ,parse_query_string/1
        ,urlsplit/1
        ,urlunsplit/1
        ,json_to_querystring/1
        ,props_to_querystring/1
        ]).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

%%--------------------------------------------------------------------
%% @doc URL decodes a URL encoded string
%%--------------------------------------------------------------------
-spec urldecode(binary()) -> binary().
urldecode(Source) ->
    urldecode(Source, <<>>).

-spec urldecode(binary(), binary()) -> binary().
urldecode(<<>>, Acc) ->
    Acc;

urldecode(<<$+, R/binary>>, Acc) ->
    urldecode(R, <<Acc/binary, " ">>);

urldecode(<<$%, H, L, R/binary>>, Acc) ->
    Code  = <<H, L>>,
    Ascii = list_to_integer(binary_to_list(Code), 16),

    urldecode(R, <<Acc/binary, Ascii>>);

urldecode(<<H, R/binary>>, Acc) ->
    urldecode(R, <<Acc/binary, H>>).

%%--------------------------------------------------------------------
%% @doc URL encodes a string
%%--------------------------------------------------------------------
-spec urlencode(binary() | atom() | integer() | float() | string()) -> binary().
urlencode(Source) when is_binary(Source) ->
    urlencode(Source, <<>>);

urlencode(Source) when is_atom(Source) ->
    urlencode(list_to_binary(atom_to_list(Source)), <<>>);

urlencode(Source) when is_list(Source) ->
    urlencode(list_to_binary(Source), <<>>);

urlencode(Source) when is_integer(Source) ->
    urlencode(list_to_binary(integer_to_list(Source)), <<>>);

%% @todo fix this when we move to > R15
urlencode(Source) when is_float(Source) ->
    List = float_to_list(Source),
    Proper = string:substr(List, 1, string:chr(List, $.)+2),
    urlencode(list_to_binary(Proper), <<>>).

-spec urlencode(binary(), binary()) -> binary().
urlencode(<<>>, Acc) ->
    Acc;

urlencode(<<$\s, R/binary>>, Acc) ->
    urlencode(R, <<Acc/binary, $+>>);

urlencode(<<C, R/binary>>, Acc) ->
    case C of
        $. -> urlencode(R, <<Acc/binary, C>>);
        $- -> urlencode(R, <<Acc/binary, C>>);
        $~ -> urlencode(R, <<Acc/binary, C>>);
        $_ -> urlencode(R, <<Acc/binary, C>>);
        C when C >= $0
               andalso C=< $9 ->
            urlencode(R, <<Acc/binary, C>>);
        C when C >= $a
               andalso C=< $z ->
            urlencode(R, <<Acc/binary, C>>);
        C when C >= $A
               andalso C=< $Z ->
            urlencode(R, <<Acc/binary, C>>);
        _NotSafe ->
            SafeChar = encode_char(C),
            urlencode(R, <<Acc/binary, "%", SafeChar/binary>>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a single character to its base-16 %-encoded form
%%--------------------------------------------------------------------
-spec encode_char(integer()) -> binary().
encode_char(Char) ->
    case integer_to_list(Char, 16) of
        Val when length(Val) < 2 -> list_to_binary(["0", Val]);
        ProperLen                -> list_to_binary(ProperLen)
    end.

%%--------------------------------------------------------------------
%% @doc Parses a query string and returns a list of key->value pairs.
%% If the input string contains a ? then everything after the ? will
%% be treated as the query string, otherwise the entire input is treated
%% as the query string. The return key->value pairs will be urldecoded.
%%--------------------------------------------------------------------
-spec parse_query_string(binary()) -> [{binary(), binary()}].
parse_query_string(Source) ->
    parse_query_string('key', Source, <<>>, <<>>, []).

-spec parse_query_string(atom(), binary(), binary(), binary(), list()) -> list().
parse_query_string(_State, <<>>, <<>>, _ValAcc, RetAcc) ->
    RetAcc;

parse_query_string(_State, <<>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc),
    Val = urldecode(ValAcc),

    RetAcc ++ [{Key, Val}];

parse_query_string(_State, <<$?, R/binary>>, _KeyAcc, _ValAcc, _RetAcc) ->
    parse_query_string('key', R, <<>>, <<>>, []);

parse_query_string('key', <<$=, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    parse_query_string('val', R, KeyAcc, <<>>, RetAcc);

parse_query_string('key', <<$;, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, <<>>}]);

parse_query_string('key', <<$&, R/binary>>, KeyAcc, _ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, <<>>}]);

parse_query_string('val', <<$;, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc),
    Val = urldecode(ValAcc),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, Val}]);

parse_query_string('val', <<$&, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    Key = urldecode(KeyAcc),
    Val = urldecode(ValAcc),

    parse_query_string('key', R, <<>>, <<>>, RetAcc ++ [{Key, Val}]);

parse_query_string('key', <<C, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    parse_query_string('key', R, <<KeyAcc/binary, C>>, ValAcc, RetAcc);

parse_query_string('val', <<C, R/binary>>, KeyAcc, ValAcc, RetAcc) ->
    parse_query_string('val', R, KeyAcc, <<ValAcc/binary, C>>, RetAcc).

%%--------------------------------------------------------------------
%% @doc Splits a URL into scheme, location, path, query, and fragment parts
%%--------------------------------------------------------------------
-spec urlsplit(binary()) -> {binary(), binary(), binary(), binary(), binary()}.
urlsplit(Source) ->
    {Scheme, Url1}      = urlsplit_s(Source),
    {Location, Url2}    = urlsplit_l(Url1),
    {Path, Query, Frag} = urlsplit_p(Url2, <<>>),

    {Scheme, Location, Path, Query, Frag}.

%%--------------------------------------------------------------------
%% @private
%% @doc Splits out the scheme portion of the URL (if present)
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc Splits out the location portion of the URL
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc Splits and returns the path, query string, and fragment portions
%% of the URL
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc Splits the query string and fragment parts of the URL
%%--------------------------------------------------------------------
-spec urlsplit_q(binary(), binary()) -> {binary(), binary()}.
urlsplit_q(<<>>, Acc) ->
    {Acc, <<>>};

urlsplit_q(<<$#, R/binary>>, Acc) ->
    {Acc, R};

urlsplit_q(<<C, R/binary>>, Acc) ->
    urlsplit_q(R, <<Acc/binary, C>>).

%%--------------------------------------------------------------------
%% @doc Joins the elements of a URL together
%%--------------------------------------------------------------------
-spec urlunsplit({binary(), binary(), binary(), binary(), binary()}) -> binary().
urlunsplit({S, N, P, Q, F}) ->
    Us = case S of <<>> -> <<>>; _ -> [S, "://"] end,
    Uq = case Q of <<>> -> <<>>; _ -> [$?, Q] end,
    Uf = case F of <<>> -> <<>>; _ -> [$#, F] end,

    iolist_to_binary([Us, N, P, Uq, Uf]).

%% Convert {key1:val1,key2:[v2_1, v2_2],key3:{k3_1:v3_1}} =>
%%   key=val&key2[]=v2_1&key2[]=v2_2&key3[key3_1]=v3_1
-spec json_to_querystring(object()) -> iolist().
json_to_querystring(JObj) -> json_to_querystring(JObj, <<>>).

%% if Prefix is empty, don't wrap keys in array tags, otherwise Prefix[key]=value
-spec json_to_querystring(object(), iolist() | binary()) -> iolist().
json_to_querystring(JObj, Prefix) ->
    {Vs, Ks} = kz_json:get_values(JObj),
    fold_kvs(Ks, Vs, Prefix, []).

-spec props_to_querystring(kz_term:proplist()) -> iolist().
props_to_querystring(Props) ->
    props_to_querystring(Props, <<>>).

-spec props_to_querystring(kz_term:proplist(), binary() | kz_term:ne_binaries()) -> iolist().
props_to_querystring(Props, Prefix) ->
    {Vs, Ks} = props:get_values_and_keys(Props),
    fold_kvs([kz_term:to_binary(K) || K <- Ks], Vs, Prefix, []).

%% foreach key/value pair, encode the key/value with the prefix and prepend the &
%% if the last key/value pair, encode the key/value with the prefix, prepend to accumulator
%% and reverse the list (putting the key/value at the end of the list)
-spec fold_kvs(keys(), json_terms(), binary() | iolist(), iolist()) -> iolist().
fold_kvs([], [], _, Acc) -> Acc;
fold_kvs([K], [V], Prefix, Acc) -> lists:reverse([encode_kv(Prefix, K, V) | Acc]);
fold_kvs([K|Ks], [V|Vs], Prefix, Acc) ->
    fold_kvs(Ks, Vs, Prefix, [<<"&">>, encode_kv(Prefix, K, V) | Acc]).

-spec encode_kv(iolist() | binary(), key(), json_term() | json_terms()) -> iolist().
%% If a list of values, use the []= as a separator between the key and each value
encode_kv(Prefix, K, Vs) when is_list(Vs) ->
    encode_kv(Prefix, kz_term:to_binary(K), Vs, <<"[]=">>, []);
%% if the value is a "simple" value, just encode it (url-encoded)
encode_kv(Prefix, K, V) when is_binary(V);
                             is_number(V) ->
    encode_kv(Prefix, K, <<"=">>, urlencode(V));
encode_kv(Prefix, K, 'true') ->
    encode_kv(Prefix, K, <<"=">>, <<"true">>);
encode_kv(Prefix, K, 'false') ->
    encode_kv(Prefix, K, <<"=">>, <<"false">>);

%% key:{k1:v1, k2:v2} => key[k1]=v1&key[k2]=v2
%% if no prefix is present, use just key to prefix the key/value pairs in the jobj
encode_kv(<<>>, K, ?JSON_WRAPPER(_)=JObj) -> json_to_querystring(JObj, [K]);
%% if a prefix is defined, nest the key in square brackets
encode_kv(Prefix, K, ?JSON_WRAPPER(_)=JObj) -> json_to_querystring(JObj, [Prefix, <<"[">>, K, <<"]">>]).

-spec encode_kv(iolist() | binary(), key(), kz_term:ne_binary(), string() | binary()) -> iolist().
encode_kv(<<>>, K, Sep, V) -> [kz_term:to_binary(K), Sep, kz_term:to_binary(V)];
encode_kv(Prefix, K, Sep, V) -> [Prefix, <<"[">>, kz_term:to_binary(K), <<"]">>, Sep, kz_term:to_binary(V)].

-spec encode_kv(iolist() | binary(), key(), [string()], kz_term:ne_binary(), iolist()) -> iolist().
encode_kv(Prefix, K, [V], Sep, Acc) ->
    lists:reverse([encode_kv(Prefix, K, Sep, urlencode(V)) | Acc]);
encode_kv(Prefix, K, [V|Vs], Sep, Acc) ->
    encode_kv(Prefix, K, Vs, Sep, [ <<"&">>, encode_kv(Prefix, K, Sep, urlencode(V)) | Acc]);
encode_kv(_, _, [], _, Acc) -> lists:reverse(Acc).
