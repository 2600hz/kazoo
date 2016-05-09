%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz
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
        ]).

-export([uri_encode/1
         ,uri_decode/1
         ,resolve_uri/2
         ,safe_urlencode/1
        ]).

-export([uri/2]).

-ifdef(TEST).
-export([resolve_uri_path/2]).
-endif.

-include_lib("kazoo/include/kz_types.hrl").

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

        C when C >= $0 andalso C=< $9 -> urlencode(R, <<Acc/binary, C>>);
        C when C >= $a andalso C=< $z -> urlencode(R, <<Acc/binary, C>>);
        C when C >= $A andalso C=< $Z -> urlencode(R, <<Acc/binary, C>>);

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

        C when C >= $a andalso C =< $z -> urlsplit_s(R, <<Acc/binary, C>>);
        C when C >= $A andalso C =< $Z -> urlsplit_s(R, <<Acc/binary, C>>);
        C when C >= $0 andalso C =< $9 -> urlsplit_s(R, <<Acc/binary, C>>);

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
    Us = case S of <<>> -> <<>>; _ -> <<S/binary, "://">> end,
    Uq = case Q of <<>> -> <<>>; _ -> <<$?, Q/binary>> end,
    Uf = case F of <<>> -> <<>>; _ -> <<$#, F/binary>> end,

    <<Us/binary, N/binary, P/binary, Uq/binary, Uf/binary>>.


-spec uri_decode(text()) -> text().
uri_decode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:decode(kz_term:to_list(Binary)));
uri_decode(String) when is_list(String) ->
    http_uri:decode(String);
uri_decode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:decode(kz_term:to_list(Atom)), 'true').

-spec uri_encode(text()) -> text().
uri_encode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:encode(kz_term:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:encode(kz_term:to_list(Atom)), 'true').

-spec resolve_uri(nonempty_string() | api_binary(), nonempty_string() | ne_binary()) -> ne_binary().
resolve_uri(Raw, 'undefined') -> kz_term:to_binary(Raw);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(<<_/binary>> = RawPath, <<_/binary>> = Relative) ->
    kz_term:join_binary(
      resolve_uri_path(RawPath, Relative)
      ,<<"/">>
     );
resolve_uri(RawPath, Relative) ->
    resolve_uri(kz_term:to_binary(RawPath), kz_term:to_binary(Relative)).

-spec resolve_uri_path(ne_binary(), ne_binary()) -> ne_binaries().
resolve_uri_path(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, ['global'])),
    UrlTokens = binary:split(Relative, <<"/">>, ['global']),

    lists:reverse(
      lists:foldl(fun resolve_uri_fold/2, PathTokensRev, UrlTokens)
     ).

-spec resolve_uri_fold(ne_binary(), ne_binaries()) -> ne_binaries().
resolve_uri_fold(<<"..">>, []) -> [];
resolve_uri_fold(<<"..">>, [_ | PathTokens]) -> PathTokens;
resolve_uri_fold(<<".">>, PathTokens) -> PathTokens;
resolve_uri_fold(<<>>, PathTokens) -> PathTokens;
resolve_uri_fold(Segment, [<<>>|DirTokens]) ->
    [Segment|DirTokens];
resolve_uri_fold(Segment, [LastToken|DirTokens]=PathTokens) ->
    case filename:extension(LastToken) of
        <<>> ->
            %% no extension, append Segment to Tokens
            [Segment | PathTokens];
        _Ext ->
            %% Extension found, append Segment to DirTokens
            [Segment|DirTokens]
    end.

-spec uri(ne_binary(), ne_binaries()) -> ne_binary().
uri(BaseUrl, Tokens) ->
    [Pro, Url] = binary:split(BaseUrl, <<"://">>),
    Uri = filename:join([Url | Tokens]),
    <<Pro/binary, "://", Uri/binary>>.


-spec safe_urlencode(binary() | number()) -> iolist().
safe_urlencode(V) when is_binary(V)
                       orelse is_number(V) ->
    kz_http_util:urlencode(kz_term:to_binary(V)).
