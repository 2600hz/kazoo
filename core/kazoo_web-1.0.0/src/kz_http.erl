%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc HTTP helper functions for Kazoo
%%%
%%% @contributors
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(kz_http).

-export([
    urldecode/1,
    parse_query_string/1,
    urlsplit/1
]).

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
    {<<>>,<<>>,<<>>,<<>>,<<>>}.


