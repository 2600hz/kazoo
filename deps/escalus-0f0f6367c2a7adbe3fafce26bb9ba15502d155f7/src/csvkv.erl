%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module for parsing DIGEST-MD5 - style k1=v1,k2="v2" content
%%% @end
%%%===================================================================

-module(csvkv).
-export([parse/1, format/1, format/2]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec parse(binary()) -> [{binary(), binary()}].
parse(Text) ->
    do_parse(rev_lex(Text, []), []).

-spec format([{binary(), binary()}]) -> binary().
format(Items) ->
    format(Items,true).

-spec format([{binary(), binary()}], boolean()) -> binary().
format(Items, QMark) ->
    IOList = join([format_item(I, QMark) || I <- Items], $,),
    list_to_binary(IOList).

%%--------------------------------------------------------------------
%% Implementation
%%--------------------------------------------------------------------

%% note: lexemes are in reverse order
do_parse([{v, V}, {k, K} | Rest], Acc) ->
    do_parse(Rest, [{K, V} | Acc]);
do_parse([], Acc) ->
    Acc.

%% note: returns lexemes in reverse order
rev_lex(<<>>, Acc) ->
    Acc;
rev_lex(Text, Acc) ->
    rev_lex_key(Text, Acc).

rev_lex_key(Text, Acc) ->
    [Key, Rest] = binary:split(Text, <<"=">>),
    rev_lex_value(Rest, [{k, Key} | Acc]).

rev_lex_value(<<"\"", Text/binary>>, Acc) ->
    rev_lex_value_quoted(Text, <<>>, Acc);
rev_lex_value(Text, Acc) ->
    rev_lex_value_bare(Text, Acc).

rev_lex_value_quoted(<<"\\\"",T/binary>>, Val, Acc) ->
    rev_lex_value_quoted(T, <<Val/binary,"\"">>, Acc);
rev_lex_value_quoted(<<"\",",T/binary>>, Val, Acc) ->
    rev_lex(T, [{v, Val} | Acc]);
rev_lex_value_quoted(<<"\"",T/binary>>, Val, Acc) ->
    rev_lex(T, [{v, Val} | Acc]);
rev_lex_value_quoted(<<H,T/binary>>, Val, Acc) ->
    rev_lex_value_quoted(T, <<Val/binary,H>>, Acc).

rev_lex_value_bare(Text, Acc) ->
    case binary:split(Text, <<",">>) of
        [Val, Rest] ->
            rev_lex(Rest, [{v, Val} | Acc]);
        [Val] ->
            rev_lex(<<>>, [{v, Val} | Acc])
    end.

join([], _) ->
    [];
join(Items, Sep) ->
    tl(lists:append([[Sep, I] || I <- Items])).

format_item({Key, Val}, QMark) ->
    EscVal = re:replace(Val, "\"", "\\\"", [global]),
    case QMark of
        true ->
            [Key, $=, $", EscVal, $"];
        _ ->
            [Key, $=, EscVal]
    end.
