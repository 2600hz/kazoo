-module(blocktrans_parser).

-export([parse/1]).

parse(Tokens) ->
    parse(Tokens, []).

parse([], Acc) ->
    lists:reverse(Acc);
parse([{open_blocktrans, _, _}, {text, _, Text}, {close_blocktrans, _}|Rest], Acc) ->
    parse(Rest, [Text|Acc]);
parse([{text, _, _}|Rest], Acc) ->
    parse(Rest, Acc).
