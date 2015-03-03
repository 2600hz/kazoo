-module(erlydtl_contrib_humanize).

-export([intcomma/1]).

intcomma(Value) when is_integer(Value) ->
    intcomma(integer_to_list(Value));
intcomma(Value) ->
    ValueBin = iolist_to_binary(Value),
    intcomma(ValueBin, size(ValueBin) rem 3, <<>>).

intcomma(<<>>, _, Acc) ->
    Acc;
intcomma(<< C, Rest/bits >>, 0, <<>>) ->
    intcomma(Rest, 2, << C >>);
intcomma(<< C, Rest/bits >>, 0, Acc) ->
    intcomma(Rest, 2, << Acc/binary, $,, C >>);
intcomma(<< C, Rest/bits >>, N, Acc) ->
    intcomma(Rest, N - 1, << Acc/binary, C >>).
