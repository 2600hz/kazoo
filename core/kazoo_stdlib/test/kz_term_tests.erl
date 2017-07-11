-module(kz_term_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef(PERF).
-define(REPEAT, 100000).

-define(BIN_INT, <<"1234567890">>).

horse_to_integer() ->
    horse:repeat(?REPEAT, kz_term:to_integer(?BIN_INT)).

to_integer_convoluted(Bin) ->
    list_to_integer(binary_to_list(Bin)).

horse_to_integer_convoluted() ->
    horse:repeat(?REPEAT, to_integer_convoluted(?BIN_INT)).

to_integer_straight(Bin) ->
    binary_to_integer(Bin).

horse_to_integer_straight() ->
    horse:repeat(?REPEAT, to_integer_straight(?BIN_INT)).

-endif.
