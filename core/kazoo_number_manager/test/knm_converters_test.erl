%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_converters_test).

-define(MODULE_TESTED, knm_converters).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% PROPER TESTING
-ifdef(PROPER).
%%
%%% 1000000000
%% (AAABBBCCCC, 1AAABBBCCCC) -> AAABBBCCCCCC.
prop_to_npan() ->
    ?FORALL(Number
            ,range(2002000000,19999999999)
            ,begin
                 BinNum = kz_term:to_binary(Number),
                 NPAN = ?MODULE_TESTED:to_npan(BinNum),
                 case byte_size(BinNum) of
                     11 -> BinNum =:= <<"1", NPAN/binary>>;
                     _ -> NPAN =:= BinNum
                 end
             end
           ).

%% (AAABBBCCCC, 1AAABBBCCCC) -> 1AAABBBCCCCCC.
prop_to_1npan() ->
    ?FORALL(Number
            ,range(2002000000,19999999999)
            ,begin
                 BinNum = kz_term:to_binary(Number),
                 OneNPAN = ?MODULE_TESTED:to_1npan(BinNum),
                 case byte_size(BinNum) of
                     11 -> OneNPAN =:= BinNum;
                     _ -> OneNPAN =:= <<"1", BinNum/binary>>
                 end
             end
           ).

%% (AAABBBCCCC, 1AAABBBCCCC) -> +1AAABBBCCCCCC.
prop_normalize() ->
    ?FORALL(Number
            ,range(2002000000,19999999999)
            ,begin
                 BinNum = kz_term:to_binary(Number),
                 E164 = ?MODULE_TESTED:normalize(BinNum),
                 case byte_size(BinNum) of
                     11 -> E164 =:= <<$+, BinNum/binary>>;
                     10 -> E164 =:= <<$+, $1, BinNum/binary>>;
                     _ -> E164 =:= BinNum
                 end
             end
           ).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{'max_shrinks', 0}]))
      ]}}.

-endif.

%% EUNIT TESTING
%%

normalize_test_() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"+12234567890">>,
    [?_assertEqual(?MODULE_TESTED:normalize(N), Ans) || N <- Ns].

to_npan_test_() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"2234567890">>,
    [?_assertEqual(?MODULE_TESTED:to_npan(N), Ans) || N <- Ns].

to_1npan_test_() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"12234567890">>,
    [?_assertEqual(?MODULE_TESTED:to_1npan(N), Ans) || N <- Ns].
