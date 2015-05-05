%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_util_test).

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
                 BinNum = wh_util:to_binary(Number),
                 NPAN = wnm_util:to_npan(BinNum),
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
                 BinNum = wh_util:to_binary(Number),
                 OneNPAN = wnm_util:to_1npan(BinNum),
                 case byte_size(BinNum) of
                     11 -> OneNPAN =:= BinNum;
                     _ -> OneNPAN =:= <<"1", BinNum/binary>>
                 end
             end
           ).

%% (AAABBBCCCC, 1AAABBBCCCC) -> +1AAABBBCCCCCC.
prop_to_e164() ->
    ?FORALL(Number
            ,range(2002000000,19999999999)
            ,begin
                 BinNum = wh_util:to_binary(Number),
                 E164 = wnm_util:to_e164(BinNum),
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

to_e164_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"+12234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(wnm_util:to_e164(N), Ans) end, Ns).

to_npan_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"2234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(wnm_util:to_npan(N), Ans) end, Ns).

to_1npan_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"12234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(wnm_util:to_1npan(N), Ans) end, Ns).
