%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_util_test).

-include_lib("kazoo/include/kz_types.hrl").

-ifdef(PROPER).
- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% PROPER TESTING
-ifdef(PROPER).

prop_pretty_print_bytes() ->
    ?FORALL({T, G, M, K, B}
            ,{range(0,3), range(0,1023), range(0,1023), range(0,1023), range(0,1023)}
            ,begin
                 Bytes = (T * ?BYTES_T) + (G * ?BYTES_G) + (M * ?BYTES_M) + (K * ?BYTES_K) + B,
                 Expected = iolist_to_binary(
                              lists:reverse(
                                lists:foldl(fun({0, "B"}, "") ->
                                                    ["B", <<"0">>];
                                               ({0, _}, Acc) -> Acc;
                                               ({N, Unit}, Acc) -> [Unit, kz_term:to_binary(N) | Acc]
                                            end
                                           ,[]
                                           ,[{T, "T"}
                                            ,{G, "G"}
                                            ,{M, "M"}
                                            ,{K, "K"}
                                            ,{B, "B"}
                                            ])
                               )
                             ),
                 Result = kz_util:pretty_print_bytes(Bytes),
                 ?WHENFAIL(io:format("~pT ~pG ~pM ~pK ~pB (~pb): ~p =:= ~p~n", [T, G, M, K, B, Bytes, Result, Expected])
                          ,Result =:= Expected
                          )
             end).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.


pretty_print_bytes_test() ->
    Tests = [{0, <<"0B">>}
            ,{1, <<"1B">>}
            ,{2, <<"2B">>}

            ,{?BYTES_K-1, <<"1023B">>}
            ,{?BYTES_K, <<"1K">>}
            ,{?BYTES_K+1, <<"1K1B">>}

            ,{?BYTES_M-1, <<"1023K1023B">>}
            ,{?BYTES_M, <<"1M">>}
            ,{?BYTES_M+1, <<"1M1B">>}

            ,{?BYTES_G-1, <<"1023M1023K1023B">>}
            ,{?BYTES_G, <<"1G">>}
            ,{?BYTES_G+1, <<"1G1B">>}

            ,{?BYTES_T-1, <<"1023G1023M1023K1023B">>}
            ,{?BYTES_T, <<"1T">>}
            ,{?BYTES_T+1, <<"1T1B">>}
            ],
    [?assertEqual({Bytes, Formatted}, {Bytes, kz_util:pretty_print_bytes(Bytes)})
     || {Bytes, Formatted} <- Tests
    ].
