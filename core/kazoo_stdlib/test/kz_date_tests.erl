%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_date_tests).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

pad_month_test_() ->
    [?_assertEqual(<<"10">>, kz_date:pad_month(10))
    ,?_assertEqual(<<"10">>, kz_date:pad_month(<<"10">>))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(3))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(<<"3">>))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(<<"03">>))
    ].

iso8601_basic_test_() ->
    Tests = [{{2015,4,7}, <<"20150407">>}
            ,{{{2015,4,7},{0,0,0}}, <<"20150407">>}
            ,{{{2015,4,7},{1,3,2}}, <<"20150407">>}
            ,{{{2015,12,12},{12,13,12}}, <<"20151212">>}
            ,{63595733389, <<"20150408">>}
            ],
    [?_assertEqual(Expected, kz_date:to_iso8601(Date))
     || {Date, Expected} <- Tests
    ].

iso8601_extended_test_() ->
    Tests = [{{2015,4,7}, <<"2015-04-07">>}
            ,{{{2015,4,7},{0,0,0}}, <<"2015-04-07">>}
            ,{{{2015,4,7},{1,3,2}}, <<"2015-04-07">>}
            ,{{{2015,12,12},{12,13,12}}, <<"2015-12-12">>}
            ,{63595733389, <<"2015-04-08">>}
            ],
    [?_assertEqual(Expected, kz_date:to_iso8601_extended(Date))
     || {Date, Expected} <- Tests
    ].

%% Found during PropEr testing
kz_dates_test_() ->
    %% {FunkyDate, NormalizedDate}
    Tests = [{{1600, 1, 65}, {1600, 3, 5}}
            ,{{1601, 1, 65}, {1601, 3, 6}}
            ,{{0,12,32}, {1,1,1}}
            ],
    [?_assertEqual(Normalized, kz_date:normalize(Date))
     || {Date, Normalized} <- Tests
    ].

-ifdef(PROPER).
%% proper_test_() ->
%%     {"Runs " ?MODULE_STRING " PropEr tests"
%%     ,[{atom_to_list(F)
%%       ,fun () ->
%%                ?assert(proper:quickcheck(?MODULE:F(), [{'to_file', 'user'}
%%                                                       ,{'numtests', 500}
%%                                                       ]))
%%        end
%%       }
%%       || {F, 0} <- ?MODULE:module_info('exports'),
%%          F > 'prop_',
%%          F < 'prop`'
%%      ]
%%     }.

prop_normalize_identity() ->
    ?FORALL(NormalizedDate
           ,date_to_normalize()
           ,NormalizedDate =:= kz_date:normalize(NormalizedDate)
           ).

prop_normalize() ->
    ?FORALL({GregorianDate, MonthsBack}
           ,{safe_date(), range(1,13)}
           ,begin
                Date = funky_date(GregorianDate, MonthsBack),
                NormalizedDate = kz_date:normalize(Date),

                ?WHENFAIL(io:format("failed to normalize ~w to ~w: got ~w(~w)~n"
                                   ,[Date, GregorianDate, NormalizedDate, MonthsBack]
                                   )
                         ,GregorianDate =:= NormalizedDate
                         )
            end
           ).

safe_date() ->
    ?LET({Y, M}
        ,{kz_year(), kz_month()}
        ,{Y, M, range(1, kz_date:days_in_month(Y, M))}
        ).

funky_date(Date, 0) -> Date;
funky_date({0, 1, D}, _MonthsBack) -> {0, 1, D};
funky_date({0, M, D}, MonthsBack) ->
    funky_date({0, M-1, D+kz_date:days_in_month(0, M-1)}, MonthsBack-1);
funky_date({Y, 1, D}, MonthsBack) ->
    funky_date({Y-1, 12, D+kz_date:days_in_month(Y-1, 12)}, MonthsBack-1);
funky_date({Y, M, D}, MonthsBack) ->
    funky_date({Y, M-1, D+kz_date:days_in_month(Y, M-1)}, MonthsBack-1).

-endif.
