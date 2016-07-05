%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_util_test).

-include_lib("eunit/include/eunit.hrl").

pretty_print_test_() ->
    [?_assertEqual(Result, knm_util:pretty_print(Format, Number))
     || {Number, Format, Result}
            <- [{<<"+14158867900">>, <<"SS(###) ### - *">>, <<"(415) 886 - 7900">>}
               ,{<<"+14158867900">>, <<"S\\SS(###\\#) ### - *\\*">>, <<"S(415#) 886 - 7900*">>}
               ,{<<"+14158867900">>, <<"*">>, <<"+14158867900">>}
               ]
    ].
