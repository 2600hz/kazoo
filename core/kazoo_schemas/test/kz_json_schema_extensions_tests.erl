-module(kz_json_schema_extensions_tests).

-include_lib("eunit/include/eunit.hrl").

find_invalid_regexps_test_() ->
    HasInvRegexps = [<<"should@work.com">>, <<"this_one@too.com">>, <<"*@fail.com">>],
    ValidRegexps = [<<"should@work.com">>, <<"this_one@too.com">>, <<"another@working.com">>],

    [?_assertEqual(kz_json_schema_extensions:find_invalid_regexps(HasInvRegexps), [<<"*@fail.com">>])
    ,?_assertEqual(kz_json_schema_extensions:find_invalid_regexps(ValidRegexps), [])
    ,?_assertEqual(kz_json_schema_extensions:find_invalid_regexps([]), [])
    ].
