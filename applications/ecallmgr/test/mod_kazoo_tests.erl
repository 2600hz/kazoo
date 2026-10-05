-module(mod_kazoo_tests).

-include_lib("eunit/include/eunit.hrl").

atom_error_test_() ->
    [?_assertEqual({'error', Reason}, mod_kazoo:api_result('error', Reason))
     || Reason <- ['timeout', 'exception', 'baduuid']
    ].

binary_response_test_() ->
    [?_assertEqual({'error', <<"NO_ROUTE_DESTINATION">>}
                  ,mod_kazoo:api_result('error', <<" NO_ROUTE_DESTINATION\n">>))
    ,?_assertEqual({'ok', <<"call-uuid">>}
                  ,mod_kazoo:api_result('ok', <<" call-uuid\n">>))
    ,?_assertEqual({'ok', 'true'}, mod_kazoo:api_result('ok', <<"true\n">>))
    ,?_assertEqual({'ok', 'false'}, mod_kazoo:api_result('ok', <<"false\n">>))
    ,?_assertEqual({'ok', 42}, mod_kazoo:api_result('ok', <<"42\n">>))
    ,?_assertEqual({'error', 'failed'}, mod_kazoo:api_result('error', <<>>))
    ,?_assertEqual('ok', mod_kazoo:api_result('ok', <<>>))
    ,?_assertEqual('error', mod_kazoo:api_result('error', 'undefined'))
    ,?_assertEqual('ok', mod_kazoo:api_result('ok', 'undefined'))
    ].
