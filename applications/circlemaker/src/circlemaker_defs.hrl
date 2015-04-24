-ifndef(CIRCLEMAKER_DEFS_HRL).

% default AAA document (it's not empty but not enabled, so this kind of the document description is helpful for editing)
-define(DEFAULT_AAA_DOCUMENT,
    [
        {<<"aaa_mode">>, <<"off">>},
        {<<"servers">>, wh_json:from_list([
            {<<"enabled">>, 'false'},
            {<<"name">>, <<"unique_server_name">>},
            {<<"address">>, <<"127.0.0.1">>},
            {<<"port">>, 1812},
            {<<"secret">>, <<"example_secret">>},
            {<<"aaa_engine">>, <<"radius">>},
            {<<"dicts">>, [<<"dictionary_3gpp">>, <<"dictionary">>]},
            {<<"avp">>, <<"strict">>},
            {<<"retries">>, 3},
            {<<"timeout">>, 5000}
        ])},
        {<<"authentication">>, [<<"unique_server_name">>]},
        {<<"authorization">>, [<<"unique_server_name">>]},
        {<<"accounting">>, [<<"unique_server_name">>]}
    ]
).

-define(CIRCLEMAKER_DEFS_HRL, 'true').
-endif.
