-ifndef(KZ_FIXTUREDB_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-type server_options() :: #{test_app => atom()
                           ,test_db => kz_term:ne_binary()
                           ,test_db_subdir => atom()
                           }.

-type server_map() :: #{url => kz_term:text()
                       ,options => server_options()
                       }.

-type db_map() :: #{server => server_map()
                   ,name => kz_term:ne_binary()
                   }.

-type fixture_errors() :: 'conflict' |
                          'db_not_found' |
                          'empty_doc_id' |
                          'invalid_view_name' |
                          'not_found' |
                          'timeout'.
-type fixture_error() :: {'error', fixture_errors()}.

-type doc_resp() :: {'ok', kz_json:object()} | fixture_error().
-type docs_resp() :: {'ok', kz_json:objects()} | fixture_error().

-define(DANGEROUS_VIEW_OPTS, ['endkey'
                             ,'group'
                             ,'group_level'
                             ,'key'
                             ,'keys'
                             ,'list'
                             ,'reduce'
                             ,'skip'
                             ,'startkey'
                             ]).

-define(KZ_FIXTUREDB_HRL, 'true').
-endif.
