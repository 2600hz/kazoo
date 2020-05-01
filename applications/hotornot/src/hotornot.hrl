-ifndef(HOTORNOT_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/kzd_ratedeck.hrl").

-define(APP_NAME, <<"hotornot">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CACHE_NAME, 'hotornot_cache').

-type trunking_options() :: kz_term:ne_binaries().
-type prefix() :: string().
-type match_error() :: {'rebuilding', pid()} |
                       'not_found' |
                       'no_trie'.
-type match_return() :: {'error', match_error()} |
                        {'ok', {prefix(), kz_term:ne_binaries()}}.

-define(HOTORNOT_HRL, 'true').
-endif.
