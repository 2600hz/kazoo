-ifndef(WEBSEQ_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type diagram_type() :: {'file', Filename::kz_term:ne_binary()} |
                        {'file', Name::kz_term:ne_binary(), Filename::kz_term:ne_binary()} |
                        {'db', Database::kz_term:ne_binary()} |
                        {'db', Name::kz_term:ne_binary(), Database::kz_term:ne_binary()}.

-type who() :: pid() | kz_term:ne_binary().
-type what() :: kz_term:ne_binary() | iolist().

-define(WEBSEQ_HRL, 'true').
-endif.
