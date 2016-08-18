-ifndef(WEBSEQ_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-type diagram_type() :: {'file', Filename::ne_binary()} |
                        {'file', Name::ne_binary(), Filename::ne_binary()} |
                        {'db', Database::ne_binary()} |
                        {'db', Name::ne_binary(), Database::ne_binary()}.

-type who() :: pid() | ne_binary().
-type what() :: ne_binary() | iolist().

-define(WEBSEQ_HRL, 'true').
-endif.
