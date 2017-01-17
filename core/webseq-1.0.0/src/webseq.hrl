-ifndef(WEBSEQ_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

%% {file Filename} |
%% {file, Name, Filename} |
%% {db, Database} |
%% {db, Name, Database}
-type diagram_type() :: {'file', ne_binary()} |
                        {'file', ne_binary(), ne_binary()} |
                        {'db', ne_binary()} |
                        {'db', ne_binary(), ne_binary()}.

-type who() :: pid() | ne_binary().
-type what() :: ne_binary() | iolist().

-define(WEBSEQ_HRL, 'true').
-endif.
