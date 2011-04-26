-module(media_importer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:set_env(sasl, errlog_type, silent),
    application:set_env(kernel, error_logger, silent),
    media_importer_sup:start_link().

stop(_State) ->
    ok.
