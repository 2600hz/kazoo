-module(kazoo_translator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("kzt.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(_, _) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_translator_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) -> 'ok'.
