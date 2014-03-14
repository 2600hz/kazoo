-module(kazoo_translator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("kzt.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(term(), term()) -> startlink_ret().
start(_StartType, _StartArgs) ->
    kazoo_translator_sup:start_link().

-spec stop(term()) -> 'ok'.
stop(_State) -> 'ok'.
