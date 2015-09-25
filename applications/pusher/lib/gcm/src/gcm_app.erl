-module(gcm_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    gcm_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
