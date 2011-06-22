-module(ecallmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Ref, Args), {Ref, {I, start_link, [{Ref, Args}]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ecallmgr:start_link().

stop(_State) ->
    ok.
