%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the winkstart application.

-module(winkstart_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for winkstart.
start(_Type, _StartArgs) ->
    winkstart_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for winkstart.
stop(_State) ->
    ok.
