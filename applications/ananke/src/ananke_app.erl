%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_app).
-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = kapi_self:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    ananke_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) -> 'ok'.
