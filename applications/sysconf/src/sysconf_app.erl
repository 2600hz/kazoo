%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(sysconf_app).

-behaviour(application).

-include("sysconf.hrl").

%% Application callbacks
-export([start/2
        ,stop/1
        ]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    sysconf_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_sysconf:declare_exchanges(),
    kapi_self:declare_exchanges().
