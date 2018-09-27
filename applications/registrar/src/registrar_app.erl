%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(registrar_app).

-behaviour(application).

-include("reg.hrl").

%% Application callbacks
-export([start/2, stop/1]).

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
    registrar_maintenance:register_views(),
    kapps_maintenance:bind_and_register_views('registrar', 'registrar_maintenance', 'register_views'),
    registrar_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_authn:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    kapi_self:declare_exchanges().
