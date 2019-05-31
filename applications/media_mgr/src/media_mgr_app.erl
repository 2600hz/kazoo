%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_mgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").


%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    media_mgr_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = media_proxy:stop(),
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_media:declare_exchanges(),
    kapi_self:declare_exchanges().
