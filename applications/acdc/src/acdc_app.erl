%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_app).

-behaviour(application).

-include("acdc.hrl").

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
    acdc_maintenance:register_views(),
    _ = kapps_maintenance:bind_and_register_views('acdc', 'acdc_maintenance', 'register_views'),
    _ = kapps_maintenance:bind({'refresh_account', <<"*">>}, 'acdc_maintenance', 'refresh_account'),
    acdc_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('register_views', 'acdc_maintenance', 'register_views'),
    _ = kapps_maintenance:unbind({'refresh_account', <<"*">>}, 'acdc_maintenance', 'refresh_account'),
    'ok'.
