%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
