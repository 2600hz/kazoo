%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(trunkstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,prep_stop/1
        ,stop/1
        ]).

-include("ts.hrl").

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    trunkstore_sup:start_link().

-spec prep_stop(any()) -> 'ok'.
prep_stop(_State) ->
    kz_nodes:unbind_for_pool_state('kz_amqp_sup', whereis('trunkstore_sup')),
    'ok'.

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    kapi_self:declare_exchanges().
