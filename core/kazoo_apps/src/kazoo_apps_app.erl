%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_app).
-behaviour(application).

-include("kazoo_apps.hrl").

-export([start/2, stop/1]).
-export([start/0]).


-spec start() -> {'ok', kz_term:atoms()}.
start() ->
    _ = io:setopts('user', [{'encoding', 'unicode'}]),
    'true' = does_system_have_network_subsystem(),
    {'ok', _Apps} = application:ensure_all_started(?APP).

%% Application callbacks

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kapps_maintenance:bind({'migrate', <<"4.0">>}, 'kazoo_voicemail_maintenance', 'migrate'),
    kazoo_apps_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    kapps_maintenance:unbind({'migrate', <<"4.0">>}, 'kazoo_voicemail_maintenance', 'migrate'),
    'ok'.

-spec does_system_have_network_subsystem() -> boolean().
does_system_have_network_subsystem() ->
    try kz_network_utils:default_binding_ip() of
        _ -> 'true'
    catch
        'throw':{'error', Reason} ->
            io:format("~n~nCRITICAL ERROR: ~s~n~n", [Reason]),
            throw({'error', Reason})
    end.
