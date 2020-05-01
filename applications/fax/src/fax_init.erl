%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_init).

-include("fax.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = declare_exchanges(),
    Dispatch = cowboy_router:compile([
                                      %% :: {HostMatch, [{PathMatch, Constraints, Handler, Opts}]}
                                      {'_', [{<<"/fax/[...]">>, [], 'fax_file_proxy', []}]}
                                     ]),

    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 50),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    _ = start_listener(Workers, Dispatch, ?PORT),

    fax_maintenance:refresh_views(),
    _ = fax_ra:start(),
    'ignore'.

start_listener(Workers, Dispatch, Port) ->
    case cowboy:start_clear('fax_file'
                           ,#{'socket_opts' => [{'port', Port}]
                             ,'num_acceptors' => Workers
                             }
                           ,#{'env' => #{'dispatch' => Dispatch}})
    of
        {error,eaddrinuse} -> start_listener(Workers, Dispatch, Port+1);
        _Other -> _Other
    end.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_fax:declare_exchanges(),
    _ = kapi_conf:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    kapi_self:declare_exchanges().
