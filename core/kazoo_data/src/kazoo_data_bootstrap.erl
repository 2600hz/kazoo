%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_data_bootstrap).

-export([start_link/0]).

-include("kz_data.hrl").

%%------------------------------------------------------------------------------
%% @doc Bootstrap.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    #data_connection{tag=Tag} = Connection = kz_dataconfig:connection(),
    kz_dataconnections:add(Connection),

    lager:info("waiting for first connection..."),
    lager:debug("to ~p", [Connection]),

    kz_dataconnections:wait_for_connection(),

    Server = #{tag => Tag, server => kz_dataconnections:get_server(Tag)},
    kz_datamgr:init_dbs(Server),
    kzs_plan:init(),
    'ignore'.
