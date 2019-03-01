%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
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
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Connection = kz_dataconfig:connection(),
    kz_dataconnections:add(Connection),
    lager:info("waiting for first connection...", []),
    kz_dataconnections:wait_for_connection(),
    #data_connection{tag=Tag} = Connection,
    Server = #{tag => Tag, server => kz_dataconnections:get_server(Tag)},
    kz_datamgr:init_dbs(Server),
    kzs_plan:init(),
    'ignore'.
