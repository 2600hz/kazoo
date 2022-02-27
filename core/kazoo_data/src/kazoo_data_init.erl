%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_data_init).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = declare_exchanges(),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_conf:declare_exchanges(),
    kapi_self:declare_exchanges().
