%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_data_init).

-include_lib("kazoo/include/kz_types.hrl").

-export([start_link/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = declare_exchanges(),
    'ignore'.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures that all exchanges used are declared
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_conf:declare_exchanges(),
    kapi_self:declare_exchanges().
