%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_init).

-include("kazoo_auth.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = init(),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> any().
init() ->
    kz_auth_keys:public_key(?SYSTEM_KEY_ID).
