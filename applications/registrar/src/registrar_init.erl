%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Init to be done
%%% @end
%%%-------------------------------------------------------------------
-module(registrar_init).

-export([start_link/0
         ,init/0
        ]).

-include("reg.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    wh_util:spawn(?MODULE, 'init', []),
    'ignore'.

-spec init() -> any().
init() ->
    wh_util:put_callid(?MODULE),
    whapps_maintenance:refresh(?WH_SIP_DB).
