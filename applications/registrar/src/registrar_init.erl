%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Init to be done
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(registrar_init).

-export([start_link/0
        ,init/0
        ]).

-include("reg.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_process:spawn(fun init/0),
    'ignore'.

-spec init() -> any().
init() ->
    kz_log:put_callid(?MODULE),
    _ = kapps_maintenance:refresh(?KZ_SIP_DB),
    'ok'.
