%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handles setup/initialization of Konami
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_init).

-export([start_link/0
        ,init/0
        ]).

-include("konami.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    _ = kz_process:spawn(fun init/0),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    %% Preload configs
    _Ns = konami_config:numbers(),
    _Ps = konami_config:patterns(),
    _Bk = konami_config:binding_digit(),
    _T = konami_config:timeout(),

    lager:debug("default numbers: ~s", [kz_json:encode(_Ns)]),
    lager:debug("default patterns: ~s", [kz_json:encode(_Ps)]),
    lager:debug("default binding digit: '~s'", [_Bk]),
    lager:debug("default timout: ~b", [_T]).
