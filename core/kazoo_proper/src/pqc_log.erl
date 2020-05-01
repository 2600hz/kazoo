%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_log).

-export([log_info/0]).

-spec log_info() -> [{atom(), iolist()}].
log_info() ->
    log_info(get('start_time')).
log_info('undefined') ->
    [];
log_info(StartTime) ->
    [{'elapsed', kz_term:to_list(kz_time:elapsed_ms(StartTime))}].
