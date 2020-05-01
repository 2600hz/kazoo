%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_tests).

-export([start/0, start/1]).


-spec start() -> tuple().
start() ->
    start(100).

-spec start(pos_integer()) -> tuple().
start(N) ->
    start(N, N, 0, 0, []).

start(N, 0, CDR, ECDR, Raw) ->
    {{total, N}
    ,{cdr, CDR, CDR div N}
    ,{ecdr, ECDR, ECDR div N}
    ,{diff, CDR - ECDR, ECDR - CDR}
    ,{avg_diff, (CDR - ECDR) div N, (ECDR - CDR) div N}
    ,{raw, Raw}
    };
start(N, X, CDRTot, ECDRTot, Raw) ->
    StartKey = start_key(),
    EndKey = end_key(StartKey),

    {ECDR, _} = timer:tc(kz_datamgr, get_results, [<<"account%2F50%2F3d%2F96f14def94f7ce08cf3e1a025375">>, <<"ecdrs/crossbar_listing">>, [{<<"startkey">>,StartKey},{<<"endkey">>,EndKey}]]),

    {CDR,_} = timer:tc(kz_datamgr, get_results, [<<"account%2F50%2F3d%2F96f14def94f7ce08cf3e1a025375">>, <<"cdrs/crossbar_listing">>, [{<<"startkey">>,StartKey},{<<"endkey">>,EndKey}]]),

    start(N, X-1, CDRTot + CDR, ECDRTot + ECDR, [ [{range, EndKey - StartKey}, {run, X}] | Raw]).

start_key() ->
    %% 365 days * secs/day + start seconds - start somewhere within the year
    (rand:uniform(365) * 86400) + 63477725277.

end_key(Start) ->
    Start + (rand:uniform(30) * 86400). % up to 30 days in the future
