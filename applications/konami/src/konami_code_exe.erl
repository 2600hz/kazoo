%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Execute a metaflow
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_code_exe).

-export([handle/2]).

-include("konami.hrl").

-spec handle(kz_term:api_object(), kapps_call:call()) -> 'ok'.
handle('undefined', _Call) ->
    lager:debug("no metaflow to execute");
handle(Metaflow, Call) ->
    kapps_call:put_callid(Call),
    M = kz_json:get_ne_binary_value(<<"module">>, Metaflow),
    Data = kz_json:get_json_value(<<"data">>, Metaflow, kz_json:new()),

    try (kz_term:to_atom(<<"konami_", M/binary>>)):handle(Data, Call) of
        {'branch', ChildBranch, Call1} ->
            lager:debug("continuing to child metaflow from konami_~s", [M]),
            handle(find_child_metaflow(ChildBranch, Metaflow), Call1);
        {'continue', Call1} ->
            lager:debug("continuing to default child from konami_~s", [M]),
            handle(find_child_metaflow('undefined', Metaflow), Call1);
        {'stop', _Call1} ->
            lager:debug("finished metaflow konami_~s", [M]);
        'ok' -> 'ok';
        _Other ->
            lager:debug("finished handling metaflow for konami_~s: ~p", [M, _Other])
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("failed to exe metaflow 'konami_~s': ~s: ~p", [M, _E, _R]),
        kz_log:log_stacktrace(ST)
        end.

-spec find_child_metaflow(kz_term:api_binary(), kz_json:object()) -> kz_term:api_object().
find_child_metaflow('undefined', Metaflow) ->
    kz_json:get_value([<<"children">>, <<"_">>], Metaflow);
find_child_metaflow(Child, Metaflow) ->
    case kz_json:get_value([<<"children">>, Child], Metaflow) of
        'undefined' -> find_child_metaflow('undefined', Metaflow);
        ChildFlow -> ChildFlow
    end.
