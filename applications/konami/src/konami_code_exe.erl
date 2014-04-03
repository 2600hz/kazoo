%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Execute a metaflow
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_code_exe).

-export([handle/2]).

-include("konami.hrl").

-spec handle(api_object(), whapps_call:call()) -> 'ok'.
handle('undefined', _Call) ->
    lager:debug("no metaflow to execute");
handle(Metaflow, Call) ->
    whapps_call:put_callid(Call),
    M = wh_json:get_value(<<"module">>, Metaflow),
    Data = wh_json:get_value(<<"data">>, Metaflow, wh_json:new()),

    try (wh_util:to_atom(<<"konami_", M/binary>>)):handle(Data, Call) of
        {'branch', ChildBranch, Call1} ->
            lager:debug("continuing to child metaflow from konami_~s", [M]),
            handle(find_child_metaflow(ChildBranch, Metaflow), Call1);
        {'continue', Call1} ->
            lager:debug("continuing to default child from konami_~s", [M]),
            handle(find_child_metaflow('undefined', Metaflow), Call1);
        _ -> lager:debug("finished handling metaflow for konami_~s", [M])
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to exe metaflow 'konami_~s': ~s: ~p", [M, _E, _R]),
            wh_util:log_stacktrace(ST)
    end.

-spec find_child_metaflow(api_binary(), wh_json:object()) -> api_object().
find_child_metaflow('undefined', Metaflow) ->
    wh_json:get_value([<<"children">>, <<"_">>], Metaflow);
find_child_metaflow(Child, Metaflow) ->
    case wh_json:get_value([<<"children">>, Child], Metaflow) of
        'undefined' -> find_child_metaflow('undefined', Metaflow);
        ChildFlow -> ChildFlow
    end.
