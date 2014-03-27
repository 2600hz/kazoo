%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Execute a metaflow
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_metaflow_exe).

-export([handle/2]).

-include("konami.hrl").

handle(Metaflow, Call) ->
    whapps_call:put_callid(Call),
    M = wh_json:get_value(<<"module">>, Metaflow),
    Data = wh_json:get_value(<<"data">>, Metaflow, wh_json:new()),

    try (wh_util:to_atom(<<"konami_", M/binary>>)):handle(Data, Call) of
        _ -> lager:debug("finished handling metaflow for ~s", [M])
    catch
        _E:_R ->
            lager:debug("failed to exe metaflow 'konami_~s': ~s: ~p", [M, _E, _R])
    end.
