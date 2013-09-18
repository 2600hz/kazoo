%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_set).

-export([handle/2]).

-include("../callflow.hrl").

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {'ok', Call1} = cf_exe:get_call(Call),
    Skills = wh_json:merge_recursive(
               whapps_call:kvs_fetch('cf_agent_skills', wh_json:new(), Call1)
               ,Data
              ),
    cf_exe:set_call(whapps_call:kvs_store('cf_agent_skills', Skills, Call1)),
    cf_exe:continue(Call).
