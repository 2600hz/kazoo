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

-include_lib("callflow/src/callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {'ok', Call1} = cf_exe:get_call(Call),
    Skills = kz_json:merge_recursive(
               kapps_call:kvs_fetch('cf_agent_skills', kz_json:new(), Call1)
               ,Data
              ),
    cf_exe:set_call(kapps_call:kvs_store('cf_agent_skills', Skills, Call1)),
    cf_exe:continue(Call).
