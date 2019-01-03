%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_set).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {'ok', Call1} = cf_exe:get_call(Call),
    Skills = kz_json:merge(kapps_call:kvs_fetch('cf_agent_skills', kz_json:new(), Call1)
                          ,Data
                          ),
    cf_exe:set_call(kapps_call:kvs_store('cf_agent_skills', Skills, Call1)),
    cf_exe:continue(Call).
