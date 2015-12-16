%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_ets).

-include("fmc.hrl").

-define(ETS_FMC_CALLS, ?MODULE).

%% API
-export([create_table/0, delete_table/0, put/3, delete/1, get/1]).

create_table() ->
    lager:debug("FMC ETS table created"),
    ets:new(?ETS_FMC_CALLS, [named_table, public]).

delete_table() ->
    lager:debug("FMC ETS table deleted"),
    ets:delete(?ETS_FMC_CALLS).

put(MsgId, CallID, Call) ->
    lager:debug("FMC ETS record ~p inserted", [{MsgId, CallID, Call}]),
    ets:insert(?ETS_FMC_CALLS, {MsgId, CallID, Call}).

get(MsgId) ->
    Res = ets:lookup(?ETS_FMC_CALLS, MsgId),
    lager:debug("FMC ETS record retrieved by key ~p. Result is ~p", [MsgId, Res]),
    Res.

delete(MsgId) ->
    lager:debug("FMC ETS record deleted by key ~p", [MsgId]),
    ets:delete(?ETS_FMC_CALLS, MsgId).
